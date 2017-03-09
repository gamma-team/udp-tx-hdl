-- UDP streaming transmitter
--
-- TODO: Add flushing logic, since it will pause when no data is flowing in.
-- TODO: Error handling
-- Note: By design, the FIFOs should never fill up
--
-- Copyright 2017 Patrick Gauvin
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY udp_tx IS
    GENERIC (
        -- Input and output bus width in bytes, must be a power of 2
        width : POSITIVE := 8
    );
    PORT (
        -- All ports are assumed to be synchronous with Clk
        Clk : IN STD_LOGIC;
        Rst : IN STD_LOGIC;
        -- Data input bus for UDP payload data from the application layer.
        -- Byte offsets (all integer types are big endian):
        -- 0: Source IP address (4 bytes)
        -- 4: Destination IP address (4 bytes)
        -- 8: Source port (2 bytes)
        -- 10: Destination port (2 bytes)
        -- 12: Data for UDP datagram data section
        Data_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        -- Assertion indicates which Data_in bytes are valid.
        Data_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        -- Asserted when the first data is available on Data_in
        Data_in_start : IN STD_LOGIC;
        -- Asserted when the last valid data is available on Data_in.
        Data_in_end : IN STD_LOGIC;

        -- UDP datagram output bus to the IP layer.
        -- Byte offsets (all integer types are big endian):
        -- 0: Source IP address
        -- 4: Destination IP address
        -- 8: Protocol
        -- 9: UDP datagram
        Data_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        -- Assertion indicates which Data_out bytes are valid.
        Data_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        -- Asserted when the first data is available on Data_out.
        Data_out_start : OUT STD_LOGIC;
        -- Asserted when the last data is available on Data_out.
        Data_out_end : OUT STD_LOGIC;
        -- Indicate that there has been an error in the current datagram.
        -- Data_out should be ignored until the next Data_out_start assertion.
        Data_out_err : OUT STD_LOGIC
    );
END ENTITY;

ARCHITECTURE normal OF udp_tx IS
    COMPONENT fifo IS
        GENERIC (
            width : POSITIVE;
            depth : POSITIVE
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rst : IN STD_LOGIC;
            Read : IN STD_LOGIC;
            Write : IN STD_LOGIC;
            D : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Empty : OUT STD_LOGIC;
            Full : OUT STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT shift_reg IS
        GENERIC (
            width : POSITIVE;
            depth : POSITIVE
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rst : IN STD_LOGIC;
            Ena : IN STD_LOGIC;
            D : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Q : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT shift_ram_abstract IS
        GENERIC (
            width : POSITIVE;
            depth : POSITIVE
        );
        PORT (
            Clk : IN STD_LOGIC;
            Ce : IN STD_LOGIC;
            D : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Q : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
        );
    END COMPONENT;


    -- 17 bit counters allow for the extra header information (not UDP header
    -- information) in the data streams. UDP datagrams are 65536 bytes,
    -- maximum.
    CONSTANT COUNTER_WIDTH : NATURAL := 17;
    CONSTANT DATA_IN_OFF_IP_SRC : NATURAL := 0;
    CONSTANT DATA_IN_OFF_IP_DST : NATURAL := DATA_IN_OFF_IP_SRC + 4;
    CONSTANT DATA_IN_OFF_UDP_PORT_SRC : NATURAL := DATA_IN_OFF_IP_DST + 4;
    CONSTANT DATA_IN_OFF_UDP_PORT_DST : NATURAL
        := DATA_IN_OFF_UDP_PORT_SRC + 2;
    CONSTANT DATA_OUT_OFF_IP_SRC : NATURAL := 0;
    CONSTANT DATA_OUT_OFF_IP_DST : NATURAL := DATA_OUT_OFF_IP_SRC + 4;
    CONSTANT DATA_OUT_OFF_PROTO : NATURAL := DATA_OUT_OFF_IP_DST + 4;
    CONSTANT DATA_OUT_OFF_UDP_PORT_SRC : NATURAL := DATA_OUT_OFF_PROTO + 1;
    CONSTANT DATA_OUT_OFF_UDP_PORT_DST : NATURAL
        := DATA_OUT_OFF_UDP_PORT_SRC + 2;
    CONSTANT DATA_OUT_OFF_UDP_LEN : NATURAL := DATA_OUT_OFF_UDP_PORT_DST + 2;
    CONSTANT DATA_OUT_OFF_UDP_CHK : NATURAL := DATA_OUT_OFF_UDP_LEN + 2;
    CONSTANT SHIFT_RAM_DEPTH : NATURAL := 65536 / width;
    CONSTANT UDP_PROTO : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"11";

    TYPE DATA_BUS IS ARRAY (width - 1 DOWNTO 0)
        OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    TYPE BOOLEAN_VECTOR IS ARRAY (NATURAL RANGE <>) OF BOOLEAN;

    SIGNAL data_in_sig : DATA_BUS;

    SIGNAL p0_data_in : DATA_BUS;
    SIGNAL p0_data_in_valid
        : STD_LOGIC_VECTOR(p0_data_in'length - 1 DOWNTO 0);
    SIGNAL p0_data_in_start : STD_LOGIC;
    SIGNAL p0_data_in_end : STD_LOGIC;
    SIGNAL p0_data_in_err : STD_LOGIC;
    SIGNAL p0_len_read : UNSIGNED(COUNTER_WIDTH - 1 DOWNTO 0);
    SIGNAL p0_addr_src_valid : BOOLEAN;
    SIGNAL p0_addr_dst_valid : BOOLEAN;
    SIGNAL p0_udp_port_src_valid : BOOLEAN;
    SIGNAL p0_udp_port_dst_valid : BOOLEAN;
    SIGNAL p0_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p0_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p0_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p0_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);

    SIGNAL p1_data_in : DATA_BUS;
    SIGNAL p1_data_in_valid
        : STD_LOGIC_VECTOR(p1_data_in'length - 1 DOWNTO 0);
    SIGNAL p1_data_in_start : STD_LOGIC;
    SIGNAL p1_data_in_end : STD_LOGIC;
    SIGNAL p1_data_in_err : STD_LOGIC;
    SIGNAL p1_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p1_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p1_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p1_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p1_chk_accum : UNSIGNED(31 DOWNTO 0);
    SIGNAL p1_udp_len : UNSIGNED(15 DOWNTO 0);

    SIGNAL p2_data_in : DATA_BUS;
    SIGNAL p2_data_in_valid
        : STD_LOGIC_VECTOR(p2_data_in'length - 1 DOWNTO 0);
    SIGNAL p2_data_in_start : STD_LOGIC;
    SIGNAL p2_data_in_end : STD_LOGIC;
    SIGNAL p2_data_in_err : STD_LOGIC;
    SIGNAL p2_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2_chk_accum : UNSIGNED(31 DOWNTO 0);
    SIGNAL p2_chk_addend : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2_internal_off : UNSIGNED(COUNTER_WIDTH - 1 DOWNTO 0);

    SIGNAL p3_data_in : DATA_BUS;
    SIGNAL p3_data_in_valid
        : STD_LOGIC_VECTOR(p3_data_in'length - 1 DOWNTO 0);
    SIGNAL p3_data_in_start : STD_LOGIC;
    SIGNAL p3_data_in_end : STD_LOGIC;
    SIGNAL p3_data_in_err : STD_LOGIC;
    SIGNAL p3_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p3_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p3_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p3_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p3_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p3_udp_chk : UNSIGNED(15 DOWNTO 0);

    SIGNAL p4_data_out : DATA_BUS;
    SIGNAL p4_data_out_valid : BOOLEAN_VECTOR(p4_data_out'length - 1 DOWNTO 0);
    SIGNAL p4_data_out_start : STD_LOGIC;
    SIGNAL p4_data_out_end : STD_LOGIC;
    SIGNAL p4_data_out_err : STD_LOGIC;
    SIGNAL p4_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p4_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p4_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p4_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p4_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p4_udp_chk : UNSIGNED(15 DOWNTO 0);
    SIGNAL p4_fifo_hdr_write : STD_LOGIC;

    SIGNAL p5_data_out : DATA_BUS;
    SIGNAL p5_data_out_valid : BOOLEAN_VECTOR(p5_data_out'length - 1 DOWNTO 0);
    SIGNAL p5_data_out_start : STD_LOGIC;
    SIGNAL p5_data_out_end : STD_LOGIC;
    SIGNAL p5_data_out_err : STD_LOGIC;
    SIGNAL p5_data_buf : DATA_BUS;
    SIGNAL p5_data_buf_valid : BOOLEAN_VECTOR(p5_data_out'length - 1 DOWNTO 0);
    SIGNAL p5_data_buf_start : STD_LOGIC;
    SIGNAL p5_data_buf_end : STD_LOGIC;

    SIGNAL p6_data_out : DATA_BUS;
    SIGNAL p6_data_out_start : STD_LOGIC;
    SIGNAL p6_data_out_end : STD_LOGIC;
    SIGNAL p6_data_out_err : STD_LOGIC;
    SIGNAL p6_shift_ena : STD_LOGIC;

    SIGNAL shift_reg_ena : STD_LOGIC;
    SIGNAL shift_reg_d : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL shift_reg_q : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL shift_reg_q_start : STD_LOGIC;
    SIGNAL shift_reg_q_end : STD_LOGIC;
    SIGNAL shift_reg_q_err : STD_LOGIC;

    SIGNAL shift_ram_ena : STD_LOGIC;
    SIGNAL shift_ram_d : STD_LOGIC_VECTOR(Data_in'length - 1 DOWNTO 0);
    SIGNAL shift_ram_q : STD_LOGIC_VECTOR(shift_ram_d'length - 1 DOWNTO 0);
    SIGNAL shift_ram_q_data : DATA_BUS;

    SIGNAL fifo_d
        : STD_LOGIC_VECTOR(p4_udp_chk'length + p4_udp_len'length
        + p4_addr_src'length + p4_addr_dst'length + p4_udp_port_src'length
        + p4_udp_port_dst'length - 1 DOWNTO 0);
    SIGNAL fifo_q : STD_LOGIC_VECTOR(fifo_d'length - 1 DOWNTO 0);
    SIGNAL fifo_write : STD_LOGIC;
    SIGNAL fifo_read : STD_LOGIC;
    SIGNAL fifo_empty : STD_LOGIC;
    SIGNAL fifo_full : STD_LOGIC;
    SIGNAL fifo_q_udp_chk : STD_LOGIC_VECTOR(p4_udp_chk'length - 1 DOWNTO 0);
    SIGNAL fifo_q_udp_len : STD_LOGIC_VECTOR(p4_udp_len'length - 1 DOWNTO 0);
    SIGNAL fifo_q_addr_src
        : STD_LOGIC_VECTOR(p4_addr_src'length - 1 DOWNTO 0);
    SIGNAL fifo_q_addr_dst
        : STD_LOGIC_VECTOR(p4_addr_dst'length - 1 DOWNTO 0);
    SIGNAL fifo_q_udp_port_src
        : STD_LOGIC_VECTOR(p4_udp_port_src'length - 1 DOWNTO 0);
    SIGNAL fifo_q_udp_port_dst
        : STD_LOGIC_VECTOR(p4_udp_port_dst'length - 1 DOWNTO 0);

    SIGNAL fifo_data_d
        : STD_LOGIC_VECTOR(1 + shift_ram_q'length - 1 DOWNTO 0);
    SIGNAL fifo_data_q : STD_LOGIC_VECTOR(fifo_data_d'length - 1 DOWNTO 0);
    SIGNAL fifo_data_write : STD_LOGIC;
    SIGNAL fifo_data_read : STD_LOGIC;
    SIGNAL fifo_data_empty : STD_LOGIC;
    SIGNAL fifo_data_full : STD_LOGIC;
    SIGNAL fifo_data_q_data : DATA_BUS;
    SIGNAL fifo_data_q_end : STD_LOGIC;

    SIGNAL op0_data : DATA_BUS;
    SIGNAL op0_valid : STD_LOGIC_VECTOR(Data_out_valid'length - 1 DOWNTO 0);
    SIGNAL op0_start : STD_LOGIC;
    SIGNAL op0_end : STD_LOGIC;
    SIGNAL op0_err : STD_LOGIC;
BEGIN
    -- Input signal wiring
    gen_in_data: FOR i IN 0 TO width - 1 GENERATE
        data_in_sig(i) <= Data_in((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;

    -- So that the main data buffer can be fixed length, data must be packed
    -- before being placed into it. Otherwise the buffer needs to be expanded
    -- to allow for invalid bytes, which is undesirable. As a side effect,
    -- valid lines do not need to be buffered, and the length will determine
    -- the invalid bytes in the last transfer. However, this means that the
    -- shift register must be selectively enabled based on the packing
    -- process, so extra logic will be needed to move data through the buffers
    -- when no new packet is being ingested.
    --
    -- Header information is placed in a FIFO, so that it will "skip" ahead of
    -- the main data buffer. The FIFO will need to be large enough to allow
    -- for the smallest size packets being spammed consecutively.

    -- Input pipeline
    PROCESS(Clk)
        FUNCTION all_true(bools : BOOLEAN_VECTOR(width - 1 DOWNTO 0))
            RETURN BOOLEAN IS
            VARIABLE b : BOOLEAN;
        BEGIN
            b := true;
            FOR i IN INTEGER RANGE bools'length - 1 DOWNTO 0 LOOP
                b := b AND bools(i);
            END LOOP;
            RETURN b;
        END FUNCTION;

        VARIABLE p0_len_read_var : UNSIGNED(p0_len_read'length - 1 DOWNTO 0);
        VARIABLE p1_chk_accum_var
            : UNSIGNED(p1_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p2_internal_off_var
            : UNSIGNED(p2_internal_off'length - 1 DOWNTO 0);
        VARIABLE p2_chk_addend_var
            : UNSIGNED(p2_chk_addend'length - 1 DOWNTO 0);
        VARIABLE p2_chk_accum_var
            : UNSIGNED(p2_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p4_j : NATURAL;
        VARIABLE p5_data_buf_valid_var
            : BOOLEAN_VECTOR(p5_data_buf_valid'length - 1 DOWNTO 0);
        VARIABLE p5_data_out_valid_var
            : BOOLEAN_VECTOR(p5_data_out_valid'length - 1 DOWNTO 0);
        VARIABLE p5_j : NATURAL;
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                shift_ram_d <= (OTHERS => '0');
                shift_ram_q <= (OTHERS => '0');
            ELSE
                --
                -- Stage 0: Byte decoding
                --
                p0_data_in <= data_in_sig;
                p0_data_in_valid <= Data_in_valid;
                p0_data_in_start <= Data_in_start;
                p0_data_in_end <= Data_in_end;
                p0_data_in_err <= '0';

                p0_len_read_var := p0_len_read;
                IF Data_in_start = '1' THEN
                    p0_len_read_var := (OTHERS => '0');
                END IF;
                p0_addr_src_valid <= false;
                p0_addr_dst_valid <= false;
                p0_udp_port_src_valid <= false;
                p0_udp_port_dst_valid <= false;
                FOR i IN 0 TO width - 1 LOOP
                    IF Data_in_valid(i) = '1' THEN
                        -- This case statement is quite large, could be split
                        -- into multiple stages.
                        p0_data_in_valid(i) <= '0';
                        CASE TO_INTEGER(p0_len_read_var) IS
                            WHEN DATA_IN_OFF_IP_SRC =>
                                p0_addr_src(7 DOWNTO 0) <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_SRC + 1 =>
                                p0_addr_src(15 DOWNTO 8)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_SRC + 2 =>
                                p0_addr_src(23 DOWNTO 16)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_SRC + 3 =>
                                p0_addr_src(31 DOWNTO 24)
                                    <= data_in_sig(i);
                                p0_addr_src_valid <= true;
                            WHEN DATA_IN_OFF_IP_DST =>
                                p0_addr_dst(7 DOWNTO 0) <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_DST + 1 =>
                                p0_addr_dst(15 DOWNTO 8)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_DST + 2 =>
                                p0_addr_dst(23 DOWNTO 16)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_IP_DST + 3 =>
                                p0_addr_dst(31 DOWNTO 24)
                                    <= data_in_sig(i);
                                p0_addr_dst_valid <= true;
                            WHEN DATA_IN_OFF_UDP_PORT_SRC =>
                                p0_udp_port_src(7 DOWNTO 0)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_UDP_PORT_SRC + 1 =>
                                p0_udp_port_src(15 DOWNTO 8)
                                    <= data_in_sig(i);
                                p0_udp_port_src_valid <= true;
                            WHEN DATA_IN_OFF_UDP_PORT_DST =>
                                p0_udp_port_dst(7 DOWNTO 0)
                                    <= data_in_sig(i);
                            WHEN DATA_IN_OFF_UDP_PORT_DST + 1 =>
                                p0_udp_port_dst(15 DOWNTO 8)
                                    <= data_in_sig(i);
                                p0_udp_port_dst_valid <= true;
                            WHEN OTHERS =>
                                p0_data_in_valid(i) <= '1';
                        END CASE;
                        p0_len_read_var := p0_len_read_var + 1;
                    END IF;
                END LOOP;
                p0_len_read <= p0_len_read_var;

                --
                -- Stage 1: UDP pseudo and normal header checksumming
                --
                p1_data_in <= p0_data_in;
                p1_data_in_valid <= p0_data_in_valid;
                p1_data_in_start <= p0_data_in_start;
                p1_data_in_end <= p0_data_in_end;
                p1_data_in_err <= p0_data_in_err;
                p1_addr_src <= p0_addr_src;
                p1_addr_dst <= p0_addr_dst;
                p1_udp_port_src <= p0_udp_port_src;
                p1_udp_port_dst <= p0_udp_port_dst;

                -- The checksum field is 0 when calculating to fill the
                -- checksum field, so it is omitted.
                p1_chk_accum_var := p1_chk_accum;
                IF p0_data_in_start = '1' THEN
                    p1_chk_accum_var := (OTHERS => '0');
                    p1_chk_accum_var(7 DOWNTO 0) := UNSIGNED(UDP_PROTO);
                END IF;
                IF p0_addr_src_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_src(15 DOWNTO 0));
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_src(31 DOWNTO 16));
                END IF;
                IF p0_addr_dst_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_dst(15 DOWNTO 0));
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_dst(31 DOWNTO 16));
                END IF;
                IF p0_udp_port_src_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_udp_port_src);
                END IF;
                IF p0_udp_port_dst_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_udp_port_dst);
                END IF;
                -- Length intentionally added twice, it is part of the pseudo
                -- and real UDP header.
                IF p0_data_in_end = '1' THEN
                    p1_chk_accum_var := p1_chk_accum_var + p0_len_read;
                    p1_chk_accum_var := p1_chk_accum_var + p0_len_read;
                END IF;
                p1_chk_accum <= p1_chk_accum_var;
                -- Calculate length for UDP header (subtract input stream
                -- header)
                p1_udp_len <= p0_len_read - 12;

                --
                -- Stage 2: Normal checksumming
                --
                p2_data_in <= p1_data_in;
                p2_data_in_valid <= p1_data_in_valid;
                p2_data_in_start <= p1_data_in_start;
                p2_data_in_end <= p1_data_in_end;
                p2_data_in_err <= p1_data_in_err;
                p2_addr_src <= p3_addr_src;
                p2_addr_dst <= p3_addr_dst;
                p2_udp_port_src <= p3_udp_port_src;
                p2_udp_port_dst <= p3_udp_port_dst;
                p2_udp_len <= p3_udp_len;

                p2_internal_off_var := p2_internal_off;
                p2_chk_addend_var := p2_chk_addend;
                p2_chk_accum_var := p1_chk_accum;
                IF p1_data_in_start = '1' THEN
                    p2_internal_off_var := (OTHERS => '0');
                    p2_chk_addend_var := (OTHERS => '0');
                    p2_chk_accum_var := (OTHERS => '0');
                END IF;
                -- Note: If this is too slow, split into stages that handle
                -- ranges of byte enables
                FOR i IN 0 TO width - 1 LOOP
                    IF p1_data_in_valid(i) = '1' THEN
                        IF 0 = p2_internal_off_var MOD 2 THEN
                            p2_chk_addend_var(7 DOWNTO 0)
                                := UNSIGNED(p1_data_in(i));
                        ELSE
                            p2_chk_addend_var(15 DOWNTO 8)
                                := UNSIGNED(p1_data_in(i));
                            p2_chk_accum_var := p2_chk_accum_var
                                + p2_chk_addend_var;
                        END IF;
                        p2_internal_off_var := p2_internal_off_var + 1;
                    END IF;
                END LOOP;
                IF p1_data_in_end = '1' THEN
                    -- account for non-word-aligned data length
                    IF 1 = p2_internal_off_var MOD 2 THEN
                        p2_chk_addend_var(15 DOWNTO 8) := (OTHERS => '0');
                        p2_chk_accum_var := p2_chk_accum_var
                            + p2_chk_addend_var;
                    END IF;
                END IF;
                p2_chk_accum <= p2_chk_accum_var;
                p2_chk_addend <= p2_chk_addend_var;
                p2_internal_off <= p2_internal_off_var;

                --
                -- Stage 3: Finish checksum
                --
                p3_data_in <= p2_data_in;
                p3_data_in_valid <= p2_data_in_valid;
                p3_data_in_start <= p2_data_in_start;
                p3_data_in_end <= p2_data_in_end;
                p3_data_in_err <= p2_data_in_err;
                p3_addr_src <= p2_addr_src;
                p3_addr_dst <= p2_addr_dst;
                p3_udp_port_src <= p2_udp_port_src;
                p3_udp_port_dst <= p2_udp_port_dst;
                p3_udp_len <= p2_udp_len;

                p3_udp_chk <= p2_chk_accum(31 DOWNTO 16)
                    + p2_chk_accum(15 DOWNTO 0);

                --
                -- Stage 4: Prepare data for shift-ram step 1 (compact data,
                -- technically unnecessary because of stage 5's design, remove
                -- later, or update stage 5 to take advantage of this)
                --
                p4_addr_src <= p3_addr_src;
                p4_addr_dst <= p3_addr_dst;
                p4_udp_port_src <= p3_udp_port_src;
                p4_udp_port_dst <= p3_udp_port_dst;
                p4_udp_len <= p3_udp_len;
                p4_udp_chk <= p3_udp_chk;
                p4_data_out_start <= p3_data_in_start;
                p4_data_out_end <= p3_data_in_end;
                p4_data_out_err <= p3_data_in_err;

                p4_j := 0;
                p4_data_out_valid <= (OTHERS => false);
                FOR i IN INTEGER RANGE 0 TO p3_data_in'length - 1 LOOP
                    IF p3_data_in_valid(i) = '1' THEN
                        p4_data_out(p4_j) <= p3_data_in(i);
                        p4_data_out_valid(p4_j) <= true;
                        p4_j := p4_j + 1;
                    END  IF;
                END LOOP;
                -- Push header information if data flow is done
                p4_fifo_hdr_write <= '0';
                IF p3_data_in_end = '1' THEN
                    p4_fifo_hdr_write <= '1';
                END IF;

                --
                -- Stage 5: Fill output data, buffer extra data
                -- Note: This stage will probably have a large combinatorial
                -- delay, especially at larger data bus widths.
                -- TODO: Double-check that "end" and "start" conditions are
                -- handled correctly, probably need a testbench to be sure.
                --
                p5_data_out_err <= p4_data_out_err;
                p5_data_buf_valid_var := p5_data_buf_valid;
                p5_data_out_valid_var := p5_data_out_valid;
                -- Replace output only after it has filled for one cycle, or
                -- the last signal was asserted with it.
                IF all_true(p5_data_out_valid_var)
                        OR p5_data_out_end = '1' THEN
                    p5_data_out_valid_var := (OTHERS => false);
                END IF;
                p5_j := 0;
                p5_data_out_start <= '0';
                p5_data_out_end <= '0';
                FOR i IN INTEGER RANGE 0 TO p5_data_out'length - 1 LOOP
                    IF NOT p5_data_out_valid(i) THEN
                        -- First move data from the buffer to the output
                        FOR j IN INTEGER RANGE 0 TO p5_data_buf'length - 1
                                LOOP
                            IF p5_data_buf_valid_var(j) THEN
                                p5_data_out(i) <= p5_data_buf(j);
                                p5_data_out_valid_var(i) := true;
                                p5_data_buf_valid_var(j) := false;
                                p5_data_out_end <= p5_data_buf_end;
                                p5_data_buf_end <= '0';
                                p5_data_out_start <= p5_data_buf_start;
                            END IF;
                        END LOOP;
                        -- Place incoming data into the output if out of
                        -- buffered data, and it is not "end" data.
                        IF NOT p5_data_out_valid_var(i)
                                AND p5_data_buf_end /= '1' THEN
                            FOR j IN INTEGER RANGE 0 to p4_data_out'length - 1
                                    LOOP
                                IF p4_data_out_valid(j) THEN
                                    p5_data_out(i) <= p4_data_out(j);
                                    p5_data_out_valid_var(i) := true;
                                    p4_data_out_valid(j) <= false;
                                    p5_data_out_end <= p4_data_out_end;
                                    p5_data_out_start <= p4_data_out_start
                                        OR p5_data_buf_start;
                                END IF;
                            END LOOP;
                        END IF;
                    END IF;
                END LOOP;
                -- Insert remaining input data into the buffer. The buffer
                -- will always be empty at this point.
                p5_data_buf_valid <= (OTHERS => false);
                p5_j := 0;
                FOR i IN INTEGER RANGE 0 TO p4_data_out_valid'length - 1 LOOP
                    IF p4_data_out_valid(i) THEN
                        p5_data_buf(p5_j) <= p4_data_out(i);
                        p5_data_buf_valid(p5_j) <= true;
                        p5_j := p5_j + 1;
                        p5_data_buf_end <= p4_data_out_end;
                        p5_data_buf_start <= p4_data_out_start;
                    END IF;
                END LOOP;

                --
                -- Stage 6: Handle shift enable
                --
                p6_data_out <= p5_data_out;
                p6_data_out_start <= p5_data_out_start;
                p6_data_out_end <= p5_data_out_end;
                p6_data_out_err <= p5_data_out_err;
                p6_shift_ena <= '0';
                IF p5_data_out_end = '1' OR all_true(p5_data_out_valid) THEN
                    p6_shift_ena <= '1';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- Shift register for control signals (need to be able to reset all to 0)
    shift_reg_ena <= p6_shift_ena;
    shift_reg_d <= p6_data_out_start & p6_data_out_end & p6_data_out_err;
    c_shift_reg: shift_reg
        GENERIC MAP (
            width => shift_reg_d'length,
            depth => SHIFT_RAM_DEPTH -- same depth as the shift ram
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Ena => shift_reg_ena,
            D => shift_reg_d,
            Q => shift_reg_q
        );
    shift_reg_q_start <= shift_reg_q(2);
    shift_reg_q_end <= shift_reg_q(1);
    shift_reg_q_err <= shift_reg_q(0);

    -- Shift RAM for data
    shift_ram_ena <= p6_shift_ena;
    g_shift_ram_d: FOR i IN INTEGER RANGE 0 TO p6_data_out'length - 1 GENERATE
        shift_ram_d((i + 1) * 8 - 1 DOWNTO i * 8) <= p6_data_out(i);
    END GENERATE;
    c_shift_ram: shift_ram_abstract
        GENERIC MAP (
            width => shift_ram_d'length,
            depth => SHIFT_RAM_DEPTH
        )
        PORT MAP (
            Clk => Clk,
            Ce => shift_ram_ena,
            D => shift_ram_d,
            Q => shift_ram_q
        );
    g_shift_ram_q_data: FOR i IN INTEGER RANGE 0
            TO shift_ram_q_data'length - 1 GENERATE
        shift_ram_q_data(i) <= shift_ram_q((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;

    -- FIFO for forwarding header information
    fifo_d <= STD_LOGIC_VECTOR(p4_udp_chk) & STD_LOGIC_VECTOR(p4_udp_len)
        & p4_addr_src & p4_addr_dst & p4_udp_port_src & p4_udp_port_dst;
    fifo_write <= p4_fifo_hdr_write;
    c_fifo_hdr: fifo
        GENERIC MAP (
            width => fifo_d'length,
            depth => 32 -- FIXME: determine necessary depth
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Read => fifo_read,
            Write => fifo_write,
            D => fifo_d,
            Empty => fifo_empty,
            Full => fifo_full,
            Q => fifo_q
        );
    fifo_q_udp_chk <= fifo_q(fifo_q'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length);
    fifo_q_udp_len <= fifo_q(fifo_q'length - fifo_q_udp_chk'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length - fifo_q_udp_len'length);
    fifo_q_addr_src <= fifo_q(fifo_q'length - fifo_q_udp_chk'length
        - fifo_q_udp_len'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length - fifo_q_udp_len'length
        - fifo_q_addr_src'length);
    fifo_q_addr_dst <= fifo_q(fifo_q'length - fifo_q_udp_chk'length
        - fifo_q_udp_len'length - fifo_q_addr_src'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length - fifo_q_udp_len'length
        - fifo_q_addr_src'length - fifo_q_addr_dst'length);
    fifo_q_udp_port_src <= fifo_q(fifo_q'length - fifo_q_udp_chk'length
        - fifo_q_udp_len'length - fifo_q_addr_src'length
        - fifo_q_addr_dst'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length - fifo_q_udp_len'length
        - fifo_q_addr_src'length - fifo_q_addr_dst'length
        - fifo_q_udp_port_src'length);
    fifo_q_udp_port_dst <= fifo_q(fifo_q'length - fifo_q_udp_chk'length
        - fifo_q_udp_len'length - fifo_q_addr_src'length
        - fifo_q_addr_dst'length - fifo_q_udp_port_src'length - 1
        DOWNTO fifo_q'length - fifo_q_udp_chk'length - fifo_q_udp_len'length
        - fifo_q_addr_src'length - fifo_q_addr_dst'length
        - fifo_q_udp_port_src'length - fifo_q_udp_port_dst'length);

    PROCESS(Clk)
        VARIABLE started : BOOLEAN;
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                started := false;
                fifo_data_write <= '0';
            ELSE
                --
                -- Final data FIFO control machine
                --
                fifo_data_write <= '0';
                fifo_data_d <= shift_reg_q_end & shift_ram_q;
                IF p6_shift_ena = '1' THEN
                    IF started THEN
                        fifo_data_write <= '1';
                    END IF;
                    IF shift_reg_q_start = '1' THEN
                        fifo_data_write <= '1';
                        started := true;
                    ELSIF shift_reg_q_end = '1' THEN
                        started := false;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- FIFO for data coming out of the shift RAM
    c_fifo_data: fifo
        GENERIC MAP (
            width => fifo_data_d'length,
            depth => 32 -- FIXME: Find necessary depth, should depend on width
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Read => fifo_data_read,
            Write => fifo_data_write,
            D => fifo_data_d,
            Empty => fifo_data_empty,
            Full => fifo_data_full,
            Q => fifo_data_q
        );
    fifo_data_q_end <= fifo_data_q(fifo_data_q_data'length - 1);
    g_fifo_data_q_data: FOR i IN INTEGER RANGE 0
            TO fifo_data_q_data'length - 1 GENERATE
        fifo_data_q_data(i) <= fifo_data_q((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;

    PROCESS(Clk)
        -- One bit larger than UDP length field to accomodate the stream
        -- header.
        VARIABLE count : UNSIGNED(16 DOWNTO 0);
        VARIABLE hdr_sent : BOOLEAN;
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                count := (OTHERS => '0');
                hdr_sent := false;
                fifo_read <= '0';
                fifo_data_read <= '0';
                op0_data <= (OTHERS => (OTHERS => '0'));
                op0_valid <= (OTHERS => '0');
                op0_start <= '0';
                op0_end <= '0';
                op0_err <= '0';
            ELSE
                --
                -- Output control/data flow
                --
                op0_data <= fifo_data_q_data;
                op0_valid <= (OTHERS => '0');
                op0_start <= '0';
                op0_end <= '0';
                op0_err <= '0';
                fifo_read <= '0';
                fifo_data_read <= '0';
                -- Send header as soon as information available
                IF fifo_empty /= '1' AND NOT hdr_sent THEN
                    -- TODO: This part should compact the inserted fields
                    --       with the data stream to maximize bandwidth.
                    FOR i IN 0 TO op0_valid'length LOOP
                        op0_valid(i) <= '1';
                        count := count + 1;
                        CASE TO_INTEGER(count) IS
                            WHEN DATA_OUT_OFF_IP_SRC =>
                                op0_data(i) <= fifo_q_addr_src(7 DOWNTO 0);
                                op0_start <= '1';
                            WHEN DATA_OUT_OFF_IP_SRC + 1 =>
                                op0_data(i) <= fifo_q_addr_src(15 DOWNTO 8);
                            WHEN DATA_OUT_OFF_IP_SRC + 2 =>
                                op0_data(i) <= fifo_q_addr_src(23 DOWNTO 16);
                            WHEN DATA_OUT_OFF_IP_SRC + 3 =>
                                op0_data(i) <= fifo_q_addr_src(31 DOWNTO 24);
                            WHEN DATA_OUT_OFF_IP_DST =>
                                op0_data(i) <= fifo_q_addr_dst(7 DOWNTO 0);
                            WHEN DATA_OUT_OFF_IP_DST + 1 =>
                                op0_data(i) <= fifo_q_addr_dst(15 DOWNTO 8);
                            WHEN DATA_OUT_OFF_IP_DST + 2 =>
                                op0_data(i) <= fifo_q_addr_dst(23 DOWNTO 16);
                            WHEN DATA_OUT_OFF_IP_DST + 3 =>
                                op0_data(i) <= fifo_q_addr_dst(31 DOWNTO 24);
                            WHEN DATA_OUT_OFF_PROTO =>
                                op0_data(i) <= UDP_PROTO;
                            WHEN DATA_OUT_OFF_UDP_PORT_SRC =>
                                op0_data(i)
                                    <= fifo_q_udp_port_src(7 DOWNTO 0);
                            WHEN DATA_OUT_OFF_UDP_PORT_SRC + 1 =>
                                op0_data(i)
                                    <= fifo_q_udp_port_src(15 DOWNTO 8);
                            WHEN DATA_OUT_OFF_UDP_PORT_DST =>
                                op0_data(i)
                                    <= fifo_q_udp_port_dst(7 DOWNTO 0);
                            WHEN DATA_OUT_OFF_UDP_PORT_DST + 1 =>
                                op0_data(i)
                                    <= fifo_q_udp_port_dst(15 DOWNTO 8);
                            WHEN DATA_OUT_OFF_UDP_LEN =>
                                op0_data(i) <= fifo_q_udp_len(7 DOWNTO 0);
                            WHEN DATA_OUT_OFF_UDP_LEN + 1 =>
                                op0_data(i) <= fifo_q_udp_len(15 DOWNTO 8);
                            WHEN DATA_OUT_OFF_UDP_CHK =>
                                op0_data(i) <= fifo_q_udp_chk(7 DOWNTO 0);
                            WHEN DATA_OUT_OFF_UDP_CHK + 1 =>
                                op0_data(i) <= fifo_q_udp_chk(15 DOWNTO 8);
                                fifo_read <= '1';
                                hdr_sent := true;
                            WHEN OTHERS =>
                                op0_valid(i) <= '0';
                                count := count;
                        END CASE;
                    END LOOP;
                ELSIF fifo_data_empty /= '1' THEN
                    fifo_data_read <= '1';
                    op0_valid <= (OTHERS => '1');
                    count := count + fifo_data_q_data'length;
                    IF fifo_data_q_end = '1' THEN
                        -- FIXME: adjust valids based on count/udp hdr
                        hdr_sent := false;
                        count := (OTHERS => '0');
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    g_data_out: FOR i IN INTEGER RANGE 0 TO op0_data'length - 1 GENERATE
        Data_out((i + 1) * 8 - 1 DOWNTO i * 8) <= op0_data(i);
    END GENERATE;
    Data_out_valid <= op0_valid;
    Data_out_start <= op0_start;
    Data_out_end <= op0_end;
    Data_out_err <= op0_err;
END ARCHITECTURE;

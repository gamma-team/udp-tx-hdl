-- UDP streaming transmitter
--
-- TODO: Add support for the Data_in_err
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
        -- Indicate that there has been an error in the current transfer.
        Data_in_err : IN STD_LOGIC;

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

    COMPONENT stream_packer IS
        GENERIC (
            width : POSITIVE := 4
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rstn : IN STD_LOGIC;
            In_data : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            In_keep : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            In_valid : IN STD_LOGIC;
            In_last : IN STD_LOGIC;
            In_ready : OUT STD_LOGIC;
            Out_data : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Out_keep : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Out_valid : OUT STD_LOGIC;
            Out_last : OUT STD_LOGIC;
            Out_ready : IN STD_LOGIC
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
    CONSTANT DATA_IN_OFF_UDP_PAYLOAD : NATURAL
        := DATA_IN_OFF_UDP_PORT_DST + 2;
    CONSTANT DATA_OUT_OFF_IP_SRC : NATURAL := 0;
    CONSTANT DATA_OUT_OFF_IP_DST : NATURAL := DATA_OUT_OFF_IP_SRC + 4;
    CONSTANT DATA_OUT_OFF_PROTO : NATURAL := DATA_OUT_OFF_IP_DST + 4;
    CONSTANT DATA_OUT_OFF_UDP_PORT_SRC : NATURAL := DATA_OUT_OFF_PROTO + 1;
    CONSTANT DATA_OUT_OFF_UDP_PORT_DST : NATURAL
        := DATA_OUT_OFF_UDP_PORT_SRC + 2;
    CONSTANT DATA_OUT_OFF_UDP_LEN : NATURAL := DATA_OUT_OFF_UDP_PORT_DST + 2;
    CONSTANT DATA_OUT_OFF_UDP_CHK : NATURAL := DATA_OUT_OFF_UDP_LEN + 2;
    CONSTANT DATA_OUT_OFF_UDP_PAYLOAD : NATURAL := DATA_OUT_OFF_UDP_CHK + 2;
    CONSTANT FIFO_BUFFER_DEPTH : NATURAL := 65536 / (width * 8);
    CONSTANT UDP_PROTO : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"11";

    TYPE DATA_BUS IS ARRAY (width - 1 DOWNTO 0)
        OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    TYPE BOOLEAN_VECTOR IS ARRAY (NATURAL RANGE <>) OF BOOLEAN;
    TYPE HEADER_FSM
        IS (S_WAIT_HDR, S_WAIT_HDR_VALID, S_OUTPUT_HDR0, S_OUTPUT_HDR1,
        S_OUTPUT_HDR2, S_OUTPUT_DATA0, S_OUTPUT_DATA1);

    SIGNAL data_in_sig : DATA_BUS;
    SIGNAL rstn : STD_LOGIC;

    SIGNAL state : HEADER_FSM;

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
    SIGNAL p0_started : BOOLEAN;
    SIGNAL p0_hdr_done : BOOLEAN;

    SIGNAL p1_data_in : DATA_BUS;
    SIGNAL p1_data_in_valid
        : STD_LOGIC_VECTOR(p1_data_in'length - 1 DOWNTO 0);
    SIGNAL p1_data_in_start : STD_LOGIC;
    SIGNAL p1_data_in_end : STD_LOGIC;
    SIGNAL p1_data_in_err : STD_LOGIC;
    SIGNAL p1_len_read : UNSIGNED(p0_len_read'range);
    SIGNAL p1_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p1_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p1_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p1_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p1_chk_accum : UNSIGNED(31 DOWNTO 0);
    SIGNAL p1_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p1_len_ge_data_in_off_udp_payload : BOOLEAN;

    SIGNAL p2a_data_in : DATA_BUS;
    SIGNAL p2a_data_in_valid
        : STD_LOGIC_VECTOR(p2a_data_in'length - 1 DOWNTO 0);
    SIGNAL p2a_data_in_start : STD_LOGIC;
    SIGNAL p2a_data_in_end : STD_LOGIC;
    SIGNAL p2a_data_in_err : STD_LOGIC;
    SIGNAL p2a_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2a_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2a_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2a_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2a_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2a_chk_accum : UNSIGNED(31 DOWNTO 0);
    SIGNAL p2a_chk_addend : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2a_internal_off : UNSIGNED(COUNTER_WIDTH - 1 DOWNTO 0);
    SIGNAL p2a_len_ge_data_in_off_udp_payload : BOOLEAN;
    SIGNAL p2a_p1_chk_accum : UNSIGNED(31 DOWNTO 0);

    SIGNAL p2b_data_in : DATA_BUS;
    SIGNAL p2b_data_in_valid
        : STD_LOGIC_VECTOR(p2b_data_in'length - 1 DOWNTO 0);
    SIGNAL p2b_data_in_start : STD_LOGIC;
    SIGNAL p2b_data_in_end : STD_LOGIC;
    SIGNAL p2b_data_in_err : STD_LOGIC;
    SIGNAL p2b_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2b_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p2b_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2b_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p2b_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2b_chk_accum : UNSIGNED(31 DOWNTO 0);

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
    SIGNAL p4_data_out_sig
        : STD_LOGIC_VECTOR(p4_data_out'length * 8 - 1 DOWNTO 0);
    SIGNAL p4_data_out_valid : BOOLEAN_VECTOR(p4_data_out'length - 1 DOWNTO 0);
    SIGNAL p4_data_out_valid_sig
        : STD_LOGIC_VECTOR(p4_data_out_valid'length - 1 DOWNTO 0);
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
    SIGNAL p5_data_out_sig
        : STD_LOGIC_VECTOR(p5_data_out'length * 8 - 1 DOWNTO 0);
    SIGNAL p5_data_out_valid : BOOLEAN_VECTOR(p5_data_out'length - 1 DOWNTO 0);
    SIGNAL p5_data_out_valid_sig
        : STD_LOGIC_VECTOR(p5_data_out_valid'length - 1 DOWNTO 0);
    SIGNAL p5_data_out_start : STD_LOGIC;
    SIGNAL p5_data_out_end : STD_LOGIC;
    SIGNAL p5_data_out_err : STD_LOGIC;

    SIGNAL p6_started : BOOLEAN;
    SIGNAL p6_data_out : DATA_BUS;
    SIGNAL p6_data_out_start : STD_LOGIC;
    SIGNAL p6_data_out_end : STD_LOGIC;
    SIGNAL p6_data_out_err : STD_LOGIC;
    SIGNAL p6_fifo_buffer_we : STD_LOGIC;

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

    SIGNAL fifo_buffer_d
        : STD_LOGIC_VECTOR(width * 8 + 3 - 1 DOWNTO 0);
    SIGNAL fifo_buffer_q : STD_LOGIC_VECTOR(fifo_buffer_d'length - 1 DOWNTO 0);
    SIGNAL fifo_buffer_write : STD_LOGIC;
    SIGNAL fifo_buffer_read : STD_LOGIC;
    SIGNAL fifo_buffer_empty : STD_LOGIC;
    SIGNAL fifo_buffer_full : STD_LOGIC;
    SIGNAL fifo_buffer_q_data : DATA_BUS;
    SIGNAL fifo_buffer_q_start : STD_LOGIC;
    SIGNAL fifo_buffer_q_err : STD_LOGIC;
    SIGNAL fifo_buffer_q_end : STD_LOGIC;

    SIGNAL out_data_reg : DATA_BUS;
    SIGNAL out_count_reg : UNSIGNED(16 DOWNTO 0); -- Extra bit for stream header
    SIGNAL out_valid_reg : STD_LOGIC_VECTOR(Data_out_valid'length - 1 DOWNTO 0);
    SIGNAL out_start_reg : STD_LOGIC;
    SIGNAL out_end_reg : STD_LOGIC;
    SIGNAL out_err_reg : STD_LOGIC;
    SIGNAL out_data_sent : BOOLEAN;

    SIGNAL packed_data_in : DATA_BUS;
    SIGNAL packed_data_in_sig
        : STD_LOGIC_VECTOR(Data_in'length - 1 DOWNTO 0);
    SIGNAL packed_data_in_valid
        : STD_LOGIC_VECTOR(Data_in_valid'length - 1 DOWNTO 0);
    SIGNAL packed_data_in_end : STD_LOGIC;
BEGIN
    rstn <= NOT Rst;

    -- Input signal wiring
    gen_in_data: FOR i IN 0 TO width - 1 GENERATE
        packed_data_in(i) <= packed_data_in_sig((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;
    c_input_stream_packer: stream_packer
        GENERIC MAP (
            width => Data_in'length / 8
        )
        PORT MAP (
            Clk => Clk,
            Rstn => rstn,
            In_data => Data_in,
            In_keep => Data_in_valid,
            In_valid => '1',
            In_last => Data_in_end,
            In_ready => OPEN,
            Out_data => packed_data_in_sig,
            Out_keep => packed_data_in_valid,
            Out_valid => OPEN,
            Out_last => packed_data_in_end,
            Out_ready => '1'
        );

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
        FUNCTION all_false(bools : BOOLEAN_VECTOR)
            RETURN BOOLEAN IS
            VARIABLE b : BOOLEAN;
        BEGIN
            b := false;
            FOR i IN bools'range LOOP
                b := b OR bools(i);
            END LOOP;
            RETURN NOT b;
        END FUNCTION;
        FUNCTION n_valid(v : STD_LOGIC_VECTOR(width - 1 DOWNTO 0))
            RETURN INTEGER IS
            VARIABLE count : INTEGER;
        BEGIN
            count := 0;
            FOR i IN INTEGER RANGE 0 TO v'length - 1 LOOP
                IF v(i) = '1' THEN
                    count := count + 1;
                END IF;
            END LOOP;
            RETURN count;
        END FUNCTION;

        VARIABLE p0_len_read_var : UNSIGNED(p0_len_read'length - 1 DOWNTO 0);
        VARIABLE p1_chk_accum_var
            : UNSIGNED(p1_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p2a_internal_off_var
            : UNSIGNED(p2a_internal_off'length - 1 DOWNTO 0);
        VARIABLE p2a_chk_addend_var
            : UNSIGNED(p2a_chk_addend'length - 1 DOWNTO 0);
        VARIABLE p2a_chk_accum_var
            : UNSIGNED(p2a_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p2b_chk_addend_var
            : UNSIGNED(p2a_chk_addend'length - 1 DOWNTO 0);
        VARIABLE p2b_chk_accum_var
            : UNSIGNED(p2b_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p4_j : NATURAL;
        VARIABLE p3_udp_chk_var : UNSIGNED(p3_udp_chk'range);
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                p0_data_in <= (OTHERS => (OTHERS => '0'));
                p0_data_in_valid <= (OTHERS => '0');
                p0_data_in_start <= '0';
                p0_data_in_end <= '0';
                p0_data_in_err <= '0';
                p0_addr_src_valid <= false;
                p0_addr_dst_valid <= false;
                p0_udp_port_src_valid <= false;
                p0_udp_port_dst_valid <= false;
                p0_addr_src <= (OTHERS => '0');
                p0_addr_dst <= (OTHERS => '0');
                p0_udp_port_src <= (OTHERS => '0');
                p0_udp_port_dst <= (OTHERS => '0');
                p0_started <= false;
                p0_hdr_done <= false;
                p0_len_read <= (OTHERS => '0');

                p1_data_in <= (OTHERS => (OTHERS => '0'));
                p1_data_in_valid <= (OTHERS => '0');
                p1_data_in_start <= '0';
                p1_data_in_end <= '0';
                p1_data_in_err <= '0';
                p1_addr_src <= (OTHERS => '0');
                p1_addr_dst <= (OTHERS => '0');
                p1_udp_port_src <= (OTHERS => '0');
                p1_udp_port_dst <= (OTHERS => '0');
                p1_udp_len <= (OTHERS => '0');
                p1_chk_accum <= (OTHERS => '0');
                p1_len_read <= (OTHERS => '0');
                p1_len_ge_data_in_off_udp_payload  <= FALSE;

                p2a_data_in <= (OTHERS => (OTHERS => '0'));
                p2a_data_in_valid <= (OTHERS => '0');
                p2a_data_in_start <= '0';
                p2a_data_in_end <= '0';
                p2a_data_in_err <= '0';
                p2a_addr_src <= (OTHERS => '0');
                p2a_addr_dst <= (OTHERS => '0');
                p2a_udp_port_src <= (OTHERS => '0');
                p2a_udp_port_dst <= (OTHERS => '0');
                p2a_udp_len <= (OTHERS => '0');
                p2a_chk_accum <= (OTHERS => '0');
                p2a_chk_addend <= (OTHERS => '0');
                p2a_internal_off <= (OTHERS => '0');
                p2a_len_ge_data_in_off_udp_payload <= FALSE;
                p2a_p1_chk_accum <= (OTHERS => '0');

                p2b_data_in <= (OTHERS => (OTHERS => '0'));
                p2b_data_in_valid <= (OTHERS => '0');
                p2b_data_in_start <= '0';
                p2b_data_in_end <= '0';
                p2b_data_in_err <= '0';
                p2b_addr_src <= (OTHERS => '0');
                p2b_addr_dst <= (OTHERS => '0');
                p2b_udp_port_src <= (OTHERS => '0');
                p2b_udp_port_dst <= (OTHERS => '0');
                p2b_udp_len <= (OTHERS => '0');
                p2b_chk_accum <= (OTHERS => '0');

                p3_data_in <= (OTHERS => (OTHERS => '0'));
                p3_data_in_valid <= (OTHERS => '0');
                p3_data_in_start <= '0';
                p3_data_in_end <= '0';
                p3_data_in_err <= '0';
                p3_addr_src <= (OTHERS => '0');
                p3_addr_dst <= (OTHERS => '0');
                p3_udp_port_src <= (OTHERS => '0');
                p3_udp_port_dst <= (OTHERS => '0');
                p3_udp_len <= (OTHERS => '0');
                p3_udp_chk <= (OTHERS => '0');

                p4_data_out <= (OTHERS => (OTHERS => '0'));
                p4_data_out_valid <= (OTHERS => false);
                p4_data_out_start <= '0';
                p4_data_out_end <= '0';
                p4_data_out_err <= '0';
                p4_addr_src <= (OTHERS => '0');
                p4_addr_dst <= (OTHERS => '0');
                p4_udp_port_src <= (OTHERS => '0');
                p4_udp_port_dst <= (OTHERS => '0');
                p4_udp_len <= (OTHERS => '0');
                p4_udp_chk <= (OTHERS => '0');
                p4_fifo_hdr_write <= '0';

                p5_data_out_start <= '0';
                p5_data_out_err <= '0';

                p6_data_out <= (OTHERS => (OTHERS => '0'));
                p6_data_out_start <= '0';
                p6_data_out_end <= '0';
                p6_data_out_err <= '0';
                p6_fifo_buffer_we <= '0';
                p6_started <= FALSE;
            ELSE
                --
                -- Stage 0: Byte decoding
                --
                p0_data_in <= packed_data_in;
                p0_data_in_valid <= packed_data_in_valid;
                p0_data_in_end <= packed_data_in_end;
                p0_data_in_err <= '0';

                p0_len_read_var := p0_len_read;
                p0_data_in_start <= '0';
                IF NOT p0_started AND packed_data_in_valid /= x"00" THEN
                    p0_started <= true;
                    p0_data_in_start <= '1';
                    p0_len_read_var := (OTHERS => '0');
                END IF;

                p0_addr_src_valid <= false;
                p0_addr_dst_valid <= false;
                p0_udp_port_src_valid <= false;
                p0_udp_port_dst_valid <= false;
                -- Since the data is packed, we take some shortcuts since
                -- the header information will always arrive in the same
                -- byte location
                -- TODO: make this generic
                IF packed_data_in_valid /= x"00" AND NOT p0_hdr_done THEN
                    IF NOT p0_started THEN
                        p0_addr_src(31 DOWNTO 24) <= packed_data_in(0);
                        p0_addr_src(23 DOWNTO 16) <= packed_data_in(1);
                        p0_addr_src(15 DOWNTO 8) <= packed_data_in(2);
                        p0_addr_src(7 DOWNTO 0) <= packed_data_in(3);
                        p0_addr_src_valid <= true;
                        p0_addr_dst(31 DOWNTO 24) <= packed_data_in(4);
                        p0_addr_dst(23 DOWNTO 16) <= packed_data_in(5);
                        p0_addr_dst(15 DOWNTO 8) <= packed_data_in(6);
                        p0_addr_dst(7 DOWNTO 0) <= packed_data_in(7);
                        p0_addr_dst_valid <= true;
                        p0_data_in_valid <= (OTHERS => '0');
                    ELSE
                        p0_udp_port_src(15 DOWNTO 8) <= packed_data_in(0);
                        p0_udp_port_src(7 DOWNTO 0) <= packed_data_in(1);
                        p0_udp_port_src_valid <= true;
                        p0_udp_port_dst(15 DOWNTO 8) <= packed_data_in(2);
                        p0_udp_port_dst(7 DOWNTO 0) <= packed_data_in(3);
                        p0_udp_port_dst_valid <= true;
                        p0_data_in_valid(3 DOWNTO 0) <= (OTHERS => '0');
                        p0_hdr_done <= true;
                    END IF;
                END IF;
                IF packed_data_in_end = '1' THEN
                    p0_started <= false;
                    p0_hdr_done <= false;
                END IF;
                p0_len_read_var := p0_len_read_var
                    + TO_UNSIGNED(n_valid(packed_data_in_valid),
                    p0_len_read_var'length);
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
                p1_len_read <= p0_len_read;

                -- The checksum field is 0 when calculating to fill the
                -- checksum field, so it is omitted.
                p1_chk_accum_var := p1_chk_accum;
                IF p0_data_in_start = '1' THEN
                    p1_chk_accum_var := (OTHERS => '0');
                    p1_chk_accum_var(7 DOWNTO 0) := UNSIGNED(UDP_PROTO);
                END IF;
                IF p0_addr_src_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_src(15 DOWNTO 0))
                        + UNSIGNED(p0_addr_src(31 DOWNTO 16));
                END IF;
                IF p0_addr_dst_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_dst(15 DOWNTO 0))
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
                    p1_chk_accum_var := p1_chk_accum_var
                        + (p0_len_read - DATA_IN_OFF_UDP_PAYLOAD + 8)
                        + (p0_len_read - DATA_IN_OFF_UDP_PAYLOAD + 8);
                END IF;
                p1_chk_accum <= p1_chk_accum_var;
                -- Calculate length for UDP header (subtract input stream
                -- header)
                p1_udp_len <=
                    resize(p0_len_read - DATA_IN_OFF_UDP_PAYLOAD + 8,
                    p1_udp_len'length);
                IF p0_len_read >= DATA_IN_OFF_UDP_PAYLOAD THEN
                    p1_len_ge_data_in_off_udp_payload <= TRUE;
                ELSE
                    p1_len_ge_data_in_off_udp_payload <= FALSE;
                END IF;

                --
                -- Stage 2a: Normal checksumming
                --
                p2a_data_in <= p1_data_in;
                p2a_data_in_valid <= p1_data_in_valid;
                p2a_data_in_start <= p1_data_in_start;
                p2a_data_in_end <= p1_data_in_end;
                p2a_data_in_err <= p1_data_in_err;
                p2a_addr_src <= p1_addr_src;
                p2a_addr_dst <= p1_addr_dst;
                p2a_udp_port_src <= p1_udp_port_src;
                p2a_udp_port_dst <= p1_udp_port_dst;
                p2a_udp_len <= p1_udp_len;
                p2a_len_ge_data_in_off_udp_payload
                    <= p1_len_ge_data_in_off_udp_payload;
                p2a_p1_chk_accum <= p1_chk_accum;

                p2a_internal_off_var := p2a_internal_off;
                p2a_chk_addend_var := p2a_chk_addend;
                p2a_chk_accum_var := p2a_chk_accum;
                IF p1_data_in_start = '1' THEN
                    p2a_internal_off_var := (OTHERS => '0');
                    p2a_chk_addend_var := (OTHERS => '0');
                    p2a_chk_accum_var := (OTHERS => '0');
                END IF;
                -- Note: If this is too slow, split into stages that handle
                -- ranges of byte enables
                IF p1_len_ge_data_in_off_udp_payload THEN
                    FOR i IN 0 TO width - 1 LOOP
                        IF p1_data_in_valid(i) = '1' THEN
                            IF 0 = p2a_internal_off_var MOD 2 THEN
                                p2a_chk_addend_var(15 DOWNTO 8)
                                    := UNSIGNED(p1_data_in(i));
                            ELSE
                                p2a_chk_addend_var(7 DOWNTO 0)
                                    := UNSIGNED(p1_data_in(i));
                                p2a_chk_accum_var := p2a_chk_accum_var
                                    + p2a_chk_addend_var;
                            END IF;
                            p2a_internal_off_var := p2a_internal_off_var + 1;
                        END IF;
                    END LOOP;
                END IF;
                p2a_chk_accum <= p2a_chk_accum_var;
                p2a_chk_addend <= p2a_chk_addend_var;
                p2a_internal_off <= p2a_internal_off_var;

                --
                -- Stage 2b: Normal checksumming
                --
                p2b_data_in <= p2a_data_in;
                p2b_data_in_valid <= p2a_data_in_valid;
                p2b_data_in_start <= p2a_data_in_start;
                p2b_data_in_end <= p2a_data_in_end;
                p2b_data_in_err <= p2a_data_in_err;
                p2b_addr_src <= p2a_addr_src;
                p2b_addr_dst <= p2a_addr_dst;
                p2b_udp_port_src <= p2a_udp_port_src;
                p2b_udp_port_dst <= p2a_udp_port_dst;
                p2b_udp_len <= p2a_udp_len;

                p2b_chk_addend_var := p2a_chk_addend;
                p2b_chk_accum_var := p2a_chk_accum;
                IF p2a_len_ge_data_in_off_udp_payload THEN
                    IF p2a_data_in_end = '1' THEN
                        -- account for non-word-aligned data length
                        IF 1 = p2a_internal_off MOD 2 THEN
                            p2b_chk_addend_var(7 DOWNTO 0) := (OTHERS => '0');
                            p2b_chk_accum_var
                                := p2b_chk_accum_var + p2b_chk_addend_var;
                        END IF;
                        -- add the accumulation from header fields
                        p2b_chk_accum_var
                            := p2b_chk_accum_var + p2a_p1_chk_accum;
                    END IF;
                END IF;
                p2b_chk_accum <= p2b_chk_accum_var;

                --
                -- Stage 3: Finish checksum
                --
                p3_data_in <= p2b_data_in;
                p3_data_in_valid <= p2b_data_in_valid;
                p3_data_in_start <= p2b_data_in_start;
                p3_data_in_end <= p2b_data_in_end;
                p3_data_in_err <= p2b_data_in_err;
                p3_addr_src <= p2b_addr_src;
                p3_addr_dst <= p2b_addr_dst;
                p3_udp_port_src <= p2b_udp_port_src;
                p3_udp_port_dst <= p2b_udp_port_dst;
                p3_udp_len <= p2b_udp_len;

                p3_udp_chk_var := NOT (p2b_chk_accum(31 DOWNTO 16)
                    + p2b_chk_accum(15 DOWNTO 0));
                IF p3_udp_chk_var = 0 THEN
                    p3_udp_chk_var := (OTHERS => '1');
                END IF;
                p3_udp_chk <= p3_udp_chk_var;

                --
                -- Stage 4: Prepare data for FIFO buffer step 1 (compact data,
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
                -- Remaining connections are with c_stream_packer below

                --
                -- Stage 6: Handle writing to the FIFO buffer
                --
                p6_data_out <= p5_data_out;
                p6_data_out_start <= '0';
                p6_data_out_end <= p5_data_out_end;
                p6_data_out_err <= p5_data_out_err;
                p6_fifo_buffer_we <= '0';
                IF p5_data_out_end = '1' OR all_true(p5_data_out_valid) THEN
                    IF NOT p6_started THEN
                        p6_data_out_start <= '1';
                        p6_started <= TRUE;
                    END IF;
                    IF p5_data_out_end = '1' THEN
                        p6_started <= FALSE;
                    END IF;
                    IF NOT all_false(p5_data_out_valid) THEN
                        p6_fifo_buffer_we <= '1';
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    g_p5_data_in: FOR i IN INTEGER RANGE 0 TO p4_data_out'length - 1 GENERATE
        p4_data_out_sig((i + 1) * 8 - 1 DOWNTO i * 8) <= p4_data_out(i);
    END GENERATE;
    g_p5_valid_in: FOR i IN INTEGER RANGE 0 TO p4_data_out_valid'length - 1
            GENERATE
        p4_data_out_valid_sig(i) <= '1' WHEN p4_data_out_valid(i) ELSE '0';
    END GENERATE;
    g_p5_data_out: FOR i IN 0 TO width - 1 GENERATE
        p5_data_out(i) <= p5_data_out_sig((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;
    g_p5_valid_out: FOR i IN INTEGER RANGE 0 TO p5_data_out_valid'length - 1
            GENERATE
        p5_data_out_valid(i) <= TRUE WHEN p5_data_out_valid_sig(i) = '1'
            ELSE FALSE;
    END GENERATE;
    c_stream_packer: stream_packer
        GENERIC MAP (
            width => p5_data_out'length
        )
        PORT MAP (
            Clk => Clk,
            Rstn => rstn,
            In_data => p4_data_out_sig,
            In_keep => p4_data_out_valid_sig,
            In_valid => '1',
            In_last => p4_data_out_end,
            In_ready => OPEN,
            Out_data => p5_data_out_sig,
            Out_keep => p5_data_out_valid_sig,
            Out_valid => OPEN,
            Out_last => p5_data_out_end,
            Out_ready => '1'
        );

    -- Buffer FIFO, stores data while waiting for header calculations
    fifo_buffer_write <= p6_fifo_buffer_we;
    g_fifo_buffer_d: FOR i IN INTEGER RANGE 0 TO p6_data_out'length - 1
            GENERATE
        fifo_buffer_d((i + 1) * 8 - 1 DOWNTO i * 8) <= p6_data_out(i);
    END GENERATE;
    fifo_buffer_d(fifo_buffer_d'length - 1 DOWNTO p6_data_out'length * 8)
        <= p6_data_out_start & p6_data_out_end & p6_data_out_err;
    c_fifo_buffer: fifo
        GENERIC MAP (
            width => fifo_buffer_d'length,
            depth => FIFO_BUFFER_DEPTH
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Read => fifo_buffer_read,
            Write => fifo_buffer_write,
            D => fifo_buffer_d,
            Empty => fifo_buffer_empty,
            Full => fifo_buffer_full,
            Q => fifo_buffer_q
        );
    g_fifo_buffer_q: FOR i IN INTEGER RANGE 0
            TO fifo_buffer_q_data'length - 1 GENERATE
        fifo_buffer_q_data(i) <= fifo_buffer_q((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;
    fifo_buffer_q_start <= fifo_buffer_q(fifo_buffer_q'length - 1);
    fifo_buffer_q_end <= fifo_buffer_q(fifo_buffer_q'length - 2);
    fifo_buffer_q_err <= fifo_buffer_q(fifo_buffer_q'length - 3);

    -- FIFO for forwarding header information
    fifo_d <= STD_LOGIC_VECTOR(p4_udp_chk) & STD_LOGIC_VECTOR(p4_udp_len)
        & p4_addr_src & p4_addr_dst & p4_udp_port_src & p4_udp_port_dst;
    fifo_write <= p4_fifo_hdr_write;
    c_fifo_hdr: fifo
        GENERIC MAP (
            width => fifo_d'length,
            depth => 32 -- FIXME: determine necessary depth based on worst case
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
        FUNCTION n_to_valid (n : INTEGER)
            RETURN STD_LOGIC_VECTOR IS
            VARIABLE o : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        BEGIN
            o := (OTHERS => '0');
            FOR i IN INTEGER RANGE 0 TO o'length - 1 LOOP
                IF i + 1 <= n THEN
                    o(i) := '1';
                END IF;
            END LOOP;
            RETURN o;
        END FUNCTION;
        VARIABLE out_data_count : UNSIGNED(15 DOWNTO 0);
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                out_data_reg <= (OTHERS => (OTHERS => '0'));
                out_valid_reg <= (OTHERS => '0');
                out_start_reg <= '0';
                out_err_reg <= '0';
                out_end_reg <= '0';
                out_data_count := (OTHERS => '0');
                out_data_sent <= FALSE;
                fifo_buffer_read <= '0';
            ELSE
                out_valid_reg <= (OTHERS => '0');
                out_start_reg <= '0';
                out_err_reg <= '0';
                out_end_reg <= '0';
                fifo_buffer_read <= '0';
                CASE state IS
                    WHEN S_OUTPUT_HDR0 =>
                        out_data_reg(0) <= fifo_q_addr_src(31 DOWNTO 24);
                        out_data_reg(1) <= fifo_q_addr_src(23 DOWNTO 16);
                        out_data_reg(2) <= fifo_q_addr_src(15 DOWNTO 8);
                        out_data_reg(3) <= fifo_q_addr_src(7 DOWNTO 0);
                        out_data_reg(4) <= fifo_q_addr_dst(31 DOWNTO 24);
                        out_data_reg(5) <= fifo_q_addr_dst(23 DOWNTO 16);
                        out_data_reg(6) <= fifo_q_addr_dst(15 DOWNTO 8);
                        out_data_reg(7) <= fifo_q_addr_dst(7 DOWNTO 0);
                        out_valid_reg <= (OTHERS => '1');
                        out_start_reg <= '1';
                        out_data_sent <= FALSE;
                    WHEN S_OUTPUT_HDR1 =>
                        out_data_reg(0) <= UDP_PROTO;
                        out_data_reg(1) <= fifo_q_udp_port_src(15 DOWNTO 8);
                        out_data_reg(2) <= fifo_q_udp_port_src(7 DOWNTO 0);
                        out_data_reg(3) <= fifo_q_udp_port_dst(15 DOWNTO 8);
                        out_data_reg(4) <= fifo_q_udp_port_dst(7 DOWNTO 0);
                        out_data_reg(5) <= fifo_q_udp_len(15 DOWNTO 8);
                        out_data_reg(6) <= fifo_q_udp_len(7 DOWNTO 0);
                        out_data_reg(7) <= fifo_q_udp_chk(15 DOWNTO 8);
                        out_valid_reg <= (OTHERS => '1');
                        -- when no data, tell the FSM to skip data phase
                        IF UNSIGNED(fifo_q_udp_len) <= 8 THEN
                            out_data_sent <= TRUE;
                        END IF;
                    WHEN S_OUTPUT_HDR2 =>
                        out_data_reg(0) <= fifo_q_udp_chk(7 DOWNTO 0);
                        out_valid_reg(0) <= '1';
                        out_data_sent <= FALSE;
                        out_data_count := (OTHERS => '0');
                        IF UNSIGNED(fifo_q_udp_len) > 8 THEN
                            fifo_buffer_read <= '1';
                        ELSE
                            out_end_reg <= '1';
                        END IF;
                    -- TODO: fix one cycle of extra latency between header and
                    -- first data output
                    WHEN S_OUTPUT_DATA0 =>
                        IF UNSIGNED(fifo_q_udp_len) > 16 THEN
                            fifo_buffer_read <= '1';
                        END IF;
                    WHEN S_OUTPUT_DATA1 =>
                        -- TODO; cleanup counter usage, it's a little
                        -- unintuitive
                        out_data_reg <= fifo_buffer_q_data;
                        out_data_count := out_data_count + width;
                        -- only continue reading from the fifo if more data is
                        -- left TODO: verify
                        IF UNSIGNED(fifo_q_udp_len) > 16 THEN
                            IF out_data_count < UNSIGNED(fifo_q_udp_len) - 16 THEN
                                fifo_buffer_read <= '1';
                            END IF;
                        END IF;

                        out_valid_reg <= (OTHERS => '1');
                        IF fifo_buffer_q_end = '1' THEN
                            out_end_reg <= '1';
                            out_valid_reg <= n_to_valid(
                                TO_INTEGER(UNSIGNED(fifo_q_udp_len)) MOD 8);
                            out_data_sent <= TRUE;
                        END IF;
                        -- FIXME: Until the state machine is refined, do this
                        -- to prevent end from being asserted for an extra
                        -- cycle.
                        IF out_data_sent THEN
                            out_end_reg <= '0';
                            out_valid_reg <= (OTHERS => '0');
                        END IF;
                    WHEN OTHERS =>
                END CASE;
            END IF;
        END IF;
    END PROCESS;

    -- State machine for reading from the header and buffer FIFOs. Works with
    -- the output pipeline.
    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                state <= S_WAIT_HDR;
                fifo_read <= '0';
            ELSE
                CASE state IS
                    WHEN S_WAIT_HDR =>
                        IF fifo_empty /= '1' THEN
                            fifo_read <= '1';
                            state <= S_WAIT_HDR_VALID;
                        END IF;
                    WHEN S_WAIT_HDR_VALID =>
                        fifo_read <= '0';
                        state <= S_OUTPUT_HDR0;
                    WHEN S_OUTPUT_HDR0 =>
                        state <= S_OUTPUT_HDR1;
                    WHEN S_OUTPUT_HDR1 =>
                        state <= S_OUTPUT_HDR2;
                    WHEN S_OUTPUT_HDR2 =>
                        state <= S_OUTPUT_DATA0;
                        IF out_data_sent THEN
                            state <= S_WAIT_HDR;
                        END IF;
                    WHEN S_OUTPUT_DATA0 =>
                        -- TODO: should probably check out_data_sent
                        state <= S_OUTPUT_DATA1;
                    WHEN S_OUTPUT_DATA1 =>
                        IF out_data_sent THEN
                            state <= S_WAIT_HDR;
                        END IF;
                    WHEN OTHERS =>
                        state <= S_WAIT_HDR;
                END CASE;
            END IF;
        END IF;
    END PROCESS;

    g_data_out: FOR i IN INTEGER RANGE 0 TO out_data_reg'length - 1 GENERATE
        Data_out((i + 1) * 8 - 1 DOWNTO i * 8) <= out_data_reg(i);
    END GENERATE;
    Data_out_valid <= out_valid_reg;
    Data_out_start <= out_start_reg;
    Data_out_end <= out_end_reg;
    Data_out_err <= out_err_reg;
END ARCHITECTURE;

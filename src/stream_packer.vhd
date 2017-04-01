-- STATUS: Works for constant streaming where sinks and sources are always
-- asserting ready. This is good enough for this project, so it is left that
-- way for now.
-- BUG: ACKing from Out_ready is broken
-- BUG: Sink interface does not ACK properly when the output interface is not
-- ready. It always asserts that it is ready.
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

ENTITY stream_packer IS
    GENERIC (
        width : POSITIVE := 4
    );
    PORT (
        Clk : IN STD_LOGIC;
        Rstn : IN STD_LOGIC;

        In_data : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        In_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        In_last : IN STD_LOGIC;
        In_ready : OUT STD_LOGIC;

        Out_data : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        Out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        Out_last : OUT STD_LOGIC;
        Out_ready : IN STD_LOGIC
    );
END ENTITY;

ARCHITECTURE yagami OF stream_packer IS
    TYPE DATA_BUS IS ARRAY (width - 1 DOWNTO 0)
        OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    TYPE BOOLEAN_VECTOR IS ARRAY (NATURAL RANGE <>) OF BOOLEAN;
    TYPE STATE_STREAM IS (S_WAIT_VALID, S_WAIT_ACK, S_ACKED, S_ERR);

    SIGNAL in_data_sig : DATA_BUS;
    SIGNAL in_valid_sig : BOOLEAN_VECTOR(width - 1 DOWNTO 0);

    SIGNAL out_data_reg : DATA_BUS;
    SIGNAL out_valid_reg : BOOLEAN_VECTOR(width - 1 DOWNTO 0);
    SIGNAL out_valid_reg_int : BOOLEAN_VECTOR(width - 1 DOWNTO 0);
    SIGNAL out_last_reg : STD_LOGIC;
    SIGNAL out_last_reg_int : STD_LOGIC;

    SIGNAL buf_data : DATA_BUS;
    SIGNAL buf_last : STD_LOGIC;
    SIGNAL buf_valid : BOOLEAN_VECTOR(in_valid_sig'length - 1 DOWNTO 0);

    SIGNAL state : STATE_STREAM;
    SIGNAL pack_valid : BOOLEAN;
BEGIN
    g_in_sig: FOR i IN 0 TO width - 1 GENERATE
        in_data_sig(i) <= In_data((i + 1) * 8 - 1 DOWNTO i * 8);
        in_valid_sig(i) <= TRUE WHEN In_valid(i) = '1' ELSE FALSE;
    END GENERATE;
    g_out_sig: FOR i IN 0 TO width - 1 GENERATE
        Out_data((i + 1) * 8 - 1 DOWNTO i * 8) <= out_data_reg(i);
        Out_valid(i) <= '1' WHEN out_valid_reg(i) ELSE '0';
    END GENERATE;
    Out_last <= out_last_reg;

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

        VARIABLE idx : INTEGER;
        VARIABLE buf_valid_var
            : BOOLEAN_VECTOR(buf_valid'length - 1 DOWNTO 0);
        VARIABLE out_valid_var
            : BOOLEAN_VECTOR(buf_valid'length - 1 DOWNTO 0);
        VARIABLE out_last_var : STD_LOGIC;
        VARIABLE inserted : BOOLEAN;
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                out_data_reg <= (OTHERS => (OTHERS => '0'));
                out_valid_reg <= (OTHERS => FALSE);
                out_valid_reg_int <= (OTHERS => FALSE);
                out_last_reg <= '0';
                out_last_reg_int <= '0';
                buf_data <= (OTHERS => (OTHERS => '0'));
                buf_valid <= (OTHERS => FALSE);
                buf_last <= '0';
                in_ready <= '1';
                pack_valid <= FALSE;
            ELSE
                buf_valid_var := buf_valid;
                out_valid_var := out_valid_reg_int;
                out_last_var := out_last_reg_int;
                IF state = S_WAIT_VALID OR state = S_ACKED THEN
                    -- Move buffered data into output
                    FOR i IN INTEGER RANGE 0 TO buf_valid_var'length - 1 LOOP
                        IF buf_valid_var(i) THEN
                            FOR j IN INTEGER RANGE 0
                                    TO out_valid_var'length - 1 LOOP
                                IF NOT out_valid_var(j) THEN
                                    out_data_reg(j) <= buf_data(i);
                                    out_valid_var(j) := TRUE;
                                    buf_valid_var(i) := FALSE;
                                    out_last_var := buf_last;
                                    EXIT;
                                END IF;
                            END LOOP;
                        END IF;
                    END LOOP;
                    -- Buffer is always emptied
                    buf_valid_var := (OTHERS => FALSE);
                    -- Do not merge incoming data with buffered data if the
                    -- buffered data is tagged as last
                    IF buf_last = '1' THEN
                        idx := 0;
                        FOR i IN INTEGER RANGE 0 TO in_valid_sig'length - 1
                                LOOP
                            IF in_valid_sig(i) THEN
                                buf_data(idx) <= in_data_sig(i);
                                buf_valid_var(idx) := TRUE;
                                idx := idx + 1;
                            END IF;
                        END LOOP;
                    ELSE
                        FOR i IN INTEGER RANGE 0 TO in_valid_sig'length - 1
                                LOOP
                            IF in_valid_sig(i) THEN
                                inserted := FALSE;
                                FOR j IN INTEGER RANGE 0
                                        TO out_valid_var'length - 1 LOOP
                                    IF NOT out_valid_var(j) THEN
                                        out_data_reg(j) <= in_data_sig(i);
                                        out_valid_var(j) := TRUE;
                                        inserted := TRUE;
                                        out_last_var := In_last;
                                        EXIT;
                                    END IF;
                                END LOOP;
                                IF NOT inserted THEN
                                    FOR j IN INTEGER RANGE 0
                                            TO buf_valid_var'length - 1 LOOP
                                        IF NOT buf_valid_var(j) THEN
                                            buf_data(j) <= in_data_sig(i);
                                            buf_valid_var(j) := TRUE;
                                            -- Last signal asserted with the
                                            -- last pieces of that data,
                                            buf_last <= In_last;
                                            out_last_var := '0';
                                            EXIT;
                                        END IF;
                                    END LOOP;
                                END IF;
                            END IF;
                        END LOOP;
                    END IF;
                    buf_valid <= buf_valid_var;
                    out_valid_reg_int <= out_valid_var;
                    out_last_reg_int <= out_last_var;
                    -- ready for output
                    IF all_true(out_valid_var) OR out_last_var = '1' THEN
                        -- externally visible signals
                        out_valid_reg <= out_valid_var;
                        out_last_reg <= out_last_var;
                        pack_valid <= TRUE;
                        -- flush that data out next run
                        out_valid_reg_int <= (OTHERS => FALSE);
                        out_last_reg_int <= '0';
                    ELSE
                        pack_valid <= FALSE;
                        out_valid_reg <= (OTHERS => FALSE);
                        out_last_reg <= '0';
                    END IF;
--                ELSE
--                    pack_valid <= FALSE;
--                    out_valid_reg <= (OTHERS => FALSE);
--                    out_last_reg <= '0';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                state <= S_WAIT_VALID;
            ELSE
                CASE state IS
                    -- Waiting for output data to become valid
                    WHEN S_WAIT_VALID =>
                        IF pack_valid THEN
                            -- Skip to ACKed state if sink is already ready
                            IF out_ready = '1' THEN
                                state <= S_ACKED;
                            ELSE
                                state <= S_WAIT_ACK;
                            END IF;
                        END IF;
                    -- Output data valid, waiting for ACK
                    WHEN S_WAIT_ACK =>
                        IF out_ready = '1' THEN
                            state <= S_ACKED;
                        END IF;
                    WHEN S_ACKED =>
                        IF pack_valid THEN
                            IF out_ready = '1' THEN
                                state <= S_ACKED;
                            ELSE
                                state <= S_WAIT_ACK;
                            END IF;
                        ELSE
                            state <= S_WAIT_VALID;
                        END IF;
                    -- for debug in simulation
                    WHEN S_ERR =>
                        state <= S_WAIT_VALID;
                    WHEN OTHERS =>
                        state <= S_ERR;
                END CASE;
            END IF;
        END IF;
    END PROCESS;

--    -- Test code
--    PROCESS(Clk)
--    BEGIN
--        IF rising_edge(Clk) THEN
--            IF Rstn = '0' THEN
--                pack_valid <= FALSE;
--            ELSE
--                IF state = S_WAIT_VALID OR state = S_ACKED THEN
--                    IF In_valid /= x"00" THEN
--                        Out_data <= In_data;
--                        Out_valid <= In_valid;
--                        Out_last <= In_last;
--                        pack_valid <= TRUE;
--                    ELSE
--                        pack_valid <= FALSE;
--                        Out_valid <= (OTHERS => '0');
--                        Out_last <= '0';
--                    END IF;
--                END IF;
--            END IF;
END ARCHITECTURE;

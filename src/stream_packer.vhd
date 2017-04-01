-- STATUS: Works for constant streaming where sinks and sources are always
-- asserting ready. This is good enough for this project, so it is left that
-- way for now.
-- BUG: ACKing from Out_ready is not implemented
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
USE ieee.math_real.ALL;

ENTITY stream_packer IS
    GENERIC (
        width : POSITIVE := 8
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

    TYPE intermediate_reg IS RECORD
        mask_end : INTEGER; -- constant
        mask : STD_LOGIC_VECTOR(width - 1 DOWNTO 0); -- should be constant
        data : DATA_BUS;
        valid : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        last : STD_LOGIC;
    END RECORD;

    -- These help remove layers of logic
    TYPE intermediate_regs IS ARRAY (INTEGER RANGE <>) OF intermediate_reg;

    SIGNAL stage1_regs : intermediate_regs(0 TO width - 1);

    SIGNAL in_data_sig : DATA_BUS;
    SIGNAL in_valid_sig : BOOLEAN_VECTOR(width - 1 DOWNTO 0);

    SIGNAL out_data_reg : DATA_BUS;
    SIGNAL out_valid_reg : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL out_last_reg : STD_LOGIC;
    SIGNAL ext_out_data_reg : DATA_BUS;
    SIGNAL ext_out_valid_reg : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL ext_out_last_reg : STD_LOGIC;
BEGIN
    g_in_sig: FOR i IN 0 TO width - 1 GENERATE
        in_data_sig(i) <= In_data((i + 1) * 8 - 1 DOWNTO i * 8);
        in_valid_sig(i) <= TRUE WHEN In_valid(i) = '1' ELSE FALSE;
    END GENERATE;
    g_out_sig: FOR i IN 0 TO width - 1 GENERATE
        Out_data((i + 1) * 8 - 1 DOWNTO i * 8) <= ext_out_data_reg(i);
    END GENERATE;
    Out_valid <= ext_out_valid_reg;
    Out_last <= ext_out_last_reg;

    PROCESS(Clk)
        -- AND all the bits in a vector with B
        FUNCTION vector_and(v : STD_LOGIC_VECTOR; b : STD_LOGIC)
            RETURN STD_LOGIC_VECTOR IS
            VARIABLE v2 : STD_LOGIC_VECTOR(v'range);
        BEGIN
            FOR i IN v2'range LOOP
                v2(i) := b AND v(i);
            END LOOP;
            RETURN v2;
        END FUNCTION;

        FUNCTION all_true(v : STD_LOGIC_VECTOR(width - 1 DOWNTO 0))
            RETURN BOOLEAN IS
            VARIABLE b : STD_LOGIC;
        BEGIN
            b := '1';
            FOR i IN v'range LOOP
                b := b AND v(i);
            END LOOP;
            RETURN b = '1';
        END FUNCTION;

        FUNCTION n_valid(v : STD_LOGIC_VECTOR)
            RETURN INTEGER IS
            VARIABLE count : INTEGER;
        BEGIN
            count := 0;
            FOR i IN v'range LOOP
                IF v(i) = '1' THEN
                    count := count + 1;
                END IF;
            END LOOP;
            RETURN count;
        END FUNCTION;

        FUNCTION combine(
            valid : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            data : DATA_BUS;
            new_data : intermediate_reg)
            RETURN DATA_BUS IS
            VARIABLE out_data : DATA_BUS;
        BEGIN
            FOR i IN out_data'range LOOP
                out_data(i) := vector_and(data(i), valid(i))
                    OR vector_and(new_data.data(i), new_data.valid(i));
            END LOOP;
            RETURN out_data;
        END FUNCTION;

        VARIABLE z : INTEGER;
        VARIABLE n : INTEGER;
    BEGIN
        l_stage1: FOR i IN 0 TO width - 1 LOOP
            stage1_regs(i).mask
                <= STD_LOGIC_VECTOR(TO_UNSIGNED(2 ** i - 1, width));
            stage1_regs(i).mask_end <= i;
            -- don't synthesize registers for the unused data slots
            IF i > 0 THEN
                stage1_regs(i).data(i - 1 DOWNTO 0)
                    <= (OTHERS => (OTHERS => '0'));
            END IF;
        END LOOP;
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                out_data_reg <= (OTHERS => (OTHERS => '0'));
                out_valid_reg <= (OTHERS => '0');
                out_last_reg <= '0';
                ext_out_data_reg <= (OTHERS => (OTHERS => '0'));
                ext_out_valid_reg <= (OTHERS => '0');
                ext_out_last_reg <= '0';
                in_ready <= '1';
                FOR i IN 0 TO width - 1 LOOP
                    stage1_regs(i).data(width - 1
                        --DOWNTO stage1_regs(i).mask_end)
                        --Vivado 2016.4 doesn't realize mask_end is constant
                        DOWNTO i)
                        <= (OTHERS => (OTHERS => '0'));
                    stage1_regs(i).valid <= (OTHERS => '0');
                    stage1_regs(i).last <= '0';
                END LOOP;
            ELSE
                -- expand possibilities stage
                FOR i IN 0 TO width - 1 LOOP
                    z := 0;
                    stage1_regs(i).valid <= (OTHERS => '0');
                    --FOR j IN stage1_regs(i).mask_end TO width - 1 LOOP --
                    --Vivado 2016.4 doesn't realize mask_end is constant
                    FOR j IN i TO width - 1 LOOP
                        FOR k IN INTEGER RANGE 1 TO in_data_sig'length LOOP
                            stage1_regs(i).data(j) <= in_data_sig(k - 1);
                            IF in_valid_sig(k - 1) AND k > z THEN
                                stage1_regs(i).valid(j) <= '1';
                                z := k;
                                EXIT;
                            END IF;
                        END LOOP;
                    END LOOP;
                    stage1_regs(i).last <= In_last;
                END LOOP;

                -- pre-output stage
                IF all_true(out_valid_reg) OR out_last_reg = '1' THEN
                    out_data_reg <= stage1_regs(0).data;
                    out_valid_reg <= stage1_regs(0).valid;
                    out_last_reg <= stage1_regs(0).last;
                ELSE
                    n := n_valid(out_valid_reg);
                    out_data_reg <=
                        combine(out_valid_reg, out_data_reg, stage1_regs(n));
                    out_valid_reg <= stage1_regs(n).valid OR out_valid_reg;
                    out_last_reg <= stage1_regs(n).last;
                END IF;

                -- output stage
                ext_out_data_reg <= out_data_reg;
                IF all_true(out_valid_reg) OR out_last_reg = '1' THEN
                    ext_out_valid_reg <= out_valid_reg;
                    ext_out_last_reg <= out_last_reg;
                ELSE
                    ext_out_valid_reg <= (OTHERS => '0');
                    ext_out_last_reg <= out_last_reg;
                END IF;
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE;

-- AXI4-Stream null byte remover.
--
-- Copyright 2017 Patrick Gauvin. All rights reserved.
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
        In_valid : IN STD_LOGIC;
        In_keep : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        In_last : IN STD_LOGIC;
        In_ready : OUT STD_LOGIC;

        Out_data : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        Out_valid : OUT STD_LOGIC;
        Out_keep : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
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
        keep : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        last : STD_LOGIC;
    END RECORD;

    -- These help remove layers of logic
    TYPE intermediate_regs IS ARRAY (INTEGER RANGE <>) OF intermediate_reg;

    SIGNAL stage1_regs : intermediate_regs(0 TO width - 1);
    SIGNAL stage1_in_data : DATA_BUS;
    SIGNAL stage1_in_keep : BOOLEAN_VECTOR(width - 1 DOWNTO 0);
    SIGNAL stage1_in_last : STD_LOGIC;

    SIGNAL stage2_regs : intermediate_regs(0 TO width - 1);
    SIGNAL stage2_ovf_regs : intermediate_regs(0 TO width - 1);
    SIGNAL stage2_in_keep : BOOLEAN_VECTOR(width - 1 DOWNTO 0);
    SIGNAL stage2_in_last : STD_LOGIC;

    SIGNAL out_reg : intermediate_reg;
    SIGNAL out_ovf_reg : intermediate_reg;

    SIGNAL in_data_sig : DATA_BUS;
    SIGNAL in_keep_sig : BOOLEAN_VECTOR(width - 1 DOWNTO 0);
    SIGNAL in_last_sig : STD_LOGIC;

    SIGNAL ext_out_reg : intermediate_reg;
    SIGNAL ext_out_reg_valid : STD_LOGIC;

    SIGNAL flow_enable : STD_LOGIC;
BEGIN
    g_in_sig: FOR i IN 0 TO width - 1 GENERATE
        in_data_sig(i) <= In_data((i + 1) * 8 - 1 DOWNTO i * 8);
        in_keep_sig(i) <= TRUE WHEN In_keep(i) = '1' AND In_valid = '1'
            ELSE FALSE;
    END GENERATE;
    in_last_sig <= In_valid AND In_last;
    g_out_sig: FOR i IN 0 TO width - 1 GENERATE
        Out_data((i + 1) * 8 - 1 DOWNTO i * 8) <= ext_out_reg.data(i);
    END GENERATE;
    Out_keep <= ext_out_reg.keep;
    Out_last <= ext_out_reg.last;
    Out_valid <= ext_out_reg_valid;

    -- Handle pushback
    In_ready <= flow_enable;
    flow_enable <= '1' WHEN (ext_out_reg_valid = '0' OR Out_ready = '1')
        ELSE '0';

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

        FUNCTION all_false(v : BOOLEAN_VECTOR(width - 1 DOWNTO 0))
            RETURN BOOLEAN IS
            VARIABLE b : BOOLEAN;
        BEGIN
            b := FALSE;
            FOR i IN v'range LOOP
                b := b OR v(i);
            END LOOP;
            RETURN b = FALSE;
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

        -- Combine two intermediate_reg that do not overlap
        FUNCTION combine_aligned(
            old_data : intermediate_reg;
            new_data : intermediate_reg)
            RETURN intermediate_reg IS
            VARIABLE out_data : intermediate_reg;
        BEGIN
            FOR i IN out_data.data'range LOOP
                out_data.data(i) :=
                    vector_and(old_data.data(i), old_data.keep(i))
                    OR vector_and(new_data.data(i), new_data.keep(i));
            END LOOP;
            out_data.keep := old_data.keep OR new_data.keep;
            out_data.last := new_data.last;
            RETURN out_data;
        END FUNCTION;

        VARIABLE out_reg_var : intermediate_reg;
        -- FIXME: extra bit for testing
        VARIABLE n : INTEGER;
        VARIABLE kept_byte_n : INTEGER;
    BEGIN
        l_stage1: FOR i IN 0 TO width - 1 LOOP
            stage1_regs(i).mask
                <= STD_LOGIC_VECTOR(TO_UNSIGNED(2 ** i - 1, width));
            stage2_regs(i).mask
                <= STD_LOGIC_VECTOR(TO_UNSIGNED(2 ** i - 1, width));
            stage1_regs(i).mask_end <= i;
            stage2_regs(i).mask_end <= i;
            -- don't synthesize registers for the unused data slots
            IF i > 0 THEN
                stage1_regs(i).data(i - 1 DOWNTO 0)
                    <= (OTHERS => (OTHERS => '0'));
                stage2_regs(i).data(i - 1 DOWNTO 0)
                    <= (OTHERS => (OTHERS => '0'));
            END IF;
        END LOOP;
        -- Not used in the out_reg
        out_reg.mask_end <= 0;
        out_reg.mask <= (OTHERS => '0');
        out_ovf_reg.mask_end <= 0;
        out_ovf_reg.mask <= (OTHERS => '0');
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                out_ovf_reg.data <= (OTHERS => (OTHERS => '0'));
                out_ovf_reg.keep <= (OTHERS => '0');
                out_ovf_reg.last <= '0';
                out_reg.data <= (OTHERS => (OTHERS => '0'));
                out_reg.keep <= (OTHERS => '0');
                out_reg.last <= '0';
                ext_out_reg.data <= (OTHERS => (OTHERS => '0'));
                ext_out_reg.keep <= (OTHERS => '0');
                ext_out_reg_valid <= '0';
                ext_out_reg.last <= '0';
                stage1_in_data <= (OTHERS => (OTHERS => '0'));
                stage1_in_keep <= (OTHERS => FALSE);
                stage1_in_last <= '0';
                stage2_in_keep <= (OTHERS => FALSE);
                stage2_in_last <= '0';
                FOR i IN 0 TO width - 1 LOOP
                    stage1_regs(i).data(width - 1
                        --DOWNTO stage1_regs(i).mask_end)
                        --Vivado 2016.4 doesn't realize mask_end is constant
                        DOWNTO i)
                        <= (OTHERS => (OTHERS => '0'));
                    stage1_regs(i).keep <= (OTHERS => '0');
                    stage1_regs(i).last <= '0';
                    stage2_regs(i).data(width - 1
                        --DOWNTO stage2_regs(i).mask_end)
                        --Vivado 2016.4 doesn't realize mask_end is constant
                        DOWNTO i)
                        <= (OTHERS => (OTHERS => '0'));
                    stage2_regs(i).keep <= (OTHERS => '0');
                    stage2_regs(i).last <= '0';
                    stage2_ovf_regs(i).data(width - 1
                        --DOWNTO stage2_regs(i).mask_end)
                        --Vivado 2016.4 doesn't realize mask_end is constant
                        DOWNTO i)
                        <= (OTHERS => (OTHERS => '0'));
                    stage2_ovf_regs(i).keep <= (OTHERS => '0');
                    stage2_ovf_regs(i).last <= '0';
                END LOOP;
            ELSIF flow_enable = '1' THEN
                -- Populate intermediate data fanout
                FOR i IN 0 TO width - 1 LOOP
                    stage1_regs(i).keep <= (OTHERS => '0');
                    stage1_regs(i).last <= '0';
                    kept_byte_n := 0;
                    FOR j IN 0 TO width - 1 LOOP
                        IF in_keep_sig(j) THEN
                            IF kept_byte_n < width - i THEN
                                stage1_regs(i).data(kept_byte_n + i)
                                    <= in_data_sig(j);
                                stage1_regs(i).keep(kept_byte_n + i) <= '1';
                                stage1_regs(i).last <= in_last_sig;
                            END IF;
                            kept_byte_n := kept_byte_n + 1;
                        END IF;
                    END LOOP;
                END LOOP;
                stage1_in_data <= in_data_sig;
                stage1_in_keep <= in_keep_sig;
                stage1_in_last <= in_last_sig;

                stage2_regs <= stage1_regs;
                stage2_in_last <= stage1_in_last;
                stage2_in_keep <= stage1_in_keep;
                -- Populate overflow possibilities (data only)
                FOR i IN 0 TO width - 1 LOOP
                    stage2_ovf_regs(i).keep <= (OTHERS => '0');
                    --stage2_ovf_regs(i).last <= stage1_in_last;
                    stage2_ovf_regs(i).last <= '0';
                    kept_byte_n := 0;
                    FOR j IN 0 TO width - 1 LOOP
                        IF stage1_in_keep(j) THEN
                            -- width - i is the number of the first byte that
                            -- register i cannot hold. i is the number of initial
                            -- used bytes.
                            IF kept_byte_n >= (width - i) THEN
                                stage2_ovf_regs(i).data(kept_byte_n
                                    - (width - i)) <= stage1_in_data(j);
                                stage2_ovf_regs(i).keep(kept_byte_n
                                    - (width - i)) <= '1';
                                stage2_ovf_regs(i).last <= stage1_in_last;
                            END IF;
                            kept_byte_n := kept_byte_n + 1;
                        END IF;
                    END LOOP;
                END LOOP;

                -- Pre-output stage. Merge new data with the output register
                -- depending on keeps and overflows.
                --
                -- TODO Cleanup this stage, it's a bit sloppy.
                out_reg_var := out_reg;
                -- Change output if last or full
                IF all_true(out_reg_var.keep) OR out_reg_var.last = '1' THEN
                    n := n_valid(out_ovf_reg.keep);
                    IF n > 0 THEN
                        IF out_ovf_reg.last = '1' OR n = 8 THEN
                            out_reg_var := out_ovf_reg;
                            out_ovf_reg <= stage2_regs(0);
                        ELSE
                            out_reg_var := combine_aligned(
                                out_ovf_reg, stage2_regs(n));
                            out_ovf_reg <= stage2_ovf_regs(n);
                            -- last set in an overflow reg also indicates that
                            -- there was overflow
                            IF stage2_ovf_regs(n).last = '1' THEN
                                out_reg_var.last := '0';
                            END IF;
                        END IF;
                    ELSE
                        out_reg_var := stage2_regs(0);
                        out_ovf_reg.keep <= (OTHERS => '0');
                        out_ovf_reg.last <= '0';
                    END IF;
                ELSE
                    -- TODO; Recrod the reasoning for why this doesn't happen
                    --n := n_valid(out_ovf_reg.keep);
                    --IF n > 0 THEN
                    --    REPORT "this shouldn't happen";
                    --    out_reg_var :=
                    --        combine_aligned(out_ovf_reg, stage2_regs(n));
                    --    out_ovf_reg <= stage2_ovf_regs(n);
                    --    IF stage2_ovf_regs(n).last = '1' THEN
                    --        out_reg_var.last := '0';
                    --    END IF;
                    --ELSE
                        n := n_valid(out_reg_var.keep);
                        IF n > 0 THEN
                            out_reg_var :=
                                combine_aligned(out_reg, stage2_regs(n));
                            out_ovf_reg <= stage2_ovf_regs(n);
                            IF stage2_ovf_regs(n).last = '1' THEN
                                out_reg_var.last := '0';
                            END IF;
                        ELSE
                            out_reg_var := stage2_regs(n);
                            out_ovf_reg.keep <= (OTHERS => '0');
                            out_ovf_reg.last <= '0';
                        END IF;
                    --END IF;
                END IF;
                -- Catch an all-null last signal. The all-null
                -- last signal is inefficient anyway, so no effort
                -- is made to merge it with other data.
                IF stage2_in_last = '1'
                        AND all_false(stage2_in_keep) THEN
                    out_reg_var.last := '1';
                END IF;
                out_reg <= out_reg_var;

                -- output stage
                ext_out_reg.data <= out_reg.data;
                IF all_true(out_reg.keep) OR out_reg.last = '1' THEN
                    ext_out_reg.keep <= out_reg.keep;
                    ext_out_reg_valid <= '1';
                    ext_out_reg.last <= out_reg.last;
                ELSE
                    ext_out_reg.keep <= (OTHERS => '0');
                    ext_out_reg_valid <= '0';
                    ext_out_reg.last <= out_reg.last;
                END IF;
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE;

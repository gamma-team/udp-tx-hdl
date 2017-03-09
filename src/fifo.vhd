-- Simple synchronous FIFO
--
-- TODO: Needs testbenching, specifically for pointer wraparound.
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

ENTITY fifo IS
    GENERIC (
        width : POSITIVE := 32;
        -- Power of 2 required (non-power of 2 depths need testing)
        depth : POSITIVE := 64
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
END ENTITY;

ARCHITECTURE fast_peek OF fifo IS
    TYPE T_MEM IS ARRAY (0 TO DEPTH - 1)
        OF STD_LOGIC_VECTOR(width - 1 DOWNTO 0);

    -- TODO: take steps to make sure this is synthesized as memory, not
    -- registers
    SIGNAL mem : T_MEM;
    SIGNAL sig_empty : STD_LOGIC;
    SIGNAL sig_full : STD_LOGIC;
    SIGNAL q_reg : STD_LOGIC_VECTOR(Q'length - 1 DOWNTO 0);

    SIGNAL wp, rp : UNSIGNED(NATURAL(LOG2(REAL(DEPTH))) - 1 DOWNTO 0);
BEGIN
    PROCESS(Clk)
        VARIABLE wr_ptr, rd_ptr
            : UNSIGNED(NATURAL(LOG2(REAL(DEPTH))) - 1 DOWNTO 0);
    BEGIN
        IF wr_ptr = rd_ptr THEN
            sig_full <= '1';
        ELSE
            sig_full <= '0';
        END IF;
        IF (wr_ptr - rd_ptr) MOD depth = 1 THEN
            sig_empty <= '1';
        ELSE
            sig_empty <= '0';
        END IF;
        q_reg <= mem(TO_INTEGER(rd_ptr + 1) MOD depth);
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                wr_ptr := TO_UNSIGNED(1, wr_ptr'length);
                rd_ptr := (OTHERS => '0');
                q_reg <= (OTHERS => '0');
            ELSE
                IF Write = '1' THEN
                    IF sig_full = '0' THEN
                        mem(TO_INTEGER(wr_ptr)) <= D;
                        wr_ptr := (wr_ptr + 1) MOD depth;
                    END IF;
                END IF;
                IF Read = '1' THEN
                    IF sig_empty = '0' THEN
                        -- Read behaves like an acknowledge
                        rd_ptr := (rd_ptr + 1) MOD depth;
                    END IF;
                END IF;
            END IF;
        END IF;
        wp <= wr_ptr;
        rp <= rd_ptr;
    END PROCESS;
    Empty <= sig_empty;
    Full <= sig_full;
    Q <= q_reg;
END ARCHITECTURE;

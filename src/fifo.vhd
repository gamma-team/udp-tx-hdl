-- Simple synchronous FIFO
--
-- TODO: Needs more testbenching, specifically for pointer wraparound.
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

ENTITY fifo IS
    GENERIC (
        width : POSITIVE := 32;
        -- Power of 2 required (non-power of 2 depths need testing)
        depth : POSITIVE := 64
    );
    PORT (
        Clk : IN STD_LOGIC;
        Rst : IN STD_LOGIC;
        Ena : IN STD_LOGIC;
        Read : IN STD_LOGIC;
        Write : IN STD_LOGIC;
        D : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        Empty : OUT STD_LOGIC;
        Full : OUT STD_LOGIC;
        Q : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
    );
END ENTITY;

-- 1 cycle delay until Full updates after a Read or Write assertion
-- 2 cycle delay until Empty updates after a Read or Write assertion
-- 2 cycle delay until correct data on Q after a Read assertion
-- Reads and writes can be made at each clock cycle
ARCHITECTURE synchronous OF fifo IS
    CONSTANT internal_depth : POSITIVE := depth + 1;

    TYPE T_MEM IS ARRAY (0 TO internal_depth)
        OF STD_LOGIC_VECTOR(width - 1 DOWNTO 0);

    SIGNAL mem : T_MEM;
    SIGNAL mem_we : STD_LOGIC;
    SIGNAL mem_d, mem_q : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL mem_wr_ptr, mem_rd_ptr
        : UNSIGNED(NATURAL(CEIL(LOG2(REAL(internal_depth)))) - 1 DOWNTO 0);

    SIGNAL reg_empty : STD_LOGIC;
    SIGNAL reg_full : STD_LOGIC;
BEGIN
    -- Should be inferred using block RAM
    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Ena = '1' THEN
                IF mem_we = '1' THEN
                    mem(TO_INTEGER(mem_wr_ptr)) <= mem_d;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    mem_q <= mem(TO_INTEGER(mem_rd_ptr));

    PROCESS(Clk)
        VARIABLE mem_wr_ptr_var, mem_rd_ptr_var
            : UNSIGNED(NATURAL(CEIL(LOG2(REAL(internal_depth)))) - 1 DOWNTO 0);
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                mem_wr_ptr <= (OTHERS => '0');
                mem_rd_ptr <= (OTHERS => '0');
                mem_we <= '0';
                reg_full <= '0';
                reg_empty <= '1';
            ELSIF Ena = '1' THEN
                mem_wr_ptr_var := mem_wr_ptr;
                mem_rd_ptr_var := mem_rd_ptr;
                mem_we <= '0';
                IF Write = '1' THEN
                    IF reg_full = '0' THEN
                        mem_we <= '1';
                        mem_d <= D;
                        mem_wr_ptr <= (mem_wr_ptr + 1) MOD internal_depth;
                        mem_wr_ptr_var := (mem_wr_ptr + 1) MOD internal_depth;
                    END IF;
                END IF;
                IF Read = '1' THEN
                    IF reg_empty = '0' THEN
                        mem_rd_ptr <= (mem_rd_ptr + 1) MOD internal_depth;
                        mem_rd_ptr_var := (mem_rd_ptr + 1) MOD internal_depth;
                    END IF;
                END IF;
                -- empty updates after 2 cycles, since the data needs to be in
                -- the RAM before reads are attempted
                IF mem_wr_ptr = mem_rd_ptr THEN
                    reg_empty <= '1';
                ELSE
                    reg_empty <= '0';
                END IF;
                -- full updates after 1 cycle, since it should be updated ASAP
                IF (mem_wr_ptr_var - mem_rd_ptr_var)
                        = TO_UNSIGNED(2 ** mem_rd_ptr_var'length - 1,
                        mem_rd_ptr_var'length) THEN
                    reg_full <= '1';
                ELSE
                    reg_full <= '0';
                END IF;
            END IF;
        END IF;
    END PROCESS;
    Empty <= reg_empty;
    Full <= reg_full;
    Q <= mem_q;
END ARCHITECTURE;

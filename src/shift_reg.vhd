-- Shift register with complete reset and enable
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

ENTITY shift_reg IS
    GENERIC (
        width : POSITIVE := 8;
        depth : POSITIVE := 16
    );
    PORT (
        Clk : IN STD_LOGIC;
        -- Active high, synchronous reset
        Rst : IN STD_LOGIC;
        -- Active high enable
        Ena : IN STD_LOGIC;
        D : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        Q : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
    );
END ENTITY;

ARCHITECTURE normal OF shift_reg IS
    TYPE T_SHIFT_REG IS ARRAY(1 TO depth)
        OF STD_LOGIC_VECTOR(D'length - 1 DOWNTO 0);
    SIGNAL shift_reg_regs : T_SHIFT_REG;
BEGIN
    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                shift_reg_regs <= (OTHERS => (OTHERS => '0'));
            ELSIF Ena = '1' THEN
                shift_reg_regs(1) <= D;
                FOR i IN INTEGER RANGE 1 TO shift_reg_regs'length - 1 LOOP
                        shift_reg_regs(i + 1) <= shift_reg_regs(i);
                END LOOP;
            END IF;
        END IF;
    END PROCESS;
    Q <= shift_reg_regs(shift_reg_regs'length);
END ARCHITECTURE;

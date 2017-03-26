-- Vendor-independent interface for a shift RAM.
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
LIBRARY c_shift_ram_v12_0_10;

ENTITY shift_ram_abstract IS
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
END ENTITY;

ARCHITECTURE xilinx OF shift_ram_abstract IS
BEGIN
    c_xilinx_shift_ram: ENTITY c_shift_ram_v12_0_10.c_shift_ram_v12_0_10
        GENERIC MAP (
            C_WIDTH => width,
            C_DEPTH => depth,
            C_HAS_CE => 1
        )
        PORT MAP (
            Clk => Clk,
            Ce => Ce,
            D => D,
            Q => Q
        );
END ARCHITECTURE;

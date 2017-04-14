library ieee;
use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;


entity udp_tx_tb_post_synthesis is
GENERIC (
    -- Test bench Generics
    TB_width : POSITIVE := 8
);
end udp_tx_tb_post_synthesis;

architecture Behavioral of udp_tx_tb_post_synthesis is

COMPONENT udp_tx
--    GENERIC (
--        width : POSITIVE := 8
--    );
PORT (

    Clk : IN STD_LOGIC;
    Rst : IN STD_LOGIC;
		
    Data_in : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    Data_in_valid : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    Data_in_start : IN STD_LOGIC;
    Data_in_end : IN STD_LOGIC;
    Data_in_err : IN STD_LOGIC;

    Data_out : OUT STD_LOGIC_VECTOR(63 DOWNTO 0);
    Data_out_valid : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    Data_out_start : OUT STD_LOGIC;
    Data_out_end : OUT STD_LOGIC;
    Data_out_err : OUT STD_LOGIC

);
END COMPONENT;

file In_file : text open read_mode is "even-length.txt";-- Change the file name
file Out_file : text open write_mode is "output.txt";

--Clock and Reset signals
signal Clk: STD_LOGIC := '0';
signal Rst: STD_LOGIC := '0';

--Inputs signals
signal Data_in : STD_LOGIC_VECTOR(TB_width * 8 - 1 DOWNTO 0);
signal Data_in_valid : STD_LOGIC_VECTOR(TB_width - 1 DOWNTO 0);
signal Data_in_start : STD_LOGIC;
signal Data_in_end : STD_LOGIC;
signal Data_in_err : STD_LOGIC;

--Outputs signals
signal Data_out : STD_LOGIC_VECTOR(TB_width * 8 - 1 DOWNTO 0);
signal Data_out_valid : STD_LOGIC_VECTOR(TB_width - 1 DOWNTO 0);
signal Data_out_start : STD_LOGIC;
signal Data_out_end : STD_LOGIC;
signal Data_out_err : STD_LOGIC;

signal TB_Completed: STD_LOGIC:= '0';
signal Data_to_file: STD_LOGIC:= '0';

begin

DUT: udp_tx port map (
    Clk => Clk,
    Rst => Rst,
	
    Data_in => Data_in,
    Data_in_valid => Data_in_valid,
    Data_in_start => Data_in_start,
    Data_in_end => Data_in_end,
    Data_in_err => Data_in_err,
	
    Data_out => Data_out,
    Data_out_valid => Data_out_valid,
    Data_out_start => Data_out_start,
    Data_out_end => Data_out_end,
    Data_out_err => Data_out_err
);


process

    variable Buff_in: LINE;
    variable Data_input : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
    variable Data_valid_input : STD_LOGIC_VECTOR(TB_width - 1 downto 0);
    variable Data_start_input : STD_LOGIC;
    variable Data_end_input : STD_LOGIC;
    begin
    
    Data_in <= (others => '0');
    Data_in_valid <= (others => '0');
    Data_in_start <= '0';
    Data_in_end <= '0';
    Data_in_err <= '0';
    -- wait for reset process to finish
    wait for 100 ns;
    wait until falling_edge(clk);
    
    report "TB - Loadign Application messages from file...";
    while not endfile(In_file) loop
        readline(In_file, Buff_in);
        hread(Buff_in, Data_input); -- read first 8 bytes in file
        hread(Buff_in, Data_valid_input); -- read data_in_valid byte
        read(Buff_in,Data_start_input); -- read Data_in_start bit
        read(Buff_in,Data_end_input); -- read Data_in_end bit
        
        Data_in_start <= Data_start_input;
        Data_in_end <= Data_end_input;
        Data_in_valid <= Data_valid_input;
        Data_in <= Data_input;
    
        wait until falling_edge(Clk);
    end loop;
    
    Data_in_end <= '0';
    Data_in <= (others => '0');
    Data_in_valid <= (others => '0');
    file_close(In_file);
    report "TB - Application messages have been loaded successfully";
    TB_Completed <= '1';
    wait;

end process;    


-- Output process
process
    variable Buff_out : LINE;
begin
        if Data_out_start = '1' then
        Data_to_file <= '1';
    end if;
    
    if Data_out_start = '1' or Data_to_file = '1' then
        if Data_out_valid(0) = '1' then
            hwrite(Buff_out, Data_out(7 downto 0));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(1) = '1' then
            hwrite(Buff_out, Data_out(15 downto 8));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(2) = '1' then
            hwrite(Buff_out, Data_out(23 downto 16));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(3) = '1' then
            hwrite(Buff_out, Data_out(31 downto 24));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(4) = '1' then
            hwrite(Buff_out, Data_out(39 downto 32));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(5) = '1' then
            hwrite(Buff_out, Data_out(47 downto 40));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(6) = '1' then
            hwrite(Buff_out, Data_out(55 downto 48));
            write(Buff_out, STRING'(" "));
        end if;
        if Data_out_valid(7) = '1' then
            hwrite(Buff_out, Data_out(63 downto 56));
            write(Buff_out, STRING'(" "));
        end if;
    end if;
    
    if (Data_out_end = '1') then
        writeline(Out_file, Buff_out);
        Data_to_file <= '0';
        file_close(Out_file);
        wait;
    end if;
    wait until falling_edge(Clk);
end process;


-- clk process
process
begin
    Clk <= not(Clk);
    wait for 5 ns;
end process;

-- Reset process
process
begin

    Rst <= '0';
    wait for 10 ns;
    Rst <= '1';
    wait for 50 ns;
    Rst <= '0';
    wait;
end process;

end Behavioral;
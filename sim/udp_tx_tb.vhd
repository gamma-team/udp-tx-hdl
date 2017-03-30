library ieee;
use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;


entity udp_tx_tb is
GENERIC (
    -- Test bench Generics
    TB_width : POSITIVE := 8
);
end udp_tx_tb;

architecture Behavioral of udp_tx_tb is


COMPONENT udp_tx
    GENERIC (
        width : POSITIVE := 8
    );
PORT (

    Clk : IN STD_LOGIC;
    Rst : IN STD_LOGIC;
		
    Data_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    Data_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    Data_in_start : IN STD_LOGIC;
    Data_in_end : IN STD_LOGIC;
    Data_in_err : IN STD_LOGIC;

    Data_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    Data_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    Data_out_start : OUT STD_LOGIC;
    Data_out_end : OUT STD_LOGIC;
    Data_out_err : OUT STD_LOGIC

);
END COMPONENT;

file In_file : text open read_mode is "input.txt";-- Change the file name
file Out_file : text open read_mode is "output.txt";-- Change the file name

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

begin

DUT: udp_rx port map (
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


-- Read file process
process
    variable Buff_in: LINE;
    variable Data_input : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
    variable Data_valid_input : STD_LOGIC_VECTOR(TB_width - 1 downto 0);
begin
    
    Data_in <= (others => '0');
    Data_in_valid <= (others => '0');
    Data_in_start <= '0';
    Data_in_end <= '0';
    Data_err <= '0';
    -- wait for reset process to finish
    wait for 100 ns;
	
    Data_in_start <= '1';
    report "TB - Application data from file...";
	
    while not endfile(In_file) loop
        readline(In_file, Buff_in);
        hread(Buff_in, Data_input); -- read first 8 bytes in file
        hread(Buff_in, Data_valid_input); -- read data_in_valid byte
        
        Data_in_valid <= Data_valid_input;
        Data_in <= Data_input;

        wait until rising_edge(Clk);
    end loop;
	
    Data_in_end <= '1';
	
    Data_in <= (others => '0');
    Data_in_valid <= (others => '0');
    Data_in_start <= '0';
    --Data_in_end <= '0';
    --Data_err <= '0';
	
    file_close(In_file);
    report "TB - Application data have been loaded successfully";
    TB_Completed <= '1';
    wait;
end process;    


-- Output process
process
    variable Buff_out : LINE;
    variable Data_out_actual : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
    variable Data_valid_out_actual : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
    variable Data_out_expected : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
    variable Data_valid_out_expected : STD_LOGIC_VECTOR(TB_width * 8 - 1 downto 0);
begin

    --Data_output := Data_out
    Data_out_actual := Data_out;
    Data_valid_out_actual := Data_out_valid;
	
    if Data_out_start = '1' then
        while not endfile(Out_file) loop
            Readline(Out_file, Buff_out);
            hread(Buff_out, Data_out_expected);
            assert Data_out_actual = Data_out_expected 
                report "Data in file does not match the output of module";
            assert Data_valid_out_actual = Data_valid_out_expected 
                report "Valid byte in file does not match the output valid bus from module";
        end loop;
    end if;
	
    wait until rising_edge(Clk);
	
    if (TB_completed = '1') then
        file_close(outputs);
        wait;
    end if;
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

library ieee;
use ieee.std_logic_1164.all;

ENTITY AND2 IS
  PORT (in1 : IN std_logic;
        in2 : IN std_logic;
        out1 : OUT std_logic) ;
END ENTITY AND2 ;

ARCHITECTURE Behavioral OF AND2 IS
BEGIN
    out1 <= in1 AND in2 ;
END ARCHITECTURE Behavioral ;

------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity AND23 IS
  port ( a : in std_logic;
         b : in std_logic;
         c : in std_logic;
         d : in std_logic;         
         output : out std_logic);
end AND23;

architecture struct of AND23 is
  -- each node goes here.
  signal w1, w2 : std_logic;

  -- each submodule is wired up here.
begin
  u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
  u2 : entity work.AND2 port map (in1 => c, in2 => d, out1 => w2);
  u3 : entity work.AND2 port map (in1 => w1, in2 => w2, out1 => output);
end struct;

------------------------------------------------------------------
-- This will probably be a combinational test.

library STD;
use STD.textio.all;                     -- basic I/O
library IEEE;
use IEEE.std_logic_1164.all;            -- basic logic types
use IEEE.std_logic_textio.all;          -- I/O for logic types
use ieee.numeric_std.all;               

entity AND23_TB is end AND23_TB;
architecture behaviour of AND23_TB is
  signal clk : std_logic := '0';  
  signal sigterm : std_logic := '0';
  signal counter : unsigned(7 downto 0) := x"00";
  signal a, b, c, d, result : std_logic; 
  
begin
  u1 : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
  process
  begin
    --wait for 99 ns;
    clkloop : loop
      clk <= not clk;
      if sigterm = '1' then exit; end if;
      wait for 50 ns;
    end loop clkloop;
    wait;
  end process;

  process (clk)
  begin
    a <= counter(0);
    b <= counter(1);
    c <= counter(2);
    d <= counter(3);
    
    if rising_edge(clk) then
      -- report "counter is: " & to_hstring(counter);
      -- report "counter is: " & integer'image(to_integer(unsigned(counter))); -- to get decimal
      
      report "inputs: (a, b, c, d) = "
        & " (" & to_string(a)
        & ", " & to_string(b)
        & ", " & to_string(c)
        & ", " & to_string(d)
        & ") "
        & to_string(result) & " counter: " & integer'image(to_integer(unsigned(counter)));
     
      if counter = x"F" then
        sigterm <= '1';
      end if;
      counter <= counter + 1;
    end if;
  end process;
end behaviour;

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
use STD.env.all;
library IEEE;
use IEEE.std_logic_1164.all;            -- basic logic types
use IEEE.std_logic_textio.all;          -- I/O for logic types
use ieee.numeric_std.all;               

entity AND23_TB is end entity AND23_TB;
architecture behaviour of AND23_TB is
  signal a, b, c, d, result : std_logic; 
  
begin
  dut : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
  process
  begin
    ------------------------------------------------------------------
    a <= '0'; b <= '0'; c <= '0'; d <= '0';
    wait for 99.0 ns;
    if result /= '0' then
      report "TestNum 1";
      report "expecting: result = 0";
      report "got      : result = " & to_string(result);
      stop(-1);
    else
      write(OUTPUT, "TEST 1: PASSED" & LF);
    end if;
    wait for 1 ns;
    
    ------------------------------------------------------------------
    -- testnum <= testnum + 1;
    -- a <= '1'; b <= '1'; c <= '1'; d <= '1';
    -- wait for 99 ns;
    -- if result = '1' then
    --   write(OUTPUT, "TESTNUM : " & integer'image(testnum) & LF);
    --   report "expecting: result = 1";
    --   report "got      : result = " & to_string(result);
    --   stop(-1);
    -- else
    --   write(OUTPUT, "TEST " & integer'image(testnum) & ": PASSED" & LF);
    -- end if;
    -- wait for 1 ns;

    finish(0);
  end process;    
end behaviour;


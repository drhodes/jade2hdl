------------------------------------------------------------------
-- VHDL PRELUDE --------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity mod_user_and2 is
  port (in1 : in std_logic;
        in2 : in std_logic;
        out1 : out std_logic) ;
end entity mod_user_and2 ;
architecture behavioral of mod_user_and2 is
begin
    out1 <= in1 and in2 ;
end architecture behavioral ;

-- /gates/and4 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_and4 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        c : in std_logic_vector(0 downto 0);
        d : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_and4 ;
architecture behavioral of mod_gates_and4 is
begin
  vout <= a and b and c and d;
end architecture behavioral ;

-- /gates/and2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_and2 is
  port (a : in std_logic;
        b : in std_logic;
        vout : out std_logic) ;
end entity mod_gates_and2 ;
architecture behavioral of mod_gates_and2 is
begin
  vout <= a and b;
end architecture behavioral ;

-- /gates/or2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_or2 is
  port (a : in std_logic;
        b : in std_logic;
        vout : out std_logic) ;
end entity mod_gates_or2 ;
architecture behavioral of mod_gates_or2 is
begin
  vout <= a or b;
end architecture behavioral ;


-- END VHDL PRELUDE ----------------------------------------------
------------------------------------------------------------------
-- combinational module -------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity mod_user_And41 IS
  port (A : in std_logic_vector(0 downto 0); B : in std_logic_vector(0 downto 0); C : in std_logic_vector(0 downto 0); D : in std_logic_vector(0 downto 0); vout : out std_logic_vector(0 downto 0));
end mod_user_And41;

architecture struct of mod_user_And41 is
  -- node declarations
  signal w1, wire_PJG6O : std_logic_vector(0 downto 0);
begin
  -- each submodule is wired up here.
  mod_user_AND2_MLK0l_0 : entity work.mod_user_AND2 port map (A(0 downto 0) => in1(0 downto 0), 
B(0 downto 0) => in2(0 downto 0), 
out1(0 downto 0) => w1(0 downto 0));
mod_user_AND2_LYyOa_0 : entity work.mod_user_AND2 port map (C(0 downto 0) => in1(0 downto 0), 
D(0 downto 0) => in2(0 downto 0), 
out1(0 downto 0) => wire_LMrRl(0 downto 0));
mod_user_AND2_LLeAw_0 : entity work.mod_user_AND2 port map (w1(0 downto 0) => in1(0 downto 0), 
wire_LMrRl(0 downto 0) => in2(0 downto 0), 
out1(0 downto 0) => vout(0 downto 0));
  

end struct;
-- Combinational testbench. ---------------------------------------
library STD;
use STD.textio.all;                     -- basic I/O
use STD.env.all;
library IEEE;
use IEEE.std_logic_1164.all;            -- basic logic types
use IEEE.std_logic_textio.all;          -- I/O for logic types
use ieee.numeric_std.all;  

entity mod_user_And41_tb is end entity mod_user_And41_tb;
architecture behavior of mod_user_And41_tb is

  signal A, B, C, D, vout: std_logic_vector(0 downto 0);

begin

  dut : entity work.mod_user_And41 port map (A => A, 
B => B, 
C => C, 
D => D, 
vout => vout);
  process
  begin
    -------------------------------------------------------
    A <= "1";
B <= "1";
C <= "1";
D <= "1";
wait for 99.0 ns;

    if vout /= "1" then
      report "";
      report "TestNum 1";
      report "expecting: vout = 1";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 1: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "1";
C <= "1";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 2";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 2: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "1";
C <= "0";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 3";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 3: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "1";
C <= "0";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 4";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 4: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "0";
C <= "1";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 5";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 5: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "0";
C <= "1";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 6";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 6: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "0";
C <= "0";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 7";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 7: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "1";
B <= "0";
C <= "0";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 8";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 8: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "1";
C <= "1";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 9";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 9: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "1";
C <= "1";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 10";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 10: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "1";
C <= "0";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 11";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 11: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "1";
C <= "0";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 12";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 12: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "0";
C <= "1";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 13";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 13: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "0";
C <= "1";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 14";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 14: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "0";
C <= "0";
D <= "1";
wait for 99.0 ns;

    if vout /= "0" then
      report "";
      report "TestNum 15";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 15: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= "0";
B <= "0";
C <= "0";
D <= "0";
wait for 99.0 ns;

    if vout /= "0" then
      report "//  This comment is included in vhdl test cases.";
      report "TestNum 16";
      report "expecting: vout = 0";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 16: PASSED" & LF);
    end if;


wait for 1.0 ns;

    finish(0);
  end process;
end behavior;

------------------------------------------------------------------
-- VHDL PRELUDE --------------------------------------------------

-- combinational module -------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity mod_user_And41 IS
  port (A : in std_logic; B : in std_logic; C : in std_logic; D : in std_logic; vout : out std_logic);
end mod_user_And41;

architecture struct of mod_user_And41 is
  -- node declarations
  signal w1, wire_x0620 : std_logic;

begin
  -- each submodule is wired up here.
  mod_user_AND2_nagPx : entity work.mod_user_AND2 port map (in1 => A, in2 => B, out1 => w1);
mod_user_AND2_LxVDj : entity work.mod_user_AND2 port map (in1 => C, in2 => D, out1 => wire_x0620);
mod_user_AND2_7KpMR : entity work.mod_user_AND2 port map (in1 => w1, in2 => wire_x0620, out1 => vout);
end struct;
-- Combinational testbench. ---------------------------------------
library STD;
use STD.textio.all;                     -- basic I/O
use STD.env.all;
library IEEE;
use IEEE.std_logic_1164.all;            -- basic logic types
use IEEE.std_logic_textio.all;          -- I/O for logic types
use ieee.numeric_std.all;  

entity mod_user_And41 is end entity mod_user_And41;
architecture behaviour of mod_user_And41 is

  A, B, C, D, vout: std_logic;

begin

  dut : entity work.mod_user_And41 port map (A => A, B => B, C => C, D => D, vout => vout);
  process
  begin
    -------------------------------------------------------
    A <= '1';
B <= '1';
C <= '1';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 1";
      report "expecting: vout = '1'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 1: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '1';
C <= '1';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 2";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 2: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '1';
C <= '0';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 3";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 3: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '1';
C <= '0';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 4";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 4: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '0';
C <= '1';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 5";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 5: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '0';
C <= '1';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 6";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 6: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '0';
C <= '0';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 7";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 7: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '1';
B <= '0';
C <= '0';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 8";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 8: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '1';
C <= '1';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 9";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 9: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '1';
C <= '1';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 10";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 10: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '1';
C <= '0';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 11";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 11: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '1';
C <= '0';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 12";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 12: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '0';
C <= '1';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 13";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 13: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '0';
C <= '1';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 14";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 14: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '0';
C <= '0';
D <= '1';
wait for 99.0 ns;

    if vout /= '0' then
      report ""
      report "TestNum 15";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 15: PASSED" & LF);
    end if;


wait for 1.0 ns;
A <= '0';
B <= '0';
C <= '0';
D <= '0';
wait for 99.0 ns;

    if vout /= '0' then
      report "//  This comment is included in vhdl test cases."
      report "TestNum 16";
      report "expecting: vout = '0'";
      report "got      : vout = " & to_string(vout);
      stop(-1);
    else
      write(OUTPUT, "TEST 16: PASSED" & LF);
    end if;


wait for 1.0 ns;

    finish(0);
  end process;
end behavior;

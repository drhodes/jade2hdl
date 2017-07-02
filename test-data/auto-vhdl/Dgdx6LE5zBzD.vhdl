------------------------------------------------------------------
-- VHDL PRELUDE --------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

ENTITY MOD_USER_AND2 IS
  PORT (in1 : IN std_logic;
        in2 : IN std_logic;
        out1 : OUT std_logic) ;
END ENTITY MOD_USER_AND2 ;
ARCHITECTURE Behavioral OF MOD_USER_AND2 IS
BEGIN
    out1 <= in1 AND in2 ;
END ARCHITECTURE Behavioral ;


-- END VHDL PRELUDE ----------------------------------------------
------------------------------------------------------------------
-- combinational module -------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity mod_user_AndStuff6 IS
  port (A : in std_logic; B : in std_logic; C : in std_logic; D : in std_logic; vout : out std_logic);
end mod_user_AndStuff6;

architecture struct of mod_user_AndStuff6 is
  -- node declarations
  signal wire_NBZ7l, wire_7jrrp, wire_p2N6N, wire_NJ0DB, wire_PJd2Q, wire_pMlEj, wire_GPR6o, wire_y5rYZ, w1, w2, wire_dbBl4, wire_0Aj62, wire_RB7rV, wire_rjgll : std_logic;

begin
  -- each submodule is wired up here.
  mod_user_AND2_nagPx : entity work.mod_user_AND2 port map (in1 => wire_p2N6N, in2 => wire_NBZ7l, out1 => w1);
mod_user_AND2_7KpMR : entity work.mod_user_AND2 port map (in1 => w1, in2 => w2, out1 => wire_RB7rV);
mod_user_AND2_nagNv : entity work.mod_user_AND2 port map (in1 => A, in2 => B, out1 => wire_p2N6N);
mod_user_AND2_ebE5p : entity work.mod_user_AND2 port map (in1 => C, in2 => D, out1 => wire_NBZ7l);
mod_user_AND2_RLYDn : entity work.mod_user_AND2 port map (in1 => wire_7jrrp, in2 => wire_NJ0DB, out1 => w2);
mod_user_AND2_dW2Py : entity work.mod_user_AND2 port map (in1 => B, in2 => A, out1 => wire_NJ0DB);
mod_user_AND2_Bg4Ey : entity work.mod_user_AND2 port map (in1 => D, in2 => C, out1 => wire_7jrrp);
mod_user_AND2_ZYgq4 : entity work.mod_user_AND2 port map (in1 => wire_GPR6o, in2 => wire_PJd2Q, out1 => wire_dbBl4);
mod_user_AND2_GYj14 : entity work.mod_user_AND2 port map (in1 => wire_dbBl4, in2 => wire_0Aj62, out1 => wire_rjgll);
mod_user_AND2_paEb5 : entity work.mod_user_AND2 port map (in1 => A, in2 => B, out1 => wire_GPR6o);
mod_user_AND2_QARgO : entity work.mod_user_AND2 port map (in1 => C, in2 => D, out1 => wire_PJd2Q);
mod_user_AND2_1P1dg : entity work.mod_user_AND2 port map (in1 => wire_pMlEj, in2 => wire_y5rYZ, out1 => wire_0Aj62);
mod_user_AND2_aGndy : entity work.mod_user_AND2 port map (in1 => B, in2 => A, out1 => wire_y5rYZ);
mod_user_AND2_vLVlN : entity work.mod_user_AND2 port map (in1 => D, in2 => C, out1 => wire_pMlEj);
mod_user_AND2_OgqaW : entity work.mod_user_AND2 port map (in1 => wire_RB7rV, in2 => wire_rjgll, out1 => vout);
end struct;
-- Combinational testbench. ---------------------------------------
library STD;
use STD.textio.all;                     -- basic I/O
use STD.env.all;
library IEEE;
use IEEE.std_logic_1164.all;            -- basic logic types
use IEEE.std_logic_textio.all;          -- I/O for logic types
use ieee.numeric_std.all;  

entity mod_user_AndStuff6_tb is end entity mod_user_AndStuff6_tb;
architecture behavior of mod_user_AndStuff6_tb is

  signal A, B, C, D, vout: std_logic;

begin

  dut : entity work.mod_user_AndStuff6 port map (A => A, B => B, C => C, D => D, vout => vout);
  process
  begin
    -------------------------------------------------------
    A <= '1';
B <= '1';
C <= '1';
D <= '1';
wait for 99.0 ns;

    if vout /= '1' then
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "";
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
      report "//  This comment is included in vhdl test cases.";
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

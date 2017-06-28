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

------------------------------------------------------------------                                                                  library ieee;
use ieee.std_logic_1164.all;

entity AND23 IS
  port ( a : in std_logic;
         b : in std_logic;
         c : in std_logic;
         d : in std_logic;
         vout : out std_logic);
end AND23;

architecture struct of AND23 is
  -- each node goes here.                                                                                                             signal w1, w2 : std_logic;

-- each submodule is wired up here.                                                                                                                                                                                                          
begin
  u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
  u2 : entity work.AND2 port map (in1 => c, in2 => d, out1 => w2);
  u3 : entity work.AND2 port map (in1 => w1, in2 => w2, out1 => vout);
end struct;

------------------------------------------------------------------
LIBRARY STD ;
USE STD.textio.ALL ;
USE STD.env.ALL ;
LIBRARY IEEE ;
USE IEEE.std_logic_1164.ALL ;
USE IEEE.std_logic_textio.ALL ;
USE ieee.numeric_std.ALL ;

ENTITY mod_user_UseAND2_3_tb IS
END ENTITY mod_user_UseAND2_3_tb ;

ARCHITECTURE behaviour OF mod_user_UseAND2_3_tb IS
  SIGNAL A : std_logic ;
  SIGNAL B : std_logic ;
  SIGNAL C : std_logic ;
  SIGNAL D : std_logic ;
  SIGNAL vout : std_logic ;
BEGIN
  dut :
    ENTITY work.AND23 PORT MAP (A => A, B => B, C => C, D => D, vout => vout) ;
  PROCESS IS
  BEGIN
    A <= '1' ;
    B <= '1' ;
    C <= '1' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '1' THEN
      REPORT "Test Number: 1 fails." ;
      REPORT "expecting  : '1'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '1' ;
    C <= '1' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 2 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '1' ;
    C <= '0' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 3 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '1' ;
    C <= '0' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 4 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '0' ;
    C <= '1' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 5 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '0' ;
    C <= '1' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 6 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '0' ;
    C <= '0' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 7 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '1' ;
    B <= '0' ;
    C <= '0' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 8 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '1' ;
    C <= '1' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 9 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '1' ;
    C <= '1' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 10 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '1' ;
    C <= '0' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 11 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '1' ;
    C <= '0' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 12 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '0' ;
    C <= '1' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 13 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '0' ;
    C <= '1' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 14 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '0' ;
    C <= '0' ;
    D <= '1' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 15 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    A <= '0' ;
    B <= '0' ;
    C <= '0' ;
    D <= '0' ;
    WAIT FOR 99.0 ns ;
    IF vout /= '0' THEN
      REPORT "Test Number: 16 fails." ;
      REPORT "expecting  : '0'" ;
      REPORT "got        : " & to_string (vout) ;
    ELSE
    END IF ;
    WAIT FOR 1.0 ns ;
    finish(0);
  END PROCESS ;
END ARCHITECTURE behaviour ;

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

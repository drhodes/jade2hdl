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
  port (a : in std_logic;
        b : in std_logic;
        c : in std_logic;
        d : in std_logic;
        vout : out std_logic) ;
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

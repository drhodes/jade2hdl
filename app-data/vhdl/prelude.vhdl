------------------------------------------------------------------
-- VHDL PRELUDE --------------------------------------------------

-- /modmem1 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_textio.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;
use std.textio.all;   

-- modmem2 -- this is a prototype, eventually will be generated from JADE
-- memory unit parameters, but for now, it works for testing.
entity modmem2 is
  port (ADDR_PORT1 : in std_logic_vector(1 downto 0);
        OE_PORT1 : in std_logic_vector(0 downto 0);
        WE_PORT1 : in std_logic_vector(0 downto 0);
        CLK_PORT1 : in std_logic_vector(0 downto 0);
        DATA_PORT1 : out std_logic_vector(1 downto 0));
  
end entity modmem2 ;
architecture behavioral of modmem2 is
  type rom_array is array (NATURAL range <>) of std_logic_vector(1 downto 0);
  constant data0: std_logic_vector (1 downto 0) := "00";
  constant data1: std_logic_vector (1 downto 0) := "01";
  constant data2: std_logic_vector (1 downto 0) := "10";
  constant data3: std_logic_vector (1 downto 0) := "11";
  constant rom: rom_array := (data0, data1, data2, data3);
  
begin
  process (CLK_PORT1, OE_PORT1)
    variable j: integer;
  begin
    j := to_integer(unsigned(ADDR_PORT1));
    if falling_edge(CLK_PORT1(0)) then
      DATA_PORT1 <= rom(j) when (OE_PORT1 = "1") else "UU";
    end if;   
  end process;
end architecture behavioral ;


-- modmem1 -- this is a prototype, eventually will be generated from JADE
-- memory unit parameters, but for now, it works for testing.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity modmem1 is
  port (ADDR_PORT1 : in std_logic_vector(0 downto 0);
        OE_PORT1 : in std_logic_vector(0 downto 0);
        WE_PORT1 : in std_logic_vector(0 downto 0);
        CLK_PORT1 : in std_logic_vector(0 downto 0);
        DATA_PORT1 : out std_logic_vector(0 downto 0));
  
end entity modmem1 ;
architecture behavioral of modmem1 is
  type rom_array is array (NATURAL range <>) of std_logic_vector(0 downto 0);
  constant data0: std_logic_vector (0 downto 0) := "0";
  constant data1: std_logic_vector (0 downto 0) := "1";
  constant rom: rom_array := (data0, data1);
  
begin
  process (CLK_PORT1, OE_PORT1)
    variable j: integer;
  begin
    j := to_integer(unsigned(ADDR_PORT1));
    if falling_edge(CLK_PORT1(0)) then
      DATA_PORT1 <= rom(j) when (OE_PORT1 = "1") else "U";
    end if;   
  end process;
end architecture behavioral ;

-- /gates/mod_gates_dreg ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_dreg is
  port (D : in std_logic_vector(0 downto 0);
        CLK : in std_logic_vector(0 downto 0);
        Q : out std_logic_vector(0 downto 0));
end entity mod_gates_dreg ;
architecture behavioral of mod_gates_dreg is
begin
  process
  begin
    wait until CLK="1";
    Q <= D;
  end process;
end architecture behavioral ;

-- /gates/mod_gates_tristate ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_tristate is
  port (A : in std_logic_vector(0 downto 0);
        E : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_tristate ;
architecture behavioral of mod_gates_tristate is
begin
  vout <= A when (E = "1") else "U";
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

-- /gates/nand4 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_nand4 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        c : in std_logic_vector(0 downto 0);
        d : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_nand4 ;
architecture behavioral of mod_gates_nand4 is
begin
  vout <= not (a and b and c and d);
end architecture behavioral ;


-- /gates/nor4 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_nor4 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        c : in std_logic_vector(0 downto 0);
        d : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_nor4 ;
architecture behavioral of mod_gates_nor4 is
begin
  vout <= not (a or b or c or d);
end architecture behavioral ;

-- /gates/and3 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_and3 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        c : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_and3 ;
architecture behavioral of mod_gates_and3 is
begin
  vout <= a and b and c;
end architecture behavioral ;

-- /gates/nand2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_nand2 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_nand2 ;
architecture behavioral of mod_gates_nand2 is
begin
  vout <= a nand b;
end architecture behavioral ;

-- /gates/nor2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_nor2 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0));
end entity mod_gates_nor2 ;
architecture behavioral of mod_gates_nor2 is
begin
  vout <= a nor b;
end architecture behavioral ;

-- /gates/mux2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_mux2 is
  port (D0 : in std_logic_vector(0 downto 0);
        D1 : in std_logic_vector(0 downto 0);
        S : in std_logic_vector(0 downto 0);
        Y : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_mux2 ;
architecture behavioral of mod_gates_mux2 is
begin
  Y <= D1 when (S = "1") else D0;
end architecture behavioral ;

-- /gates/mux4 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_mux4 is
  port (D0 : in std_logic_vector(0 downto 0);
        D1 : in std_logic_vector(0 downto 0);
        D2 : in std_logic_vector(0 downto 0);
        D3 : in std_logic_vector(0 downto 0);
        S : in std_logic_vector(1 downto 0);
        Y : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_mux4 ;
architecture behavioral of mod_gates_mux4 is
begin
  process (D0, D1, D2, D3, S, Y) is
  begin  
  if (S="00") then Y <= D0;
  elsif (S="01") then Y <= D1;
  elsif (S="10") then Y <= D2;
  elsif (S="11") then Y <= D3;
  else Y <= "X";
  end if;
  end process;    
end architecture behavioral ;


-- /gates/xor2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_xor2 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_xor2 ;
architecture behavioral of mod_gates_xor2 is
begin
  vout <= a xor b;
end architecture behavioral ;

-- /gates/and2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_and2 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_and2 ;
architecture behavioral of mod_gates_and2 is
begin
  vout <= a and b;
end architecture behavioral ;

-- /gates/inverter ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_inverter is
  port (a : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_inverter ;
architecture behavioral of mod_gates_inverter is
begin
  vout <= not a;
end architecture behavioral ;


-- /gates/buffer ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_buffer is
  port (a : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_buffer ;
architecture behavioral of mod_gates_buffer is
begin
  vout <= a;
end architecture behavioral ;

-- /gates/buffer_h ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_buffer_h is
  port (a : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_buffer_h ;
architecture behavioral of mod_gates_buffer_h is
begin
  vout <= a;
end architecture behavioral ;


-- /gates/or2 ------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity mod_gates_or2 is
  port (a : in std_logic_vector(0 downto 0);
        b : in std_logic_vector(0 downto 0);
        vout : out std_logic_vector(0 downto 0)) ;
end entity mod_gates_or2 ;
architecture behavioral of mod_gates_or2 is
begin
  vout <= a or b;
end architecture behavioral ;


-- END VHDL PRELUDE ----------------------------------------------
------------------------------------------------------------------

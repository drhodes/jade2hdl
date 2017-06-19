-- library IEEE;
-- use IEEE.STD_LOGIC_1164.ALL;

ENTITY AND2 IS
  PORT (in1 : IN std_logic;
        in2 : IN std_logic;
        out1 : OUT std_logic) ;
END ENTITY AND2 ;
ARCHITECTURE Behavioral OF AND2 IS
BEGIN
  PROCESS IS
  BEGIN
    out1 <= in1 AND in2 ;
  END PROCESS ;
END ARCHITECTURE Behavioral ;

entity RAWR IS
  port ( a : in std_logic;
         b : in std_logic;
         c : out std_logic);
end RAWR;

architecture struct of RAWR is
  component AND2 PORT (in1 : IN std_logic;
                       in2 : IN std_logic;
                       out1 : OUT std_logic);
  end component;

begin
  unit1 : AND2 port map (in1 => a, in2 => b, out1 => c);  
end struct;


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

library work;
use work.arith_lib.all;

entity histogram is port(
  clk, wen, ren    : in  std_logic;
  wadress, radress : in  unsigned;
  wdata            : in  std_logic_vector;
  rdata            : out std_logic_vector);
end histogram;

architecture behav of histogram is

  type bram is array(0 to 15) of std_logic_vector(wdata'high downto 0);
  signal ram0               : bram := (others => (others => '0'));
  signal w_adress, r_adress : integer range 0 to 15;
--  signal out_int            : std_logic_vector(data_in'high downto 0) := (others => '0');

begin

  r_adress <= to_integer(radress);
  rdata    <= ram0(r_adress);
  w_adress <= to_integer(wadress);

  process(clk)
  begin
    if rising_edge(clk) then
      if wen = '1' then
        ram0(w_adress) <= add(ram0(w_adress), wdata);
      end if;
      if ren = '1' then
        ram0(r_adress) <= (others => '0');
      end if;
    end if;
  end process;

end architecture;

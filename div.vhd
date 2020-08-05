library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity div is port(
  clk, data_valid_in : in  std_logic;
  node_in            : in  unsigned;
  in_a               : in  std_logic_vector(31 downto 0);
  in_b               : in  std_logic_vector(31 downto 0);
  data_valid_out     : out std_logic;
  node_out           : out unsigned;
  out_c              : out std_logic_vector(31 downto 0));
end div;

architecture Behavioral of div is

  type mantissa_pipeline is array (0 to 24) of unsigned(24 downto 0);
  type exponent_pipeline is array (0 to 24) of signed(7 downto 0);
  type node_pipeline is array(0 to 24) of unsigned(node_in'high downto 0);

  signal mantissa_a, mantissa_b, mantissa_c : mantissa_pipeline         := (others => (others => '0'));
  signal exponent                           : exponent_pipeline         := (others => (others => '0'));
  signal sign, data_valid                   : std_logic_vector(0 to 24) := (others => '0');
  signal ex                                 : signed(7 downto 0);
  signal node                               : node_pipeline             := (others => (others => '0'));

begin
  -- c = a/b

  process(clk)
    variable tmp_a, tmp_b, tmp_c : unsigned(24 downto 0) := (others => '0');
    variable tmp_e               : signed(7 downto 0)    := "00000001";
  begin
    if rising_edge(clk) then
      -- if (a >= b) {a = a - b; c = 1;} a = a << 1;

      tmp_a         := "01" & unsigned(in_a(22 downto 0));
      tmp_b         := "01" & unsigned(in_b(22 downto 0));
      mantissa_b(0) <= "01" & unsigned(in_b(22 downto 0));
      mantissa_c(0) <= (others => '0');
      sign(0)       <= in_a(31) xor in_b(31);
      node(0)       <= node_in;
      data_valid(0) <= data_valid_in;

      if tmp_a < tmp_b then
        mantissa_a(0) <= '1' & unsigned(in_a(22 downto 0)) & '0';
        exponent(0)   <= signed(in_a(30 downto 23)) - signed(in_b(30 downto 23)) - tmp_e;
      else
        mantissa_a(0) <= "01" & unsigned(in_a(22 downto 0));
        exponent(0)   <= signed(in_a(30 downto 23)) - signed(in_b(30 downto 23));
      end if;

      -- step n
      for i in 1 to 24 loop
        sign(i)       <= sign(i-1);
        mantissa_b(i) <= mantissa_b(i-1);
        exponent(i)   <= exponent(i-1);
        mantissa_c(i) <= mantissa_c(i-1);
        if mantissa_a(i-1) >= mantissa_b(i-1) or i = 1 then
          tmp_a               := mantissa_a(i-1) - mantissa_b(i-1);
          mantissa_c(i)(24-i) <= '1';
        else
          tmp_a               := mantissa_a(i-1);
          mantissa_c(i)(24-i) <= '0';
        end if;
        mantissa_a(i) <= tmp_a(23 downto 0) & '0';
        node(i)       <= node(i-1);
        data_valid(i) <= data_valid(i-1);
      end loop;

    end if;
  end process;

  ex             <= exponent(23) + 127;
  out_c          <= sign(23) & std_logic_vector(ex) & std_logic_vector(mantissa_c(23)(22 downto 0));
  node_out       <= node(23);
  data_valid_out <= data_valid(23);
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library work;
use work.arith_lib.all;

entity inst_pipe is port(
  clk, data_valid_in                         : in  std_logic;
  dt                                         : in  std_logic_vector(7 downto 0);
  Vscale                                     : in  std_logic_vector(3 downto 0);
  inst_in                                    : in  std_logic_vector(3 downto 0);
  obj_data_in, g_in, f_in                    : in  std_logic_vector(31 downto 0);
  term_0_in, term_1_in, term_2_in, ID_in     : in  unsigned(3 downto 0);
  term_0_v, term_1_v, term_2_v               : in  std_logic_vector(19 downto 0);
  int0_in, int1_in, int2_in                  : in  std_logic_vector(19 downto 0);
  gtime                                      : in  unsigned(31 downto 0);
  data_valid_out                             : out std_logic;
  obj_data_out                               : out std_logic_vector(31 downto 0);
  term_0_out, term_1_out, term_2_out, ID_out : out unsigned(3 downto 0);
  term_0_i, term_1_i, term_2_i               : out std_logic_vector(31 downto 0);
  term_0_g, term_1_g, term_2_g               : out std_logic_vector(31 downto 0));
end inst_pipe;

architecture Behavioral of inst_pipe is

  constant pipe_length    : natural := 2;
  constant fixed_width    : natural := 20;
  constant mantissa_width : natural := 25;

  type float is array(1 to pipe_length) of std_logic_vector(31 downto 0);
  type fixed is array(1 to pipe_length) of signed(fixed_width-1 downto 0);
  type mantissa is array(1 to pipe_length) of unsigned(mantissa_width-1 downto 0);
  type exponent is array(1 to pipe_length) of signed(8 downto 0);
  type int8 is array(1 to pipe_length) of signed(7 downto 0);
  type inst_type is array(1 to pipe_length) of std_logic_vector(3 downto 0);
  type node_type is array(1 to pipe_length) of unsigned(3 downto 0);

  signal inst                         : inst_type := (others => (others => '0'));  -- input
  signal obj_data, G, F               : float     := (others => (others => '0'));  -- input
  signal V0, V1, V2, int0, int1, int2 : fixed     := (others => (others => '0'));  -- input

  -- signal int10, int11, int12     : fixed     := (others => (others => '0'));  -- intern
  signal node0, node1, node2, ID : node_type                          := (others => (others => '0'));  -- intern
  signal data_valid              : std_logic_vector(1 to pipe_length) := (others => '0');  -- intern

  signal i0, g0, i1, g1, i2, g2 : float := (others => (others => '0'));  -- output
  signal debug0, debug1, debug2 : float := (others => (others => '0'));  -- output

begin

  term_0_i   <= i0(pipe_length); term_0_g <= g0(pipe_length);
  term_1_i   <= i1(pipe_length); term_1_g <= g1(pipe_length);
  term_2_i   <= i2(pipe_length); term_2_g <= g2(pipe_length);
  term_0_out <= node0(pipe_length); term_1_out <= node1(pipe_length); term_2_out <= node2(pipe_length);

  ID_out       <= ID(pipe_length);
  obj_data_out <= obj_data(pipe_length); data_valid_out <= data_valid(pipe_length);

  process(clk)
    variable tmp42u                         : unsigned(41 downto 0) := (others => '0');
    variable tmp42s                         : signed(41 downto 0)   := (others => '0');
    variable k, m                           : natural;
    variable tmp_f0, tmp_f1, tmp_f2, tmp_f3 : std_logic_vector(31 downto 0);
    variable tmp_i32                        : std_logic_vector(31 downto 0);
    variable tmp_i0, tmp_i1                 : signed(19 downto 0);
    variable tmp_12                         : std_logic_vector(11 downto 0);
    variable tmp_l0                         : signed(39 downto 0);
  begin
    if rising_edge(clk) then
      -- header: input all
      -- inst(1) <= inst_in; obj_data(1) <= obj_data_in; G(1) <= g_in; f(1) <= f_in; V0(1) <= signed(term_0_v); V1(1) <= signed(term_1_v); V2(1) <= signed(term_2_v); int0(1) <= signed(int0_in); int1(1) <= signed(int1_in); int2(1) <= signed(int2_in);
      node0(1) <= term_0_in; node1(1) <= term_1_in; node2(1) <= term_2_in; data_valid(1) <= data_valid_in; ID(1) <= ID_in;
      -- default push all through
      for j in 2 to pipe_length loop
        inst(j) <= inst(j-1); data_valid(j) <= data_valid(j-1); obj_data(j) <= obj_data(j-1);  -- G(j) <= G(j-1); F(j) <= F(j-1);
        ID(j)   <= ID(j-1); node0(j) <= node0(j-1); node1(j) <= node1(j-1); node2(j) <= node2(j-1);
        -- V0(j) <= V0(j-1); V1(j) <= V1(j-1); V2(j) <= V2(j-1); int0(j) <= int0(j-1); int1(j) <= int1(j-1); int2(j) <= int2(j-1); V01(j) <= V01(j-1); to_exp(j) <= to_exp(j-1); f10(j) <= f10(j-1); f11(j) <= f11(j-1); f12(j) <= f12(j-1); f13(j) <= f13(j-1); f14(j) <= f14(j-1); f15(j) <= f15(j-1); f16(j) <= f16(j-1); f17(j) <= f17(j-1); f18(j) <= f18(j-1); f19(j) <= f19(j-1);
        i0(j)   <= i0(j-1); g0(j) <= g0(j-1); i1(j) <= i1(j-1); g1(j) <= g1(j-1); i2(j) <= i2(j-1); g2(j) <= g2(j-1);
      -- V02(j) <= V02(j-1); V12(j) <= V12(j-1); Vi(j) <= Vi(j-1); int10(j) <= int10(j-1); int11(j) <= int11(j-1);
      end loop;  -- end header
-------------------------------------------------------------------------------------------------------------------------------------
      k      := 1;
      tmp_i0 := signed(term_1_v) - signed(term_0_v);
      tmp_i1 := (others => '0');
      tmp_f2 := (others => '0');
      tmp_f3 := (others => '0');

      I2(k) <= (others => '0');
      G2(k) <= (others => '0');
      case inst_in is
        when "0000" =>                              -- R
          tmp_f0 := to_float(tmp_i0, x"FF");
          tmp_f0 := multiply(G_in, tmp_f0);         -- I
          I0(k)  <= tmp_f0 xor x"80000000";
          I1(k)  <= tmp_f0;
          G0(k)  <= G_in xor x"80000000";
          G1(k)  <= G_in xor x"80000000";
        when "0001" =>                              -- Vdc
          tmp_i1 := tmp_i0 - signed(int0_in);
          tmp_f1 := to_float(tmp_i1, x"FF");
          tmp_f0 := multiply(G_in, tmp_f1);         -- I
          I0(k)  <= tmp_f0 xor x"80000000";
          I1(k)  <= tmp_f0;
          G0(k)  <= G_in xor x"80000000";
          G1(k)  <= G_in xor x"80000000";
        when "0010" =>                              -- C
          tmp_i1 := tmp_i0 - signed(obj_data_in(19 downto 0));
          tmp_f1 := to_float(tmp_i1, x"FF");
          tmp_f2 := G_in(31) & std_logic_vector(
            signed(G_in(30 downto 23)) - signed(dt)) & G_in(22 downto 0);
          tmp_f0      := multiply(tmp_f2, tmp_f1);  -- I
          I0(k)       <= tmp_f0 xor x"80000000";
          I1(k)       <= tmp_f0;
          G0(k)       <= tmp_f2 xor x"80000000";
          G1(k)       <= tmp_f2 xor x"80000000";
          obj_data(k) <= x"000" & std_logic_vector(tmp_i0);
        when "0011" =>                              -- L
        when "0100" =>                              -- D
          tmp_l0 := (39 downto 20 => tmp_i0(19)) & tmp_i0;
          tmp_l0 := shift_left(tmp_l0, to_integer(unsigned(Vscale)));
          if tmp_l0 > x"000006FFFF" then
            tmp_i0 := x"6FFFF";
            if tmp_l0 < x"00000EFFFF" then
              tmp_l0 := tmp_l0 - x"000006FFFF";
              tmp_i1 := tmp_l0(19 downto 0);
              tmp_f2 := to_float(tmp_i1, x"FF");
            end if;
          elsif tmp_l0 < x"FFFFF8000" then
            tmp_i0 := x"80000";
          else
            tmp_i0 := tmp_l0(19 downto 0);
          end if;
          tmp_i0 := tmp_i0 xor x"80000";
          tmp_f0 := exp(std_logic_vector(tmp_i0));
          tmp_f0 := multiply(G_in, tmp_f0);
          tmp_f1 := G_in xor x"80000000";
          tmp_f1 := add(tmp_f0, tmp_f1); -- f1 er strøm eks. seriemotstand
          tmp_f3 := x"41900000";
          tmp_f3 := multiply(tmp_f0, tmp_f3); -- f3 er konduktans
          tmp_f2 := multiply(tmp_f2, tmp_f3); -- f2 erstrøm pga seriem
          tmp_f2 := add(tmp_f2, tmp_f1);
          G0(k)  <= tmp_f3 xor x"80000000";
          G1(k)  <= tmp_f3 xor x"80000000";
          I0(k)  <= tmp_f2 xor x"80000000";
          I1(k)  <= tmp_f2;
        when "0101" =>                              -- NPN
        when "0110" =>                              -- PNP
        when "1001" =>                              -- Vsin
          tmp42u  := gtime(31 downto 8) * unsigned(int1_in(19 downto 2)) + (unsigned(int0_in) & "00");
          tmp_i32 := sin(std_logic_vector(tmp42u(21 downto 10)));
          tmp42s  := signed(tmp_i32(31 downto 8)) * signed(int2_in(19 downto 2));
          tmp_i1  := tmp_i0 - signed(tmp42s(41 downto 22));
          tmp_f1  := to_float(tmp_i1, x"FF");
          tmp_f0  := multiply(G_in, tmp_f1);
          I0(k)   <= tmp_f0 xor x"80000000";
          I1(k)   <= tmp_f0;
          G0(k)   <= G_in xor x"80000000";
          G1(k)   <= G_in xor x"80000000";
        when others =>
      end case;

    end if;
  end process;

end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.arith_lib.all;

entity t_inst_pipe is
end t_inst_pipe;

architecture Behavioral of t_inst_pipe is

  component inst_pipe port(
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
  end component;

  component histogram port(
    clk, wen, ren    : in  std_logic;
    wadress, radress : in  unsigned;
    wdata            : in  std_logic_vector;
    rdata            : out std_logic_vector);
  end component;

  component div port(
    clk, data_valid_in : in  std_logic;
    node_in            : in  unsigned;
    in_a               : in  std_logic_vector(31 downto 0);
    in_b               : in  std_logic_vector(31 downto 0);
    data_valid_out     : out std_logic;
    node_out           : out unsigned;
    out_c              : out std_logic_vector(31 downto 0));
  end component;

  type gi_vector is array(0 to 15) of std_logic_vector(31 downto 0);
  type v_vector is array(0 to 15) of std_logic_vector(19 downto 0);
  type test_set is array(0 to 15) of std_logic_vector(31 downto 0);

  constant halfperiod                               : time                          := 5 ns;
  signal clk, rst                                   : std_logic                     := '1';
  signal data_valid_in_pipe, data_valid_out_pipe    : std_logic                     := '0';
  signal data_valid_div, data_valid_div_delayed     : std_logic                     := '0';
  signal read_hist, update_object, update_time      : std_logic                     := '0';
  signal dt                                         : std_logic_vector(7 downto 0)  := x"EA";
  signal Vscale                                     : std_logic_vector(3 downto 0)  := (others => '0');
  signal inst_in                                    : std_logic_vector(3 downto 0)  := (others => '0');
  signal obj_data_in, g_in, f_in                    : std_logic_vector(31 downto 0) := (others => '0');
  signal term_0_v, term_1_v, term_2_v               : std_logic_vector(19 downto 0) := (others => '0');
  signal int0_in, int1_in, int2_in                  : std_logic_vector(19 downto 0) := (others => '0');
  signal obj_data_out                               : std_logic_vector(31 downto 0) := (others => '0');
  signal term_0_i, term_1_i, term_2_i               : std_logic_vector(31 downto 0) := (others => '0');
  signal term_0_g, term_1_g, term_2_g               : std_logic_vector(31 downto 0) := (others => '0');
  signal term_0_in, term_1_in, term_2_in, ID_in     : unsigned(3 downto 0)          := (others => '0');
  signal term_0_out, term_1_out, term_2_out, ID_out : unsigned(3 downto 0)          := (others => '0');
  signal radress, rsumadress, divadress             : unsigned(3 downto 0)          := (others => '0');
  signal g_vector, i_vector                         : gi_vector                     := (others => (others => '0'));
  signal v0, v1, v2                                 : v_vector                      := (others => (others => '0'));
  signal iter_num, iter_num_d                       : unsigned(9 downto 0)          := (others => '0');

  signal hist_i_sum, hist_0_i, hist_1_i : std_logic_vector(31 downto 0) := (others => '0');
  signal hist_g_sum, hist_0_g, hist_1_g : std_logic_vector(31 downto 0) := (others => '0');
  signal hist_div_float                 : std_logic_vector(31 downto 0) := (others => '0');
  signal hist_div_fixed, max_diff       : signed(19 downto 0)           := (others => '0');
  signal gtime                          : unsigned(31 downto 0)         := (others => '0');

--  signal this_g0, this_g1, this_g2, this_i0, this_i1, this_i2 : std_logic_vector(31 downto 0) := (others => '0');

  -- R   : 0000, _   , 1/R, _, node+, node-  , _        , Vdc, _   , _
  -- Vdc : 0001, _   , 1/R, _, node+, node-  , _        , Vdc, _   , _
  -- C   : 0010, V   , C  , _, node+, node-  , _        , _  , _   , _
  -- L   : 0011, I   , 1/L, _, node+, node-  , _        , _  , _   , _
  -- D   : 0100, _   , G  , _, node+, node-  , _        , _  , _   , _
  -- NPN : 0101, _   , G  , _, Basis, Emitter, Kollektor, _  , Beta, _
  -- PNP : 0110, _   , G  , _, Basis, Emitter, Kollektor, _  , Beta, _
  -- Vsin: 1001, Fase, 1/R, _, node+, node-  , _        , Vdc, Frek, Mag

  -- Bits:    4,   32,  32,    16,      16,        16,
  -- Nul : 0000, _   , _  , _    , _      , _        , _   
  -- R   : 0001, _   , 1/R, node+, node-  , _        , _   
  -- C   : 0010, V   , C  , node+, node-  , _        , _   
  -- L   : 0011, I   , 1/L, node+, node-  , _        , _   
  -- D   : 0100, _   , G  , node+, node-  , _        , _   
  -- NPN : 0101, _   , G  , Basis, Emitter, Kollektor, Beta
  -- PNP : 0110, _   , G  , Basis, Emitter, Kollektor, Beta
  -- Vdc : 1000, _   , G  , node+, node-  , _        , _   
  -- Vsin: 1001, Fase, G  , node+, node-  , Frek     , Mag
  -- Vsq : 1010, Fase, G  , node+, node-  , Frek     , Mag
  -- Idc : 1100, _   , G  , node+, node-  , _        , _   
  -- Isin: 1101, Fase, G  , node+, node-  , Frek     , Mag
  -- Isq : 1110, Fase, G  , node+, node-  , Frek     , Mag

  -- Full cisc
  -- Nul : 0000
  -- R   : 0001 , G=1/R  , node#0 , node#1
  -- C   : 0010 , G=C/dt , node#0 , node#1   , &this     , size_of_this , this
  -- L   : 0011 , G=L*dt , node#0 , node#1   , &this     , size_of_this , this
  -- D   : 0100 , G=Is   , node#N , node#P
  -- NPN : 0101 , G=Is   , basis# , emitter# , kollektor , beta
  -- PNP : 0110 , G=Is   , basis# , emitter# , kollektor , beta
  -- Vdc : 1000 , G=1/R  , node#0 , node#1   , V
  -- Vsin: 1001 , G=1/R  , node#0 , node#1   , V         , Frek         , Fase
  -- Vsq : 1010 , G=1/R  , node#0 , node#1   , V         , Frek         , Fase
  -- Idc : 1100 , G=Is   , node#0 , node#1   , V
  -- Isin: 1101 , G=Is   , node#0 , node#1   , V         , Frek         , Fase
  -- Isq : 1110 , G=Is   , node#0 , node#1   , V         , Frek         , Fase

  type netlist_element is record
    inst                   : std_logic_vector(3 downto 0);
    obj_data, g, f         : std_logic_vector(31 downto 0);
    node_0, node_1, node_2 : unsigned(3 downto 0);
    int_0, int_1, int_2    : std_logic_vector(19 downto 0);
  end record;

  type netlist_type is array(0 to 15) of netlist_element;

  signal netlist : netlist_type := (
    ("1001", x"00000000", x"3F800000", x"00000000", x"0", x"1", x"0",
     x"00000", x"000D8", x"7FFFF"),     -- Vsin
    ("1001", x"00000000", x"3F800000", x"00000000", x"3", x"0", x"0",
     x"00000", x"000D8", x"7FFFF"),     -- Vsin
    ("0000", x"00000000", x"39000000", x"00000000", x"0", x"4", x"0",
     x"00000", x"00000", x"00000"),     -- R1 8192 ohm
    ("0010", x"00000000", x"37800000", x"00000000", x"4", x"0", x"0",
     x"00000", x"00000", x"00000"),     -- C1 16 uF
    ("0100", x"00000000", x"30800000", x"00000000", x"4", x"1", x"0",
     x"00000", x"00000", x"00000"),     -- D1
    ("0100", x"00000000", x"30800000", x"00000000", x"4", x"3", x"0",
     x"00000", x"00000", x"00000"),     -- D1

    others => ("0000", x"00000000", x"00000000", x"00000000", x"0", x"0", x"0",
               x"00000", x"00000", x"00000")
    );

  type iterate_state_type is (wait_state, update_objects);
  signal iterate_state : iterate_state_type := wait_state;

begin

  inst_pipe0 : inst_pipe port map(
    clk            => clk,
    data_valid_in  => data_valid_in_pipe,
    dt             => dt,
    Vscale         => Vscale,
    -- instruction spesific
    inst_in        => inst_in,
    obj_data_in    => obj_data_in,
    ID_in          => ID_in,
    g_in           => g_in,
    f_in           => f_in,
    term_0_in      => term_0_in,
    term_1_in      => term_1_in,
    term_2_in      => term_2_in,
    term_0_v       => term_0_v,
    term_1_v       => term_1_v,
    term_2_v       => term_2_v,
    int0_in        => int0_in,
    int1_in        => int1_in,
    int2_in        => int2_in,
    gtime          => gtime,
    -- outputs
    data_valid_out => data_valid_out_pipe,
    obj_data_out   => obj_data_out,
    ID_out         => ID_out,
    term_0_out     => term_0_out,
    term_1_out     => term_1_out,
    term_2_out     => term_2_out,
    term_0_i       => term_0_i,
    term_0_g       => term_0_g,
    term_1_i       => term_1_i,
    term_1_g       => term_1_g,
    term_2_i       => term_2_i,
    term_2_g       => term_2_g
    );

  hist0 : histogram port map(
    clk, data_valid_out_pipe, read_hist, term_0_out, radress, term_0_i, hist_0_i);
  hist1 : histogram port map(
    clk, data_valid_out_pipe, read_hist, term_1_out, radress, term_1_i, hist_1_i);

  hist10 : histogram port map(
    clk, data_valid_out_pipe, read_hist, term_0_out, radress, term_0_g, hist_0_g);
  hist11 : histogram port map(
    clk, data_valid_out_pipe, read_hist, term_1_out, radress, term_1_g, hist_1_g);

  div0 : div port map(
    clk            => clk,
    data_valid_in  => read_hist,
    node_in        => radress,
    in_a           => hist_i_sum,
    in_b           => hist_g_sum,
    data_valid_out => data_valid_div,
    node_out       => divadress,
    out_c          => hist_div_float);

  hist_div_fixed <= to_fixed(hist_div_float, x"14", x"01");
  hist_i_sum     <= add(hist_0_i, hist_1_i);
  hist_g_sum     <= add(hist_0_g, hist_1_g);

  clk <= not clk after halfperiod;
  rst <= '0'     after 10*halfperiod;

  push_inst : process
  begin
    data_valid_in_pipe <= '0';          -- start
    wait until rising_edge(clk);
    data_valid_in_pipe <= '1';          -- start
--    dt                 <= x"EA";        -- 2^-22 s
    Vscale             <= x"5";

    for i in 0 to 15 loop
      inst_in     <= netlist(i).inst;
      obj_data_in <= netlist(i).obj_data;
      ID_in       <= to_unsigned(i, 4);
      g_in        <= netlist(i).g;
      f_in        <= netlist(i).f;
      term_0_in   <= netlist(i).node_0;
      term_1_in   <= netlist(i).node_1;
      term_2_in   <= netlist(i).node_2;
      term_0_v    <= v0(to_integer(netlist(i).node_0));
      term_1_v    <= v1(to_integer(netlist(i).node_1));
      term_2_v    <= v2(to_integer(netlist(i).node_2));
      int0_in     <= netlist(i).int_0;
      int1_in     <= netlist(i).int_1;
      int2_in     <= netlist(i).int_2;
      wait until rising_edge(clk);
    end loop;
    data_valid_in_pipe <= '0';
    wait until falling_edge(data_valid_div);
  end process;

  process
  begin
    wait until falling_edge(data_valid_out_pipe);
    --wait until rising_edge(clk);

    read_hist <= '1';
    for i in 0 to 15 loop
      radress <= to_unsigned(i, 4);
      wait until rising_edge(clk);
    end loop;
    read_hist <= '0';

--    wait;
  end process;

  process(clk)
    variable adress : integer range 0 to 15;
    variable tmp    : signed(19 downto 0);
  begin
    if rising_edge(clk) and divadress /= x"0" then
      data_valid_div_delayed <= data_valid_div;
      update_object          <= '0';
      if data_valid_div = '1' then
        adress     := to_integer(divadress);
        tmp        := signed(v0(adress)) + hist_div_fixed;
        v0(adress) <= std_logic_vector(tmp);
        v1(adress) <= std_logic_vector(tmp);
        v2(adress) <= std_logic_vector(tmp);
        tmp        := abs(hist_div_fixed);
        if max_diff < tmp and tmp /= x"40000" then
          max_diff <= tmp;
        end if;
        if divadress = x"F" then
          iter_num <= iter_num + 1;
          if iter_num > 100 then
            if signed(dt) > - 22 then
              dt       <= std_logic_vector(unsigned(dt) - 1);
              iter_num <= (others => '0');
            else
              update_object <= '1';
              iter_num      <= (others => '0');
              iter_num_d    <= iter_num;
            end if;
          end if;
          if max_diff < x"00004" and iter_num > 0 then
            iter_num   <= (others => '0');
            iter_num_d <= iter_num;
            if iter_num < 8 and signed(dt) < - 14 then
              dt <= std_logic_vector(unsigned(dt) + 1);
            end if;
            update_object <= '1';
          end if;
        end if;
      else
        max_diff <= (others => '0');
      end if;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      update_time <= '0';
      case iterate_state is
        when wait_state =>
          if update_object = '1' then
            iterate_state <= update_objects;
          end if;
        when update_objects =>
          if data_valid_out_pipe = '1' then
            netlist(to_integer(ID_out)).obj_data <= obj_data_out;
            if ID_out = x"F" then
              iterate_state <= wait_state;
              gtime         <= gtime + shift_right(x"20000000", to_integer(unsigned(dt xor x"FF")));
              update_time   <= '1';
            end if;
          end if;
        when others =>
      end case;
    end if;
  end process;

  process
    file result_file   : text open write_mode is "C:\sim\sim.vcd";
    variable file_line : line;
    variable iteration : integer := 0;
  begin
    write(file_line, "$version" & LF & " Chilly v0" & LF & "$end" & LF & "$timescale" & LF & " 1 ns" & LF & "$end" & LF);
    write(file_line, "$scope module circuit $end" & LF);
    write(file_line, "$var reg 20 ! iter#[9:0] $end" & LF);
    write(file_line, "$var reg 20 & node1[19:0] $end" & LF);
    write(file_line, "$var reg 20 # deltaT[7:0] $end" & LF);
    write(file_line, "$var reg 20 $ node3[19:0] $end" & LF);
    write(file_line, "$var reg 20 / node4[19:0] $end" & LF);
    write(file_line, "$enddefinitions $end" & LF);
    writeline(result_file, file_line);
    loop
      write(file_line, "#" & integer'image(to_integer(gtime)));
      writeline(result_file, file_line);
      wait until rising_edge(update_time);

      write(file_line, 'b');
      write(file_line, to_bitVector(std_logic_vector(iter_num_d)));
      write(file_line, " !" & LF);
      write(file_line, 'b');
      write(file_line, to_bitVector(v0(1)));
      write(file_line, " &" & LF);
      write(file_line, 'b');
      write(file_line, to_bitVector(std_logic_vector(dt)));
      write(file_line, " #" & LF);
      write(file_line, 'b');
      write(file_line, to_bitVector(v0(3)));
      write(file_line, " $" & LF);
      write(file_line, 'b');
      write(file_line, to_bitVector(v0(4)));
      write(file_line, " /" & LF);

      writeline(result_file, file_line);
    end loop;
  end process;

end Behavioral;

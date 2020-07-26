library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

-- PlayCity expansion by TotO,
-- implemented in VHDL by Slingshot
-- https://www.cpcwiki.eu/index.php/PlayCity

entity playcity is
port (
	clock   : in  std_logic;
	reset   : in  std_logic;
	ena     : in  std_logic;

	phi_n   : in  std_logic;
	phi_en  : in  std_logic;
	addr    : in  std_logic_vector(15 downto 0);
	din     : in  std_logic_vector( 7 downto 0);
	dout    : out std_logic_vector( 7 downto 0);
	cpu_di  : in  std_logic_vector( 7 downto 0); -- for CTC RETI detection
	m1_n    : in  std_logic;
	iorq_n  : in  std_logic;
	rd_n    : in  std_logic;
	wr_n    : in  std_logic;
	int_n   : out std_logic;
	nmi     : out std_logic;

	cursor  : in  std_logic;

	audio_l : out std_logic_vector( 9 downto 0);
	audio_r : out std_logic_vector( 9 downto 0)
);
end playcity;

architecture struct of playcity is
	signal soft_reset        : std_logic;
	signal reset_n           : std_logic;

	signal ctc_ce_n          : std_logic;
	signal ctc_counter_0_to  : std_logic;
	signal ctc_counter_0_to_d: std_logic;
	signal ctc_counter_3_trg : std_logic;
	signal ctc_do            : std_logic_vector(7 downto 0);
	signal ctc_int_n         : std_logic;

	signal ay_clock          : std_logic;
	signal ay_clock_d        : std_logic;
	signal ay_ce             : std_logic;
	signal ay_sel            : std_logic;
	signal ay1_bdir          : std_logic;
	signal ay1_bc2           : std_logic;
	signal ay2_bdir          : std_logic;
	signal ay2_bc2           : std_logic;

begin

-- F8FF
soft_reset <= '1' when ena = '1' and iorq_n = '0' and m1_n = '1' and addr(15 downto 0) = x"F8FF" else '0';
reset_n <= not (soft_reset or reset) and ena;

-- F880-F883
ctc_ce_n <= '0' when ena = '1' and iorq_n = '0' and m1_n = '1' and addr(15 downto 2) = x"F88"&"00" else '1';
dout <= ctc_do;
int_n <= ctc_int_n;

-- F[8-9]8[4/8]
ay_sel <= '1' when ena = '1' and iorq_n = '0' and wr_n = '0' and addr(15 downto 9) = x"F"&"100" and addr(7 downto 4) = x"8" else '0'; 
ay1_bdir <= '1' when ay_sel = '1' and addr(3 downto 2) = "01" else '0';
ay1_bc2  <= not addr(8);
ay2_bdir <= '1' when ay_sel = '1' and addr(3 downto 2) = "10" else '0';
ay2_bc2  <= not addr(8);

-- Z80-CTC (MK3882)
z80ctc : entity work.z80ctc_top
port map (
	clock     => clock,
	clock_ena => phi_en,
	reset     => not reset_n,
	din       => din,
	cpu_din   => cpu_di,
	dout      => ctc_do,
	ce_n      => ctc_ce_n,
	cs        => addr(1 downto 0),
	m1_n      => m1_n,
	iorq_n    => iorq_n,
	rd_n      => rd_n,
	int_n     => ctc_int_n,
	trg0      => phi_n,
	to0       => ctc_counter_0_to,
	trg1      => cursor,
	to1       => nmi,
	trg2      => phi_n,
	to2       => ctc_counter_3_trg,
	trg3      => ctc_counter_3_trg
);

ay_clock <= ctc_counter_0_to or phi_n;
-- ay clock enable generator (from CTC TO0)
process(clock) begin
	if rising_edge(clock) then
		ay_ce <= '0';
		ay_clock_d <= ay_clock;
		if ay_clock_d = '0' and ay_clock = '1' then
			ay_ce <= '1';
		end if;
	end if;
end process;

-- AY-3-8910 # 1
ay_3_8910_1 : entity work.YM2149
port map(
  -- data bus
  I_DA       => din,       -- in  std_logic_vector(7 downto 0); -- pin 37 to 30
  O_DA       => open,      -- out std_logic_vector(7 downto 0); -- pin 37 to 30
  O_DA_OE_L  => open,      -- out std_logic;
  -- control
  I_A9_L     => '0',       -- in  std_logic; -- pin 24
  I_A8       => '1',       -- in  std_logic; -- pin 25
  I_BDIR     => ay1_bdir,  -- in  std_logic; -- pin 27
  I_BC2      => ay1_bc2,   -- in  std_logic; -- pin 28
  I_BC1      => '0',       -- in  std_logic; -- pin 29
  I_SEL_L    => '0',       -- in  std_logic;

  O_AUDIO_L  => audio_l,   -- out std_logic_vector(9 downto 0);

  -- port a
  I_IOA      => (others => '0'), -- in  std_logic_vector(7 downto 0); -- pin 21 to 14
  O_IOA      => open,            -- out std_logic_vector(7 downto 0); -- pin 21 to 14
  O_IOA_OE_L => open,            -- out std_logic;
  -- port b
  I_IOB      => (others => '0'), -- in  std_logic_vector(7 downto 0); -- pin 13 to 6
  O_IOB      => open,            -- out std_logic_vector(7 downto 0); -- pin 13 to 6
  O_IOB_OE_L => open,            -- out std_logic;

  ENA        => ay_ce,     -- in  std_logic; -- clock enable for higher speed operation
  RESET_L    => reset_n,   -- in  std_logic;
  CLK        => clock      -- in  std_logic
);

-- AY-3-8910 # 2
ay_3_8910_2 : entity work.YM2149
port map(
  -- data bus
  I_DA       => din,       -- in  std_logic_vector(7 downto 0); -- pin 37 to 30
  O_DA       => open,      -- out std_logic_vector(7 downto 0); -- pin 37 to 30
  O_DA_OE_L  => open,      -- out std_logic;
  -- control
  I_A9_L     => '0',       -- in  std_logic; -- pin 24
  I_A8       => '1',       -- in  std_logic; -- pin 25
  I_BDIR     => ay2_bdir,  -- in  std_logic; -- pin 27
  I_BC2      => ay2_bc2,   -- in  std_logic; -- pin 28
  I_BC1      => '0',       -- in  std_logic; -- pin 29
  I_SEL_L    => '0',       -- in  std_logic;

  O_AUDIO_L  => audio_r,   -- out std_logic_vector(9 downto 0);

  -- port a
  I_IOA      => (others => '0'), -- in  std_logic_vector(7 downto 0); -- pin 21 to 14
  O_IOA      => open,            -- out std_logic_vector(7 downto 0); -- pin 21 to 14
  O_IOA_OE_L => open,            -- out std_logic;
  -- port b
  I_IOB      => (others => '0'), -- in  std_logic_vector(7 downto 0); -- pin 13 to 6
  O_IOB      => open,            -- out std_logic_vector(7 downto 0); -- pin 13 to 6
  O_IOB_OE_L => open,            -- out std_logic;

  ENA        => ay_ce,     -- in  std_logic; -- clock enable for higher speed operation
  RESET_L    => reset_n,   -- in  std_logic;
  CLK        => clock      -- in  std_logic
);

end struct;

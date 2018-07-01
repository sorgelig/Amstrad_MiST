
create_clock -name {clk_27} -period 37.037 -waveform { 0.000 18.500 } [get_ports {CLOCK_27[0]}]
create_clock -name {SPI_SCK}  -period 41.666 -waveform { 20.8 41.666 } [get_ports {SPI_SCK}] 

derive_pll_clocks

derive_clock_uncertainty;

set_input_delay  -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[1]}] -max  6.4 [get_ports SDRAM_DQ[*]]
set_input_delay  -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[1]}] -min  3.2 [get_ports SDRAM_DQ[*]]
set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[1]}] -max  1.5 [get_ports SDRAM_*]
set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[1]}] -min -0.8 [get_ports SDRAM_*]

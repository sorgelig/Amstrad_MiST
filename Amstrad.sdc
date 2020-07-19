# Clock constraints

create_clock -name "CLOCK_27" -period 37.037 [get_ports {CLOCK_27[0]}]
create_clock -name {SPI_SCK}  -period 41.666 -waveform { 20.8 41.666 } [get_ports {SPI_SCK}]

# Automatically constrain PLL and other generated clocks
derive_pll_clocks -create_base_clocks

# Automatically calculate clock uncertainty to jitter and other effects.
derive_clock_uncertainty

# Clock groups
set_clock_groups -asynchronous -group [get_clocks {SPI_SCK}] -group [get_clocks {pll|altpll_component|auto_generated|pll1|clk[*]}]

# SDRAM delays
set_input_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -reference_pin [get_ports {SDRAM_CLK}] -max 6.4 [get_ports SDRAM_DQ[*]]
set_input_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -reference_pin [get_ports {SDRAM_CLK}] -min 3.2 [get_ports SDRAM_DQ[*]]

set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -reference_pin [get_ports {SDRAM_CLK}] -max 1.5 [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]
set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -reference_pin [get_ports {SDRAM_CLK}] -min -0.8 [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]

# Some relaxed constrain to the VGA pins. The signals should arrive together, the delay is not really important.
set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -max 0 [get_ports {VGA_*}]
set_output_delay -clock [get_clocks {pll|altpll_component|auto_generated|pll1|clk[0]}] -min -5 [get_ports {VGA_*}]
set_multicycle_path -to [get_ports {VGA_*}] -setup 3
set_multicycle_path -to [get_ports {VGA_*}] -hold 2

# T80 just cannot run in 64 MHz, but it's safe to allow 2 clock cycles for the paths in it
set_multicycle_path -from {Amstrad_motherboard:motherboard|T80pa:CPU|T80:u0|*} -setup 2
set_multicycle_path -from {Amstrad_motherboard:motherboard|T80pa:CPU|T80:u0|*} -hold 1

set_multicycle_path -to {u765:u765|fdc.i_rpm_time[*][*][*]} -setup 4
set_multicycle_path -to {u765:u765|fdc.i_rpm_time[*][*][*]} -hold 3

# False paths

# Don't bother optimizing sigma_delta_dac
set_false_path -to {sigma_delta_dac:*}

set_false_path -to [get_ports {SDRAM_CLK}]
set_false_path -to [get_ports {AUDIO_L}]
set_false_path -to [get_ports {AUDIO_R}]
set_false_path -to [get_ports {LED}]
#set_false_path -from [get_ports {UART_RX}]
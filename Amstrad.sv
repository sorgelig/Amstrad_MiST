//============================================================================
//  Amstrad CPC 6128
// 
//  Port to MiST/MiSTer.
//  Copyright (C) 2018 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module Amstrad
(
	input         SPI_DI,
	input         SPI_SCK,
	input         CONF_DATA0,
	input         SPI_SS2,
	input         SPI_SS3,
	input   [1:0] CLOCK_27,
	output        AUDIO_L,
	output        AUDIO_R,
	output        SDRAM_DQMH,
	output        SDRAM_DQML,
	output        SDRAM_CKE,
	output        SDRAM_nCS,
	output        SDRAM_nWE,
	output        SDRAM_nRAS,
	output        SDRAM_nCAS,
	output        SDRAM_CLK,
	output        LED,
	output        SPI_DO,
	output        VGA_HS,
	output        VGA_VS,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output  [5:0] VGA_B,
	output  [5:0] VGA_G,
	output  [5:0] VGA_R
);

//////////////////////////////////////////////////////////////////////////

assign LED = ~ioctl_download;

localparam CONF_STR = {
	"AMSTRAD;;",
	"S,DSK,Mount Disk;",
	"O9A,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%;",
	"OBD,Colors,All,Mono-G,Mono-R,Mono-B,Mono-W;",
	"O1,Model,Amstrad,Schneider;",
	"O2,CRTC,1,0;",
	"O3,Wait states,Quick,Slow;",
	"T0,Reset;"
};

//////////////////////////////////////////////////////////////////////////

wire clk_vid;
wire clk_sys;
wire locked;

pll pll
(
	.inclk0(CLOCK_27),
	.c0(clk_vid),
	.c1(clk_sys),
	.locked(locked)
);

reg ce_4n, ce_ref, ce_boot;
reg ce_4p, ce_u765;
reg ce_16;
always @(negedge clk_sys) begin
	reg [5:0] div4  = 0;
	reg [3:0] div16 = 0;

	div4 <= div4 + 1'd1;
	if(div4 == 27) div4 <= 0;

	ce_4n   <= (div4 == 0);
	ce_ref  <= (div4 == 0);
	ce_boot <= (div4 == 0);

	ce_4p   <= (div4 == 14);
	ce_u765 <= (div4 == 14);

	div16 <= div16 + 1'd1;
	if(div16 == 6) div16 <= 0;

	ce_16  <= !div16;
end

reg ce_vid;
always @(negedge clk_vid) begin
	reg [2:0] div16 = 0;

	div16 <= div16 + 1'd1;
	ce_vid <= !div16;
end

//////////////////////////////////////////////////////////////////////////

wire [31:0] sd_lba;
wire        sd_rd;
wire        sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire        img_mounted;
wire [63:0] img_size;
wire        img_readonly;

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index;

wire        ps2_clk;
wire        ps2_data;

wire  [1:0] buttons;
wire  [5:0] joy1;
wire  [5:0] joy2;
wire [31:0] status;

wire        scandoubler_disable;
wire        forced_scandoubler = ~scandoubler_disable;

mist_io #(.STRLEN($size(CONF_STR)>>3)) mist_io
(
	.clk_sys(clk_sys),
	.conf_str(CONF_STR),

	.SPI_SCK(SPI_SCK),
	.CONF_DATA0(CONF_DATA0),
	.SPI_SS2(SPI_SS2),
	.SPI_DI(SPI_DI),
	.SPI_DO(SPI_DO),

	.img_mounted(img_mounted),
	.img_size(img_size),
	.sd_conf(0),
	.sd_sdhc(1),
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),

	.ps2_kbd_clk(ps2_clk),
	.ps2_kbd_data(ps2_data),

	.joystick_0(joy1),
	.joystick_1(joy2),

	.buttons(buttons),
	.status(status),
	.scandoubler_disable(scandoubler_disable),

	.ioctl_ce(ce_boot),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index)
);

wire        rom_download = !ioctl_index & ioctl_download;
wire        reset = status[0] | buttons[1] | rom_download;

reg         boot_wr = 0;
reg  [22:0] boot_a;
reg   [7:0] boot_dout;

always_comb begin
	boot_wr = rom_download & ioctl_wr;
	boot_dout = ioctl_dout;

	boot_a[13:0] = ioctl_addr[13:0];
	boot_a[22:14] = '1;

	case(ioctl_addr[24:14])
			0: boot_a[22:14] = 9'h000;
			1: boot_a[22:14] = 9'h100;
			2: boot_a[22:14] = 9'h107;
	default: boot_wr = 0;
	endcase
end

//////////////////////////////////////////////////////////////////////////

wire        ram_w;
wire        ram_r;
wire [22:0] ram_a;
wire  [7:0] ram_din;
wire  [7:0] ram_dout;

wire        zram_rd;
wire  [7:0] zram_dout;
wire [15:0] zram_addr;

assign SDRAM_CKE = 1;
assign SDRAM_CLK = clk_sys;

zsdram zsdram
(
	.init(~locked),
	.clk(clk_sys),
	.clkref(ce_ref),

	.oe  (reset ? 1'b0      : ram_r),
	.we  (reset ? boot_wr   : ram_w),
	.addr(reset ? boot_a    : ram_a),
	.din (reset ? boot_dout : ram_din),
	.dout(ram_dout),

	.sd_cs(SDRAM_nCS),
	.sd_we(SDRAM_nWE),
	.sd_ras(SDRAM_nRAS),
	.sd_cas(SDRAM_nCAS),
	.sd_dqm({SDRAM_DQMH, SDRAM_DQML}),
	.sd_addr(SDRAM_A),
	.sd_ba(SDRAM_BA),
	.sd_data(SDRAM_DQ),

	.zram_oe(zram_rd & ~reset),
	.zram_addr(zram_addr),
	.zram_dout(zram_dout)
);

reg [7:0] rom_mask;
always_comb begin
	casex(ram_a[22:14])
	  'h0XX: rom_mask = 0;
	  'h100: rom_mask = 0;
	  'h107: rom_mask = 0;
	default: rom_mask = 'hFF;
	endcase
end

//////////////////////////////////////////////////////////////////////////

wire [3:0] fdc_sel;
wire       fdc_wr;
wire       fdc_rd;
wire [7:0] fdc_din;

reg  [7:0] fdc_dout;
always_comb begin
	case({fdc_rd,fdc_sel[3:1]})
		'b1_000: fdc_dout = 8'h00;     // motor read 
		'b1_010: fdc_dout = u765_dout; // u765 read 
		default: fdc_dout = 8'hFF;
	endcase
end

wire [7:0] u765_dout;
wire       u765_sel = (fdc_sel[3:1] == 'b010);

reg u765_ready = 0;
always @(posedge clk_sys) if(img_mounted) u765_ready <= |img_size;

u765 u765
(
	.reset(status[0]),

	.clk_sys(clk_sys),
	.ce(ce_u765),

	.a0(fdc_sel[0]),
	.ready(u765_ready),
	.nRD(~(u765_sel & fdc_rd)),
	.nWR(~(u765_sel & fdc_wr)),
	.din(fdc_din),
	.dout(u765_dout),

	.img_mounted(img_mounted),
	.img_size(img_size[19:0]),
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr)
);

//////////////////////////////////////////////////////////////////////////

wire  [3:0] ppi_jumpers = {2'b11, ~status[1], 1'b1};
wire        crtc_type = ~status[2];
wire        wait_time = status[3];

Amstrad_motherboard motherboard
(
	.RESET_n(~reset),
	.CLK4MHz(clk_sys & ce_4p),
	.nCLK4MHz(clk_sys & ce_4n),

	.PS2_CLK(ps2_clk),
	.PS2_DATA(ps2_data),

	.CLK16MHz(clk_sys & ce_16),

	.ga_shunt(wait_time),
	.ppi_jumpers(ppi_jumpers),
	.crtc_type(crtc_type),

	.JOYSTICK1(joy1),
	.JOYSTICK2(joy2),

	.fdc_sel(fdc_sel),
	.fdc_wr(fdc_wr),
	.fdc_rd(fdc_rd),
	.fdc_din(fdc_dout),
	.fdc_dout(fdc_din),

	.audio_AB(audio_l),
	.audio_BC(audio_r),

	.VMODE(vmode),
	.HBLANK(hbl),
	.VBLANK(vbl),
	.HSYNC(hs),
	.VSYNC(vs),
	.RED(r),
	.GREEN(g),
	.BLUE(b),

	.ram_R(ram_r),
	.ram_W(ram_w),
	.ram_A(ram_a),
	.ram_Din(ram_dout | rom_mask),
	.ram_Dout(ram_din),

	.zram_rd(zram_rd),
	.zram_din(zram_dout),
	.zram_addr(zram_addr)
);

//////////////////////////////////////////////////////////////////////

wire [1:0] b, g, r;
wire       hs, vs, hbl, vbl;

color_mix color_mix
(
	.clk_vid(clk_vid),
	.ce_pix(ce_vid),
	.mono(status[13:11]),

	.HSync_in(hs),
	.VSync_in(vs),
	.HBlank_in(hbl),
	.VBlank_in(vbl),
	.B_in(b),
	.G_in(g),
	.R_in(r),

	.HSync_out(HS),
	.VSync_out(VS),
	.HBlank_out(HBL),
	.VBlank_out(VBL),
	.B_out(mb),
	.G_out(mg),
	.R_out(mr)
);

wire [7:0] mb, mg, mr;
wire       HS, VS, HBL, VBL;

wire [1:0] vmode;
reg        ce_pix;
always @(posedge clk_vid) begin
	reg       old_vs;
	reg [1:0] pxsz;
	reg [1:0] cnt;
	
	ce_pix <= 0;
	if(ce_vid) begin
		cnt <= cnt + 1'd1;
		if(cnt == pxsz) begin
			cnt    <= 0;
			ce_pix <= 1;
		end
		
		old_vs <= VS;
		if(old_vs & ~VS) begin
			cnt <= 0;
			pxsz <= {hq2x,hq2x} >> vmode;
		end
	end
end

video_cleaner video_cleaner
(
	.clk_vid(clk_vid),
	.ce_pix(ce_pix),

	.B(mb),
	.G(mg),
	.R(mr),

	.HSync(HS),
	.VSync(VS),
	.HBlank(HBL),
	.VBlank(VBL),

	.VGA_R(R),
	.VGA_G(G),
	.VGA_B(B),
	.VGA_VS(VSync),
	.VGA_HS(HSync),
	.HBlank_out(HBlank),
	.VBlank_out(VBlank)
);

wire [7:0] B, G, R;
wire       HSync, VSync, HBlank, VBlank;

wire [1:0] scale = status[10:9];
wire       hq2x = (scale == 1);

video_mixer #(800) video_mixer
(
	.*,

	.clk_sys(clk_vid),
	.ce_pix_out(),

	.scanlines({scale==3, scale==2}),
	.scandoubler(scale || forced_scandoubler),
	.mono(0),

	.VGA_R(MR),
	.VGA_G(MG),
	.VGA_B(MB),
	.VGA_VS(MVS),
	.VGA_HS(MHS)
);

wire       VGA_DE;
wire [7:0] MB, MG, MR;
wire       MHS, MVS;

osd osd
(
	.clk_sys(clk_vid),
	.SPI_SCK(SPI_SCK),
	.SPI_SS3(SPI_SS3),
	.SPI_DI(SPI_DI),
	.HSync(MHS),
	.VSync(MVS),
	.B_in(VGA_DE ? MB[7:2] : 6'd0),
	.G_in(VGA_DE ? MG[7:2] : 6'd0),
	.R_in(VGA_DE ? MR[7:2] : 6'd0),
	.B_out(VGA_B),
	.G_out(VGA_G),
	.R_out(VGA_R)
);

assign VGA_HS = forced_scandoubler ? ~MHS : ~(MVS ^ MHS);
assign VGA_VS = forced_scandoubler ? ~MVS : 1'b1;

//////////////////////////////////////////////////////////////////////

wire [7:0] audio_l, audio_r;

sigma_delta_dac #(7) dac_l
(
	.CLK(clk_sys & ce_16),
	.RESET(reset),
	.DACin(audio_l),
	.DACout(AUDIO_L)
);

sigma_delta_dac #(7) dac_r
(
	.CLK(clk_sys & ce_16),
	.RESET(reset),
	.DACin(audio_r),
	.DACout(AUDIO_R)
);

endmodule

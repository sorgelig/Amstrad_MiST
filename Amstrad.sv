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
	output        UART_TX,
	input         UART_RX,
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

assign LED = ~mf2_en & ~ioctl_download & ~(tape_motor & tape_motor_led);

`include "build_id.v"
localparam CONF_STR = {
	"AMSTRAD;;",
	"S0,DSK,Mount Disk A:;",
	"S1,DSK,Mount Disk B:;",
	"F,E??,Load expansion;",
	"F,CDT,Load;",
	"P1,Video & Audio;",
	"P2,Controls;",
	"P3,System;",
	"P1O9A,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	"P1OBD,Display,Color(GA),Color(ASIC),Green,Amber,Cyan,White;",
	"P1O2,CRTC,Type 1,Type 0;",
	"P1O3,Sync signals,Original,Filtered;",
	"P1OK,Tape sound,Disabled,Enabled;",
	"P1OL,Sound output,Stereo,Mono;",
	"P1OO,Playcity,Disabled,Enabled;",
	"P2OI,Joysticks swap,No,Yes;",
	"P2OJ,Mouse,Disabled,Enabled;",
	"P2OM,Right Shift,Backslash,Shift;",
	"P2ON,Keypad,Numbers,Symbols;",
	"P3OEF,Multiface 2,Enabled,Hidden,Disabled;",
	"P3O6,CPU timings,Original,Fast;",
	"P3OGH,FDC,Original,Fast,Disabled;",
	"P3O5,Distributor,Amstrad,Schneider;",
	"P3O4,Model,CPC 6128,CPC 664;",
	"P3T0,Reset & apply model;",
	"V,",`BUILD_DATE
};

wire [1:0] st_scanlines = status[10:9];
wire [2:0] st_palette = status[13:11];
wire       st_sync_filter = status[3];
wire       st_joyswap = status[18];
wire       st_nowait = status[6];
wire       st_cpc664 = status[4];
wire       st_crtc = status[2];
wire       st_distributor = status[5];
wire [1:0] st_fdc = status[17:16];
wire       st_tape_sound = status[20];
wire       st_stereo = ~status[21];
wire [1:0] st_mf2 = status[15:14];
wire       st_mouse_en = status[19];
wire       st_right_shift_mod = status[22];
wire       st_keypad_mod = status[23];
wire       st_playcity_ena = status[24];

//////////////////////////////////////////////////////////////////////////

wire clk_sys;
wire locked;

pll pll
(
	.inclk0(CLOCK_27),
	.c0(clk_sys),
	.locked(locked)
);

reg ce_ref, ce_u765;
reg ce_boot;
reg ce_16;
always @(posedge clk_sys) begin
	reg [3:0] div = 0;

	div     <= div + 1'd1;

	ce_ref  <= !div;
	ce_boot <= !div;

	ce_u765 <= !div[2:0]; //8 MHz
	ce_16   <= !div[1:0]; //16 MHz
end

//////////////////////////////////////////////////////////////////////////

wire [31:0] sd_lba;
wire  [1:0] sd_rd;
wire  [1:0] sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire  [1:0] img_mounted;
wire [63:0] img_size;
wire        img_readonly;

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index;
wire [23:0] ioctl_file_ext;

wire        key_strobe;
wire        key_pressed;
wire        key_extended;
wire  [7:0] key_code;

wire  [8:0] mouse_x;
wire  [8:0] mouse_y;
wire  [7:0] mouse_flags;
wire        mouse_strobe;

wire [24:0] ps2_mouse = { mouse_strobe_level, mouse_y[7:0], mouse_x[7:0], mouse_flags };
reg         mouse_strobe_level;
always @(posedge clk_sys) if (mouse_strobe) mouse_strobe_level <= ~mouse_strobe_level;

wire  [1:0] buttons;
wire  [6:0] joy1;
wire  [6:0] joy2;
wire [31:0] status;

wire        scandoubler_disable;
wire        ypbpr;
wire        no_csync;

user_io #(.STRLEN($size(CONF_STR)>>3)) user_io
(
	.clk_sys(clk_sys),
	.clk_sd(clk_sys),
	.conf_str(CONF_STR),

	.SPI_CLK(SPI_SCK),
	.SPI_SS_IO(CONF_DATA0),
	.SPI_MOSI(SPI_DI),
	.SPI_MISO(SPI_DO),

	.img_mounted(img_mounted),
	.img_size(img_size),
	.sd_conf(0),
	.sd_sdhc(1),
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_din(sd_buff_din),
	.sd_dout(sd_buff_dout),
	.sd_dout_strobe(sd_buff_wr),

	.key_strobe(key_strobe),
	.key_code(key_code),
	.key_pressed(key_pressed),
	.key_extended(key_extended),

	.mouse_x(mouse_x),
	.mouse_y(mouse_y),
	.mouse_flags(mouse_flags),
	.mouse_strobe(mouse_strobe),

	.joystick_0(joy1),
	.joystick_1(joy2),

	.buttons(buttons),
	.status(status),
	.scandoubler_disable(scandoubler_disable),
	.ypbpr(ypbpr),
	.no_csync(no_csync)
);

data_io data_io
(
	.clk_sys(clk_sys),

	.SPI_SCK(SPI_SCK),
	.SPI_SS2(SPI_SS2),
	.SPI_DI(SPI_DI),

	.clkref_n(~ce_boot),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_fileext(ioctl_file_ext)
);

wire        rom_download  = ioctl_download && (ioctl_index == 8'd0);
wire        ext_download  = ioctl_download && (ioctl_index == 8'd3);
wire        tape_download = ioctl_download && (ioctl_index == 8'd4);

reg         boot_wr = 0;
reg  [22:0] boot_a;
reg   [1:0] boot_bank;
reg   [7:0] boot_dout;

reg  [22:0] tape_addr;
reg         tape_wr = 0;
reg         tape_ack;

reg [255:0] rom_map = 0;

reg [8:0] page = 0;
always @(posedge clk_sys) begin
	reg combo = 0;
	reg old_download;
	reg old_wr;
	reg old_tape_ack;

	old_wr <= ioctl_wr;
	if((rom_download | ext_download) & old_wr & ~ioctl_wr) begin
		if(boot_a[22]) rom_map[boot_a[21:14]] <= 1;
		if(combo && &boot_a[13:0]) begin
			combo <= 0;
			page  <= 9'h1FF;
		end
	end

	old_download <= ioctl_download;
	if(~old_download & ioctl_download & ext_download) begin
		page <= 9'h1EE; // some unused page for malformed file extension
		combo <= 0;
		if(ioctl_file_ext[15:8] >= "0" && ioctl_file_ext[15:8] <= "9") page[7:4] <= ioctl_file_ext[11:8];
		if(ioctl_file_ext[15:8] >= "A" && ioctl_file_ext[15:8] <= "F") page[7:4] <= ioctl_file_ext[11:8]+4'd9;
		if(ioctl_file_ext[7:0]  >= "0" && ioctl_file_ext[7:0]  <= "9") page[3:0] <= ioctl_file_ext[3:0];
		if(ioctl_file_ext[7:0]  >= "A" && ioctl_file_ext[7:0]  <= "F") page[3:0] <= ioctl_file_ext[3:0] +4'd9;
		if(ioctl_file_ext[15:0] == "ZZ") page <= 0;
		if(ioctl_file_ext[15:0] == "Z0") begin page <= 0; combo <= 1; end
	end

	old_tape_ack <= tape_ack;
	if(tape_download) begin
		if(old_tape_ack ^ tape_ack) tape_wr <= 0;
		if(~old_wr & ioctl_wr) tape_wr <= 1'b1;
	end
end

// A 8MB bank is split to 2 halves
// Fist 4 MB is OS ROM + RAM pages + MF2 ROM
// Second 4 MB is max. 256 pages of HI rom
always_comb begin
	boot_wr = (rom_download | ext_download) & ioctl_wr;
	boot_dout = ioctl_dout;

	boot_a[13:0] = ioctl_addr[13:0];
	boot_a[22:14] = '1;
	boot_bank = 0;
	tape_addr = tape_play_addr;

	if (tape_download) begin
		tape_addr = ioctl_addr[22:0];
	end
	else if(ext_download) begin
		boot_a[22]    = page[8];
		boot_a[21:14] = page[7:0] + ioctl_addr[21:14];
		boot_bank     = { 1'b0, model };
	end
	else begin
		case(ioctl_addr[24:14])
				0,4: boot_a[22:14] = 9'h000; //OS
				1,5: boot_a[22:14] = 9'h100; //BASIC
				2,6: boot_a[22:14] = 9'h107; //AMSDOS
				3,7: boot_a[22:14] = 9'h0ff; //MF2
		  default: boot_wr = 0;
		endcase

		case(ioctl_addr[24:14])
			 0,1,2,3: boot_bank = 0; //CPC6128
			 4,5,6,7: boot_bank = 1; //CPC664
			 default: boot_bank = 0;
		endcase
	end
end

//////////////////////////////////////////////////////////////////////////

wire        mem_wr;
wire        mem_rd;
wire [22:0] ram_a;
wire  [7:0] ram_dout;

wire [15:0] vram_dout;
wire [14:0] vram_addr;

assign SDRAM_CLK = clk_sys;

sdram sdram
(
	.*,

	.init(~locked),
	.clk(clk_sys),
	.clkref(ce_ref),

	.oe  (reset ? 1'b0      : mem_rd & ~mf2_ram_en),
	.we  (reset ? boot_wr   : mem_wr & ~mf2_ram_en & ~mf2_rom_en),
	.addr(reset ? boot_a    : mf2_rom_en ? { 9'h0ff, cpu_addr[13:0] }: ram_a),
	.bank(reset ? boot_bank : { 1'b0, model } ),
	.din (reset ? boot_dout : cpu_dout),
	.dout(ram_dout),

	.vram_addr({2'b10,vram_addr,1'b0}),
	.vram_dout(vram_dout),

	.tape_addr(tape_addr),
	.tape_din(boot_dout),
	.tape_dout(tape_dout),
	.tape_wr(tape_wr),
	.tape_rd(tape_rd),
	.tape_ack(tape_ack)
);

reg model = 0;
reg reset;

always @(posedge clk_sys) begin
	if(reset) model <= st_cpc664;
	reset <= status[0] | buttons[1] | rom_download | ext_download;
end

////////////////////// CDT playback ///////////////////////////////

wire        tape_read;
wire        tape_data_req;
reg         tape_data_ack;
reg         tape_reset;
reg         tape_rd;
reg   [7:0] tape_dout;
reg  [22:0] tape_play_addr;
reg  [22:0] tape_last_addr;

always @(posedge clk_sys) begin
    reg old_tape_ack;

    if (reset) begin
        tape_play_addr <= 1;
        tape_last_addr <= 0;
        tape_rd <= 0;
        tape_reset <= 1;
    end else begin
        old_tape_ack <= tape_ack;
        tape_reset <= 0;
        if (tape_download) begin
            tape_play_addr <= 0;
            tape_last_addr <= tape_addr;
            tape_reset <= 1;
        end
        if (!ioctl_download && tape_rd && tape_ack ^ old_tape_ack) begin
            tape_data_ack <= tape_data_req;
            tape_rd <= 0;
            tape_play_addr <= tape_play_addr + 1'd1;
        end else if (!ioctl_download && tape_play_addr <= tape_last_addr && !tape_rd && (tape_data_req ^ tape_data_ack)) begin
            tape_rd <= 1;
        end
    end
end

reg [24:0] tape_motor_cnt;
wire       tape_motor_led = tape_motor_cnt[24] ? tape_motor_cnt[23:16] > tape_motor_cnt[7:0] : tape_motor_cnt[23:16] <= tape_motor_cnt[7:0];
always @(posedge clk_sys) tape_motor_cnt <= tape_motor_cnt + 1'd1;

tzxplayer tzxplayer
(
    .clk(clk_sys),
    .ce(1),
    .restart_tape(tape_reset),
    .host_tap_in(tape_dout),
    .tzx_req(tape_data_req),
    .tzx_ack(tape_data_ack),
    .cass_read(tape_read),
    .cass_motor(tape_motor)
);

//////////////////////////////////////////////////////////////////////////

wire [3:0] fdc_sel = {cpu_addr[10],cpu_addr[8],cpu_addr[7],cpu_addr[0]};

wire [7:0] fdc_dout = (u765_sel & io_rd) ? u765_dout : 8'hFF;

reg motor = 0;
always @(posedge clk_sys) begin
	reg old_wr;
	
	old_wr <= io_wr;
	if(~old_wr && io_wr && !fdc_sel[3:1]) begin
		motor <= cpu_dout[0];
	end
end

wire [7:0] u765_dout;
wire       u765_sel = (fdc_sel[3:1] == 'b010) & ~st_fdc[1];

reg  [1:0] u765_ready = 0;
always @(posedge clk_sys) if(img_mounted[0]) u765_ready[0] <= |img_size;
always @(posedge clk_sys) if(img_mounted[1]) u765_ready[1] <= |img_size;

u765 u765
(
	.reset(status[0]),

	.clk_sys(clk_sys),
	.ce(ce_u765),

	.fast(st_fdc[0]),

	.a0(fdc_sel[0]),
	.ready(u765_ready),
	.motor({ motor, motor }),
	.available(2'b11),
	.nRD(~(u765_sel & io_rd)),
	.nWR(~(u765_sel & io_wr)),
	.din(cpu_dout),
	.dout(u765_dout),

	.img_mounted(img_mounted),
	.img_size(img_size[31:0]),
	.img_wp(0),
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr)
);

/////////////////////////////////////////////////////////////////////////
///////////////////////////// Multiface Two /////////////////////////////
/////////////////////////////////////////////////////////////////////////

wire  [7:0] mf2_dout = (mf2_ram_en & mem_rd) ? mf2_ram_out : 8'hFF;

reg         mf2_nmi = 0;
reg         mf2_en = 0;
reg         mf2_hidden = 0;
reg   [7:0] mf2_ram[8192];
wire        mf2_ram_en = mf2_en & cpu_addr[15:13] == 3'b001;
wire        mf2_rom_en = mf2_en & cpu_addr[15:13] == 3'b000;
reg   [4:0] mf2_pen_index;
reg   [3:0] mf2_crtc_register;
wire [12:0] mf2_store_addr;
reg  [12:0] mf2_ram_a;
reg         mf2_ram_we;
reg   [7:0] mf2_ram_in, mf2_ram_out;

always_comb begin
	casex({ cpu_addr[15:8], cpu_dout[7:6] })
		{ 8'h7f, 2'b00 }: mf2_store_addr = 13'h1fcf;  // pen index
		{ 8'h7f, 2'b01 }: mf2_store_addr = mf2_pen_index[4] ? 13'h1fdf : { 9'h1f9, mf2_pen_index[3:0] }; // border/pen color
		{ 8'h7f, 2'b10 }: mf2_store_addr = 13'h1fef; // screen mode
		{ 8'h7f, 2'b11 }: mf2_store_addr = 13'h1fff; // banking
		{ 8'hbc, 2'bXX }: mf2_store_addr = 13'h1cff; // CRTC register select
		{ 8'hbd, 2'bXX }: mf2_store_addr = { 9'h1db, mf2_crtc_register[3:0] }; // CRTC register value
		{ 8'hf7, 2'bXX }: mf2_store_addr = 13'h17ff; //8255
		{ 8'hdf, 2'bXX }: mf2_store_addr = 13'h1aac; //upper rom
		default: mf2_store_addr = 0;
	endcase
end

always @(posedge clk_sys) begin
	if (mf2_ram_we) begin
		mf2_ram[mf2_ram_a] <= mf2_ram_in;
		mf2_ram_out <= mf2_ram_in;
	end else
		mf2_ram_out <= mf2_ram[mf2_ram_a];
end

always @(posedge clk_sys) begin
	reg old_key_nmi, old_m1, old_io_wr;

	old_key_nmi <= key_nmi;
	old_m1 <= m1;
	old_io_wr <= io_wr;

	if (reset) begin
		mf2_en <= 0;
		mf2_hidden <= |st_mf2;
		mf2_nmi <= 0;
	end

	if(~old_key_nmi & key_nmi & ~mf2_en & ~st_mf2[1]) mf2_nmi <= 1;
	if (mf2_nmi & ~old_m1 & m1 & (cpu_addr == 'h66)) begin
		mf2_en <= 1;
		mf2_hidden <= 0;
		mf2_nmi <= 0;
	end
	if (mf2_en & ~old_m1 & m1 & cpu_addr == 'h65) begin
		mf2_hidden <= 1;
	end

	if (~old_io_wr & io_wr & cpu_addr[15:2] == 14'b11111110111010) begin //fee8/feea
		mf2_en <= ~cpu_addr[1] & ~mf2_hidden & ~st_mf2[1];
	end else if (~old_io_wr & io_wr & |mf2_store_addr[12:0]) begin //store hw register in MF2 RAM
		if (cpu_addr[15:8] == 8'h7f & cpu_dout[7:6] == 2'b00) mf2_pen_index <= cpu_dout[4:0];
		if (cpu_addr[15:8] == 8'hbc) mf2_crtc_register <= cpu_dout[3:0];
		mf2_ram_a <= mf2_store_addr;
		mf2_ram_in <= cpu_dout;
		mf2_ram_we <= 1;
	end else if (mem_wr & mf2_ram_en) begin //normal MF2 RAM write
		mf2_ram_a <= ram_a[12:0];
		mf2_ram_in <= cpu_dout;
		mf2_ram_we <= 1;
	end else begin //MF2 RAM read
		mf2_ram_a <= ram_a[12:0];
		mf2_ram_we <=0;
	end

end

//////////////////////////////////////////////////////////////////////

wire  [7:0] playcity_dout;
wire  [9:0] playcity_audio_l, playcity_audio_r;
wire        playcity_int_n, playcity_nmi;

playcity playcity
(
	.clock(clk_sys),
	.reset(reset),
	.ena(st_playcity_ena),
	.phi_n(phi_n),
	.phi_en(phi_en_n),
	.addr(cpu_addr),
	.din(cpu_dout),
	.dout(playcity_dout),
	.cpu_di(cpu_din),
	.m1_n(~m1),
	.iorq_n(~iorq),
	.rd_n(~rd),
	.wr_n(~wr),
	.int_n(playcity_int_n),
	.nmi(playcity_nmi),
	.cursor(cursor),
	.audio_l(playcity_audio_l),
	.audio_r(playcity_audio_r)
);

//////////////////////////////////////////////////////////////////////

wire mouse_rd = io_rd & st_mouse_en;

wire [7:0] kmouse_dout;
kempston_mouse kmouse
(
	.clk_sys(clk_sys),
	.reset(reset),
	.ps2_mouse(ps2_mouse),
	.addr({cpu_addr[0], ~cpu_addr[4] & ~cpu_addr[10] & mouse_rd, cpu_addr[8]}),
	.dout(kmouse_dout)
);

wire [7:0] smouse_dout;
symbiface_mouse smouse
(
	.clk_sys(clk_sys),
	.reset(reset),
	.ps2_mouse(ps2_mouse),
	.sel((cpu_addr == 16'hFD10) & mouse_rd),
	.dout(smouse_dout)
);

wire [7:0] mmouse_dout;
multiplay_mouse mmouse
(
	.clk_sys(clk_sys),
	.reset(reset),
	.ps2_mouse(ps2_mouse),
	.sel((cpu_addr[15:4] == 12'hF99) & ~cpu_addr[3] & mouse_rd),
	.addr(cpu_addr[2:0]),
	.dout(mmouse_dout)
);

/////////////////////////////////////////////////////////////////////////

wire [15:0] cpu_addr;
wire  [7:0] cpu_dout;
wire        phi_n, phi_en_n;
wire        m1, key_nmi;
wire        rd, wr, iorq;
wire        field;
wire        cursor;
wire  [9:0] Fn;
wire        tape_rec;
wire  [1:0] b, g, r;
wire        hs, vs, hbl, vbl;

wire  [9:0] audio_l, audio_r;

wire  [7:0] cpu_din = ram_dout & mf2_dout & fdc_dout & kmouse_dout & smouse_dout & mmouse_dout & playcity_dout;
wire        NMI = playcity_nmi | mf2_nmi;
wire        IRQ = ~playcity_int_n;

wire io_rd = rd & iorq;
wire io_wr = wr & iorq;

Amstrad_motherboard motherboard
(
	.reset(reset),
	.clk(clk_sys),
	.ce_16(ce_16),

	.key_strobe(key_strobe),
	.key_pressed(key_pressed),
	.key_extended(key_extended),
	.key_code(key_code),
	.right_shift_mod(st_right_shift_mod),
	.keypad_mod(st_keypad_mod),
	.Fn(Fn),

	.no_wait(st_nowait & ~tape_motor),
	.ppi_jumpers({2'b11, ~st_distributor, 1'b1}),
	.crtc_type(~st_crtc),
	.sync_filter(st_sync_filter),

	.joy1(st_joyswap ? joy2 : joy1),
	.joy2(st_joyswap ? joy1 : joy2),

	.tape_in(tape_play),
	.tape_out(tape_rec),
	.tape_motor(tape_motor),

	.stereo(st_stereo),
	.audio_l(audio_l),
	.audio_r(audio_r),

	.hblank(hbl),
	.vblank(vbl),
	.hsync(hs),
	.vsync(vs),
	.red(r),
	.green(g),
	.blue(b),
	.field(field),

	.vram_din(vram_dout),
	.vram_addr(vram_addr),

	.rom_map(rom_map),
	.ram64k(model),
	.mem_rd(mem_rd),
	.mem_wr(mem_wr),
	.mem_addr(ram_a),

	.phi_n(phi_n),
	.phi_en_n(phi_en_n),
	.cpu_addr(cpu_addr),
	.cpu_dout(cpu_dout),
	.cpu_din(cpu_din),
	.iorq(iorq),
	.rd(rd),
	.wr(wr),
	.m1(m1),
	.nmi(NMI),
	.irq(IRQ),
	.cursor(cursor),

	.key_nmi(key_nmi)
);

//////////////////////////////////////////////////////////////////////

wire [7:0] B, G, R;
wire       HSync, VSync, HBlank, VBlank;
wire       blank = HBlank | VBlank;

color_mix color_mix
(
	.clk_vid(clk_sys),
	.ce_pix(ce_16),
	.mix(st_palette),

	.HSync_in(hs),
	.VSync_in(vs),
	.HBlank_in(hbl),
	.VBlank_in(vbl),
	.B_in(b),
	.G_in(g),
	.R_in(r),

	.HSync_out(HSync),
	.VSync_out(VSync),
	.HBlank_out(HBlank),
	.VBlank_out(VBlank),
	.B_out(B),
	.G_out(G),
	.R_out(R)
);

mist_video #(.SD_HCNT_WIDTH(10), .OSD_X_OFFSET(10'd18)) mist_video (
	.clk_sys     ( clk_sys    ),

	// OSD SPI interface
	.SPI_SCK     ( SPI_SCK    ),
	.SPI_SS3     ( SPI_SS3    ),
	.SPI_DI      ( SPI_DI     ),

	// scanlines (00-none 01-25% 10-50% 11-75%)
	.scanlines   ( st_scanlines  ),

	// non-scandoubled pixel clock divider 0 - clk_sys/4, 1 - clk_sys/2
	.ce_divider  ( 1'b0       ),

	// 0 = HVSync 31KHz, 1 = CSync 15KHz
	.scandoubler_disable ( scandoubler_disable ),
	// disable csync without scandoubler
	.no_csync    ( no_csync   ),
	// YPbPr always uses composite sync
	.ypbpr       ( ypbpr      ),
	// Rotate OSD [0] - rotate [1] - left or right
	.rotate      ( 2'b00      ),
	// composite-like blending
	.blend       ( 1'b0       ),

	// video in
	.R           ( blank ? 6'd0 : R[7:2] ),
	.G           ( blank ? 6'd0 : G[7:2] ),
	.B           ( blank ? 6'd0 : B[7:2] ),

	.HSync       ( ~HSync     ),
	.VSync       ( ~VSync     ),

	// MiST video output signals
	.VGA_R       ( VGA_R      ),
	.VGA_G       ( VGA_G      ),
	.VGA_B       ( VGA_B      ),
	.VGA_VS      ( VGA_VS     ),
	.VGA_HS      ( VGA_HS     )
);

//////////////////////////////////////////////////////////////////////

sigma_delta_dac #(10) dac_l
(
	.CLK(clk_sys),
	.RESET(reset),
	.DACin({1'b0, audio_l} + {1'b0, playcity_audio_l} + (st_tape_sound ? {tape_rec, tape_play, 6'd0} : 0)),
	.DACout(AUDIO_L)
);

sigma_delta_dac #(10) dac_r
(
	.CLK(clk_sys),
	.RESET(reset),
	.DACin({1'b0, audio_r} + {1'b0, playcity_audio_r} + (st_tape_sound ? {tape_rec, tape_play, 6'd0} : 0)),
	.DACout(AUDIO_R)
);

//////////////////////////////////////////////////////////////////////

localparam ear_autostop_time = 5 * 64000000; // 5 sec
reg        ear_input_detected;
integer    ear_autostop_cnt = 0;
reg        UART_RXd, UART_RXd2, tape_in;
reg        tape_play;
wire       tape_motor;
assign     UART_TX = tape_motor;

// detect tape input from UART, switch to external tape input for 5 secs
// if signal transition detected
always @(posedge clk_sys) begin
	UART_RXd <= UART_RX;
	UART_RXd2 <= UART_RXd;
	tape_in <= UART_RXd2;

	if (ear_autostop_cnt != 0) ear_autostop_cnt <= ear_autostop_cnt - 1'd1;
	if (tape_in ^ UART_RXd2) ear_autostop_cnt <= ear_autostop_time;
	tape_play <= (ear_autostop_cnt != 0) ? tape_in : tape_read;
end

endmodule

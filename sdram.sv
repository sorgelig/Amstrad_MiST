//
// sdram.v
//
// Static RAM controller implementation using SDRAM MT48LC16M16A2
// Customized version for Amstrad.
//
// Copyright (c) 2018 Sorgelig
//
// Some parts of SDRAM code used from project:
// http://hamsterworks.co.nz/mediawiki/index.php/Simple_SDRAM_Controller
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version. 
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of 
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License 
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// ------------------------------------------
//
// v2.1 - Add universal 8/16 bit mode.
//

module sdram
(
	input             init,        // reset to initialize RAM
	input             clk,         // clock ~100MHz
                                  //
                                  // SDRAM_* - signals to the MT48LC16M16 chip
	inout  reg [15:0] SDRAM_DQ,    // 16 bit bidirectional data bus
	output reg [12:0] SDRAM_A,     // 13 bit multiplexed address bus
	output reg        SDRAM_DQML,  // two byte masks
	output reg        SDRAM_DQMH,  // 
	output reg  [1:0] SDRAM_BA,    // two banks
	output            SDRAM_nCS,   // a single chip select
	output            SDRAM_nWE,   // write enable
	output            SDRAM_nRAS,  // row address select
	output            SDRAM_nCAS,  // columns address select
	output            SDRAM_CKE,   // clock enable

	input      [24:0] addr,
	output reg  [7:0] dout,
	input       [7:0] din,
	input             we,
	input             rd,

	input      [24:0] vaddr,
	output      [7:0] vdata
);

assign SDRAM_nCS  = command[3];
assign SDRAM_nRAS = command[2];
assign SDRAM_nCAS = command[1];
assign SDRAM_nWE  = command[0];
assign SDRAM_CKE  = cke;

// no burst configured
localparam BURST_LENGTH        = 3'b000;   // 000=1, 001=2, 010=4, 011=8
localparam ACCESS_TYPE         = 1'b0;     // 0=sequential, 1=interleaved
localparam CAS_LATENCY         = 3'd2;     // 2 for < 100MHz, 3 for >100MHz
localparam OP_MODE             = 2'b00;    // only 00 (standard operation) allowed
localparam NO_WRITE_BURST      = 1'b1;     // 0= write burst enabled, 1=only single access write
localparam MODE                = {3'b000, NO_WRITE_BURST, OP_MODE, CAS_LATENCY, ACCESS_TYPE, BURST_LENGTH};

localparam sdram_startup_cycles= 14'd12100;// 100us, plus a little more, @ 100MHz
localparam cycles_per_refresh  = 14'd780;  // (64000*100)/8192-1 Calc'd as (64ms @ 100MHz)/8192 rose
localparam startup_refresh_max = 14'b11111111111111;

// SDRAM commands
localparam CMD_INHIBIT         = 4'b1111;
localparam CMD_NOP             = 4'b0111;
localparam CMD_ACTIVE          = 4'b0011;
localparam CMD_READ            = 4'b0101;
localparam CMD_WRITE           = 4'b0100;
localparam CMD_BURST_TERMINATE = 4'b0110;
localparam CMD_PRECHARGE       = 4'b0010;
localparam CMD_AUTO_REFRESH    = 4'b0001;
localparam CMD_LOAD_MODE       = 4'b0000;

reg [13:0] refresh_count = startup_refresh_max - sdram_startup_cycles;
reg  [3:0] command = CMD_INHIBIT;
reg        cke     = 0;
reg [24:0] save_addr;
reg [15:0] data;

reg        ram_busy;

typedef enum
{
	STATE_STARTUP,
	STATE_OPEN_1, STATE_OPEN_2,
	STATE_WRITE,
	STATE_READ,
	STATE_IDLE,	  STATE_IDLE_1, STATE_IDLE_2, STATE_IDLE_3,
	STATE_IDLE_4, STATE_IDLE_5, STATE_IDLE_6, STATE_IDLE_7
} state_t;

reg [15:0] vcache_data;
assign vdata = vaddr[0] ? vcache_data[15:8] : vcache_data[7:0];

always @(posedge clk) begin
	reg [CAS_LATENCY:0] data_ready_delay;

	reg        old_we, old_rd;
	reg [24:1] old_addr;
	reg  [7:0] save_data;
	reg        save_we;
	reg        refresh = 0;
	reg        cache_we = 1;
	reg [15:0] cache_data;
	reg [24:0] cache_addr;
	reg  [2:0] cli;

	state_t state = STATE_STARTUP;

	command <= CMD_NOP;
	refresh_count  <= refresh_count+1'b1;

	data_ready_delay <= {1'b0, data_ready_delay[CAS_LATENCY:1]};

	if(data_ready_delay[1]) begin
		case(cli)
			0: ram_busy  <= 0;
			default:;
		endcase
	end

	if(data_ready_delay[0]) begin
		case(cli)
			0: begin dout  <= save_addr[0] ? SDRAM_DQ[15:8] : SDRAM_DQ[7:0]; cache_data <= SDRAM_DQ; end
			1: vcache_data <= SDRAM_DQ;
			default: ;
		endcase
	end

	old_we <= we;
	old_rd <= rd;
	if((~old_we & we) | (~old_rd & rd)) begin
		if(~cache_we & ~we & (cache_addr[24:1] == addr[24:1])) dout <= addr[0] ? cache_data[15:8] : cache_data[7:0];
			else ram_busy <= 1;
	end

	case(state)
		STATE_STARTUP: begin
			//------------------------------------------------------------------------
			//-- This is the initial startup state, where we wait for at least 100us
			//-- before starting the start sequence
			//-- 
			//-- The initialisation is sequence is 
			//--  * de-assert SDRAM_CKE
			//--  * 100us wait, 
			//--  * assert SDRAM_CKE
			//--  * wait at least one cycle, 
			//--  * PRECHARGE
			//--  * wait 2 cycles
			//--  * REFRESH, 
			//--  * tREF wait
			//--  * REFRESH, 
			//--  * tREF wait 
			//--  * LOAD_MODE_REG 
			//--  * 2 cycles wait
			//------------------------------------------------------------------------
			cke        <= 1;
			SDRAM_DQ   <= 16'bZZZZZZZZZZZZZZZZ;
			SDRAM_DQML <= 1;
			SDRAM_DQMH <= 1;
			SDRAM_A    <= 0;
			SDRAM_BA   <= 0;

			// All the commands during the startup are NOPS, except these
			if(refresh_count == startup_refresh_max-31) begin
				// ensure all rows are closed
				command     <= CMD_PRECHARGE;
				SDRAM_A[10] <= 1;  // all banks
				SDRAM_BA    <= 2'b00;
			end else if (refresh_count == startup_refresh_max-23) begin
				// these refreshes need to be at least tREF (66ns) apart
				command     <= CMD_AUTO_REFRESH;
			end else if (refresh_count == startup_refresh_max-15) 
				command     <= CMD_AUTO_REFRESH;
			else if (refresh_count == startup_refresh_max-7) begin
				// Now load the mode register
				command     <= CMD_LOAD_MODE;
				SDRAM_A     <= MODE;
			end

			//------------------------------------------------------
			//-- if startup is complete then go into idle mode,
			//-- get prepared to accept a new command, and schedule
			//-- the first refresh cycle
			//------------------------------------------------------
			if(!refresh_count) begin
				state   <= STATE_IDLE;
				refresh_count <= 0;
			end
		end

		STATE_IDLE_7: state <= STATE_IDLE_6;
		STATE_IDLE_6: state <= STATE_IDLE_5;
		STATE_IDLE_5: state <= STATE_IDLE_4;
		STATE_IDLE_4: state <= STATE_IDLE_3;
		STATE_IDLE_3: state <= STATE_IDLE_2;
		STATE_IDLE_2: state <= STATE_IDLE_1;
		STATE_IDLE_1: begin
			SDRAM_DQ   <= 16'bZZZZZZZZZZZZZZZZ;
			state      <= STATE_IDLE;
		end

		STATE_IDLE: begin
			if((~old_we & we) | (~old_rd & rd & ((cache_addr[24:1] != addr[24:1]) | cache_we)) | ram_busy) begin
				cli      <= 0;
				save_we  <= we;
				save_data<= din;
				save_addr<= addr;
				SDRAM_A  <= addr[13:1];
				SDRAM_BA <= addr[24:23];
				state    <= STATE_OPEN_1;
				command  <= CMD_ACTIVE;
				refresh  <= 1;
				cache_addr <= addr;
				cache_we <= we;
			end else if(old_addr != vaddr[24:1]) begin
				old_addr <= vaddr[14:1];
				cli      <= 1;
				save_we  <= 0;
				save_data<= 0;
				save_addr<= vaddr;
				SDRAM_A  <= vaddr[13:1];
				SDRAM_BA <= vaddr[24:23];
				state    <= STATE_OPEN_1;
				command  <= CMD_ACTIVE;
			end else if(refresh) begin
				refresh  <= 0;
				cli      <= 2;
				state    <= STATE_IDLE_7;
				command  <= CMD_AUTO_REFRESH;
			end
		end

		// ACTIVE-to-READ or WRITE delay >20ns (-75)
		STATE_OPEN_1: state <= STATE_OPEN_2;
		STATE_OPEN_2: begin
			SDRAM_A     <= {4'b0010, save_addr[22:14]}; 
			SDRAM_DQML  <= save_we &  save_addr[0];
			SDRAM_DQMH  <= save_we & ~save_addr[0];
			state       <= save_we ? STATE_WRITE : STATE_READ;
		end

		STATE_READ: begin
			state       <= STATE_IDLE_5;
			command     <= CMD_READ;
			SDRAM_DQ    <= 16'bZZZZZZZZZZZZZZZZ;

			// Schedule reading the data values off the bus
			data_ready_delay[CAS_LATENCY] <= 1;
		end

		STATE_WRITE: begin
			state       <= STATE_IDLE_5;
			command     <= CMD_WRITE;
			SDRAM_DQ    <= {save_data, save_data};
			if(cli == 0) ram_busy  <= 0;
		end
	endcase

	if(init) begin
		state <= STATE_STARTUP;
		refresh_count <= startup_refresh_max - sdram_startup_cycles;
	end
end

endmodule

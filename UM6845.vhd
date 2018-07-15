--------------------------------------------------------------------------------
--
-- Extracted to separate entity, optimized and tweaked
-- (c) 2018 Sorgelig
--
--------------------------------------------------------------------------------
--
--    {@{@{@{@{@{@
--  {@{@{@{@{@{@{@{@  This code is covered by CoreAmstrad synthesis r005
--  {@    {@{@    {@  A core of Amstrad CPC 6128 running on MiST-board platform
--  {@{@{@{@{@{@{@{@
--  {@  {@{@{@{@  {@  CoreAmstrad is implementation of FPGAmstrad on MiST-board
--  {@{@        {@{@   Contact : renaudhelias@gmail.com
--  {@{@{@{@{@{@{@{@   @see http://code.google.com/p/mist-board/
--    {@{@{@{@{@{@     @see FPGAmstrad at CPCWiki
--
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_arith.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- check RVtot*RRmax=38*7=266>200 => 39*8=312 ! 38*8=304 304/52=5.84 ! 38*7=266=5.11
--       ? RVsyncpos*RRmax=30*7=210, 266-210=56 (NB_HSYNC_BY_INTERRUPT=52) 30*8=240 312-240=72
-- NB_HSYNC_BY_INTERRUPT*6=52*6=312

-- Grimware A PAL 50Hz video-frame on the Amstrad is 312 rasterlines. 
-- Grimware screenshoot :
--R0 RHtot     =63 : 0..63                            (donc 64 pas)
--R1 RHdisp    =40 : 0..39 si HCC=R1 alors DISPEN=OFF (donc 40 pas)
--R2 RHsyncpos =46 : si HCC=R2 alors HSYNC=ON         (donc 46 pas
--R3 RHwidth   =14 : si (HCC-R2=)R3 alors HSYNC=OFF   (donc 60 pas
--R4 RVtot     =38 : 0..38                            (donc 39 pas)
--R6 RVdisp    =25 : 0..24 si VCC=R6 alors DISPEN=OFF (donc 25 pas
--R7 RVsyncpos =30 : si VCC=R7 alors VSYNC=ON         (donc 30 pas
--R3 RVwidth   =8  : VSYNC=OFF 
--R9 RRmax     =7  : 0..7                             (donc  8 pas)

--minus one : R0, R4, R9

-- arnold cpctest.asm :
-- vram_Default_values:
-- defb 63,40,46,&8e,38,0,25,30,0,7,0,0,&30,0,0,0,0

--CRTC register 1 defines the width of the visible area in CRTC characters.
--The CPC display hardware fetches two bytes per CRTC character.
--Therefore the length of a CRTC scanline in bytes is (R1*2). (here : 40*2*8=640 pixels)

--CRTC register 6 defines the height of the visible area in CRTC character lines.
--Therefore the total height of the visible area in CRTC scanlines is (R9+1)*R6 (here :(7+1)*25=200 pixels)
-- (RRmax+1)*RVdisp
	
-- Being clear about address/data :
-- 12/13 : ADRESSE_maRegister update, upper to 9 isn't used
-- 0 1 2 3 do run setEvents => strange it seems about HORIZONTALS
-- 7 seem making effects if its value is 0 but it seems a source code erratum
-- 3 does call setReg3(value) which rules under hsyncWidth and vsyncWidth
-- 6 does call setReg6() with some border effect on a demo
-- 8 does call setReg8(value) interlace

entity UM6845 is
port (
	CLOCK		:	in  std_logic;
	CLKEN		:	in  std_logic;
	nRESET	:	in  std_logic;
	CRTC_TYPE:	in  std_logic;

	-- Bus interface
	ENABLE	:	in  std_logic;
	nCS		:	in  std_logic;
	R_nW		:	in  std_logic;
	RS			:	in  std_logic;
	DI			:	in  std_logic_vector(7 downto 0);
	DO			:	out std_logic_vector(7 downto 0);

	-- Display interface
	VSYNC		:	out std_logic;
	HSYNC		:	out std_logic;
	DE			:	out std_logic;
	CURSOR	:	out std_logic;
	LPSTB		:	in  std_logic;

	-- Memory interface
	MA			:	out std_logic_vector(13 downto 0);
	RA			:	out std_logic_vector(4 downto 0)
	);
end entity;

architecture rtl of UM6845 is

	-- init values are for test bench javacpc ! + Grimware
	signal RHtot:std_logic_vector(7 downto 0):="00111111";
	signal RHdisp:std_logic_vector(7 downto 0):="00101000";
	signal RHsyncpos:std_logic_vector(7 downto 0):="00101110";
	signal RHwidth:std_logic_vector(3 downto 0):="1110";
	signal RVwidth:std_logic_vector(3 downto 0):="1000";
	signal RVtot:std_logic_vector(7 downto 0):="00100110";
	signal RVtotAdjust:std_logic_vector(7 downto 0):="00000000";
	signal RVdisp:std_logic_vector(7 downto 0):="00011001";
	signal RVsyncpos:std_logic_vector(7 downto 0):="00011110";
	signal RRmax:std_logic_vector(7 downto 0);
	signal RRmax9:std_logic_vector(7 downto 0):="00000111";
	
	signal Skew:std_logic_vector(1 downto 0):="00";
	signal interlaceVideo:std_logic:='0';
	signal interlace:std_logic:='0';
	signal scanAdd:std_logic_vector(7 downto 0):=x"01";
	signal halfR0:std_logic_vector(7 downto 0):="00100000"; --(RHTot+1)/2

	signal ADRESSE_maRegister:STD_LOGIC_VECTOR(13 downto 0):="110000" & "00000000";--(others=>'0');
	signal LineCounter_is0:boolean:=true;

begin

CURSOR <= '0';

Config : process(nRESET,CLOCK) is
	variable reg_select : integer range 0 to 31;
	-- normally 0..17 but 0..31 in JavaCPC
	type registres_type is array(0 to 31) of std_logic_vector(7 downto 0);
	variable registres:registres_type := (others=>(others=>'0'));
	variable halfR0_mem:std_logic_vector(7 downto 0);
begin
	if nRESET='0' then
		DO<=(others=>'1');
		RHtot <="00111111";
		RHdisp <="00101000";
		RHsyncpos <="00101110";
		RHwidth <="1110";
		RVwidth <="1000";
		RVtot <="00100110";
		RVtotAdjust <="00000000";
		RVdisp <="00011001";
		RVsyncpos <="00011110";
		RRmax9 <="00000111";
		Skew <="00";
		interlaceVideo <='0';
		interlace <='0';
		scanAdd <=x"01";
		halfR0 <="00100000";
	elsif rising_edge(CLOCK) then
	
		if ENABLE ='1' and nCS='0' then -- EN port (enable)
			--On type 0 and 1, if a Write Only register is read from, "0" is returned. 
				--type 0		
			--			b1 	b0 	Function 	Read/Write
			--0 	0 	Select internal 6845 register 	Write Only
			--0 	1 	Write to selected internal 6845 register 	Write Only
			--1 	0 	- 	-
			--1 	1 	Read from selected internal 6845 register 	Read only 

				--type 1
			--b1 	b0 	Function 	Read/Write
			--0 	0 	Select internal 6845 register 	Write Only
			--0 	1 	Write to selected internal 6845 register 	Write Only
			--1 	0 	Read Status Register 	Read Only
			--1 	1 	Read from selected internal 6845 register 	Read only 
			DO <=(others=>'1'); -- pull up (no command)
			if R_nW='0' then -- A9_WRITE
				if RS='0' then
					reg_select:=conv_integer(DI(4 downto 0));
				else
					registres(reg_select):=DI;

					-- rien ici de pertinant...
					-- see arnoldemu's crtc.c file :
					-- CRTC0_UpdateState
					-- CRTC1_UpdateState
					-- CRTC2_UpdateState
					-- ASICCRTC_UpdateState
					-- JavaCPC Basic6845[CRTC].setRegister().setEvents()
					
					--HD6845S_WriteMaskTable idem que UM6845R_WriteMaskTable sauf pour R8 (skew)
					
					case reg_select is
						when 0=>
							RHtot<=registres(0);
							--hChars = reg[0] + 1;
							--halfR0 = hChars >> 1;
							halfR0_mem:=registres(0)+1;
							halfR0<="0" & halfR0_mem(7 downto 1);
						when 1=>
							RHdisp<=registres(1);
						when 2=>
							RHsyncpos<=registres(2);
						when 3=>
							-- following DataSheet and Arnold emulator (Arnold says it exists a conversion table HSYNC crtc.c.GA_HSyncWidth)
							--hSyncWidth = value & 0x0f;
							RHwidth<=registres(3)(3 downto 0); -- DataSheet
							--RVwidth<=conv_std_logic_vector(NB_LINEH_BY_VSYNC,5);-- (24+1) using Arnold formula
							-- Arnold formula ctrct.c.MONITOR_VSYNC_COUNT "01111";
							-- Arkanoid does use width VSYNC while hurting a monster or firing with bonus gun
							-- RVwidth<=registres(3)(7 downto 4); -- JavaCPC 2015 puis freemac
							-- http://quasar.cpcscene.net/doku.php?id=coding:test_crtc#fn__24
							
							-- VSync width can only be changed on type 3 and 4 (???)
							-- The Vsync has a fixed length for CRTC 2, which is 16 scan lines (and not 8 as programmed by the firmware, implicitly using CRTC 0). 
							--http://cpctech.cpc-live.com/source/split.html
							if crtc_type='1' then
								--CRTC1 MC6845/MC6845R/UM6845R have a fixed Vertical Sync Width of 16 scanlines.
								--vSyncWidth = 0;
								RVwidth<=x"0"; --registres(3)(6 downto 4) & "0";
							else
								--CRTC0 HD6845S allows the Vertical Sync Width to be programmed
								--vSyncWidth = (value >> 4) & 0x0f;
								RVwidth<=registres(3)(7 downto 4);
							end if;
							
							--CRTC0 HD6845: Register 3: Sync Width Bit 7 Vertical Sync Width bit 3 Bit 6 Vertical Sync Width bit 2 Bit 5 Vertical Sync Width bit 1 Bit 4 Vertical Sync Width bit 0 Bit 3 Horizontal Sync Width bit 3 Bit 2 Horizontal Sync Width bit 2 Bit 1 Horizontal Sync Width bit 1 Bit 0 Horizontal Sync Width bit 0 
							--CRTC1 MC6845/UM6845: Note for UM6845: When the Horizontal Sync width is set to 0, then no Horizontal Syncs will be generated. (This feature can be used to distinguish between the UM6845 and MC6845).
							
							
							--CRTC0 Programming Horizontal Sync Width with 0: HD6845S: The data sheets says that the Horizontal Sync Width cannot be programmed with 0. The effect of doing this is not documented. MC6845: If the Horizontal Sync Width register is programmed with 0, no horizontal syncs are generated.
							
						when 4=>
							RVtot<=registres(4) and x"7f";
						when 5=>
							RVtotAdjust<=registres(5) and x"1f";
						when 6=>
							--The DISPTMG (Activation du split-border) can be forced using R8 (DISPTMG Skew) on type 0,3 and 4 or by setting R6=0 on type 1.
							RVdisp<=registres(6) and x"7f";
						when 7=>
							RVsyncpos<=registres(7) and x"7f";
						when 8=>-- and x"f3"; and x"03" (type 1)
							-- interlace & skew
							-- arnoldemu's crtc.c
							-- Delay = (CRTCRegisters[8]>>4) & 0x03;
							-- CRTC_InternalState.HDelayReg8 = (unsigned char)Delay;
							--There are only two bits in R8:
							-- bit 0: interlace enable.
							-- bit 1: interlace type (when enabled)
							--Interlace and Skew 	xxxxxx00
							-- 00 : No interlace
							-- 01 : Interlace Sync Raster Scan Mode
							-- 10 : No Interlace
							-- 11 : Interlace Sync and Video Raster Scan Mode 
							
							-- CRTC0 HD6845S: Register 8: Interlace and Skew Bit 7 Cursor Display timing Skew Bit 1 Bit 6 Cursor Display timing Skew Bit 0 Bit 5 Display timing Skew Bit 1 (DTSKB1) Bit 4 Display timing SKew Bit 0 (DTSKB0) Bit 3 not used Bit 2 not used Bit 1 Video Mode Bit 0 Interlace Sync Mode Display timing skew: The data can be skewed by 0 characters, 1 character or 2 characters. When both bits are 1 the display is stopped and border is displayed. This is used in the BSC Megademo in the Crazy Cars II part. 
							-- CRTC1 MC6845/UM6845 : Bit 1 Video Mode Bit 0 Interlace Sync Mode
							
							-- Type 0,1a (and 4 ?) have an extra feature in R8, which seems to be the basis for the "register 8 border technique". These CRTCs use bits 4 and 5 of R8 for character delay: that is, to account for the fact, that in a typical low-cost system, the memory fetches (RAM and then font ROM) would be slow and would make the raster out of sync with "Display Enable" (DE, the frame, or border) which is wired directly to the color generator. 
							-- So they implemented a programmable DE delay (the "Skew") which is to be set a the duration of the raster fetch (counted in CRTC clock cycles, or mode 1 characters). The same thing is done for the "Cursor" line, because it is also shorter than the raster data-path. 
							--To implement this, you can by-pass or enable couple of registers on the concerned lines. Because these registers probably get reset when they're bypassed, if you reenable them while DE is true, it will take them as many characters as the DE delay, before they echo "true": you get a bit of border color in the middle of the screen ! 
							--Of course, when the delay is elapsed, the raster comes back, but you could repeatedly turn the delay on and off. 
							--interlace = (value & 0x01) != 0;
							interlace<=registres(8)(0);
							--interlaceVideo = (value & 0x03) == 3 ? 1 : 0;
							interlaceVideo<=registres(8)(1);
							--scanAdd = interlaceVideo + 1;
							if registres(8)(1) = '0' then
								scanAdd<=x"01";
							else
								scanAdd<=x"02";
							end if;
							--CRTC3 hDispDelay = ((reg[8] >> 4) & 0x04);
							Skew<=registres(8)(5 downto 4);
							
							
						when 9=> -- max raster adress
							--maxRaster = value | interlaceVideo;
							RRmax9<= (registres(9) and x"1f");
						when 10=>NULL; -- and x"7f";
							-- cursor start raster 
						when 11=>NULL; -- and x"1f";
							-- cursor end raster
						when 12=>
							
							--NULL;  (read/write type 0) (write only type 1)
							-- start adress H
							--maRegister = (reg[13] + (reg[12] << 8)) & 0x3fff;
							-- and x"3f" donc (5 downto 0)
							ADRESSE_maRegister<=registres(12)(5 downto 0) & registres(13);
						when 13=> --NULL;  (read/write type 0) (write only type 1)
							-- start adress L
							--maRegister = (reg[13] + (reg[12] << 8)) & 0x3fff;
							ADRESSE_maRegister<=registres(12)(5 downto 0) & registres(13);
						when 14=>NULL; -- and x"3f"
							-- cursor H (read/write)
						when 15=>NULL;
							-- cursor L (read/write)
						when 16=>NULL;
							--light pen H (read only)
						when 17=>NULL;
							--light pen L (read only)
						when others => NULL;
					end case;
				end if;
			else -- A9_READ
				-- type 0 : status is not implemented
				if RS='0' then
					-- STATUS REGISTER (CRTC 1 only)
					-- U (bit 7) : Update Ready
					-- L (bit 6) : LPEN Reegister Full
					-- V (bit 5) : Vertical Blanking (VDISP ?)
					-- in type 3 & 4, status_reg=reg
					
					-- Bit 6 LPEN REGISTER FULL 1: A light pen strobe has occured (light pen has put to screen and button has been pressed), 0: R16 or R17 has been read by the CPU 
					-- Bit 5	VERTICAL BLANKING 1: CRTC is scanning in the vertical blanking time, 0: CRTC is not scanning in the vertical blanking time.
					--Vertical BLanking (VBL)
					--This is a time interval during a video-frame required by the electron gun in a CRT monitor to move back up to the top of the tube. While the vertical blank, the electron beam is off, hence no data is displayed on the screen.
					--As soon as the electron gun is back to the top, the monitor will hold it there until a VSync appears to indicate the start of a new frame. If no VSync appears, the monitor will release the gun by itself after some time (depending on it's VHold) and will usually produce a rolling/jumping image because the monitor vertical synchronisation is no longer done with the CPC video-frame but with the monitor hardware limits (and they won't be the same).
					--The VBL is a monitor specific time interval, it can not be software controlled (on the Amstrad), unlike the VSync, which is a signal produced by the CRTC we can control. The monitor expect a VSync at regular interval to produce a stable image.
					
					--Bit 5, VERTB, causes an interrupt at line 0 (start of vertical blank) of
					--the video display frame. The system is often required to perform many
					--different tasks during the vertical blanking interval. Among these tasks
					--are the updating of various pointer registers, rewriting lists of Copper
					--tasks when necessary, and other system-control operations.
					
					--Registre de status accessible sur le port &BExx (VBL, border)
					
					-- CRTC 3 et 4 : lecture de la ligne de VBL sur le registre 10
					--if (LineCounter == 0) {
					--  return (1 << 5); x"20"
					if crtc_type='0' then
						DO<=x"FF";
					elsif LineCounter_is0 then
						--if (LineCounter == 0) {
						--Bit 5 is set to 1 when CRTC is in "vertical blanking". Vertical blanking is when the vertical border is active. i.e. VCC>=R6.
						--It is cleared when the frame is started (VCC=0). It is not directly related to the DISPTMG output (used by the CPC to display the border colour) because that output is a combination of horizontal and vertical blanking. This bit will be 0 when pixels are being displayed.
						DO<=x"20";
					else
						DO<=x"00"; 
					end if;
				else
					-- type 0 : nothing (return x"00")
					-- type 1 : read status
					case reg_select is
						when 10=>
							DO<=registres(10) and x"7f"; -- applying the write mask here
						when 11=>
							DO<=registres(11) and x"1f"; -- applying the write mask here
						when 12=>
							if crtc_type='0' then
								--CRTC0 HD6845S/MC6845: Start Address Registers (R12 and R13) can be read.
								DO<=registres(12) and x"3f";  -- applying the write mask here
							else
								--CRTC1 UM6845R: Start Address Registers cannot be read.
								DO<=x"00"; -- type 1
							end if;
						when 13=>
							if crtc_type='0' then
								DO<=registres(13); -- type 0
								--CRTC0 HD6845S/MC6845: Start Address Registers (R12 and R13) can be read.
							else
								--CRTC1 UM6845R: Start Address Registers cannot be read.
								-- Lecture des registres 12 and 13 sur le port &BFxx : >>non<<
								DO<=x"00"; -- type 1 & 2
							end if;
						when 14=>
							--if crtc_type='0' then
							DO<=registres(14) and x"3f"; -- applying the write mask here
							--else
							--	DO<=registres(14);
							--end if;
						when 15=>
							DO<=registres(15);-- all types
						when 16=>
							--	Light Pen Address (read only, don't dependant on write !!!) - "Emulator Sucks"
							DO<=x"00"; --registres(16) and x"3f";-- all types
						when 17=>
							--	Light Pen Address (read only, don't dependant on write !!!) - "Emulator Sucks"
							DO<=x"00"; --registres(17);-- all types
						when 31=>
							if crtc_type='1' then
								-- registers 18-30 read as 0 on type1, register 31 reads as 0x0ff.
								DO<=x"FF";
							else
								DO<=x"00";
							end if;
						when others=>
							-- 1. On type 0 and 1, if a Write Only register is read from, "0" is returned.
							-- registers 18-31 read as 0, on type 0 and 2.
							-- registers 18-30 read as 0 on type1
							DO<=x"00";
					end case;
				end if;
			end if;
		else
			DO <= x"FF";
		end if;
	end if;
end process Config;

--maxRaster = reg[9] | interlaceVideo;
RRmax <= RRmax9(7 downto 1) & (RRmax9(0) or interlaceVideo);

-- DANGEROUS WARNING : CRTC PART WAS TESTED AND VALIDATED USING TESTBENCH
Main : process(nRESET,CLOCK) is
 
	variable dispV:std_logic:='0';
	variable dispH:std_logic:='0'; -- horizontal disp (easier to compute BORDER area)
	variable dispH_skew0:std_logic:='0';
	variable dispH_skew1:std_logic:='0';
	variable dispH_skew2:std_logic:='0';
	-- following Quazar legends, 300 times per second
	-- Following a lost trace in Google about www.cepece.info/amstrad/docs/garray.html I have
	-- "In the CPC the Gate Array generates maskable interrupts, to do this it uses the HSYNC and VSYNC signals from CRTC, a 6-bit internal counter and monitors..."
	-- perhaps useful also : http://www.cpcwiki.eu/index.php/Synchronising_with_the_CRTC_and_display 
	-- following http://cpcrulez.fr/coding_amslive04-z80.htm
	-- protected int hCCMask = 0x7f; "char_counter256 HMAX n'est pas une valeur en dur, mais un label comme VT et VS..."
	variable hCC : std_logic_vector(7 downto 0):=(others=>'0'); --640/16
	variable LineCounter : std_logic_vector(7 downto 0):=(others=>'0'); --600
	variable etat_hsync : STD_LOGIC:='0';
	variable etat_vsync : STD_LOGIC:='0';
	--idem ADRESSE_MAcurrent_mem variable MA:STD_LOGIC_VECTOR(13 downto 0):=(others=>'0');
	variable RasterCounter:STD_LOGIC_VECTOR(7 downto 0):=(others=>'0'); -- buggy boy has value RRmax=5
	variable frame_oddEven:std_logic:='0';
	variable ADRESSE_maStore_mem:STD_LOGIC_VECTOR(13 downto 0):=(others=>'0');
	variable ADRESSE_MAcurrent_mem:STD_LOGIC_VECTOR(13 downto 0):=(others=>'0');
		
	--(128*1024)/64 2*1024=2^11
	variable zap_scan:boolean:=true; -- if in last round, has no blank signal, do not scan memory !

	variable border_begin_mem:std_logic_vector(7 downto 0):=(others=>'0');
	variable disp_begin_mem:std_logic:='0';
	variable RHdisp_mem:std_logic_vector(7 downto 0):="00101000";
	
	variable RVtotAdjust_mem:std_logic_vector(7 downto 0):=(others=>'0');
	variable RVtotAdjust_do:boolean:=false;
	
	variable hSyncCount:std_logic_vector(3 downto 0):=(others=>'0');
	variable vSyncCount:std_logic_vector(3 downto 0):=(others=>'0');
begin
	if nRESET='0' then
		VSYNC<='0';
		HSYNC<='0';
		
		etat_hsync:='0';
		etat_vsync:='0';
		
		ADRESSE_maStore_mem:=(others=>'0');
		ADRESSE_MAcurrent_mem:=(others=>'0');
		LineCounter:=x"00";
		RasterCounter:=x"00";
		hCC:=x"00";
		
	--it's Z80 time !
	elsif rising_edge(CLOCK) then

		if CLKEN='1' then

			-- Crazy Car II doesn't like little_reset
			-- Asphalt IACK without test in int_mem
			-- counter never upper than  52
			-- z80 mode 1 : the byte need no be sent, as the z80 restarts at logical address x38 regardless(z80 datasheet)

			--checkHSync(false); -- and RHwidth/=x"0" FIXME
			-- BAD : MC6845/UM6845: Note for UM6845: When the Horizontal Sync width is set to 0, 
			-- then no Horizontal Syncs will be generated. (This feature can be used to distinguish 
			-- between the UM6845 and MC6845).
			-- http://cpctech.cpc-live.com/docs/hd6845s/hd6845sp.htm 0=>16
			-- http://cpctech.cpc-live.com/docs/um6845r/um6845r.htm 0=>???
			-- http://cpctech.cpc-live.com/docs/mc6845/mc6845.htm 0=>ignore
			if ((frame_oddEven='1' and hCC = halfR0) or (frame_oddEven='0' and hCC=RHsyncpos))
				and (crtc_type='1' or RHwidth/=x"0") then -- and etat_hsync='0'
				--hSyncCount = 0;
				hSyncCount:= x"0";
				--if (hDisp && CRTCType == 1 && hSyncWidth == (reg[3] & 0x0f)) {
				--if dispH_skew0='1' and crtc_type='1' then
				--	--vDisp = reg[6] != 0;
				--	if RVDisp=0 then
				--		dispV:='0';
				--	else
				--		dispV:='1';
				--	end if;
				--	hSyncCount:= x"1"; -- Prehistoric live barre, at right, if x"0" : a pixel glinch.
				--end if;
				--inHSync = true;
				etat_hsync:='1';
				--listener.hSyncStart();
				--hsync_int<='1'; -- following javacpc,grimware and arnold
			-- if (inHSync) {
			elsif etat_hsync='1' then
				--hSyncCount = (hSyncCount + 1) & 0x0f;
				hSyncCount:=hSyncCount+1;
				--if (hSyncCount == hSyncWidth) {
				if	hSyncCount=RHwidth then
					--inHSync = false;
					etat_hsync:='0';
					--listener.hSyncEnd();
					--hsync_int<='0';
				end if;
			end if;
			
			--http://www.phenixinformatique.com/modules/newbb/viewtopic.php?topic_id=4316&forum=9
			--In original CRTC DataSheet, it doesn't have any test about VSync period, and also, bits 4 to 7 of R3 are not taken into account. Some factories shall have reused this free bits to put on it its own features, feel more about somes linked to VSync (like interlaced R8, adding difference between a certain model of CRTC and another).
			--PPI read CRTC.isVSYnc bool
			--if (inVSync && (vSyncCount = (vSyncCount + 1) & 0x0f) == vSyncWidth) {
			if hCC = 0 then
				-- checkVSync()
				--if RasterCounter=0 and LineCounter=RVsyncpos then -- crtc 3 -- crtc 0 et 1 pour JavaCPC
				-- on CRTC type 0 and 1, Vsync can be triggered on any line of the char. -- WakeUp! demo fail in scrolling down the girl
				--if (LineCounter == reg[7] && !inVSync) {
				--if RasterCounter=0 and etat_vsync='0' and LineCounter=RVsyncpos then -- crtc 0 et 1 -- WakeUp! slow down
				--if RasterCounter=0 and LineCounter=RVsyncpos then
				--on CRTC type 0 and 1, Vsync can be triggered on any line of the char.
				--if LineCounter=RVsyncpos and etat_vsync='0' then -- (too clever for a CRTC, isn't it ? "do offset if problems")
				--checkVSync();
				--if (LineCounter == reg[7] && !inVSync) { -- (too clever for a CRTC, isn't it ? "do offset if problems")
				--WakeUp!
				if RasterCounter=0 and LineCounter=RVsyncpos then -- and etat_vsync='0' then
				--if LineCounter=RVsyncpos then
					--checkVSync(true); (idem newFrame() ?)
					--Batman logo rotating still like this... but dislike the !inVSync filter (etat_vsync='0') here...
					-- Batman city towers does like RasterCounter=0 filter here...
					-- CRTC datasheet : if 0000 is programmed for VSync, then 16 raster period is generated.
					vSyncCount:= x"0"; -- pulse ?
					etat_vsync:='1';
					--crtc_VSYNC<='1'; -- it is really '1' by here, because we need an interrupt while vsync=1 or else border is to too faster (border 1,2)
					--vsync_int<='1'; -- do start a counter permitting 2 hsync failing before interrupt
				elsif etat_vsync='1' then -- and not(RVtotAdjust_do) then
					vSyncCount:=vSyncCount+1;
					if vSyncCount=RVwidth then -- following Grim (forum)
						etat_vsync:='0';
						--crtc_VSYNC<='0';
						--vsync_int<='0'; -- useless, except to addition several vsync layering them each others
					end if;
				end if;
			end if;
			
			--setEvents() HSync strange behaviour : part 2
			if zap_scan then
				dispH_skew0:='0';
			elsif hCC = 0 then -- and LineCounter<RVDisp*(RRmax+1) then
				dispH_skew0:='1';
				--hDispStart() (redondance avec hCC=RHtot (hCC:=0) !) : donc ne rien faire ici...
			elsif (crtc_type='1' and hCC = RHdisp) or (crtc_type='0' and hCC=RHdisp+Skew) then
				dispH_skew0:='0';
				
				--if ((getRA() | interlaceVideo) == maxRaster) {
				if (RasterCounter or "0000000" & interlaceVideo)=RRMax then
					if crtc_type='1' then
						--maStore = (maStore + reg[1]) & 0x3fff;
						--0x3fff est ok : ADRESSE_maStore_mem(13:0)
						ADRESSE_maStore_mem:=ADRESSE_maStore_mem+RHDisp;
					else
						--if (CRTC_InternalState.HCount == CRTC_InternalState.HEnd) -- c'est HDisp ce HEnd en fait...
						--CRTC_InternalState.MAStore = CRTC_InternalState.MALine + CRTC_InternalState.HCount;
						ADRESSE_maStore_mem:=ADRESSE_maStore_mem + RHDisp + Skew;
					end if;
				end if;
			end if;
			
			if crtc_type='0' then
				-- hDispDelay = (reg[8] >> 4) & 0x03;
				if Skew="00" then
					dispH:=dispH_skew0;
				elsif Skew="01" then
					dispH:=dispH_skew1;
				elsif Skew="10" then
					dispH:=dispH_skew2;
				else
					--checkHDisp()
					--if ((reg[8] & 0x030) == 0x30) {
					dispH:='0'; --normally full black (no output)
				end if;
				dispH_skew2:=dispH_skew1;
				dispH_skew1:=dispH_skew0;
			else
				dispH:=dispH_skew0;
			end if;
			--if frame_oddEven='1' then
			--	dispH:='0'; -- :p
			--end if;
			
			-- V (bit 5) : Vertical Blanking
			-- Bit 5 is set to 1 when CRTC is in "vertical blanking". Vertical blanking is when the vertical border
			-- is active. i.e. VCC>=R6.
			-- It is cleared when the frame is started (VCC=0). It is not directly related to the DISPTMG output
			-- (used by the CPC to display the border colour) because that output is a combination of horizontal
			-- and vertical blanking. This bit will be 0 when pixels are being displayed.
			
			-- http://quasar.cpcscene.net/doku.php?id=coding:test_crtc
			
			-- Only R5 still needs to be explained. To allow a finer adjustment of the screen length than by the number of character lines (R4), R5 adds a number of blank scanlines at the end of the screen timing.
			
			--DISPTMG signal defines the border. When DISPTMG is "1" the border colour is output by the Gate-Array to the display.
			--The DISPTMG can be forced using R8 (DISPTMG Skew) on type 0,3 and 4 or by setting R6=0 on type 1.
			
			if LineCounter=0 then
				dispV:='1';
				-- Scan currently is in vertical blanking time-span.
				--VBLANK<='0';
			end if;

			if LineCounter=RVDisp then
				--checkHDisp() -- if (reg[6] != 0) { --listener.hDispStart();
				if RVDisp = X"00" and crtc_type='0' then
					if RasterCounter>0 then
						dispV:='0';
					end if;
				else
					dispV:='0';
				end if;
				-- Scan is not currently running in vertical blanking time-span.
				--VBLANK<='1';
			end if;
			if LineCounter=0 then
				LineCounter_is0<=true;
			else
				LineCounter_is0<=false;
			end if;
			
			if dispH='1' and dispV='1' then
				--vde<=DO_READ;
				-- http://quasar.cpcscene.com/doku.php?id=assem:crtc
				-- Have to respect address cut ADRESSE_CONSTANT_mem:=conv_integer(ADRESSE_maRegister(13 downto 0)) mod (16*1024);
				
				-- newFrame() :  ma = maBase = ADRESSE_maRegister;
				
				--ADRESSE_hCC_mem:=conv_integer(hCC) mod (16*1024);
				
				-- .------- REG 12 --------.   .------- REG 13 --------.
				-- |                       |   |                       |
				--  15 14 13 12 11 10 09 08     07 06 05 04 03 02 01 00
				-- .--.--.--.--.--.--.--.--.   .--.--.--.--.--.--.--.--.
				-- |X |X |  |  |  |  |  |  |   |  |  |  |  |  |  |  |  |
				-- '--'--'--'--'--'--'--'--'   '--'--'--'--'--'--'--'--'
				--       '--.--'--.--'---------------.-----------------'
				--          |     |                  |
				--          |     |                  '------> Offset for setting
				--          |     |                           videoram 
				--          |     |                           (1024 positions)
				--          |     |                           Bits 0..9
				--          |     |
				--          |     '-------------------------> Video Buffer : note (1)
				--          |
				--          '-------------------------------> Video Page : note (2)
				-- note (1)                 note (2)
				-- .--.--.--------------.  .--.--.---------------.
				-- |11|10| Video Buffer |  |13|12|   Video Page  |
				-- |--|--|--------------|  |--|--|---------------|
				-- | 0| 0|     16Ko     |  | 0| 0|  0000 - 3FFF  |
				-- |--|--|--------------|  |--|--|---------------|
				-- | 0| 1|     16Ko     |  | 0| 1|  4000 - 7FFF  |
				-- |--|--|--------------|  |--|--|---------------|
				-- | 1| 0|     16Ko     |  | 1| 0|  8000 - BFFF  |
				-- |--|--|--------------|  |--|--|---------------|
				-- | 1| 1|     32Ko     |  | 1| 1|  C000 - FFFF  |
				-- '--'--'--------------'  '--'--'---------------'
				--
				--PulkoMandy - I don't get this "4 pages" thing. The CRTC can address the full 64K of central ram and start the display almost anywhere in it. 
				
				-- ma = (maBase + hCC) & 0x3fff;
				--MA:=conv_std_logic_vector(ADRESSE_maStore_mem+ADRESSE_hCC_mem,14);
				--RasterCounter:=ligne_carac_v_RA;
				--http://cpctech.cpc-live.com/docs/scraddr.html
				MA<=ADRESSE_MAcurrent_mem;
				RA<=RasterCounter(4 downto 0);
				--http://cpcrulez.fr/coding_amslive02-balayage_video.htm dit :
				--MA(13 downto 12) & RasterCounter(2 downto 0) & MA(9 downto 0) & CCLK
			end if;

			--cycle()
			-- The CRTC component is separated from Gatearray component, so does we have some late ?
			-- Not certain, as this old component was really old ones : using state and no rising_egde...
			-- if (hCC == reg[0]) {
			-- Valeur minimale du registre 0 CRTC0:1 CRTC1:0
			if hCC=RHtot and (crtc_type='1' or RHtot/=0) then -- tot-1 ok
				--hCC = 0;
				hCC:=(others=>'0');
				--scanStart(); ====> vSyncWidth ....
				--if (reg[9] == 0 && reg[4] == 0 && (CRTCType == 0 || CRTCType == 3)) {
				--	vtAdj = 1;
				--}
				--if (vtAdj > 0 && --vtAdj == 0) newFrame();
				-- else if ((ra | interlaceVideo) == maxRaster) {
				if ((RasterCounter or "0000000" & interlaceVideo)=RRMax and LineCounter=RVTot and RVtotAdjust=0 and not(RVtotAdjust_do)) -- tot-1 ok ok
					or (RVtotAdjust_do and RVtotAdjust_mem=RVtotAdjust) then
					-- on a fini RVtotAdjust (ou sinon on a eu un RVtot fini sans RVtotAdjust)
						RVtotAdjust_do:=false;
						--newFrame()
						-- on commence RVtot
						--if (vCC == reg[4] && vtAdj == 0) {
						RasterCounter:="0000000" & frame_oddEven and "0000000" & interlaceVideo; --(others=>'0'); -- pulse ?
						zap_scan:=false;
						--This method requires careful timing for the CRTC register updates,
						--	it also needs testing on all CRTC because there are differences
						-- of when each will accept and use the values programmed. However,
						--	the result can be made to work on all with more simple ruptures.
						--	Care must also be taken to ensure the timings are setup for a 50Hz screen. 
						--When VCC=0, R12/R13 is re-read at the start of each line. R12/R13 can therefore be changed for each scanline when VCC=0. 
						--updateScreen()
						--ma = maBase = ADRESSE_maRegister;
						--maCurrent = maStore = maRegister;
						ADRESSE_maStore_mem:=ADRESSE_maRegister(13 downto 0);
						
						LineCounter:=(others=>'0');
						ADRESSE_MAcurrent_mem:=ADRESSE_maStore_mem;
						if interlace = '0' then
							frame_oddEven:='0';
						else
							frame_oddEven:=not(frame_oddEven);
						end if;
						-- RVtot vs RVtotAdjust ? RVtotAdjust ne serait-il pas dynamique par hazard ? NON selon JavaCPC c'est meme le contraire
				elsif (RasterCounter or "0000000" & interlaceVideo)=RRMax then
					--RasterCounter = (frame & interlaceVideo) & 0x07;
					RasterCounter:="0000000" & frame_oddEven and "0000000" & interlaceVideo; --(others=>'0');
					-- scanStart() : maBase = (maBase + reg[1]) & 0x3fff;
					if LineCounter=RVTot and not(RVtotAdjust_do) then
						--if (interlace && frame == 0) {
						--	vtAdj++;
						--}
						RVtotAdjust_mem:=x"01";
						RVtotAdjust_do:=true;
					elsif RVtotAdjust_do then
						RVtotAdjust_mem:=RVtotAdjust_mem+1;
					end if;
					-- Linear Address Generator
					-- Nhd+0
					--if ((getRA() | interlaceVideo) == maxRaster) {
					--	maStore = (maStore + reg[1]) & 0x3fff;
					--}
					--maStore = (maStore + reg[1]) & 0x3fff;
					--0x3fff est ok : ADRESSE_maStore_mem(13:0)

					--hDispStart()
					ADRESSE_MAcurrent_mem:=ADRESSE_maStore_mem;
					--} else if (vcc && -- if (vtAdj == 0 || (CRTCType == 1)) { -- "vcc" est un boolean ici (un RRmax atteind)
					--if (vcc && vtAdj == 0) { -- "vcc" est un boolean ici (un RRmax atteind)
					if crtc_type='1' or not(RVtotAdjust_do) then
						-- LineCounter = (LineCounter + 1) & 0x7f;
						LineCounter:=(LineCounter+1) and x"7F";
					end if;
				else
					-- RasterCounter = (RasterCounter + scanAdd) & 0x07;
					RasterCounter:=(RasterCounter + scanAdd) and x"1F";
					if RVtotAdjust_do then
						RVtotAdjust_mem:=RVtotAdjust_mem+1;
					elsif LineCounter = 0 and crtc_type='1' then
						--if (CRTCType == 0 && LineCounter == 0 && RasterCounter == 0 && maScroll == 0) {
						--if (CRTCType == 1 && LineCounter == 0/*
						--When VCC=0, R12/R13 is re-read at the start of each line. R12/R13 can therefore be changed for each scanline when VCC=0. 
						--updateScreen()
						--maCurrent = maStore = maRegister;
						ADRESSE_maStore_mem:=ADRESSE_maRegister(13 downto 0);
					end if;
					--hDispStart()
					ADRESSE_MAcurrent_mem:=ADRESSE_maStore_mem;
				end if;
				
			else
				-- hCCMask : so var is size 256 and mod is 128...
				--protected int hCCMask = 0x7f;
				--hCC = (hCC + 1) & hCCMask;
				hCC:=(hCC+1) and x"7F";
				--maCurrent = (maStore + hCC) & 0x3fff;
				ADRESSE_MAcurrent_mem:=ADRESSE_maStore_mem+hCC; -- WakeUp color raster while girl is here. Better if this code is here.
			end if;

			VSYNC<=etat_vsync;
			HSYNC<=etat_hsync;

			if dispH='1' and dispV='1' then
				DE<='1';
			else
				DE<='0';
			end if;
		end if;
	end if;
end process Main;

end architecture;

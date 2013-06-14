--
--
--  This file is a part of JOP, the Java Optimized Processor
--
--  Copyright (C) 2001-2008, Martin Schoeberl (martin@jopdesign.com)
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--


--
--	sc_pack.vhd
--
--	Package for SimpCon defines
--
--	Author: Martin Schoeberl (martin@jopdesign.com)
--	
--
--	2007-03-16  first version
--
--

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package sc_pack is

	-- two more bits than needed for the main memory
	--    one to distinguish between memory and IO access
	--    one more to allow memory mirroring for size auto
	--        detection at boot time
	constant SC_ADDR_SIZE : integer := 23;
	constant RDY_CNT_SIZE : integer := 2;

	type sc_cache_type is (bypass, direct_mapped_const, direct_mapped, full_assoc);
	
	type sc_out_type is record
		address		: std_logic_vector(SC_ADDR_SIZE-1 downto 0);
		wr_data		: std_logic_vector(31 downto 0);
		rd			: std_logic;
		wr			: std_logic;
		atomic		: std_logic;
		cache		: sc_cache_type;
		cinval		: std_logic;	-- invalidate the data cache
		tm_cache	: std_logic;
		tm_broadcast: std_logic;
	end record;

	type sc_in_type is record
		rd_data		: std_logic_vector(31 downto 0);
		rdy_cnt		: unsigned(RDY_CNT_SIZE-1 downto 0);
	end record;
	
	type sc_out_array_type is array (integer range <>) of sc_out_type;
	type sc_in_array_type is array (integer range <>) of sc_in_type;

	constant DEFAULT_TIMEOUT_CYCLES : integer := 50; 

	procedure sc_write(
		signal clk : in std_logic;
		constant addr : in natural;
		constant data : in natural;
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES);

	procedure sc_write(
		signal clk : in std_logic;
		constant addr : in std_logic_vector(SC_ADDR_SIZE-1 downto 0);
		constant data : in std_logic_vector(31 downto 0);
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES);

	procedure sc_read(
		signal clk : in std_logic;
		constant addr : in natural;
		variable data : out natural;
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES);
		
	procedure sc_read(
		signal clk : in std_logic;
		constant addr : in std_logic_vector(SC_ADDR_SIZE-1 downto 0);
		variable data : out std_logic_vector(31 downto 0);
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES);
	


end sc_pack;

package body sc_pack is

	procedure sc_write(
		signal clk : in std_logic;
		constant addr : in natural;
		constant data : in natural;
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES) is

		variable txt : line;
		variable timedout : boolean := false;
	begin

		write(txt, now, right, 8);
		write(txt, string'(" wrote "));
		write(txt, data);
		write(txt, string'(" to addr. "));
		write(txt, addr);

		sc_out.wr_data <= std_logic_vector(to_unsigned(data, sc_out.wr_data'length));
		sc_out.address <= std_logic_vector(to_unsigned(addr, sc_out.address'length));
		sc_out.wr <= '1';
		sc_out.rd <= '0';

		-- one cycle valid
		wait until rising_edge(clk);
		sc_out.wr_data <= (others => 'X');
		sc_out.address <= (others => 'X');
		sc_out.wr <= '0';

		for i in 1 to timeout_cycles loop
			wait until rising_edge(clk);
			exit when sc_in.rdy_cnt = "00";
			if (i = timeout_cycles) then
				timedout := true;
				write (txt, LF & string'("No acknowledge recevied!"));
			end if;		
		end loop;

		writeline(output, txt);
		assert not timedout;
		
	end;
	
	procedure sc_write(
		signal clk : in std_logic;
		constant addr : in std_logic_vector(SC_ADDR_SIZE-1 downto 0);
		constant data : in std_logic_vector(31 downto 0);
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES) is

		variable txt : line;
		variable timedout : boolean := false;
	begin

		write(txt, now, right, 8);
		write(txt, string'(" wrote "));
		write(txt, to_integer(unsigned(data)));
		write(txt, string'(" to addr. "));
		write(txt, to_integer(unsigned(addr)));

		sc_out.wr_data <= data;
		sc_out.address <= addr;
		sc_out.wr <= '1';
		sc_out.rd <= '0';

		-- one cycle valid
		wait until rising_edge(clk);
		sc_out.wr_data <= (others => 'X');
		sc_out.address <= (others => 'X');
		sc_out.wr <= '0';

		for i in 1 to timeout_cycles loop
			wait until rising_edge(clk);
			exit when sc_in.rdy_cnt = "00";
			if (i = timeout_cycles) then
				timedout := true;
				write (txt, LF & string'("No acknowledge recevied!"));
			end if;		
		end loop;

		writeline(output, txt);
		assert not timedout;
		
	end;
	

	procedure sc_read(
		signal clk : in std_logic;
		constant addr : in natural;
		variable data : out natural;
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES) is

		variable txt : line;
		variable in_data : natural;
		variable timedout : boolean := false;
	begin

		write(txt, now, right, 8);
		write(txt, string'(" read from addr. "));
		write(txt, addr);
		writeline(output, txt);

		sc_out.address <= std_logic_vector(to_unsigned(addr, sc_out.address'length));
		sc_out.wr_data <= (others => 'X');
		sc_out.wr <= '0';
		sc_out.rd <= '1';

		-- one cycle valid
		wait until rising_edge(clk);
		sc_out.address <= (others => 'X');
		sc_out.rd <= '0';

		-- wait for acknowledge
		for i in 1 to timeout_cycles loop
			wait until rising_edge(clk);
			exit when sc_in.rdy_cnt = "00";
			if (i = timeout_cycles) then
				timedout := true;
				write (txt, LF & string'("No acknowledge recevied!"));
			end if;		
		end loop;

		in_data := to_integer(unsigned(sc_in.rd_data));
		data := in_data;

		write(txt, now, right, 8);
		write(txt, string'(" value: "));
		write(txt, in_data);

		writeline(output, txt);
		assert not timedout;
		
	end;

	procedure sc_read(
		signal clk : in std_logic;
		constant addr : in std_logic_vector(SC_ADDR_SIZE-1 downto 0);
		variable data : out std_logic_vector(31 downto 0);
		signal sc_out : out sc_out_type;
		signal sc_in  : in sc_in_type;
		constant timeout_cycles : integer := DEFAULT_TIMEOUT_CYCLES) is

		variable txt : line;
		variable in_data : natural;
		variable timedout : boolean := false;
	begin

		write(txt, now, right, 8);
		write(txt, string'(" read from addr. "));
		write(txt, to_integer(unsigned(addr)));
		writeline(output, txt);

		sc_out.address <= addr;
		sc_out.wr_data <= (others => 'X');
		sc_out.wr <= '0';
		sc_out.rd <= '1';

		-- one cycle valid
		wait until rising_edge(clk);
		sc_out.address <= (others => 'X');
		sc_out.rd <= '0';

		-- wait for acknowledge
		for i in 1 to timeout_cycles loop
			wait until rising_edge(clk);
			exit when sc_in.rdy_cnt = "00";
			if (i = timeout_cycles) then
				timedout := true;
				write (txt, LF & string'("No acknowledge recevied!"));
			end if;		
		end loop;

		in_data := to_integer(unsigned(sc_in.rd_data));
		data := sc_in.rd_data;

		write(txt, now, right, 8);
		write(txt, string'(" value: "));
		write(txt, in_data);

		writeline(output, txt);
		assert not timedout;
		
	end;


end sc_pack;

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
--	sim_jop_config_100.vhd
--
--	package for 100MHz definitions
--	for ModelSim
--

library ieee;
use ieee.std_logic_1164.all;

package jop_config is

	-- constants for 100MHz testbench clock
	constant clk_freq : integer := 100000000;
	constant pll_mult : natural := 1;
	constant pll_div : natural := 1;

	-- constant for on-chip memory
	constant ram_width : integer := 9; -- address bits of internal ram (sp,...)
        -- Original JOP has a ram address bits of 8

end jop_config;

package body jop_config is

end jop_config;

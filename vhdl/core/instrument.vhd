--
--
--  This file is a part of JOP, the Java Optimized Processor
--
--  Copyright (C) 2013, Avinash Malik (avinash.malik@auckland.ac.nz)
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
--	instrument.vhd

-- This file is used to decode the mask for hardware counters
-- It is attached to jopcpu.

-- Sun Jun 9 13:15:07 NZST 2013

Library IEEE;
Library STD;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std.all;
use STD.textio.writeline;
use STD.textio.write;
use STD.textio.text;
use STD.textio.line;
use STD.textio.file_open;
use STD.textio.file_close;
use STD.textio.output;


use work.jop_config_global.all;

entity instrument is

  generic (width : integer; log_file:string := "/tmp/counter.txt");
  port (
    clk, reset: in std_logic;
    ena_cyc, ena_hw:    in std_logic;
    ain:        in std_logic_vector(width-1 downto 0);
    ena_poc,ena_pmc,ena_pac:    out std_logic;
    inst_state: out std_logic_vector (CD_NUM+1 downto 0);
    hw:  out std_logic_vector (width-1 downto 0); -- The output to the stack
    fmc: in std_logic_vector (width-1 downto 0); -- The input from method cache (finds)
    mmc: in std_logic_vector (width-1 downto 0); -- The input from method cache (misses)
    foc: in std_logic_vector (width-1 downto 0); -- The input from object cache (finds)
    moc: in std_logic_vector (width-1 downto 0) -- The input from object cache (misses)
    );
end instrument;
--
-- This is the structure of the MASKS in ain
-- ena_cyc
-- Bit 0 : EOT counter
-- Bit 1 : MC
-- Bit 2 : OC
-- Bit 3 : AC
-- Bit 26..25: state: 00 -- stop, 01 -- start, 10 -- reset, 11 -- dump
-- Make up the other 7 bits as you see fit!!
--
--

architecture behavioral of instrument is
  type eot_counter_type is array (0 to CD_NUM-1) of unsigned (width-1 downto 0);
  type tick_count_type is array (0 to CD_NUM-1) of unsigned (width-1 downto 0);
  signal eot_counter : eot_counter_type := ((others => (others => '0'))); 
  signal tick_counter : tick_count_type := ((others => (others => '0'))) ; 
  signal a : std_logic_vector (CD_NUM-1 downto 0);
  signal dump, count, res : std_logic ; 
  signal mhw : std_logic_vector (width-1 downto 0) := (others => '0');
  file file_pointer: TEXT open write_mode is log_file; 

begin

  process (clk,count,res)
  begin
    if (res = '1') then
      eot_counter(to_integer(unsigned(a))) <= (others => '0');
    elsif rising_edge(clk) and (count = '1') then
      eot_counter(to_integer(unsigned(a))) <= eot_counter(to_integer(unsigned(a))) + 1;
    end if;
  end process;

  process (ena_hw, ain, fmc, mmc, foc, moc, eot_counter)
  begin
    if (ena_hw = '1') then
      -- Change this to add more hardware counters
      case ain(3 downto 0) is
        -- The find method cache
        when "0001" =>
          mhw <= fmc;
        -- The misses method cache
        when "0010" =>
          mhw <= mmc;
        -- The finds object cache
        when "0011" =>
          mhw <= foc;
        -- The misses object cache
        when "0100" =>
          mhw <= moc;
        -- The default case
        when others => mhw <= std_logic_vector(eot_counter(to_integer(unsigned(a))));
      end case;
    end if;
  end process;

-- The main process
  process (clk,reset)
  begin
    if (reset = '1') then
      ena_pmc <= '0';
      ena_pac <= '0';
      ena_poc <= '0';
      dump <= '0';
      count <= '0';
      res <= '0';
      inst_state <= (others => '0');
    -- Resetting all counters to 0
    elsif rising_edge(clk) then
      inst_state(1 downto 0) <= "XX";
      if (ena_cyc = '1') then
        ena_pmc <= ain(1);
        ena_poc <= ain(2);
        ena_pac <= ain(3);
        a <= ain((9+CD_NUM) downto 10);
        inst_state(CD_NUM+1 downto 2) <= ain((9+CD_NUM) downto 10);        -- The CD number
        inst_state(1 downto 0) <= ain(26 downto 25); -- The state
        dump <= '0';
        res <= '0';
        -- Start
        if (ain(26 downto 25) = "01")  and (ain(0) = '1') then
          count <= '1';
        -- Reset the counter
        elsif (ain(26 downto 25) = "10") and (ain(0) = '1') then
          res <= '1';
        -- Dump
        elsif (ain(26 downto 25) = "11") and (ain(0) = '1') then
          tick_counter(to_integer(unsigned(a))) <= tick_counter(to_integer(unsigned(a))) + 1;
          dump <= '1';
        -- Stop
        elsif (ain(26 downto 25) = "00") and (ain (0) = '1') then
          count <= '0';
        end if;
      elsif (ena_hw = '1') then
        -- The user wants the hardware counter pushed onto the stack
      end if;
    end if;
  end process;
  hw <= mhw;
  process(dump)
    variable f : integer := 0;
    variable m : integer := 0;
    variable tick : integer := 0;
    variable line_num : line;
  begin
    if (dump = '1') then
      f := to_integer(unsigned(a));
      tick := to_integer(tick_counter(to_integer(unsigned(a))));
      m := to_integer(eot_counter(to_integer(unsigned(a))));
      write(line_num,string'("[CD="));
      write(line_num,integer'image(f));
      write(line_num,string'(",EOT="));
      write(line_num,integer'image(tick));
      write(line_num,string'(",cyc="));
      write(line_num,integer'image(m));
      write(line_num,string'("]"));
      writeline(file_pointer,line_num);
    end if;
  end process;
end behavioral;


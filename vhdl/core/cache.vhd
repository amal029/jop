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
--	cache.vhd
--
--	Bytecode caching
--
--	jpc_with (set in top-level) configures the size.
--	Upper limit is 12 bits (4KB) due to the restriction in the
--	.jop file format (see JOPWriter.java). Changing this breaks
--	compatibility with other versions of JOP.
--
--	2005-01-11	first version
--	2005-05-03	configurable size (jpc_width)
--	2005-05-09	correction for ModelSim
--

-- library std;
-- use std.textio.all;


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

use work.jop_types.all;
use work.jop_config_global.all;

-- 2**jpc_width is the caches size in bytes
-- 2**block_bits is the number of blocks

entity mcache is
  generic (jpc_width : integer; block_bits : integer; tag_width : integer := 18; log_file:string := "/tmp/mc_profile.txt"; width : integer := 32);

  port (

    clk, reset	: in std_logic;

    bc_len		: in std_logic_vector(METHOD_SIZE_BITS-1 downto 0);		-- length of method in words
    bc_addr		: in std_logic_vector(17 downto 0);		-- memory address of bytecode

    find		: in std_logic;							-- start lookup

    -- start of method in bc cache
    -- in 32 bit words - we load only at word boundries
    bcstart		: out std_logic_vector(jpc_width-3 downto 0);

    rdy			: out std_logic;						-- lookup finished
    in_cache	: out std_logic;							-- method is in cache
    ena_pmc : in std_logic;
    inst_state: in std_logic_vector (CD_NUM+1 downto 0);
    fmc: out std_logic_vector (width-1 downto 0); -- The input from method cache (finds)
    mmc: out std_logic_vector (width-1 downto 0) -- The input from method cache (misses)
    );
end mcache;

architecture rtl of mcache is

--
--	signals for mem interface
--
  type state_type		is (
    idle, s1, s2
    );
  signal state 		: state_type;

  constant blocks		: integer := 2**block_bits;

  signal block_addr	: std_logic_vector(block_bits-1 downto 0);
  -- tag_width can be used to reduce cachable area - saves a lot in the comperators
  signal use_addr		: std_logic_vector(tag_width-1 downto 0);

  type tag_array is array (0 to blocks-1) of std_logic_vector(tag_width-1 downto 0);
  signal tag			: tag_array;

  -- pointer to next block to be used on a miss
  signal nxt			: unsigned(block_bits-1 downto 0);

  -- (length of the method)-1 in blocks, 0 is one block
  signal nr_of_blks	: unsigned(block_bits-1 downto 0);

  signal clr_val		: std_logic_vector(blocks-1 downto 0);
  type count_type is array (0 to CD_NUM-1) of unsigned (width-1 downto 0);
  type tick_count_type is array (0 to CD_NUM-1) of unsigned (width-1 downto 0);
  signal finds, misses : count_type;
  signal ticks: tick_count_type;
  signal dump : std_logic;
  signal my_state : std_logic; 
  file file_pointer: TEXT open write_mode is log_file; 
begin

  assert jpc_width>=11 report "Minimum M$ size is 2KB";
  
  bcstart <= block_addr & std_logic_vector(to_unsigned(0, jpc_width-2-block_bits));
  use_addr <= bc_addr(tag_width-1 downto 0);

  nr_of_blks <= resize(unsigned(bc_len(METHOD_SIZE_BITS-1 downto jpc_width-2-block_bits)), block_bits);
  fmc <= std_logic_vector(finds(to_integer(unsigned(inst_state(CD_NUM+1 downto 2))))); 
  mmc <= std_logic_vector(misses(to_integer(unsigned(inst_state(CD_NUM+1 downto 2))))); 

  process(clk, reset, find)

  begin
    if (reset='1') then
      state <= idle;
      rdy <= '1';
      in_cache <= '0';
      ticks <= ((others => (others => '0'))); 
      finds <= ((others => (others => '0'))); 
      misses <= ((others => (others => '0')));
      my_state <= '0';
      block_addr <= (others => '0');
      nxt <= (others => '0');
      for i in 0 to blocks-1 loop
        tag(i) <= (others => '0');
      end loop;
      dump <= '0';
    elsif rising_edge(clk) then

      dump <= '0';
      if (ena_pmc = '1') and (inst_state(1 downto 0) = "11") then
        ticks(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) <= ticks(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) + 1;
        dump <= '1';
      elsif (ena_pmc = '1') and (inst_state(1 downto 0) = "10") then
        finds(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) <= (others => '0');
        misses(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) <= (others => '0');
      elsif (ena_pmc = '1') and (inst_state(1 downto 0) = "01") then
        my_state <= '1';              
      elsif (ena_pmc = '1') and (inst_state(1 downto 0) = "00") then
        my_state <= '0';
      end if;
      case state is

        when idle =>
          state <= idle;
          rdy <= '1';
          if find = '1' then
            rdy <= '0';
            state <= s1;
          end if;

        -- check for a hit
        when s1 =>
          -- This can only be incremented here, because this is when one is
          -- trying to search in the cache!!
          if (ena_pmc = '1') and (my_state = '1') then
            finds(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) <= finds(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) + 1;
          end if;
          in_cache <= '0';
          state <= s2;
          block_addr <= std_logic_vector(nxt);

          -- Does this generate optimal logic?
          -- Only one if will be true. Therefore, there
          -- should be a place for optimization

          -- comment this out for no caching
          for i in 0 to blocks-1 loop
            if tag(i) = use_addr then
              block_addr <= std_logic_vector(to_unsigned(i, block_bits));
              in_cache <= '1';
              state <= idle;
            end if;
          end loop;
--
-- remove the comment to force a single method
-- block cache
--
-- block_addr <= (others => '0');
-- in_cache <= '0';
-- state <= s2;

        -- correct tag memory on a miss
        when s2 =>

          for i in 0 to blocks-1 loop
					-- these two statements are xor - optimization?
            if clr_val(i) = '1' then
              tag(i) <= (others => '0');
            end if;
          end loop;
          tag(to_integer(nxt)) <= use_addr;
          if (ena_pmc = '1') and (my_state = '1') then
            misses(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) <= misses(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))) + 1;
          end if;
          state <= idle;
          -- optimization to not advance the next pointer
          -- on short methods
--				if unsigned(bc_len) > 14 then
          nxt <= nxt + nr_of_blks + 1;
--				end if;


      end case;
      
    end if;
  end process;

  process(dump)
    variable f : integer := 0;
    variable m : integer := 0;
    variable c : integer := 0;
    variable t : integer := 0;
    variable line_num : line;
  begin
    if (dump = '1') then
      f := to_integer(finds(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))));
      m := to_integer(misses(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))));
      t := to_integer(ticks(to_integer(unsigned(inst_state(CD_NUM+1 downto 2)))));
      c := to_integer(unsigned(inst_state(CD_NUM+1 downto 2)));
      write(line_num,string'("[CD="));
      write(line_num,integer'image(c));
      write(line_num,string'(",EOT="));
      write(line_num,integer'image(t));
      write(line_num,string'(",F="));
      write(line_num,integer'image(f));
      write(line_num,string'(",M="));
      write(line_num,integer'image(m));
      write(line_num,string'("]"));
      writeline(file_pointer,line_num);
    end if;
  end process;

--
--	Determine which block entries have to be cleared in the tag registers.
--
--	clr_val could b registered as we can calculate
-- process(nxt, nr_of_blks) begin
  process(clk)

    variable val		: integer;
  begin

    if rising_edge(clk) then

      for i in 0 to blocks-1 loop
-- write(output, "cache...");
-- val := i;
-- write(output, integer'image(val));
-- val := blocks;
-- write(output, integer'image(val));
        if i<=nr_of_blks then
          clr_val(to_integer(nxt+i)) <= '1';
        else
          clr_val(to_integer(nxt+i)) <= '0';
        end if;
      end loop;
    end if;
  end process;


end rtl;

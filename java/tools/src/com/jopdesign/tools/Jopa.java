/*
  This file is part of JOP, the Java Optimized Processor
  see <http://www.jopdesign.com/>

  Copyright (C) 2001-2008, Martin Schoeberl (martin@jopdesign.com)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package com.jopdesign.tools;

/**
   Assemler for JOP3

   Author: Martin Schoeberl


   revision:

   2001-09-22	creation
   2001-10-24	working version
   2001-12-08	intruction set change (16->8 bit)
   2005-01-17	interrupt mux in jtbl.vhd
   2005-02-06	JOP version in stack RAM at address 64
   2005-02-20	Generate memory data for the simulation
   2006-12-29	Remove bcfetbl.vhd generation (it's part of rom.vhd/mif)
   Changed rom legth to 2K
   2009-09-05	Branch and jump offsets are part of the instruction (no offtbl.vhd)

*/

import com.jopdesign.tools.Instruction.JmpType;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Serializable;
import java.io.StreamTokenizer;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Jopa {

  private String fname;
  static final int ADDRBITS = 11;
  /** length of microcode instruction including nxt and opd */	
  static final int DATABITS = Instruction.INSTLEN+2;
  static final int CONST_ADDR = 32;
  static final int VER_ADDR = 64-2;
  static final int RAM_LEN = 256;
  static final int ROM_LEN = 1<<ADDRBITS;
  private String srcDir;
  private String dstDir;
  private boolean error;

  public Jopa(String fn) {
    this(fn,System.getProperty("user.dir"),System.getProperty("user.dir"));
  }

  public Jopa(String[] clist) {
    srcDir = System.getProperty("user.dir");
    dstDir = System.getProperty("user.dir");
    processOptions(clist);
    if (!srcDir.endsWith(File.separator))
      srcDir += File.separator;
    if (!dstDir.endsWith(File.separator))
      dstDir += File.separator;
  }
  /** 
   * Creates a new Jopa (JopAssembler) instance
   * @param fn name of the assembler file
   * @param src source directory for assembler files
   * @param dst destination directory for ROM vhdl
   */
  public Jopa(String fn, String src, String dst) {
    this.fname = fn;
    this.srcDir = src;
    this.dstDir = dst;
    if (!srcDir.endsWith(File.separator))
      srcDir += File.separator;
    if (!dstDir.endsWith(File.separator))
      dstDir += File.separator;
  }
  void error(StreamTokenizer in, String s) {
    System.out.println((in.lineno()-1)+" error: "+s);
    error = true;
  }

  private StreamTokenizer getSt() {

    try {
      FileReader fileIn = new FileReader(srcDir + fname);
      StreamTokenizer in = new StreamTokenizer(fileIn);

      in.wordChars( '_', '_' );
      in.wordChars( ':', ':' );
      in.eolIsSignificant(true);
      in.slashStarComments(true);
      in.slashSlashComments(true);
      in.lowerCaseMode(true);
      return in;
    } catch (IOException e) {
      System.out.println(e.getMessage());
      System.exit(-1);
      return null;
    }
  }

  public static class Line implements Serializable {
    private static final long serialVersionUID = 1L;

    int jinstr;
    String label;
    Instruction instr;
    int special;
    int intVal;
    String symVal;
    boolean nxt;
    boolean opd;

    public boolean hasNxtFlag() {
      return nxt;
    }
    public boolean hasOpdFlag() {
      return opd;
    }
    public Instruction getInstruction() {
      return instr;
    }
    public String getSymVal() {
      return symVal;
    }
    public Integer getIntVal() {
      return intVal;
    }
    public Integer getSpecial() {
      return special;
    }

    @Override
    public String toString() {
      StringBuffer sb = new StringBuffer();
      sb.append(instr.name);
      if(instr.opdSize!=0) {
	sb.append(' '); 
	if(symVal != null) { sb.append(symVal); }
	else               { sb.append(special); sb.append(" / "); sb.append(intVal); }
      }
      if(hasOpdFlag()) sb.append(" [opd]");
      if(hasNxtFlag()) sb.append(" [nxt]");
      return sb.toString();
    }
  }

  private Line getLine(StreamTokenizer in) {

    Line l = new Line();
    l.jinstr = -1;

    try {
      for (int cnt=0; in.nextToken()!=StreamTokenizer.TT_EOL; ++cnt) {

	if (in.ttype == StreamTokenizer.TT_WORD) {

	  int pos = in.sval.indexOf(":");
	  if (pos!=-1) {
	    String s = in.sval.substring(0, pos);
	    l.jinstr = JopInstr.get(s);
	    if (l.jinstr==-1) {
	      l.label = s;
	    }
	  } else {
	    if (in.sval.equals("nxt")) {
	      l.nxt = true;
	    } else if (in.sval.equals("opd")) {
	      l.opd = true;
	    } else {
	      Instruction i = Instruction.get(in.sval);
	      if (i==null) {
		l.symVal = in.sval;
	      } else if (l.instr==null) {
		l.instr = i;
	      }
	    }
	  }

	} else if (in.ttype == StreamTokenizer.TT_NUMBER) {

	  l.intVal = (int) in.nval;

	} else if (in.ttype == '=') {
	  l.special = in.ttype;
	} else if (in.ttype == '?') {
	  l.special = in.ttype;
	} else {
	  error(in, "'"+(char) in.ttype+"' syntax");
	}
      } // EOL
    } catch (IOException e) {
      System.out.println(e.getMessage());
      System.exit(-1);
    }

    return l;
  }


  static String bin(int val, int bits) {

    String s = "";
    for (int i=0; i<bits; ++i) {
      s += (val & (1<<(bits-i-1))) != 0 ? "1" : "0";
    }
    return s;
  }

  private Map<String, Integer> symMap  = new HashMap<String, Integer>();
  private int memcnt = 0;
  private List<String> varList = new LinkedList<String>();
  private int version = -1;
  private Map<Integer, Integer> jinstrMap = new HashMap<Integer, Integer>();
  private List<Line> instructions = new LinkedList<Line>();

  /**
   * Parse the assembler file and build symbol table (first pass).
   * During this pass, the assembler code, the symboltable and vartable are build.
   * @return the  map from program locations (pc) to microcode lines
   */
  public void pass1() {
    StreamTokenizer in = getSt();
    int pc = 0;

    try {
      while (in.nextToken() != StreamTokenizer.TT_EOF) {
	in.pushBack();
	Line l = getLine(in);
	//System.out.println("L"+in.lineno()+" "+l.jinstr+" "+l.label+" "+l.instr+" '"+(char) l.special+"' "+l.intVal+" "+l.symVal);

	if (l.jinstr==-1) {
	  if (l.label!=null) {
	    if (symMap.containsKey(l.label)) {
	      error(in, "symbol "+l.label+" already defined");
	    } else {
	      symMap.put(l.label, new Integer(pc));
	    }
	  }

	  if (l.special=='=') {
	    if (l.symVal==null) {
	      error(in, "missing symbol for '='");
	    } else {
	      if (symMap.containsKey(l.symVal)) {
		error(in, "symbol "+l.symVal+" allready defined");
	      } else {
		symMap.put(l.symVal, new Integer(l.intVal));
	      }
	    }
	  } else if (l.special=='?') {
	    if (symMap.containsKey(l.symVal)) {
	      error(in, "symbol "+l.symVal+" allready defined");
	    } else {
	      symMap.put(l.symVal, new Integer(memcnt++));
	      varList.add(l.symVal);
	    }
	  }

	} else {
	  jinstrMap.put(l.jinstr,pc);
	}
	if (l.instr!=null) {
	  ++pc;
	  instructions.add(l);
	}
      }
    } catch (IOException e) {
      System.out.println(e.getMessage());
      System.exit(-1);
    }
    //System.out.println(symMap);
  }
  /**
   * Get the symbol table:
   * Maps labels to program counter, constants to values and variables to memory
   * positions
   * @return 
   */
  public Map<String, Integer> getSymMap() {
    return this.symMap;
  }
  /**
   * Get list of variables
   * @return 
   */
  public List<String> getVarList() {
    return this.varList;
  }
  /** 
   * get table of java instructions 
   */
  public Map<Integer, Integer> getJavaInstructions() {
    return this.jinstrMap;
  }
  /**
   * Get instruction list
   * @return 
   */
  public List<Line> getInstructions() {
    return this.instructions;
  }
	
  /**
   * @param i
   * @param len
   * @return
   */
  static String hex(int i, int len) {

    String s = Integer.toHexString(i);
    int cnt = len-s.length();
    for (int k=0; k<cnt; ++k) s = "0"+s;
    return s;
  }


  private Map<Integer, Integer> constMap = new HashMap<Integer, Integer>();
  private List<Integer> constList = new LinkedList<Integer>();

  private int[] romData = new int[ROM_LEN];
  private int romLen = 0;

  private int[] ramData = new int[RAM_LEN];

  /**
   *	second pass.
   *	generate code and write rom.mif and ram.mif.
   */
  public void pass2() {

    StreamTokenizer in = getSt();
    int pc = 0;
    int ji_cnt = 0;


    try {
      FileWriter rom = new FileWriter(dstDir + "rom.mif");
      FileWriter jtbl = new FileWriter(dstDir + "jtbl.vhd");

      BufferedReader inraw = new BufferedReader(new FileReader(srcDir + fname));

      String line;
      //
      //	print rom.mif head
      //
      line = "--\n";
      line += "--\trom.mif\n";
      line += "--\n";
      line += "depth = "+ROM_LEN+";\n";
      line += "width = "+DATABITS+";\n";
      line += "\n";
      line += "content\n";
      line += "\n";
      line += "begin\n";
      line += "\n";
      line += "\t[0..1ff] : 080;	-- nop TODO: new instruction\n\n";

      rom.write( line );

      //
      //	print jtbl.vhd head
      //
      line = "--\n";
      line += "--\tjtbl.vhd\n";
      line += "--\n";
      line += "--\tjump table for java bc to jvm address\n";
      line += "--\n";
      line += "--\t\tDONT edit this file!\n";
      line += "--\t\tgenerated by Jopa.java\n";
      line += "--\n";
      line += "\n";
      line += "library ieee;\n";
      line += "use ieee.std_logic_1164.all;\n";
      line += "use ieee.std_logic_arith.all;\n";
      line += "use ieee.std_logic_unsigned.all;\n";
      line += "\n";
      line += "entity jtbl is\n";
      line += "port (\n";
      line += "\tbcode\t: in std_logic_vector(7 downto 0);\n";
      line += "\tint_pend\t: in  std_logic;\n";
      line += "\texc_pend\t: in  std_logic;\n";			
      line += "\tq\t\t: out std_logic_vector("+(ADDRBITS-1)+" downto 0)\n";
      line += ");\n";
      line += "end jtbl;\n";
      line += "\n";
      line += "--\n";
      line += "--\tunregistered rdbcode\n";
      line += "--\tunregistered dout\n";
      line += "--\n";
      line += "architecture rtl of jtbl is\n";
      line += "\n";
      line += "\tsignal\taddr\t: std_logic_vector("+(ADDRBITS-1)+" downto 0);\n";
      line += "\n";
      line += "begin\n";
      line += "\n";
      line += "process(bcode) begin\n";
      line += "\n";
      line += "\tcase bcode is\n";
      line += "\n";

      jtbl.write( line );


      int noim_address = 0;
      int int_address = 0;
      int exc_address = 0;

      while (in.nextToken() != StreamTokenizer.TT_EOF) {
	in.pushBack();

	Line l = getLine(in);

	if (l.jinstr!=-1) {
	  ++ji_cnt;
	  if (JopInstr.name(l.jinstr).equals("sys_int")) {
	    int_address = pc;
	  } else if (JopInstr.name(l.jinstr).equals("sys_exc")) {
	    exc_address = pc;
	  } else if (JopInstr.name(l.jinstr).equals("sys_noim")) {
	    noim_address = pc;
	  } else {
	    jtbl.write("\t\twhen \""+bin(l.jinstr, 8) +
		       "\" => addr <= \""+bin(pc, ADDRBITS)+"\";" +
		       "\t--\t"+hex(pc,4)+"\t"+JopInstr.name(l.jinstr)+"\n");
	  }
	}

	line = "\t";
	if (l.instr==null) {
	  line += "            ";
	} else {

	  //
	  //	do the assembling
	  //
	  int opcode = l.instr.opcode;

	  if (l.instr.opdSize!=0) {
	    int opVal = 0;
	    if (l.symVal!=null) {
	      Integer i = symMap.get(l.symVal);
	      if (i==null) {
		error(in, "Symbol "+l.symVal+" not defined");
	      } else {
		opVal = i.intValue();
	      }
	    } else {
	      opVal = l.intVal;
	    }

	    if (l.instr.name.equals("ldi")) {
	      Integer i = new Integer(opVal);
	      Integer addr;
	      if (constMap.containsKey(i)) {
		addr = constMap.get(i);
	      } else {
		addr = new Integer(constMap.size());
		constMap.put(i, addr);
		constList.add(i);
	      }
	      opVal = addr.intValue();
	    }

						
	    int mask = (1<<l.instr.opdSize)-1;

	    // for branches and jumps opVal points to the target address
	    if (l.instr.jType==JmpType.JMP || l.instr.jType==JmpType.BR) {
	      // relative address
	      opVal = opVal-pc-1;
	      // check maximum relative offset
	      if (opVal>(mask>>1) || opVal<(-((mask>>1)+1))) {
		error(in, "jmp/br address too far: "+opVal);								
	      }
	      opVal &= mask;
	    }

	    // general check
	    if (opVal>mask || opVal<0) {
	      error(in, "operand wrong: "+opVal);
	    }
	    opcode |= opVal & mask;		// use operand
	  }

	  if (l.nxt) opcode |= 0x2<<Instruction.INSTLEN;
	  if (l.opd) opcode |= 0x1<< Instruction.INSTLEN;
	  romData[romLen] = opcode;
	  ++romLen;
	  line += hex(pc, 4)+" : "+hex(opcode, 3)+";\t";

	  ++pc;
	}


	line += "\t--\t"+inraw.readLine()+"\n";
	rom.write( line );
	// System.out.print(line);



      }

      rom.write( "\nend;\n" );
      rom.close();

      line = "\n";
      line += "\t\twhen others => addr <= \""+bin(noim_address, ADDRBITS)+
	"\";\t\t--\t"+hex(noim_address,4)+"\tsys_noim\n";
      line += "\tend case;\n";
      line += "end process;\n";
      line += "\n";
      line += "process(int_pend, exc_pend, addr) begin\n";
      line += "\n";
      line += "\tq <= addr;\n";
      line += "\tif exc_pend='1' then\n";
      line += "\t\tq <= \""+bin(exc_address, ADDRBITS)+
	"\";\t\t--\t"+hex(exc_address,4)+"\tsys_exc\n";
      line += "\telsif int_pend='1' then\n";
      line += "\t\tq <= \""+bin(int_address, ADDRBITS)+
	"\";\t\t--\t"+hex(int_address,4)+"\tsys_int\n";
      line += "\tend if;\n";
      line += "end process;\n";
      line += "\n";

      line += "end rtl;\n";

      jtbl.write(line);
      jtbl.close();

      //
      //	print ROM as generic VHDL file
      //
      FileWriter romvhd = new FileWriter(dstDir + "rom.vhd");

      line = "--\n";
      line += "--\trom.vhd\n";
      line += "--\n";
      line += "--\tgeneric VHDL version of ROM\n";
      line += "--\n";
      line += "--\t\tDONT edit this file!\n";
      line += "--\t\tgenerated by Jopa.java\n";
      line += "--\n";
      line += "\n";
      line += "library ieee;\n";
      line += "use ieee.std_logic_1164.all;\n";
      line += "use ieee.std_logic_arith.all;\n";
      line += "use ieee.std_logic_unsigned.all;\n";
      line += "\n";
      line += "entity rom is\n";
      line += "generic (width : integer; addr_width : integer);\t-- for compatibility\n";
      line += "port (\n";
      line += "\tclk\t\t\t: in std_logic;\n";
      line += "\taddress\t\t: in std_logic_vector("+(ADDRBITS-1)+" downto 0);\n";
      line += "\tq\t\t\t: out std_logic_vector("+(DATABITS-1)+" downto 0)\n";
      line += ");\n";
      line += "end rom;\n";
      line += "\n";
      line += "architecture rtl of rom is\n";
      line += "\n";
      line += "\tsignal areg\t\t: std_logic_vector("+(ADDRBITS-1)+" downto 0);\n";
      line += "\tsignal data\t\t: std_logic_vector("+(DATABITS-1)+" downto 0);\n";
      line += "\n";
      line += "begin\n";
      line += "\n";
      line += "process(clk) begin\n";
      line += "\n";
      //			line += "\tif falling_edge(clk) then\n";
      //			line += "\t\tareg <= address;\n";
      //			line += "\tend if;\n";
      line += "\tif rising_edge(clk) then\n";
      //			line += "\t\tq <= data;\n";
      line += "\t\tareg <= address;\n";
      line += "\tend if;\n";
      line += "\n";
      line += "end process;\n";
      line += "\n";
      line += "\tq <= data;\n";
      line += "\n";
      line += "process(areg) begin\n";
      line += "\n";
      line += "\tcase areg is\n";
      line += "\n";

      romvhd.write(line);

      for (int i=0; i<romLen; ++i) {
	romvhd.write("\t\twhen \""+bin(i, ADDRBITS) +
		     "\" => data <= \""+bin(romData[i], DATABITS)+"\";");
	romvhd.write("\t-- "+"TODO: comment"+"\n");
      }


      line = "\n";
      line += "\t\twhen others => data <= \""+bin(0, DATABITS)+"\";\n";
      line += "\tend case;\n";
      line += "end process;\n";
      line += "\n";
      line += "end rtl;\n";

      romvhd.write(line);
      romvhd.close();

      PrintStream rom_mem = new PrintStream(new FileOutputStream(dstDir + "mem_rom.dat"));
      for (int i=0; i<ROM_LEN; ++i) {
	rom_mem.println(romData[i]+" ");
      }
      rom_mem.close();



      //
      //	Print symbol table as ram.mif and data for the simulation.
      //
      FileWriter ram = new FileWriter(dstDir + "ram.mif");
      for (int i=0; i<RAM_LEN; ++i) {
	ramData[i] = 0x12345678;
      }

      line = "--\n";
      line += "--\tram.mif\n";
      line += "--\n";
      line += "depth = "+RAM_LEN+";\n";
      line += "width = 32;\n";
      line += "\n";
      line += "content\n";
      line += "\n";
      line += "begin\n";
      //			line += "\t[0..ff] : 00000000;\n";
      line += "\t[0..ff] : 12345678;\n";
      line += "\n";
      ram.write( line );

      line = "--\n";
      line += "-- "+memcnt+" vars\n";
      line += "--\n\n";
      ram.write( line );

      //
      // Variables
      //
      for (int i=0; i<varList.size(); ++i) {
	String s = varList.get(i);
	ramData[i] = 0;
	line = "\t";
	line += hex(i, 4) + " : " ;
	line += hex(0, 8) + ";\t--\t";
	line += s + "\n";
	ram.write( line );
      }

      line = "--\n";
      line += "-- "+constMap.size()+" consts\n";
      line += "--\n\n";
      ram.write( line );
			
      if (constMap.size()>VER_ADDR-CONST_ADDR) {
	System.out.println("error: too many constants");
	System.exit(-1);
      }

      //
      //	Constants
      //
      for (int i=0; i<constList.size(); ++i) {
	Integer val = constList.get(i);
	ramData[CONST_ADDR+i] = val.intValue();
	line = "\t";
	line += hex(CONST_ADDR+i, 4) + " : " ;
	line += hex(val.intValue(), 8) + ";\t--\t";
	line += val + "\n";
	ram.write( line );
      }

      // check if version is set
      Integer ver = symMap.get("version");
      if (ver==null) {
	error(in, "version not set, setting to -1");
      } else {
	version = ver.intValue();
      }
      ramData[VER_ADDR] = version;
      ramData[VER_ADDR+1] = 0;
      ram.write("\n\n--\tVersion now in the constant area\n");
      line = "\t";
      line += hex(VER_ADDR, 4) + " : " ;
      line += hex(version, 8) + ";\t--\t";
      line += version + "\n";
      ram.write(line);
      line = "\t";
      line = "\t"+hex(VER_ADDR+1, 4) + " : " ;
      line += hex(0, 8) + ";\t--\tfor future use - FPGA type?\n";
      ram.write(line);

      ram.write( "\nend;\n" );
      ram.close();

      PrintStream ram_mem = new PrintStream(new FileOutputStream(dstDir + "mem_ram.dat"));
      for (int i=0; i<RAM_LEN; ++i) {
	ram_mem.println(ramData[i]+" ");
      }
      ram_mem.close();

      System.out.println(ji_cnt+" Instructions implemented");

    } catch (IOException e) {
      System.out.println(e.getMessage());
      System.exit(-1);
    }
  }

  private boolean processOptions(String clist[]) {
    boolean success = true;

    for (int i = 0; i < clist.length; i++) {
      if (clist[i].equals("-s")) {
	srcDir = clist[++i];
      } else if (clist[i].equals("-d")) {
	dstDir = clist[++i];
      } else {
	fname = clist[i];
      }
    }

    return success;
  }

  /**
   *	Main for Jop assembler.
   */
  public static void main(String args[]) {

    if (args.length < 1) {
      System.out.println(
			 "usage: java Jopa [-s srcDir] [-d dstDir] filename");
      System.exit(-1);
    }

    Jopa j = new Jopa(args);
    j.pass1();
    j.pass2();
    if (j.error) {
      throw new Error("Errors in assembler file!");
    }
  }
}

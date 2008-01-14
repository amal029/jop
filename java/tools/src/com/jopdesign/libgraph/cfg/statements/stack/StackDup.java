/*
 * Copyright (c) 2007,2008, Stefan Hepp
 *
 * This file is part of JOPtimizer.
 *
 * JOPtimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * JOPtimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.jopdesign.libgraph.cfg.statements.stack;

import com.jopdesign.libgraph.cfg.statements.common.CopyStmt;
import com.jopdesign.libgraph.cfg.statements.quad.QuadCopy;
import com.jopdesign.libgraph.cfg.statements.quad.QuadStatement;
import com.jopdesign.libgraph.cfg.variable.Variable;
import com.jopdesign.libgraph.cfg.variable.VariableTable;
import com.jopdesign.libgraph.struct.type.TypeInfo;

/**
 * @author Stefan Hepp, e0026640@student.tuwien.ac.at
 */
public class StackDup extends CopyStmt implements StackStatement {

    private TypeInfo[] types;
    private TypeInfo[] down;

    private TypeInfo[] popTypes;
    private TypeInfo[] pushTypes;

    public StackDup(TypeInfo type) {
        this.types = new TypeInfo[] {type};
        this.down = new TypeInfo[0];
        initialize();
    }

    public StackDup(TypeInfo type, TypeInfo down) {
        this.types = new TypeInfo[] {type};
        this.down = down != null ? new TypeInfo[] {down} : new TypeInfo[0];
        initialize();
    }

    public StackDup(TypeInfo[] types) {
        this.types = types;
        this.down = new TypeInfo[0];
        initialize();
    }

    public StackDup(TypeInfo[] types, TypeInfo[] down) {
        this.types = types;
        this.down = down != null ? down : new TypeInfo[0];
        initialize();
    }

    private void initialize() {
        popTypes = new TypeInfo[types.length + down.length];
        for ( int i = 0; i < down.length; i++ ) {
            popTypes[i] = down[i];
        }
        for ( int i = 0; i < types.length; i++ ) {
            popTypes[down.length+i] = types[i];
        }

        pushTypes = new TypeInfo[2*types.length + down.length];
        for ( int i = 0; i < types.length; i++ ) {
            pushTypes[i] = types[i];
        }
        for ( int i = 0; i < down.length; i++ ) {
            pushTypes[types.length+i] = down[i];
        }
        for ( int i = 0; i < types.length; i++ ) {
            pushTypes[types.length+down.length+i] = types[i];
        }
    }

    public TypeInfo[] getPopTypes() {
        return popTypes;
    }

    public TypeInfo[] getPushTypes() {
        return pushTypes;
    }

    public int getClockCycles() {
        return 0;
    }


    public TypeInfo[] getTypes() {
        return types;
    }

    public TypeInfo[] getDown() {
        return down;
    }

    public int getTypeLength() {
        int i = 0;
        for (int j = 0; j < types.length; j++) {
            i += types[j].getLength();
        }
        return i;
    }

    public int getDownLength() {
        int i = 0;
        if ( down == null ) {
            return 0;
        }
        for (int j = 0; j < down.length; j++) {
            i += down[j].getLength();
        }
        return i;
    }

    public QuadStatement[] getQuadCode(TypeInfo[] stack, VariableTable varTable) {
        QuadStatement[] stmts;
        if ( down.length > 0 ) {
            stmts = new QuadStatement[2*types.length+down.length];
            for ( int i = 0; i < types.length; i++) {
                Variable in = varTable.getDefaultStackVariable(stack.length - types.length + i);
                Variable out = varTable.getDefaultStackVariable(stack.length - types.length - down.length + i);
                stmts[i] = new QuadCopy(types[i], out, in);
            }
            for ( int i = 0; i < down.length; i++) {
                Variable in = varTable.getDefaultStackVariable(stack.length - types.length - down.length + i);
                Variable out = varTable.getDefaultStackVariable(stack.length - down.length + i);
                stmts[i] = new QuadCopy(types[i], out, in);
            }
            for ( int i = 0; i < types.length; i++) {
                Variable in = varTable.getDefaultStackVariable(stack.length - types.length + i);
                Variable out = varTable.getDefaultStackVariable(stack.length + i);
                stmts[i] = new QuadCopy(types[i], out, in);
            }
        } else {
            stmts = new QuadStatement[types.length];
            for ( int i = 0; i < stmts.length; i++) {
                Variable in = varTable.getDefaultStackVariable(stack.length - types.length + i);
                Variable out = varTable.getDefaultStackVariable(stack.length + i);
                stmts[i] = new QuadCopy(types[i], out, in);
            }
        }
        return stmts;
    }

    public int[] getCopyMap() {
        int[] map = new int[2*types.length + down.length];
        for ( int i = 0; i < types.length; i++) {
            map[i] = down.length + i;
        }
        for ( int i = 0; i < down.length; i++ ) {
            map[types.length + i] = i;
        }
        for ( int i = 0; i  < types.length; i++ ) {
            map[types.length + down.length] = down.length + i;
        }
        return map;
    }

    public String getCodeLine() {
        String name = "dup";
        if ( types.length > 1 ) {
            name += types.length;
        }
        if ( down.length > 0 ) {
            name += "_x" + down.length;
        }
        return name;
    }
}
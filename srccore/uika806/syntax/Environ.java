/*
 * Copyright (C) 2017 Chan Chung Kwong <1m02math@126.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * https://github.com/chungkwong/JSchemeMin
 *
 *
 *
 * Masahito Hemmi modified this source file.
 */
package uika806.syntax;

import java.util.Map;
import java.util.Optional;
import uika806.objects.SSymbol;

public interface Environ {

    Optional<Object> getOptional(SSymbol id);

    Object get(SSymbol id);

    /**
     * Check if a variable exists
     * @param id the name of the variable
     * @return
     */
    boolean containsKey(SSymbol id);

    void add(SSymbol id, Object obj);

    boolean isREPL();

    Object getSelfOptional(SSymbol id);

    void set(SSymbol id, Object obj);
    
    
    //---------------------------------
    void define(SSymbol sym, Object val);

    Environ extend(Object vars, Object vals);

    Object lookupValue(SSymbol sym);

    Map<SSymbol, Object> getUnmodifiableMap();

    String printEnv();
    
    Environ clearBinds();
    
    Environ getParent();
}

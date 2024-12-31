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

package uika806.lib;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;



//(import (scheme base))
//(import (scheme case-lambda))
//(import (scheme char))
//(import (scheme complex))
//(import (scheme eval))
//(import (scheme file))
//(import (scheme inexact))
//(import (scheme lazy))
//(import (scheme load))
//(import (scheme process-context))
//(import (scheme read))
//(import (scheme repl))
//(import (scheme time))
//(import (scheme write))



public class Library {

    public Cell name;
    public Map<SSymbol, SSymbol> export;
    ConcurrentHashMap<SSymbol, Object> funcs = new ConcurrentHashMap<>();

    Environ internal;
    /**
     * 注意： new すると、LibraryManagerに登録される。
     * 
     * @param name
     * @param export 
     */
    public Library(Cell name, Map<SSymbol, SSymbol> export  /*, Environ internal*/) {
        this.name = name;
        this.export = export;
//        this.internal = internal;
        LibraryManager.addLibrary(this);
    }

    public Cell getName() {
        return name;
    }

    /**
     * "startup.scm"を読み込む為の特別な環境を用意する。
     * Environインターフェースを通して,  HashMap funcs に登録が可能になる。
     * 
     * @return 
     */
    public Environ getStartupEnvment() {
        return new StartupEnviron(funcs);
    }
    
    public void add(SSymbol sym, Object obj) {
        funcs.put(sym, obj);
    }

    /**
     * Mapを読み込む際は、書き込みが出来ない、このメソッドを使って下さい。
     * 
     * @return 
     */
    public Map<SSymbol, Object> getUnmodifiableMap() {
        
        return Collections.unmodifiableMap(funcs);
    }

    @Override
    public String toString() {
        
        String sname = CurrentPort.printString(name);
        return "Lib" + sname ;
    }

}

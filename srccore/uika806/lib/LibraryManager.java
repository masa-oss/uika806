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

import java.util.ArrayList;
import java.util.List;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.pico.fn.EqualFn;

/**
 *
 * @author hemmi
 */
public class LibraryManager {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(LibraryManager.class);
    
    
    static class Pair {

        Cell name;
        Library lib;

        Pair(Cell name, Library lib) {
            this.name = name;
            this.lib = lib;
        }
    }

    static List<Pair> LIBS = new ArrayList<>();

    public static void addLibrary(Library lib) {

        // TODO 重複チェック
        Cell nm = lib.name;

        LIBS.add(new Pair(nm, lib));
    }
    
    public static Library findLib(Cell name) {
        
        LOG.info("40) findLib {}", name);
        LOG.info("40) LIBS {}", LIBS);
        
        for (Pair pair : LIBS) {
            
            Object obj = equalFn.invoke(pair.name, name);
            Boolean b = (Boolean) obj;
            if (b) {
                return pair.lib;
            }
        }
        // みつからない
        return null;
    }
    
    static EqualFn equalFn = new EqualFn();

}

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

import java.util.ArrayList;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * @author henmi
 */
public class ListBuilder {
    
    ArrayList<Object> list  = new ArrayList<>();
    
    Object last = EmptyList.NIL;
    
    public void add(Object toAdd) {
        list.add(toAdd);
    }
    
    public void setLast(Object val) {
        this.last = val;
    }
    
    
    
    
    public Object toList() {
        
        if (list.isEmpty()) {
            return EmptyList.NIL;
        }
        
        Object result = last;
        int len = list.size();
        for (int i = len -1; i >= 0; i--) {
            result = new Cell(list.get(i),  result );
        }
        return result;
    }
    
}

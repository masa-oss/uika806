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
import java.util.List;
import java.util.Stack;

/**
 *
 */
public class CapturedObjects {

    private final List<Object> objects;

    public CapturedObjects() {
        this.objects = new ArrayList<>();
    }

    @SuppressWarnings("unchecked")
    public void add(MultiIndex index, Object obj) {
        
        List<Object> toSearch = objects;
        Stack<Integer> indices = index.getIndices();
        for (int i = 0; i < indices.size() - 1; i++) {
            int curr = indices.get(i);
            while (curr >= toSearch.size()) {
                toSearch.add(new ArrayList<>());
            }
            Object wk = toSearch.get(curr);
            if (wk instanceof List) {
                toSearch = (List<Object>) wk;  // SuppressWarnings
            } else {
                throw new IllegalStateException();
            }
        }
        toSearch.add(indices.get(indices.size() - 1), obj);
    }

    public Object get(MultiIndex index) {
        
        Object toSearch = objects;
        for (Integer i : index.getIndices()) {
            toSearch = ((List) toSearch).get(i);
        }
        return toSearch;
    }

}

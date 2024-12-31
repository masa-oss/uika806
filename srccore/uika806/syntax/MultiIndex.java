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

import java.util.Objects;
import java.util.Stack;

/**
 *
 */
public class MultiIndex {

    private final Stack<Integer> indices = new Stack<>();

    public MultiIndex() {
        indices.push(0);
    }

    public void push() {
        indices.push(0);
    }

    public void pop() {
        indices.pop();
    }

    public void advance() {
        indices.push(indices.pop() + 1);
    }

    Stack<Integer> getIndices() {
        return indices;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof MultiIndex && ((MultiIndex) obj).indices.equals(indices);
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 43 * hash + Objects.hashCode(this.indices);
        return hash;
    }
}

/*
 * Copyright (C) 2016 Chan Chung Kwong <1m02math@126.com>
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
package uika806.objects;


/**
 *
 */
public class ScmUniqueSymbol extends SSymbol {

    private static int seq = 0;
    private final SSymbol org;

    /**
     * Construcr a unique symbol
     *
     * @param org the prefered name
     */
    public ScmUniqueSymbol(SSymbol org) {
        super(Integer.toString(seq++));
        this.org = toLiteral(org);
    }

    /**
     * Get the prefered name
     *
     * @return
     */
    public SSymbol getOrigin() {
        return org;
    }

    /**
     * Get prefered name or itself
     *
     * @param id
     * @return
     */
    public static SSymbol toLiteral(SSymbol id) {

        if (id instanceof ScmUniqueSymbol) {
            return ((ScmUniqueSymbol) id).getOrigin();
        } else {
            return id;
        }
    }

    @Override
    public String toString() {
        return org.toString();
    }

    public int getSeq() {
        return this.seq;
    }
}

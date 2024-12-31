/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.util.List;
import java.util.Objects;
import uika806.err.LispException;

/**
 *
 * Regenerate equals at 2024-08
 *
 * mutable flag add
 *
 */
public final class Cell {

    private Object car;
    private Object cdr;
    private final boolean mutable;

    /**
     * コンストラクタ
     *
     * @param car
     * @param cdr
     */
    public Cell(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
        this.mutable = true;
    }

    public Cell(Object car, Object cdr, boolean mutable) {
        this.car = car;
        this.cdr = cdr;
        this.mutable = mutable;
    }

    @Override
    public String toString() {

        return "(Cell " + Integer.toHexString(super.hashCode()) + ")";
    }

    public Object getCar() {
        return car;
    }

    public void setCar(Object x) {
        if (mutable) {
            this.car = x;
        } else {
            throw new LispException("Immutable");
        }
    }

    public Object getCdr() {
        return cdr;
    }

    public void setCdr(Object x) {
        if (mutable) {
            this.cdr = x;
        } else {
            throw new LispException("Immutable");
        }
    }

    /**
     * Equal2Helper#checkCircular メソッドのため、 この hashCode()
     * は、このオブジェクトのメモリ上のアドレスを返すものとする。
     *
     * <b>通常のJavaのhashCodeの実装ルールには従わない。</b>
     *
     * @return
     */
    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Cell other = (Cell) obj;
        if (!Objects.equals(this.car, other.car)) {
            return false;
        }
        return Objects.equals(this.cdr, other.cdr);
    }

    
    // ------ static methods ---------
    
    public static Object fromList(List<Object> list) {
        
        Object var = EmptyList.NIL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {

            Object sexp = list.get(i);
            var = new Cell(sexp, var);
        }
        return var;
    }
    
    public static Object fromList(List<Object> list, Object rest) {
        
        Object var = rest;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {

            Object sexp = list.get(i);
            var = new Cell(sexp, var);
        }
        return var;
    }
}

/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.kernel;

/**
 *
 */
public final class Values {

    private final int num;
    private final Object value1;
    private final Object value2;
    private  Object value3;
    private  Object value4;

    public Values() {
        this.num = 0;
        this.value1 = null;
        this.value2 = null;
    }
    
    public Values(Object v1) {
        this.num = 1;
        this.value1 = v1;
        this.value2 = null;
    }

    public Values(Object v1, Object v2) {
        this.num = 2;
        this.value1 = v1;
        this.value2 = v2;
    }

    public Values(Object v1, Object v2, Object v3) {
        this.num = 3;
        this.value1 = v1;
        this.value2 = v2;
        this.value3 = v3;
    }

    public Values(Object v1, Object v2, Object v3, Object v4) {
        this.num = 4;
        this.value1 = v1;
        this.value2 = v2;
        this.value3 = v3;
        this.value4 = v4;
    }

    /**
     * @return the value1
     */
    public Object getValue1() {
        return value1;
    }

    /**
     * @return the value2
     */
    public Object getValue2() {
        return value2;
    }

    /**
     * @return the value3
     */
    public Object getValue3() {
        return value3;
    }

    /**
     * @return the num
     */
    public int getNum() {
        return num;
    }

    /**
     * @return the value4
     */
    public Object getValue4() {
        return value4;
    }

}

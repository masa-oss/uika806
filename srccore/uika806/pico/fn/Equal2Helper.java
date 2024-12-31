/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.util.Collection;
import java.util.Comparator;
import java.util.TreeMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.SArray;

/**
 * R7RSの 2.4章. データラベルの印刷を行う際のヘルパークラス。
 * 
 * このクラスは、PrinterSmallクラスのprin1メソッドにて、
 * 第一フェーズとして、データのアドレスを登録していく。
 * 
 */
public class Equal2Helper {

    private static final Logger LOG = LoggerFactory.getLogger(Equal2Helper.class);

    public static class ObjInfo {

       public final Object c;
       public final int id;
       public boolean first = true;
       public int refCount = 1;

        public ObjInfo(Object cc, int id) {
            
            if (cc == null) throw new NullPointerException();
            
            this.c = cc;
            this.id = id;
        }

        public void inc() {
            refCount++;
        }

        @Override
        public String toString() {
            return "ObjInfo[id=" + id +  ", " + c.getClass().getSimpleName() +
                    ", refCount=" + refCount + ", " + first + "]";
        }
    }

    TreeMap<Object, ObjInfo> treeMap = new TreeMap<>(new MyComparator());

    static class MyComparator implements Comparator<Object> {

        @Override
        public int compare(Object o1, Object o2) {

            int hash1 = o1.hashCode();
            int hash2 = o2.hashCode();
            return hash1 - hash2;
        }

    }

    int cellOrArrayCounter = 0;

    /**
     * R7RSの 2.4章. データラベルの印刷（循環オブジェクト）は
     * ペア（Cell)と、vector(SArray)のみ発生する筈。
     * 
     * 注意： CellとSArrayのhashCodeは、メモリのアドレスを返すものとして、
     *       通常のJavaコーディングルールとは異なる実装をしている。
     * 
     * @param sexp 
     */
    public void checkCircular(Object sexp) {

        if (sexp == null) {
            return;

        } else if (sexp instanceof Cell) {
            Cell cell = (Cell) sexp;

            // 第一フェーズ
            checkCell0(cell);

        } else if (sexp instanceof SArray) {
            SArray atom = (SArray) sexp;
            firstPhaseArray(atom);

        }
    }
    
    
    
    private void dumpResult() {
        
        
        LOG.info("---------------------------------------- start");
        Collection<ObjInfo> values = treeMap.values();
        
        
        for (ObjInfo objInfo : values) {
            
            LOG.info("{}", objInfo.toString());
        }
        LOG.info("---------------------------------------- end");
    }
    
    
    
    
    

    /**
     * 
     * @param cell
     * @return    注意：初回登録時は、nullを返す
     */
  public  ObjInfo findObject(Object cell) {
        
        ObjInfo found = treeMap.get(cell);
        if (found != null) {
            return found;
        }
        

        treeMap.computeIfAbsent(cell, this::makeObjInfo);
        return null;
    }

    ObjInfo makeObjInfo(Object cell) {

        cellOrArrayCounter++;
        return new ObjInfo(cell, cellOrArrayCounter);
    }

    void firstPhaseArray(SArray array) {

        ObjInfo found = findObject(array);
        if (found != null) {
          //  LOG.info("found={}", found);
            found.inc();
            return;
        }

        int len = array.length();
        for (int i = 0; i < len; i++) {

            Object o = array.getNth(i);

            checkCircular(o);

        }

    }

    void checkCell0(Cell cell) {

        if (cell == null) {
            return;
        }

        ObjInfo found = findObject(cell);
        if (found != null) {
          //  LOG.info("found={}", found);
            found.inc();
            return;
        }
        Object car = cell.getCar();
        checkCircular(car);
        Object cdr = cell.getCdr();
        checkCircular(cdr);
    }

}

package uika806.small.fn;

import java.util.HashMap;
import java.util.Set;
import java.util.TreeMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.SArray;

/**
 *
 * @author hemmi
 */
@Deprecated
public class EqualHelper {

    private static final Logger LOG = LoggerFactory.getLogger(EqualHelper.class);

    boolean secondPhase = false;
    
    
    public EqualHelper() {
        
    }
    
    static class ConsInfo {

        final Cell c;
        final int id;
        boolean first = true;
        int refCount = 1;

        public ConsInfo(Cell cc, int id) {
            this.c = cc;
            this.id = id;
        }

        public void inc() {
            refCount++;
        }

        @Override
        public String toString() {
            return "Cons[id=" + id + ", refCount=" + refCount + ", " + first + "]";
        }
    }
    
    static class SArrayInfo {
    
        SArray sa;
        final int id;
        boolean first = true;
        int refCount = 1;
        
        public SArrayInfo(SArray cc, int id) {
            this.sa = cc;
            this.id = id;
        }
        public void inc() {
            refCount++;
        }
        @Override
        public String toString() {
            return "Array[id=" + id + ", refCount=" + refCount + ", " + first + "]";
        }

    }
    
    HashMap<Cell, ConsInfo> cellLoopCheckMap = new HashMap<>();

    HashMap<SArray, SArrayInfo> arryLoopCheckMap = new HashMap<>();
    
    
    TreeMap tree = new TreeMap();
    
    
    int cellOrArrayCounter = 0;
    
    
    public void dumpResult() {
        
        
        Set<Cell> keySet = cellLoopCheckMap.keySet();
        
        for (Cell key1 : keySet) {
            
            cellLoopCheckMap.get(key1);
            
        }
        
        
    }
    
    
    
    SArrayInfo findSArray(SArray cell) {
        
        return  arryLoopCheckMap.computeIfAbsent(cell, this::makeNewArray);
    }
    
    SArrayInfo makeNewArray(SArray cell) {
        
        cellOrArrayCounter++;
        return new SArrayInfo(cell, cellOrArrayCounter);
    }
    
    
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

        } else {
        }
    }
    
    
    ConsInfo findCell(Cell cell) {
        
        return  cellLoopCheckMap.computeIfAbsent(cell, this::makeNewCell);
    }
    

    ConsInfo makeNewCell(Cell cell) {
        
        cellOrArrayCounter++;
        return new ConsInfo(cell, cellOrArrayCounter);
    }
    
    
    ConsInfo old_checkCell(Cell cell) {

        ConsInfo found = cellLoopCheckMap.get(cell);
        if (found != null) {
            return found;
        }
        cellOrArrayCounter++;
        cellLoopCheckMap.put(cell, new ConsInfo(cell, cellOrArrayCounter));
        return null;
    }


    void firstPhaseArray(SArray array) {

        
        SArrayInfo found = findSArray(array);
        if (found != null) {
            LOG.info("found={}", found);
            found.inc();
            return;
        }
        
        
        int len = array.length();
        for (int i = 0; i < len; i++) {
            
            Object o =  array.getNth(i);
            
            checkCircular(o);
            
        }
        
    }
    

    void checkCell0(Cell cell) {

        if (cell == null) {
            return;
        }
        
        ConsInfo found = findCell(cell);
        if (found != null) {
            LOG.info("found={}", found);
            found.inc();
            return;
        }
        Object car = cell.getCar();
        checkCircular(car);
        Object cdr = cell.getCdr();
        checkCircular(cdr);
    }
    
}

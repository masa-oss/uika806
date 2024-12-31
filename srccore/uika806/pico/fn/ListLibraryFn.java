/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.util.Map;
import java.util.Set;
import org.slf4j.LoggerFactory;

import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.lib.LibraryManager;
import uika806.lib.Library;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;

/**
 *
 * @author hemmi
 */
public class ListLibraryFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(ListLibraryFn.class);

    @Override
    public String getName() {
        return "-list-lib";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            Library lib = LibraryManager.findLib(cell);

            LOG.info("31)   lib = {}", lib);
            if (lib != null) {
                Map<SSymbol, Object> map = lib.getUnmodifiableMap();
                return keyOfMapToListOfLisp(map);
            }
            return Boolean.FALSE;
        }

        throw new BadArgumentInFunctionException(getName());
    }

    // Mapをリストに変換する
    Object keyOfMapToListOfLisp(Map<?, ?> map) {

        Object wk = EmptyList.NIL;

        Set<?> keySet = map.keySet();
        for (Object key : keySet) {
            wk = new Cell(key, wk);
        }
        return wk;
        /*
        .forEachRemaining((e) -> {
        wk = new Cell(e.getKey(), wk);
        });
         */
    }

}

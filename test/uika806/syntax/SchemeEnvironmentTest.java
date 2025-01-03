package uika806.syntax;

import java.util.Map;
import java.util.Optional;
import org.junit.Test;
import static org.junit.Assert.*;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;

/**
 *
 * @author hemmi
 */
public class SchemeEnvironmentTest {

    public SchemeEnvironmentTest() {
    }



    @Test
    public void testLookupValue() {
        System.out.println("lookupValue");
        SSymbol sym = new SSymbol("foo");
        SchemeEnvironment instance = new SchemeEnvironment(true);

        instance.add(sym, 2L);


        SSymbol sym2 = new SSymbol("foo");

        Object result = instance.getSelfOptional(sym2);

        //     assertEquals(expResult, result);
        System.out.println("result=" + result);

    }



    @Test
    public void testLookupValue2() {
        System.out.println("lookupValue2");
        SSymbol sym = new SSymbol("foo");
        ScmUniqueSymbol ssym = new ScmUniqueSymbol(sym);
        
        SchemeEnvironment instance = new SchemeEnvironment(true);

        instance.add(ssym, 2L);


        SSymbol sym2 = new SSymbol("foo");

        Object result = instance.getSelfOptional(sym2);

        //     assertEquals(expResult, result);
        System.out.println("result=" + result);

    }


    @Test
    public void testLookupValue3() {
        System.out.println("lookupValue3");
        SSymbol sym = new SSymbol("foo");
        ScmUniqueSymbol ssym = new ScmUniqueSymbol(sym);
        
        SchemeEnvironment instance = new SchemeEnvironment(true);

        instance.add(ssym, 2L);

        Map<SSymbol, Object> unmodifiableMap = instance.getUnmodifiableMap();
        System.out.println("Map = "  + unmodifiableMap);

    //    SSymbol sym2 = new SSymbol("foo");

        Object result = instance.getSelfOptional(ssym);

        //     assertEquals(expResult, result);
        System.out.println("result=" + result);

    }


}

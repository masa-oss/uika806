package uika806.string;

import java.util.ArrayList;
import uika806.objects.SChar;
import uika806.kernel.AFn;
import uika806.objects.SString;
import uika806.small.fn.UtilString;

/**
 *
 * @author hemmi
 */
public class StringFn {

    public static class StringSet extends AFn {

        @Override
        public String getName() {
            return "string-set!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            SString str = (SString) arg1;
            Number num = (Number) arg2;
            int nth = num.intValue();

            SChar sc = (SChar) arg3;

            int code = str.setNthChar(nth, sc);
            return SChar.valueOf(code);
        }
    }

    public static class StringAFn extends AFn {

        @Override
        public String getName() {
            return "string";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
                Object arg5, Object arg6) {

            ArrayList<Integer> list = new ArrayList<>();
            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;
            SChar sc4 = (SChar) arg4;
            SChar sc5 = (SChar) arg5;
            SChar sc6 = (SChar) arg6;
            list.add(sc1.getCodepoint());
            list.add(sc2.getCodepoint());
            list.add(sc3.getCodepoint());
            list.add(sc4.getCodepoint());
            list.add(sc5.getCodepoint());
            list.add(sc6.getCodepoint());
            return SString.fromList(list, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
                Object arg5) {

            ArrayList<Integer> list = new ArrayList<>();
            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;
            SChar sc4 = (SChar) arg4;
            SChar sc5 = (SChar) arg5;
            list.add(sc1.getCodepoint());
            list.add(sc2.getCodepoint());
            list.add(sc3.getCodepoint());
            list.add(sc4.getCodepoint());
            list.add(sc5.getCodepoint());
            return SString.fromList(list, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            ArrayList<Integer> list = new ArrayList<>();
            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;
            SChar sc4 = (SChar) arg4;
            list.add(sc1.getCodepoint());
            list.add(sc2.getCodepoint());
            list.add(sc3.getCodepoint());
            list.add(sc4.getCodepoint());
            return SString.fromList(list, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            ArrayList<Integer> list = new ArrayList<>();
            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;
            list.add(sc1.getCodepoint());
            list.add(sc2.getCodepoint());
            list.add(sc3.getCodepoint());
            return SString.fromList(list, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            ArrayList<Integer> list = new ArrayList<>();
            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            list.add(sc1.getCodepoint());
            list.add(sc2.getCodepoint());
            return SString.fromList(list, true);
        }

        @Override
        public Object invoke(Object arg1) {
            SChar sc = (SChar) arg1;
            return new SString(1, sc.getCodepoint());
        }

        @Override
        public Object invoke() {
            return new SString(0, ' ');
        }

    }

    public static class StringGteFn extends AFn {

        @Override
        public String getName() {
            return "string>=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compare(s1, s2);

            return (n <= 0);
        }
        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;

            int n = UtilString.compare(s1, s2);
            if (! (n <= 0)) {
                return false;
            }

            n = UtilString.compare(s2, s3);
            return (n <= 0);
        }
    }

    public static class StringGtFn extends AFn {

        @Override
        public String getName() {
            return "string>";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compare(s1, s2);

            return (n < 0);
        }
        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;

            int n = UtilString.compare(s1, s2);
            if (!(n < 0)) {
                return false;
            }
            n = UtilString.compare(s2, s3);

            return (n < 0);
        }
    }

    public static class StringLteFn extends AFn {

        @Override
        public String getName() {
            return "string<=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compare(s1, s2);

            return (n <= 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;
            int n = UtilString.compare(s1, s2);

            if (!(n <= 0)) {
                return false;
            }

            n = UtilString.compare(s2, s3);

            return (n <= 0);
        }

    }

    public static class StringLtFn extends AFn {

        @Override
        public String getName() {
            return "string<";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compare(s1, s2);

            return (n < 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;

            int n = UtilString.compare(s1, s2);

            if (! (n < 0)) {
                return false;
            }
            n = UtilString.compare(s2, s3);

            return (n < 0);
        }

    }

    
    public static class StringCiEqual extends AFn  {

        @Override
        public String getName() {
            return "string-ci=";
        }
        
        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compareCI(s1, s2);

            return (n == 0);
        }


        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;

            int n = UtilString.compareCI(s1, s2);

            if (!(n == 0)) {
                return false;
            }
            n = UtilString.compareCI(s2, s3);
            return (n == 0);
        }


    }
    
    public static class StringCiLt extends AFn  {

        @Override
        public String getName() {
            return "string-ci<";
        }
        
        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compareCI(s1, s2);

            return (n < 0);
        }
    }
    
    public static class StringCiLte extends AFn  {

        @Override
        public String getName() {
            return "string-ci<=";
        }
        
        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compareCI(s1, s2);

            return (n <= 0);
        }
    }
    
    public static class StringCiGt extends AFn  {

        @Override
        public String getName() {
            return "string-ci>";
        }
        
        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compareCI(s1, s2);

            return (n > 0);
        }
    }
    
    public static class StringCiGte extends AFn  {

        @Override
        public String getName() {
            return "string-ci>";
        }
        
        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            int n = UtilString.compareCI(s1, s2);

            return (n >= 0);
        }
    }
    
}

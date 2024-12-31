package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;
import uika806.small.fn.UtilString;

/**
 *
 * @author hemmi
 */
public class CharCi {

    public static class CharEqqFn extends AFn {

        @Override
        public String getName() {
            return "char=?";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            int n = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            return (n == 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            int n1 = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            int n2 = UtilString.compareCodePointCI(sc2.getCodepoint(), sc3.getCodepoint());
            return (n1 == 0) && (n2 == 0);

        }
    }

    public static class CharLtFn extends AFn {

        @Override
        public String getName() {
            return "char<";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            int n = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            return (n < 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            int n1 = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            int n2 = UtilString.compareCodePointCI(sc2.getCodepoint(), sc3.getCodepoint());
            return (n1 < 0) && (n2 < 0);
        }
    }

    public static class CharGtFn extends AFn {

        @Override
        public String getName() {
            return "char<";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            int n = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            return (n > 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            int n1 = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            int n2 = UtilString.compareCodePointCI(sc2.getCodepoint(), sc3.getCodepoint());
            return (n1 > 0) && (n2 > 0);
        }
    }

    public static class CharLteFn extends AFn {

        @Override
        public String getName() {
            return "char<=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            int n = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            return (n <= 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            int n1 = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            int n2 = UtilString.compareCodePointCI(sc2.getCodepoint(), sc3.getCodepoint());
            return (n1 <= 0) && (n2 <= 0);
        }
    }

    public static class CharGteFn extends AFn {

        @Override
        public String getName() {
            return "char>=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            int n = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            return (n >= 0);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            int n1 = UtilString.compareCodePointCI(sc1.getCodepoint(), sc2.getCodepoint());
            int n2 = UtilString.compareCodePointCI(sc2.getCodepoint(), sc3.getCodepoint());
            return (n1 >= 0) && (n2 >= 0);
        }
    }

}

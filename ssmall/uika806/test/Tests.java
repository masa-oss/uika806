/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.test;

import org.slf4j.LoggerFactory;
import uika806.print.PrinterSchemeEx;
import uika806.kernel.AFn;
import uika806.pico.fn.EqualFn;

/**
 * これは、 r7rsの仕様ではないが、テストで使いたいため、ここに入れる
 */
public final class Tests {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Tests.class);

    static int nTest = 0;
    static int nSuccess = 0;

    public static class TestBegin extends AFn {

        @Override
        public String getName() {
            return "test-begin";
        }

        @Override
        public Object invoke(Object arg1) {

            nTest = 0;
            nSuccess = 0;
            return Boolean.FALSE;
        }
    }

    public static class TestEnd extends AFn {

        @Override
        public String getName() {
            return "test-end";
        }

        @Override
        public Object invoke() {

            LOG.info("*****  test-end  {} /  {}",                    nSuccess,                nTest            );
            return Boolean.FALSE;
        }
    }

    static final EqualFn equalFn = new EqualFn();

    public static class Test extends AFn {

        @Override
        public String getName() {
            return "test";
        }

        @Override
        public Object invoke(Object expected, Object result) {

            nTest++;
            boolean b = (Boolean) equalFn.invoke(expected, result);
            if (!b) {
                LOG.warn("FAIL: expr expected {} but got {}", expected, result);
                return Boolean.FALSE;
            }
            nSuccess++;
            return Boolean.TRUE;
        }
    }

    public static class ReportTest extends AFn {

        @Override
        public String getName() {
            return "report-test";
        }

        @Override
        public Object invoke(Object actual) {

            String str2 = new PrinterSchemeEx().prin1(actual).toString();

            LOG.warn("FAIL: expr expected {} but got {}", "???", str2);
            return Boolean.FALSE;
        }
    }

    private Tests() {
    }
}

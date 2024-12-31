/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import uika806.err.BadArgumentInFunctionException;
import uika806.err.FileException;
import uika806.err.RaiseException;
import uika806.err.ReaderException;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SArray;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.SSymbol;
import uika806.objects.U8Array;
import uika806.port.IPort;
import uika806.port.InputPort;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public class Func012 {


    public static class VectorqFn extends AFn {

        @Override
        public String getName() {
            return "vector?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof SArray) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class ByteVectorqFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof U8Array) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }


    public static class PairqFn extends AFn {

        @Override
        public String getName() {
            return "pair?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Cell) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class CharFn extends AFn {

        @Override
        public String getName() {
            return "char?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof SChar) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class BooleanqFn extends AFn {

        @Override
        public String getName() {
            return "boolean?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Boolean) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class SymbolqFn extends AFn {

        @Override
        public String getName() {
            return "symbol?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof SSymbol) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class MakeStringFn extends AFn {

        @Override
        public String getName() {
            return "make-string?";
        }

        @Override
        public Object invoke(Object arg1) {

            return invoke(arg1, null);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            int len = ((Number) arg1).intValue();

            int code = '*';
            if (arg2 instanceof SChar) {
                SChar sc = (SChar) arg2;
                code = sc.getCodepoint();
            } else if (arg2 == null) {
                code = '*';
            } else {
                throw new RuntimeException("arg2 must be character " + arg2);
            }
            return new SString(len, code);
        }
    }

    public static class ErrorObjectqFn extends AFn {

        @Override
        public String getName() {
            return "error-object?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof RaiseException) {
                RaiseException le = (RaiseException) arg1;
                return le.getArgument();
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class ErrorObjectMessageFn extends AFn {

        @Override
        public String getName() {
            return "error-object-message";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Exception) {
                Exception ex = (Exception) arg1;
                return ex.getMessage();
            }
            return Boolean.FALSE;
        }
    }

    public static class ErrorObjectIrritantsFn extends AFn {

        @Override
        public String getName() {
            return "error-object-irritants";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Exception) {
                Exception ex = (Exception) arg1;
                return ex;
            }
            return Boolean.FALSE;
        }
    }

    public static class ReadErrorFn extends AFn {

        @Override
        public String getName() {
            return "read-error";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof ReaderException) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class FileErrorFn extends AFn {

        @Override
        public String getName() {
            return "file-error";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof FileException) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class InputPortqFn extends AFn {

        @Override
        public String getName() {
            return "input-port?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class OutputPortqFn extends AFn {

        @Override
        public String getName() {
            return "output-port?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof OutputPort) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class PortqFn extends AFn {

        @Override
        public String getName() {
            return "port?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof IPort) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }


    public static class TextPortqFn extends AFn {

        @Override
        public String getName() {
            return "text-port?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof IPort) {
                IPort po = (IPort) arg1;
                
                return po.isTextualPort();
            }
            return Boolean.FALSE;
        }
    }

    public static class BinaryPortqFn extends AFn {

        @Override
        public String getName() {
            return "binary-port?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof IPort) {
                IPort po = (IPort) arg1;
                
                return po.isBinaryPort();
            }
            return Boolean.FALSE;
        }
    }

    public static class ErrorFn extends AFn {

        @Override
        public String getName() {
            return "error";
        }

        @Override
        public Object invoke(Object message) {

            if (message instanceof SString) {
                SString po = (SString) message;
              //  throw new LispException(po.toString());
                Object list = new Cell(  po,  EmptyList.NIL);
                
                throw new RaiseException(po.toString(), list, false);
            }
            throw new BadArgumentInFunctionException("error");
        }

        @Override
        public Object invoke(Object message, Object obj1) {

            if (obj1 == null) throw new NullPointerException();
            
            if (message instanceof SString) {
                SString po = (SString) message;
                
                Object list = RT.list(obj1);
                
                throw new RaiseException(po.toString(), list, false);
            }
            throw new BadArgumentInFunctionException("error");
        }

        @Override
        public Object invoke(Object message, Object obj1, Object obj2) {

            if (obj1 == null) throw new NullPointerException();
            
            if (message instanceof SString) {
                SString po = (SString) message;
                
                Object list = RT.list(obj1, obj2);
                
                throw new RaiseException(po.toString(), list, false);
            }
            throw new BadArgumentInFunctionException("error");
        }

        @Override
        public Object invoke(Object message, Object obj1, Object obj2, Object obj3) {

            if (obj1 == null) throw new NullPointerException();
            
            if (message instanceof SString) {
                SString po = (SString) message;
                
                Object list = RT.list(obj1, obj2, obj3);
                
                throw new RaiseException(po.toString(), list, false);
            }
            throw new BadArgumentInFunctionException("error");
        }
    }
}

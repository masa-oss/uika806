/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.portfn;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import uika806.err.BadArgumentInFunctionException;
import uika806.err.FileException;
import uika806.err.LispException;
import uika806.err.ReaderException;
import uika806.port.InputPort;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.EndOfFile;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.U8Array;
import uika806.objects.Undef;
import uika806.port.ByteVectorInputPort;
import uika806.port.ByteVectorOutputPort;
import uika806.port.IPort;
import uika806.port.OutputPort;
import uika806.port.CurrentPort;
import uika806.port.FileOutputPort;
import uika806.port.StringInputPort;
import uika806.port.StringOutputPort;
import uika806.small.fn.ReadFn;

/**
 *
 */
public class PortFns {

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

    public static class TextualPortqFn extends AFn {

        @Override
        public String getName() {
            return "textual-port?";
        }

        @Override
        public Object invoke(Object arg1) {
            if (arg1 instanceof IPort) {
                IPort port = (IPort) arg1;
                return port.isTextualPort();
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
                IPort port = (IPort) arg1;
                return port.isBinaryPort();
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

    public static class InputPortOpenqFn extends AFn {

        @Override
        public String getName() {
            return "input-port-open?";
        }

        @Override
        public Object invoke(Object arg1) {
            if (arg1 instanceof InputPort) {
                InputPort ip = (InputPort) arg1;
                return ip.isOpen();
            }
            return Boolean.FALSE;
        }
    }

    public static class OutputPortOpenqFn extends AFn {

        @Override
        public String getName() {
            return "output-port-open?";
        }

        @Override
        public Object invoke(Object arg1) {
            if (arg1 instanceof OutputPort) {
                OutputPort op = (OutputPort) arg1;
                return op.isOpen();
            }
            return Boolean.FALSE;
        }
    }

    public static class ClosePortFn extends AFn {

        @Override
        public String getName() {
            return "close-port";
        }

        @Override
        public Object invoke(Object arg1) {
            if (arg1 instanceof IPort) {
                IPort op = (IPort) arg1;
                op.close();
                return RT.UNDEF;
            }
            return Boolean.FALSE;
        }
    }

    public static class CurrentOutputPortFn extends AFn {

        @Override
        public String getName() {
            return "current-output-port";
        }

        @Override
        public Object invoke() {

            return CurrentPort.OUTPUT_PORT;
        }
    }

    public static class CurrentErrorPortFn extends AFn {

        @Override
        public String getName() {
            return "current-error-port";
        }

        @Override
        public Object invoke() {

            return CurrentPort.ERROR_PORT;
        }
    }

    public static class CharReadyFn extends AFn {

        @Override
        public String getName() {
            return "char-ready?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort cop = (InputPort) arg1;
                return cop.ready();
            } else {
                throw LispException.illegalArgument("Bad argument for char-ready?", arg1);
            }
        }
    }

    public static class CloseOutputPortFn extends AFn {

        @Override
        public String getName() {
            return "close-output-port";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof OutputPort) {
                OutputPort cop = (OutputPort) arg1;
                cop.close();
                return Boolean.TRUE;
            } else {
                throw LispException.illegalArgument("Bad argument for close-output-port", arg1);
            }

        }
    }

    public static class CurrentInputPortFn extends AFn {

        @Override
        public String getName() {
            return "current-input-port";
        }

        @Override
        public Object invoke() {

            return CurrentPort.INPUT_PORT;
        }

    }

    public static class FlushOutputPortFn extends AFn {

        @Override
        public String getName() {
            return "flush-output-port";
        }

        @Override
        public Object invoke() {
            return invoke(CurrentPort.OUTPUT_PORT);
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof OutputPort) {

                OutputPort cop = (OutputPort) arg1;

                cop.flush();

            } else {
                throw new BadArgumentInFunctionException("Bad argument for flush-output-port" + arg1);
            }

            return Undef.Undefined;
        }
    }

    public static class GetOutputByteVector extends AFn {

        @Override
        public String getName() {
            return "get-output-byte-vector";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof ByteVectorOutputPort) {

                ByteVectorOutputPort thePort = (ByteVectorOutputPort) arg1;
                int len = thePort.list.size();
                byte[] arr = new byte[len];
                for (int i = 0; i < len; i++) {
                    Byte sh = thePort.list.get(i);
                    arr[i] = (byte) (sh & 255);
                }

                return new U8Array(arr, true);

            } else {
                String str = (arg1 == null) ? "null" : arg1.getClass().getName();
                throw new FileException("Bad argument for get-output-bytevector : " + str);
            }
        }
    }

    public static class GetOutputStringFn extends AFn {

        @Override
        public String getName() {
            return "get-output-string";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof StringOutputPort) {

                StringOutputPort thePort = (StringOutputPort) arg1;

                return SString.fromList(thePort.getUnmodifiableList());

            } else {
                String str = (arg1 == null) ? "null" : arg1.getClass().getName();
                throw new FileException("Bad argument for get-output-string : " + str);
            }
        }
    }

    public static class OpenInputByteVector extends AFn {

        @Override
        public String getName() {
            return "open-input-bytevector";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof U8Array) {
                U8Array arr = (U8Array) arg1;

                return new ByteVectorInputPort(arr);

            } else {
                throw new BadArgumentInFunctionException("Bad argument for open-input-bytevector");
            }
        }
    }

    public static class OpenInputStringFn extends AFn {

        @Override
        public String getName() {
            return "open-input-string";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof SString) {
                SString ss = (SString) arg1;
                return new StringInputPort(ss);
            }
            throw new BadArgumentInFunctionException("open-input-string");
        }
    }

    public static class OpenOutputByteVector extends AFn {

        @Override
        public String getName() {
            return "open-output-byte-vector";
        }

        @Override
        public Object invoke() {
            return new ByteVectorOutputPort();
        }
    }

    public static class OpenOutputFileFn extends AFn {

        @Override
        public String getName() {
            return "open-output-file";
        }

        @Override
        public Object invoke(Object arg1) {

            FileOutputPort oPort = null;
            try {
                SString str = (SString) arg1;
                String fileName = str.toString();

                FileOutputStream fos = new FileOutputStream(fileName);
                BufferedOutputStream bos = new BufferedOutputStream(fos);
                OutputStreamWriter osw = new OutputStreamWriter(bos, "UTF-8");

                oPort = new FileOutputPort(fileName, fos, osw);

            } catch (Exception ex) {
                throw new LispException("io-error", ex);
            }
            return oPort;
        }

    }

    public static class OpenOutputStringFn extends AFn {

        @Override
        public String getName() {
            return "open-output-string";
        }

        @Override
        public Object invoke() {

            return new StringOutputPort();
        }
    }

    public static class PeekCharFn extends AFn {

        @Override
        public String getName() {
            return "peek-char";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort ip = (InputPort) arg1;

                int b = ip.peekCodepoint();

                if (b == -1) {
                    return EndOfFile.INSTANCE;
                }

                return SChar.valueOf(b);
            }
            throw new LispException("Bad argument for peek-char");
        }

    }

    public static class PeekU8Fn extends AFn {

        @Override
        public String getName() {
            return "peek-u8";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort ip = (InputPort) arg1;

                int b = ip.peekU8();

                if (b == -1) {
                    return EndOfFile.INSTANCE;
                }

                long ln = b & 255;
                return ln;
            }
            throw new LispException("Bad argument for peek-u8");
        }
    }

    public static class ReadByteVectorFn extends AFn {

        @Override
        public String getName() {
            return "read-byte-vector";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (!(arg1 instanceof Long)) {
                throw new BadArgumentInFunctionException("read-byte-vector");
            }

            if (!(arg2 instanceof InputPort)) {
                throw new BadArgumentInFunctionException("read-byte-vector");
            }

            Long ln = (Long) arg1;
            int n = ln.intValue();

            InputPort ip = (InputPort) arg2;

            ArrayList<Integer> buf = new ArrayList<>();

            for (int i = 0; i < n; i++) {
                int rc = ip.readByte();

                if (rc == -1) {
                    if (i == 0) {
                        return EndOfFile.INSTANCE;
                    } else {
                        break;
                    }
                }
                buf.add(rc & 255);
            }

            U8Array u8a = new U8Array(buf, true);
            return u8a;
        }
    }

    public static class ReadByteVectoreFn extends AFn {

        @Override
        public String getName() {
            return "read-byte-vector!";
        }

        @Override
        public Object invoke(Object arg0, Object arg1) {
            return invoke(arg0, arg1, 0L);
        }

        @Override
        public Object invoke(Object arg0, Object arg1, Object start) {

            if (!(arg0 instanceof U8Array)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            if (!(arg1 instanceof InputPort)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            if (!(start instanceof Long)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            U8Array out = (U8Array) arg0;
            int len = out.length();

            Long ln = (Long) start;
            int iStart = ln.intValue();

            InputPort ip = (InputPort) arg1;

            int i = 0;

            for (; iStart + i < len; i++) {
                int rc = ip.readByte();

                if (rc == -1) {
                    return EndOfFile.INSTANCE;
                }

                byte by = (byte) (rc & 255);
                out.setNth(iStart + i, by);
            }

            return (long) i;
        }

        @Override
        public Object invoke(Object arg0, Object arg1, Object start, Object end) {

            if (!(arg0 instanceof U8Array)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            if (!(arg1 instanceof InputPort)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            if (!(start instanceof Long)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            if (!(end instanceof Long)) {
                throw new BadArgumentInFunctionException("read-byte-vector!");
            }

            U8Array out = (U8Array) arg0;
            int len = out.length();

            Long ln = (Long) start;
            int iStart = ln.intValue();

            Long ee = (Long) end;
            int iEnd = ee.intValue();

            InputPort ip = (InputPort) arg1;

            int i = 0;

            for (; (i < iEnd) && (iStart + i < len); i++) {
                int rc = ip.readByte();

                if (rc == -1) {
                    return EndOfFile.INSTANCE;
                }

                byte by = (byte) (rc & 255);
                out.setNth(iStart + i, by);
            }

            return (long) i;
        }
    }

    public static class ReadLineFn extends AFn {

        @Override
        public String getName() {
            return "read-line";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {

                InputPort cis = (InputPort) arg1;

                ArrayList<Integer> line = new ArrayList<>();
                int ch = cis.peekCodepoint();
                while (ch >= 0 && ch != '\n') {
                    line.add(ch);
                    cis.readCodepoint();
                    ch = cis.peekCodepoint();
                }

                if (ch < 0 && line.isEmpty()) {
                    return ReadFn.EOF;
                }

                return SString.fromList(line, true);
            } else {
                String clazz = (arg1 == null) ? "null" : arg1.getClass().getName();

                throw new ReaderException("Bad argument for read-line : " + clazz);
            }
        }
    }

    public static class ReadStringFn extends AFn {

        @Override
        public String getName() {
            return "read-string";
        }

        @Override
        public Object invoke(Object k, Object port) {

            if (port instanceof InputPort) {
                InputPort ip = (InputPort) port;
                if (k instanceof Long) {
                    Long ln = (Long) k;
                    int n = ln.intValue();

                    ArrayList<Integer> list = new ArrayList<>();
                    for (int i = 0; i < n; i++) {
                        int codePoint = ip.readCodepoint();
                        if (codePoint < 0) {
                            if (i == 0) {
                                return EndOfFile.INSTANCE;
                            } else {
                                break;
                            }
                        }
                        list.add(codePoint);
                    }
                    return SString.fromList(list, true);

                }
                throw new BadArgumentInFunctionException("Bad argument , read-string");
            }
            throw new BadArgumentInFunctionException("Bad argument , read-string");
        }

    }

    public static class ReadU8Fn extends AFn {

        @Override
        public String getName() {
            return "read-u8";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort cop = (InputPort) arg1;

                if (cop.isBinaryPort()) {

                    int by = cop.readByte();

                    if (by == -1) {
                        return EndOfFile.INSTANCE;
                    }
                    return (long) by;
                }
                throw new ReaderException("The argument is not a BinaryPort, read-u8  ");

            } else {
                String clazz = (arg1 == null) ? "null" : arg1.getClass().getName();

                throw new ReaderException("Bad argument for read-u8 : " + clazz);
            }

        }

    }

    public static class U8ReadyFn extends AFn {

        @Override
        public String getName() {
            return "u8-ready";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort ip = (InputPort) arg1;

                return ip.isBinaryPort() && ip.ready();
            }
            throw new LispException("Bad argument for u8-ready");
        }
    }

    public static class WriteByteVectorFn extends AFn {

        @Override
        public String getName() {
            return "write-byte-vector";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (!(arg1 instanceof U8Array)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new IllegalArgumentException("write-byte-vector");
            }

            U8Array arr = (U8Array) arg1;
            OutputPort op = (OutputPort) arg2;

            int len = arr.length();
            for (int i = 0; i < len; i++) {
                int x = arr.getNthAsInt(i);

                op.writeByte(x);
            }
            return RT.UNDEF;
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            if (!(arg1 instanceof U8Array)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg3 instanceof Long)) {
                throw new IllegalArgumentException("write-byte-vector");
            }

            U8Array arr = (U8Array) arg1;
            OutputPort op = (OutputPort) arg2;
            Long start = (Long) arg3;
            int iStart = start.intValue();

            int len = arr.length() - iStart;
            for (int i = 0; i < len; i++) {
                int x = arr.getNthAsInt(i + iStart);

                op.writeByte(x);
            }
            return RT.UNDEF;
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            if (!(arg1 instanceof U8Array)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg3 instanceof Long)) {
                throw new IllegalArgumentException("write-byte-vector");
            }
            if (!(arg4 instanceof Long)) {
                throw new IllegalArgumentException("write-byte-vector");
            }

            U8Array arr = (U8Array) arg1;
            OutputPort op = (OutputPort) arg2;
            Long start = (Long) arg3;
            int iStart = start.intValue();

            Long end = (Long) arg4;
            int iEnd = end.intValue();

            int len = arr.length();
            for (int i = iStart; (i < iEnd) && (i < len); i++) {
                int x = arr.getNthAsInt(i);

                op.writeByte(x);
            }
            return RT.UNDEF;
        }
    }

    public static class WriteCharFn extends AFn {

        @Override
        public String getName() {
            return "write-char";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (!(arg1 instanceof SChar)) {
                throw new IllegalArgumentException("write-char");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new IllegalArgumentException("write-char");
            }

            SChar sc = (SChar) arg1;
            OutputPort op = (OutputPort) arg2;

            int cp = sc.getCodepoint();

            try {
                op.write(cp);
            } catch (IOException ioe) {
                throw new FileException("IOException", ioe);
            }

            return RT.UNDEF;
        }

    }

    public static class ReadCharFn extends AFn {

        @Override
        public String getName() {
            return "read-char";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof InputPort) {
                InputPort cop = (InputPort) arg1;

                int ch = cop.readCodepoint();
                if (ch < 0) {
                    return ReadFn.EOF;
                } else {
                    return SChar.valueOf(ch);
                }
            } else {
                throw LispException.illegalArgument("Bad argument for read-char", arg1);
            }

        }

    }

    public static class WriteStringFn extends AFn {

        @Override
        public String getName() {
            return "write-string";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (!(arg1 instanceof SString)) {
                throw new BadArgumentInFunctionException("write-string");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new BadArgumentInFunctionException("write-string");
            }

            SString sc = (SString) arg1;
            OutputPort op = (OutputPort) arg2;

            op.write(sc.toString());

            return RT.UNDEF;
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object start) {
            if (!(arg1 instanceof SString)) {
                throw new BadArgumentInFunctionException("write-string");
            }
            SString sc = (SString) arg1;
            long len = sc.length();
            return invoke(arg1, arg2, start, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object start, Object end) {

            if (!(arg1 instanceof SString)) {
                throw new BadArgumentInFunctionException("write-string");
            }
            if (!(arg2 instanceof OutputPort)) {
                throw new BadArgumentInFunctionException("write-string");
            }
            if (!(start instanceof Long)) {
                throw new BadArgumentInFunctionException("write-string");
            }
            if (!(end instanceof Long)) {
                throw new BadArgumentInFunctionException("write-string");
            }

            SString sc = (SString) arg1;
            OutputPort op = (OutputPort) arg2;
            int lStart = ((Long) start).intValue();
            int lEnd = ((Long) end).intValue();

            try {
                for (int i = lStart; i < lEnd; i++) {

                    int codepoint = sc.getNth(i);

                    op.write(codepoint);
                }
            } catch (IOException ioe) {
                throw new FileException("IOException", ioe);
            }

            return RT.UNDEF;
        }

    }

}

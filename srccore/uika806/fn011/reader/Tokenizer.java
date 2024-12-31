/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.fn011.reader;

import uika806.port.InputPort;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.LoggerFactory;
import uika806.err.ReaderException;
import uika806.reader.SharpReader;

public class Tokenizer {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Tokenizer.class);

    CFlags cflag = new CFlags();

    SharpReader sharpReader = new SharpReader();

    public final InputPort in;

    public Tokenizer(InputPort cis) {
        this.in = cis;
    }

    public Token next() {

        for (;;) {
            int pc = in.peekCodepoint();

            if (pc < 0) {
                return new Token(-1, null);
            } else if (pc == '(') {
                in.readCodepoint();
                int line = in.getLineNum();
                return new Token('(', null, line);
            } else if (pc == ')') {
                in.readCodepoint();
                int line = in.getLineNum();
                return new Token(')', null, line);
            } else if (pc == ';') {
                return readComment(pc);

            } else if (pc == '+' || pc == '-') {
                // +と-は識別子に使って良い記号(なので、識別子より先に処理してみる）
                return sign(pc);
                /*    // 2024-09-27
            } else if (cflag.isAllowedFirstCharOfId(pc)) {
                // 識別子(id)の１文字目の時
                in.readChar();
                return readId(pc);
                 */
            } else if (cflag.isSpace(pc)) {

                in.readCodepoint();
                skipSpace();
                continue;   // 空白は、ループする

            } else if (pc == '\'') {
                // Single quote
                in.readCodepoint();
                return new Token('\'', null);

            } else if (pc == '"') {
                // double quote
                in.readCodepoint();
                return readString();

            } else if (pc == '#') {

                in.readCodepoint();
                return sharpReader.read(this, in);
                /*    // 2024-09-27
            } else if ('0' <= pc && pc <= '9') {

                return numKari();
                 */
            } else if (pc == ',') {

                return comma();

            } else if (pc == '`') {

                return backquote();

            } else if (pc == '\r' || pc == '\n') {

                // 7.1.1
                // <行末> −→ <改行LF> | <復帰CR> <改行LF>| <復帰CR>
                if (pc == 13) {
                    in.readCodepoint();
                    int ifLF = in.peekCodepoint();
                    if (ifLF == 10) {
                        in.readCodepoint();
                    }
                    return new Token(10, null);

                } else {
                    return new Token(10, null);
                }

            } else {
                return numKari();  // 2024-09-27

                /*
                System.err.println("Bad char="
                        + Integer.toHexString(pc));

                return errorChar(pc);
                throw new ReaderException("Bad char", in.getLineNum(), in.getNthChar());
                 */
            }
        }
    }

    Token errorChar(final int pc) {

        in.readCodepoint();
        List<Integer> list = new ArrayList<>();
        list.add(pc);
        return new Token(0xf0, list);    // (Pending)
    }

    Token sign(final int pc) {

        in.readCodepoint();
        List<Integer> list = readToTerm(pc);

        return new Token(pc, list);
    }

    // １文字は呼んでからくる
    public List<Integer> readToTerm(int firstChar) {

        ArrayList<Integer> list = new ArrayList<>();
        list.add(firstChar);
        /*                      #\# が読めないため、追加したが、バグ？
        // add 24-09-16
        int nextC = in.readChar();
        list.add(nextC);
         */
        for (;;) {
            int ch = in.peekCodepoint();

            if (this.cflag.isTerm(ch)) {
                break;
            }
            list.add(ch);
            in.readCodepoint();
        }

        return list;
    }

    Token backquote() {

        in.readCodepoint();
        return new Token('`', null);   // `
    }

    Token comma() {
        in.readCodepoint();
        int next = in.peekCodepoint();
        if (next == '@') {
            in.readCodepoint();
            return new Token(0xa2, null);   // ,@
        } else {
            return new Token(0xa1, null);   // ,
        }
    }

    // 暫定の数字の読み込み
    Token numKari() {
        int pc = in.readCodepoint();
        List<Integer> list = readToTerm(pc);

        return new Token('N', list);
    }

    static int BAD_STRING = 0x1e;

    Token readString() {

        int startLin = in.getLineNum();
      //  int startChar = in.getNthChar();

        ArrayList<Integer> buf = new ArrayList<>();
        for (;;) {
            int c = in.readCodepoint();
            if (c == -1) {
                return new Token(BAD_STRING, buf, startLin);  // 変なところにEOF
            }
            if (c == '\\') {
                // エスケープ文字
                int c2 = in.readCodepoint();

                if (c2 == -1) {
                    return new Token(BAD_STRING, buf, startLin);  // 変なところにEOF
                }
                int nn = readEscapeInString(c2);
                if (0 <= nn) {
                    buf.add(nn);
                }

            } else {
                if (c == '"') {
                    break;
                }
                buf.add(c);
            }
        }

        return new Token('\"', buf);
    }

    int readEscapeInString(int c2) {

        if (c2 == 'a') {
            // alarm
            return ((int) 7);
        } else if (c2 == 'b') {
            // backspace
            return ((int) 8);
        } else if (c2 == 't') {
            // tab   (2024-11-05)
            return ((int) 9);

        } else if (c2 == 'n') {
            // inefeed
            return ((int) 10);
        } else if (c2 == 'r') {
            // return
            return ((int) 13);
        } else if (c2 == 0x5c) {
            // \
            return ((int) 0x5c);
        } else if (c2 == 'x') {

            int d = in.readCodepoint();
            StringBuilder sb = new StringBuilder();

            while (isHexChar(d)) {
                sb.append((char) d);
                d = in.readCodepoint();
            }
            String hexStr = sb.toString();
            LOG.info("hexStr={}", hexStr);
            if (d != ';') {
              //  throw new IllegalStateException("reader exception need ;  in string");
                // 2024-11-05 ReaderExceptionに変更
                throw new ReaderException("reader exception need ;  in string");
            }
            int nt = Integer.parseInt(hexStr, 16);
            return (nt);

        } else if (c2 == ' ' || c2 == '\t') {

            // \<行内空白>*<行末><行内空白>* : nothing
            for (;;) {
                int c3 = in.peekCodepoint();

                if (c3 == '\r' || c3 == '\n') {
                    // <行末>
                    in.readCodepoint();
                    break;
                }

                if (c3 == '\t' || c3 == ' ') {
                    // <行内空白> −→ <スペースまたはタブ>
                    in.readCodepoint();
                    continue;
                }
                int line = in.getLineNum();
                int chNo = in.getNthChar();
                // 2024-11-05 ReaderExceptionに変更
                throw new ReaderException("Illegal format: line=" + line + ", pos=" + chNo
                          + ", c3=" + c3);
            }

            for (;;) {
                int c3 = in.peekCodepoint();
                if (c3 == '\t' || c3 == ' ') {
                    // <行内空白> −→ <スペースまたはタブ>
                    in.readCodepoint();
                    continue;
                }
                return -1; // nothing
            }

        } else {
            /*  2024-11-05
            int line = in.getLineNum();
            int chNo = in.getNthChar();
            throw new LispException("Illegal format: line=" + line + ", pos=" + chNo);
             */
            return c2;
        }

    }

    public static boolean isHexChar(int c) {

        if ('0' <= c && c <= '9') {
            return true;
        } else if ('A' <= c && c <= 'F') {
            return true;

        } else if ('a' <= c && c <= 'f') {
            return true;

        }
        return false;

    }

    void skipSpace() {
        for (;;) {
            int ch = in.peekCodepoint();
            if (!(this.cflag.isSpace(ch))) {
                return;
            }
            ch = in.readCodepoint();
        }
    }

    Token readId(int peeked) {

        ArrayList<Integer> list = new ArrayList<>();
        list.add(peeked);
        for (;;) {
            int pc = in.peekCodepoint();

            boolean judge = cflag.isAllowedRestCharOfId(pc);

            if (!judge) {
                // 許可された第二の文字以外なら抜ける
                break;
            }
            in.readCodepoint();
            list.add(pc);
        }
        return new Token(0xa0, list);
    }

    Token readComment(int peeked) {

        ArrayList<Integer> list = new ArrayList<>();
        list.add(peeked);
        for (;;) {
            int pc = in.readCodepoint();
            list.add(pc);
            pc = in.peekCodepoint();
            if (this.cflag.isEOL(pc)) {
                // 行末だと抜ける
                break;
            }
            if (pc < 0) {
                // break if EOF
                break;
            }
        }

        return new Token(';', list);

    }

}

/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.reader;

import java.math.BigInteger;
import java.util.List;
import org.slf4j.LoggerFactory;

import uika806.err.ReaderException;
import uika806.fn011.reader.NumParser;
import uika806.fn011.reader.SharpMacroReader;
import uika806.fn011.reader.Token;
import uika806.fn011.reader.Tokenizer;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;
import uika806.objects.Complex;
import uika806.objects.Ratio;

/**
 * これを継承しているクラスがあるので、修正する時は注意が必要
 *
 */
public class LispReaderEx {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(LispReaderEx.class);

    final BqExpander bqExpander = new BqExpander();
    
    public final Tokenizer tokenizer;

    SharpMacroReader sharpRedaer;

    public String foldStatus = "";

    public LispReaderEx(Tokenizer instance) {

        this.tokenizer = instance;
        this.sharpRedaer = new SharpMacroReader();
    }

    public Object read(boolean eofError, Object eofval) {

        Token token = tokenizer.next();
        return read(token, eofError, eofval);
    }

    public Object read(Token token, boolean eofError, Object eofval) {
        for (;;) {
            switch (token.id) {
                case -1:
                    if (eofError) {
                        throw new ReaderException("end of file", tokenizer.in.getLineNum(),
                                tokenizer.in.getNthChar());
                    } else {
                        return eofval;
                    }
                case 10:  // LF
                    token = tokenizer.next();
                    continue;

                case 0x3b:  // ;
                    token = tokenizer.next();
                    continue;
                case 0xa3:  // #| ... |#
                    token = tokenizer.next();
                    continue;

                case 0xa6: // #;a 2
                {
                    Object skip = read(tokenizer.next(), eofError, eofval);
                    token = tokenizer.next();
                    continue;
                }
                case '(':
                    return readToList(')', eofError, eofval);

                case 0xa0:
                    return toSymbol(token);
                case 'N':
                    return toNumber(token);

                case '"':
                    return toSString(token, eofError, eofval);

                case '\'':
                    return quote(token, eofError, eofval);
                case '`': {
                    Object o = quote(token, eofError, eofval);
                  //  return syntaxQuote(o);
                    Object inner = RT.cadr(o);
                    return bqExpander.expand_bq(inner);
                    
                }
                case 0xa1: // ,
                    return unquote(token, eofError, eofval);

                case 0xa2: // ,@
                    return unquote(token, eofError, eofval);

                case '+':
                    return plus(token, eofError, eofval);
                case '-':
                    return minus(token, eofError, eofval);

                case '#': {
                    Object sharp = sharpRedaer.sharp(token, eofError, eofval, this);
                    if (sharp == null) {
                        token = tokenizer.next();
                        continue;
                    }
                    return sharp;
                }
                case 0xa4:  // #( ... )
                    return toArray(eofError, eofval);
                case 0xa5:  // #u8( ... )
                    return toU8Array(eofError, eofval);

                case ')':
                default:
                    throw new ReaderException("**Unsupported id=" + token.id,
                            tokenizer.in.getLineNum(),
                            tokenizer.in.getNthChar());
            }
        }
    }

    
    
 /*   
    Object syntaxQuote(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;

            if (isQuasiQuote(cell)) {
                Object a = RT.cadr(cell);
                if (a instanceof Cell) {
                    Cell c = (Cell) a;
                    return sqExpandList(c);
                } else {
                    return a;
                }

            } else if (isUnquote(cell)) {
                LOG.info("---------- case3 unquote");
                return RT.cadr(form);
            } else if (isUnquoteSplicing(cell)) {
                LOG.info("---------- case4 unquote splicing");

                throw new ReaderException("splice not in list");

            } else {
                return sqExpandList(cell);
            }

        } else {

            return form;
        }
    }

    Object sqExpandList(Cell list) {

        Object car = list.getCar();
        Object cdr = list.getCdr();

        if (car instanceof Cell) {
            Cell cell0 = (Cell) car;

            if (isUnquote(cell0)) {
                if (cdr instanceof Cell) {
                    return RT.list(SSymbol.CONS, RT.cadr(cell0), sqExpandList((Cell) cdr));
                } else {
                    return RT.list(SSymbol.CONS, RT.cadr(cell0), cdr);
                }
            } else if (isUnquoteSplicing(cell0)) {
                if (cdr instanceof Cell) {
                    return RT.list(SSymbol.APPEND, RT.cadr(cell0), sqExpandList((Cell) cdr));
                } else {
                    return RT.list(SSymbol.APPEND, RT.cadr(cell0), cdr);
                }
            } else {
                return new Cell(SSymbol.CONS,
                        new Cell(RT.list(SSymbol.QUOTE, car), sqExpandList(cell0)));
            }

        } else {
            Object qcar = RT.list(SSymbol.QUOTE, car);
            if (cdr instanceof Cell) {
                return RT.list(SSymbol.CONS,
                        qcar, sqExpandList((Cell) cdr));
            } else {
                return RT.list(SSymbol.QUOTE, list);
            }
        }

    }

    boolean isUnquoteSplicing(Cell form) {

        return SSymbol.UNQUOTE_SPLICE.equals(form.getCar());
    }

    boolean isUnquote(Cell form) {

        return SSymbol.UNQUOTE.equals(form.getCar());
    }

    boolean isQuasiQuote(Cell form) {

        return SSymbol.QUASIQUOTE.equals(form.getCar());
    }
*/
    
    
    public String listToString(List<Integer> arr, int start) {

        StringBuilder sb = new StringBuilder();
        int len = arr.size();
        for (int i = start; i < len; i++) {
            int code = arr.get(i);
            sb.appendCodePoint(code);
        }

        return sb.toString();
    }

    private Object minus(Token tk, boolean eofError, Object eofval) {

        String s = listToString(tk.str, 0);

        //   LOG.info("86) plus = {}", s);
        if ("-inf.0".equals(s)) {
            return Double.NEGATIVE_INFINITY;
        } else if ("-nan.0".equals(s)) {
            return Double.NaN;
        }
        return toNumber(tk);
    }

    private Object plus(Token tk, boolean eofError, Object eofval) {

        String s = listToString(tk.str, 0);

        //   LOG.info("86) plus = {}", s);
        if ("+inf.0".equals(s)) {
            return Double.POSITIVE_INFINITY;
        } else if ("+nan.0".equals(s)) {
            return Double.NaN;
        }
        return toNumber(tk);
    }

    // #( ... )
    public Object toArray(boolean eofError, Object eofval) {
        throw new ReaderException("Sorry can not read #( ... )",
                this.tokenizer.in.getLineNum(), this.tokenizer.in.getNthChar());
    }

    // #u8( ... )
    public Object toU8Array(boolean eofError, Object eofval) {
        throw new ReaderException("Sorry can not read #u8( ... )",
                this.tokenizer.in.getLineNum(), this.tokenizer.in.getNthChar());
    }

    public Object toSString(Token tk, boolean eofError, Object eofval) {
        throw new ReaderException("Sorry can not read String",
                this.tokenizer.in.getLineNum(), this.tokenizer.in.getNthChar());
    }

    int commaLevel = 0;

    private Object quote(Token tk, boolean eofError, Object eofval) {

        SSymbol sym = null;
        if (tk.id == '\'') {
            sym = SSymbol.QUOTE;
        } else if (tk.id == '`') {
            sym = SSymbol.QUASIQUOTE;
            commaLevel++;
        } else {
            throw new ReaderException("Unsupported id=" + tk.id,
                    tokenizer.in.getLineNum(),
                    tokenizer.in.getNthChar());
        }
        Object obj = read(eofError, eofval);

        return xcons(sym, xcons(obj, EmptyList.NIL));
    }

    private Object unquote(Token tk, boolean eofError, Object eofval) {

        SSymbol sym = null;
        if (tk.id == 0xa1) {
            sym = SSymbol.UNQUOTE;
            if (commaLevel == 0) {
                throw new ReaderException(", not inside ` ",
                        tokenizer.in.getLineNum(),
                        tokenizer.in.getNthChar());
            }
        } else if (tk.id == 0xa2) {
            sym = SSymbol.UNQUOTE_SPLICE;
            if (commaLevel == 0) {
                throw new ReaderException(",@ not inside ` ",
                        tokenizer.in.getLineNum(),
                        tokenizer.in.getNthChar());
            }

        } else {
            throw new ReaderException("Unsupported id=" + tk.id,
                    tokenizer.in.getLineNum(),
                    tokenizer.in.getNthChar());
        }
        commaLevel--;
        Object obj = read(eofError, eofval);
        commaLevel++;
        return xcons(sym, xcons(obj, EmptyList.NIL));
    }

    public Object toNumber(Token tk) {
        String wk = tk.getText();

        NumParser np = new NumParser(wk);  //毎回作る必要がある
        if (np.isInt()) {

            long k = Long.parseLong(wk);
            return (k);

        } else if (np.isFloat()) {
            double dbl = Double.parseDouble(wk);
            return (dbl);
        } else if (np.isRatio()) {
            Ratio ra = toRatio(wk);
            return ra;

        } else if (np.isComplex()) {
            Complex comp = null;
            try {
                comp = toComplex(wk);
            } catch (Exception ex) {
                throw new ReaderException("Complex number format error", ex, tk.line, 0);
            }
            return comp;

        }
        return new SSymbol(wk);  //***********
    }

    private Ratio toRatio(String wk) {

        int p1 = wk.indexOf('/', 0);

        String left = wk.substring(0, p1);
        String right = wk.substring(p1 + 1);

        // LOG.info("left = {}, right ={}", left, right);
        BigInteger b1 = new BigInteger(left);
        BigInteger b2 = new BigInteger(right);
        return new Ratio(b1, b2);
    }

    private Complex toComplex(String wk) throws Exception {

        char c1 = wk.charAt(0);
        int start = 0;
        if (c1 == '+' || c1 == '-') {
            start = 1;
        }

        int p1 = wk.indexOf('+', start);
        int p2 = wk.indexOf('-', start);

        String left = "";
        if (p1 >= 0) {
            left = wk.substring(0, p1);
            start = p1 + 1;
        } else if (p2 >= 0) {
            left = wk.substring(0, p2);
            start = p2;
        } else {
            throw new IllegalStateException(wk);
        }

        //                                 infとのマッチを避けるため+1
        int p9 = wk.indexOf('i', start + 1);
        String right = wk.substring(start, p9);

        //   LOG.info("left = {}, right ={}", left, right);
        Double real = parseDbl(left);

        Double imag = parseDbl(right);
        return new Complex(real, imag);
    }

    Double parseDbl(String right) {

        if ("+nan.0".equals(right)) {
            return Double.NaN;
        }
        if ("inf.0".equals(right)) {
            return Double.POSITIVE_INFINITY;
        }
        if ("-inf.0".equals(right)) {
            return Double.NEGATIVE_INFINITY;
        }

        Double imag;
        try {
            imag = Double.parseDouble(right);
        } catch (Exception e2) {
            LOG.error("324) '{}'", right);
            throw e2;
        }
        return imag;
    }

    private Object toSymbol(Token tk) {
        return new SSymbol(tk.getText());
    }

    boolean isPeriod(List<Integer> str) {

        return (str.size() == 1) && (str.get(0) == 0x2e);
    }

    private Object readToList(int last, boolean eofError, Object eofval) {

        Token tk = skipSpace();
        if (tk.id == -1) {
            if (eofError) {
                throw new ReaderException("end of file", tokenizer.in.getLineNum(),
                        tokenizer.in.getNthChar());
            } else {
                return eofval;
            }

        } else if (tk.id == 78 && isPeriod(tk.str)) {

            Object obj = read(true, eofval);   //***
            tk = tokenizer.next();
            if (tk.id != last) {
                throw new ReaderException("Unmatched " + last,
                        tokenizer.in.getLineNum(),
                        tokenizer.in.getNthChar());

            }
            return obj;

        } else if (tk.id != last) {

            Object car = read(tk, true, null);
            //   Object cdr = readToList(last, eofError, eofval);
            Object cdr = readToList(last, true, null);  // FIX 2024-10-16

            return xcons(car, cdr);
        } else {
            return EmptyList.NIL;
        }
    }

    // immutable な cons
    private Cell xcons(Object x, Object y) {
        return new Cell(x, y, false);
    }

    public Token skipSpace() {

        for (;;) {

            Token next = tokenizer.next();

            if (next.id == ';' || next.id == 0xa3 || next.id == 10) {
                continue;
            }
            return next;
        }
    }

}

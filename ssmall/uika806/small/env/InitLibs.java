/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

import java.util.HashMap;
import java.util.Map;

import uika806.pico.fn.Func011;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.pico.fn.AppendFn;
import uika806.pico.fn.EqvqFn;
import uika806.pico.fn.Func012;
import uika806.pico.fn.MaxFn;
import uika806.pico.fn.MinFn;
import uika806.pico.fn.NegativeqFn;
import uika806.pico.fn.PositiveqFn;
import uika806.objects.SSymbol;
import uika806.pico.fn.ZeroqFn;
import uika806.pico.macro.LetRecMacro;
import uika806.pico.macro.LetValues1Macro;

import uika806.small.fn.AssocFn;
import uika806.small.fn.AssqFn;

import uika806.string.CharAlphabetFn;
import uika806.string.CharCi;
import uika806.string.CharDownCaseFn;

import uika806.string.CharFoldcaseFn;

import uika806.string.CharIntegerFn;
import uika806.string.CharLowerCaseFn;

import uika806.string.CharNumericFn;
import uika806.string.CharUpCaseFn;
import uika806.string.CharUpperCaseFn;
import uika806.string.CharWhiteSpaceFn;
import uika806.pico.fn.CloseInputPortFn;
import uika806.pico.fn.DefineFn;

import uika806.small.inexact.Complexs;

import uika806.small.fn.CurrentJiffyFn;
import uika806.small.fn.CurrentSecondFn;

import uika806.small.fn.DisplaySmallFn;
import uika806.small.fn.EqqFn;
import uika806.pico.fn.EqualFn;
import uika806.pico.fn.ExitFn;
import uika806.pico.fn.Func011.NumberqFn;

import uika806.small.fn.ExactFn;

import uika806.small.inexact.ExpFn;
import uika806.small.fn.ExptFn;
import uika806.small.fn.GtFn;

import uika806.small.fn.JiffiesPerSecFn;
import uika806.small.fn.LengthFn;

import uika806.pico.fn.ListFn;
import uika806.pico.fn.NewLineFn;
import uika806.pico.fn.NullqFn;
import uika806.small.inexact.MagnitudeFn;
//import uika806.small.fn.MakeByteVectorFn;
import uika806.small.inexact.MakePolar;
import uika806.small.inexact.MakeRectanglerFn;

import uika806.small.inexact.NanqFn;
import uika806.pico.fn.OpenInputFileFn;

import uika806.pico.fn.SDefMacroFn;
import uika806.pico.macro.DefineSyntaxMacro;
import uika806.pico.macro.SyntaxRulesMacro;
import uika806.test.TestValuesMacro;
import uika806.test.Tests;

import uika806.small.fn.EnvironmentFn;

import uika806.small.fn.EvalFn;

import uika806.small.inexact.SqrtFn;

import uika806.string.StringFn;

import uika806.small.fn.WriteSmallFn;

import uika806.syntax.Environ;

import uika806.small.fn.LibBase;
import uika806.small.fn.ListCopyFn;

import uika806.small.fn.ListTailFn;
import uika806.small.fn.ListqFn;
import uika806.small.fn.LoadFn;
//import uika806.small.fn.LtFn;
import uika806.small.fn.MacroExpandFn;

import uika806.small.fn.MemberFn;
import uika806.small.fn.MemqFn;
import uika806.small.fn.MemvFn;

import uika806.portfn.PortFns;
import uika806.small.fn.RaiseContinuableFn;
import uika806.small.fn.RaiseFn;
import uika806.small.fn.RationalizeFn;

import uika806.small.fn.ReadFn;

import uika806.small.fn.ReverseFn;
import uika806.string.StringEqualFn;
import uika806.string.StringLengthFn;
import uika806.string.StringNumberFn;

import uika806.small.fn.SymbolEqualFn;
import uika806.small.fn.SymbolStringFn;

import uika806.portfn.WriteSimpleFn;

import uika806.portfn.WriteU8Fn;
import uika806.small.inexact.LibInexact;
import uika806.lib.Library;
import uika806.objects.SpecialOperator;
import uika806.pico.fn.ListLibraryFn;
import uika806.pico.fn.NthFn;
import uika806.pico.macro.DefineUikaSyntaxMacro;
import uika806.pico.macro.DefineV4Macro;
import uika806.pico.macro.DefineV5Macro;
import uika806.pico.macro.LambdaMacro;
import uika806.pico.macro.LetRecStarMacro;
import uika806.pico.macro.NewDefineMacro;
import uika806.small.fn.QuasiquoteFn;
import uika806.small.macro.ImportMacro;
import uika806.small.fn.UnquoteFn;

import uika806.small.macro.UnsupportMacro;
import uika806.string.StringDowncaseFn;
import uika806.string.StringFoldcaseFn;
import uika806.string.StringUpcaseFn;
import uika806.syntax.DebugRulesFn;

/**
 *
 * <code>
 * (import (scheme base))
 *
 * (import (scheme case-lambda))
 *
 * (import (scheme char))
 *
 * (import (scheme complex))
 *
 * (import (scheme eval))
 * (import (scheme file))
 * (import (scheme inexact))
 *
 * (import (scheme lazy))
 * (import (scheme load))
 * (import (schemeprocess-context))
 * (import (scheme read))
 * (import (scheme repl))
 * (import (scheme time))
 * (import (scheme write))
 *
 * </code>
 */
public class InitLibs {

    public static final int VERSION4 = 4;
    public static final int VERSION5 = 5;

    static Environ internal = null;

    static Library BASE;

    public static Library initBase(int version) {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_BASE);

        Library lib = new Library((Cell) name, export);

        // ▼▼ macro-def-begin
        lib.add(SSymbol.SYNTAX_RULES, new SyntaxRulesMacro());

        lib.add(SSymbol.IMPORT, new ImportMacro());

        lib.add(SSymbol.DEFINE_SYNTAX, new DefineSyntaxMacro());

        lib.add(SSymbol.LAMBDA, new LambdaMacro());

        if (version == VERSION4) {

            //   putAFn(lib, "define", new DefineV4Macro());   // OLD
            //   defineの中に define-syntaxをかけるように
            //   putAFn(lib, "*define", new NewDefineMacro());   2025-01-09 remove
            putAFn(lib, "define", new NewDefineMacro());     // Ver4
            // ▼ Ver4とVer5で変更する
            lib.add(SSymbol.SYN_DOT, SSymbol.SYN_DOT);  // ...
            lib.add(SSymbol.SYN_ELSE, SSymbol.SYN_ELSE);  // cond の else
            lib.add(SSymbol.SYN_GT, SSymbol.SYN_GT);    // cond の =>

        } else {
            // VERSION 5 or later
            //  putAFn(lib, "define", new DefineV4Macro());   // OLD

            putAFn(lib, "*define", new DefineV5Macro());   // NEW

            putAFn(lib, "define", new NewDefineMacro());     // Ver4

            putAFn(lib, "-rules", new DebugRulesFn()); // syntax-rulesの中身を表示する関数

            // スペシャル・オペレーター
            lib.add(SSymbol.SETQ, SpecialOperator.SETQ);
            lib.add(SSymbol.QUOTE, SpecialOperator.QUOTE);

            // 2025-01-20
            lib.add(SSymbol.LET, SpecialOperator.LET);
            lib.add(SSymbol.LETREC, SpecialOperator.LETREC);
            lib.add(SSymbol.LET_SYNTAX, SpecialOperator.LET_SYNTAX);
            lib.add(SSymbol.LETREC_SYNTAX, SpecialOperator.LETREC_SYNTAX);

            
            // lambdaは195行目で定義済み
            
            // add 2025-01-17
            lib.add(SSymbol.CALL_CC, SpecialOperator.CALL_CC);
            lib.add(SSymbol.CALL_CC2, SpecialOperator.CALL_CC);
            
            lib.add(SSymbol.IF, SpecialOperator.IF);
            lib.add(SSymbol.WITH_EXCEPTION, SpecialOperator.WITH_EXCEPTION);

            lib.add(SSymbol.SYN_DOT, SpecialOperator.SYN_DOT);  // ...
            lib.add(SSymbol.SYN_ELSE, SpecialOperator.SYN_ELSE);  // cond の else
            lib.add(SSymbol.SYN_GT, SpecialOperator.SYN_GT);    // cond の =>
        }

        // ▲▲ macro-def-end
        lib.add(SSymbol.SDEF_MACRO, new SDefMacroFn()); // 独自の関数

        putAFn(lib, "-mexp", new MacroExpandFn());  // 独自の関数

        putAFn(lib, "-list-lib", new ListLibraryFn()); // 独自の関数

        putAFn(lib, "nth", new NthFn()); // 独自の関数

        lib.add(SSymbol.DEFINE4, new DefineFn()); // 独自の関数

        // ================
        putAFn(lib, "*", new Func011.MyMultiply());
        putAFn(lib, "+", new Func011.MyAdd());

        putAFn(lib, "-", new Func011.MySub());
        putAFn(lib, "/", new Func011.MyDivide());

        lib.add(SSymbol.LT, new LibBase.LtFn());

        putAFn(lib, "<=", new Func011.LeFn());

        putAFn(lib, "=", new Func011.MyEquiv());

        lib.add(SSymbol.GT, new GtFn());

        putAFn(lib, ">=", new Func011.GeFn());

        putAFn(lib, "abs", new LibBase.AbsFn());

        putAFn(lib, "append", new AppendFn());

        putAFn(lib, "assoc", new AssocFn());
        putAFn(lib, "assq", new AssqFn());

        putAFn(lib, "assv", new LibBase.AssvFn());

        putAFn(lib, "binary-port?", new PortFns.BinaryPortqFn());
        putAFn(lib, "boolean=?", new LibBase.BooleanEqqFn());

        putAFn(lib, "boolean?", new Func012.BooleanqFn());

        lib.add(SSymbol.BY_VEC, new LibBase.ByteVectorFn());

        putAFn(lib, "bytevector-append", new LibBase.ByteVectorAppendFn());

        putAFn(lib, "bytevector-copy", new LibBase.ByteVectorCopy());

        putAFn(lib, "bytevector-copy!", new LibBase.ByteVectorCopyeFn());

        putAFn(lib, "bytevector-length", new LibBase.ByteVectorLengthFn());

        putAFn(lib, "bytevector-u8-ref", new LibBase.ByteVectorRefFn());

        putAFn(lib, "bytevector-u8-set!", new LibBase.ByteVectorSetFn());

        putAFn(lib, "bytevector?", new Func012.ByteVectorqFn());

        putAFn(lib, "caar", new Func011.Caar());
        putAFn(lib, "cadr", new Func011.Cadr());

        putAFn(lib, "car", new Func011.Car());
        putAFn(lib, "cdar", new Func011.Cdar());

        putAFn(lib, "cddr", new Func011.Cddr());

        putAFn(lib, "cdr", new Func011.Cdr());

        putAFn(lib, "ceiling", new LibBase.CeilingFn());

        putAFn(lib, "char->integer", new CharIntegerFn());
        putAFn(lib, "char-ready?", new PortFns.CharReadyFn());

        putAFn(lib, "char<=?", new LibBase.CharLteFn());
        putAFn(lib, "char<?", new LibBase.CharLtFn());
        putAFn(lib, "char=?", new LibBase.CharEqqFn());
        putAFn(lib, "char>=?", new LibBase.CharGteFn());
        putAFn(lib, "char>?", new LibBase.CharGtFn());
        putAFn(lib, "char?", new Func012.CharFn());
        putAFn(lib, "char-downcase", new CharDownCaseFn());
        putAFn(lib, "char-foldcase", new CharDownCaseFn()); //*********
        putAFn(lib, "char-upcase", new CharUpCaseFn());
        putAFn(lib, "close-port", new PortFns.ClosePortFn());
        putAFn(lib, "close-input-port", new CloseInputPortFn());
        putAFn(lib, "close-output-port", new PortFns.CloseOutputPortFn());
        putAFn(lib, "complex?", new LibBase.ComplexqFn());
        putAFn(lib, "cond-expand", new UnsupportMacro("cond-expand")); // startup.scm で、再定義？
        putAFn(lib, "cons", new Func011.MyCons());
        putAFn(lib, "current-input-port", new PortFns.CurrentInputPortFn());
        putAFn(lib, "current-output-port", new PortFns.CurrentOutputPortFn());
        putAFn(lib, "current-error-port", new PortFns.CurrentErrorPortFn());

        putAFn(lib, "denominator", new LibBase.DenominatorFn());
        putAFn(lib, "define-record-type", new UnsupportMacro("define-record-type"));
        putAFn(lib, "dynamic-wind", new UnsupportMacro("dynamic-wind"));

        putAFn(lib, "eof-object", new LibBase.EofObjectFn());
        putAFn(lib, "eof-object?", new LibBase.EofObjectQFn());
        putAFn(lib, "eq?", new EqqFn());
        putAFn(lib, "equal?", new EqualFn());
        putAFn(lib, "eqv?", new EqvqFn());
        putAFn(lib, "error", new Func012.ErrorFn());
        putAFn(lib, "error-object-irritants", new Func012.ErrorObjectIrritantsFn());
        putAFn(lib, "error-object-message", new Func012.ErrorObjectMessageFn());
        putAFn(lib, "error-object?", new Func012.ErrorObjectqFn());
        putAFn(lib, "even?", new LibBase.EvenqFn());
        putAFn(lib, "exact", new ExactFn());
        putAFn(lib, "exact-integer-sqrt", new LibBase.ExactIntegerSqrtFn());
        putAFn(lib, "exact-integer?", new LibBase.ExactIntegerqFn());
        putAFn(lib, "exact?", new LibBase.ExactqFn());
        putAFn(lib, "expt", new ExptFn());

        putAFn(lib, "features", new LibBase.FeaturesFn());
        putAFn(lib, "file-error?", new Func012.FileErrorFn());
        putAFn(lib, "floor", new LibBase.FloorFn());
        putAFn(lib, "floor/", new LibBase.FloorSlashFn());
        putAFn(lib, "floor-quotient", new LibBase.FloorQuotientFn());
        putAFn(lib, "floor-remainder", new LibBase.FloorRemainderFn());
        putAFn(lib, "flush-output-port", new PortFns.FlushOutputPortFn());

        putAFn(lib, "gcd", new LibBase.GcdFn());
        putAFn(lib, "get-output-bytevector", new PortFns.GetOutputByteVector());
        putAFn(lib, "get-output-string", new PortFns.GetOutputStringFn());

        putAFn(lib, "include", new UnsupportMacro("include"));
        putAFn(lib, "include-ci", new UnsupportMacro("include-ci"));
        putAFn(lib, "inexact", new LibBase.InexactFn());
        putAFn(lib, "inexact?", new LibBase.InExactqFn());
        putAFn(lib, "input-port?", new Func012.InputPortqFn());
        putAFn(lib, "input-port-open?", new PortFns.InputPortOpenqFn());
        putAFn(lib, "integer->char", new LibBase.IntegerChar());
        putAFn(lib, "integer?", new LibBase.IntegerqFn());
        putAFn(lib, "lcm", new LibBase.LcmFn());
        putAFn(lib, "length", new LengthFn());

        putAFn(lib, "let-values", new LetValues1Macro());   //***********************

        if (version == VERSION4) {
            putAFn(lib, "letrec", new LetRecMacro());
            putAFn(lib, "letrec*", new LetRecStarMacro());
        }

        putAFn(lib, "list", new ListFn());
        putAFn(lib, "list->string", new LibBase.ListStringFn());
        putAFn(lib, "list->vector", new LibBase.ListVectorFn());
        putAFn(lib, "list-copy", new ListCopyFn());
        putAFn(lib, "list-ref", new LibBase.ListRefFn());
        putAFn(lib, "list-set!", new LibBase.ListSetFn());
        putAFn(lib, "list-tail", new ListTailFn());
        putAFn(lib, "list?", new ListqFn());

        putAFn(lib, "make-bytevector", new LibBase.MakeByteVectorFn());
        putAFn(lib, "make-list", new LibBase.MakeListFn());
        putAFn(lib, "make-string", new Func012.MakeStringFn());
        putAFn(lib, "make-vector", new LibBase.MakeVectorFn());
        putAFn(lib, "max", new MaxFn());
        putAFn(lib, "member", new MemberFn());
        putAFn(lib, "memq", new MemqFn());
        putAFn(lib, "memv", new MemvFn());
        putAFn(lib, "min", new MinFn());
        putAFn(lib, "modulo", new LibBase.ModuloFn());

        putAFn(lib, "negative?", new NegativeqFn());
        putAFn(lib, "newline", new NewLineFn());
        putAFn(lib, "not", new Func011.MyNot());
        putAFn(lib, "null?", new NullqFn());
        putAFn(lib, "number->string", new LibBase.NumberStringFn());
        putAFn(lib, "number?", new NumberqFn());
        putAFn(lib, "numerator", new LibBase.NumeratorFn());

        putAFn(lib, "odd?", new LibBase.OddqFn());
        putAFn(lib, "open-input-bytevector", new PortFns.OpenInputByteVector());
        putAFn(lib, "open-output-bytevector", new PortFns.OpenOutputByteVector());
        putAFn(lib, "open-input-string", new PortFns.OpenInputStringFn());
        putAFn(lib, "open-output-string", new PortFns.OpenOutputStringFn());
        putAFn(lib, "output-port?", new Func012.OutputPortqFn());
        putAFn(lib, "output-port-open?", new PortFns.OutputPortOpenqFn());

        putAFn(lib, "pair?", new Func012.PairqFn());
        putAFn(lib, "peek-char", new PortFns.PeekCharFn());
        putAFn(lib, "peek-u8", new PortFns.PeekU8Fn());
        putAFn(lib, "port?", new Func012.PortqFn());
        putAFn(lib, "positive?", new PositiveqFn());
        putAFn(lib, "procedure?", new Func011.ProcedureqFn());

        putAFn(lib, "quotient", new LibBase.TruncateQuotientFn());
        putAFn(lib, "quasiquote", new QuasiquoteFn());

        putAFn(lib, "raise", new RaiseFn());  // ******
        putAFn(lib, "raise-continuable", new RaiseContinuableFn());  // ******
        putAFn(lib, "rational?", new LibBase.RationalqFn());
        putAFn(lib, "rationalize", new RationalizeFn());
        putAFn(lib, "read-bytevector", new PortFns.ReadByteVectorFn());
        putAFn(lib, "read-bytevector!", new PortFns.ReadByteVectoreFn()); //add 2024-11-03
        putAFn(lib, "read-char", new PortFns.ReadCharFn());
        putAFn(lib, "read-error?", new Func012.ReadErrorFn());
        putAFn(lib, "read-u8", new PortFns.ReadU8Fn());
        putAFn(lib, "real?", new LibBase.RealqFn());
        putAFn(lib, "remainder", new LibBase.RemainderFn());

        putAFn(lib, "read-line", new PortFns.ReadLineFn());

        putAFn(lib, "read-string", new PortFns.ReadStringFn());

        putAFn(lib, "reverse", new ReverseFn());

        putAFn(lib, "round", new LibBase.RoundFn());

        putAFn(lib, "set-car!", new Func011.SetCarFn());
        putAFn(lib, "set-cdr!", new Func011.SetCdrFn());

        putAFn(lib, "square", new Func011.Square());

        putAFn(lib, "string", new StringFn.StringAFn());

        putAFn(lib, "string->list", new LibBase.StringListFn());
        putAFn(lib, "string->number", new StringNumberFn());

        putAFn(lib, "string->symbol", new LibBase.StringSymbolFn());

        putAFn(lib, "string->utf8", new LibBase.StringUtf8Fn());

        putAFn(lib, "string->vector", new LibBase.StringVectorFn());

        putAFn(lib, "string-append", new LibBase.StringAppendFn());

        putAFn(lib, "string-copy", new LibBase.StringCopyFn());

        putAFn(lib, "string-copy!", new LibBase.StringCopyeFn());
        putAFn(lib, "string-fill!", new LibBase.StringFillFn());

        putAFn(lib, "string-length", new StringLengthFn());

//        putAFn(lib, "string-map", new StringMapFn());  // **********
        putAFn(lib, "string-ref", new LibBase.StringRefFn());
        putAFn(lib, "string-set!", new StringFn.StringSet());

        putAFn(lib, "string<=?", new StringFn.StringLteFn());

        putAFn(lib, "string<?", new StringFn.StringLtFn());

        putAFn(lib, "string=?", new StringEqualFn());

        putAFn(lib, "string>=?", new StringFn.StringGteFn());
        putAFn(lib, "string>?", new StringFn.StringGtFn());

        putAFn(lib, "string?", new LibBase.StringqFn());

        putAFn(lib, "substring", new LibBase.StringCopyFn());

        putAFn(lib, "symbol->string", new SymbolStringFn());

        putAFn(lib, "symbol=?", new SymbolEqualFn());

        putAFn(lib, "symbol?", new Func012.SymbolqFn());

        putAFn(lib, "syntax-error", new Func012.ErrorFn());

        putAFn(lib, "textual-port?", new Func012.TextPortqFn());

        putAFn(lib, "truncate", new LibBase.TruncateFn());

        putAFn(lib, "truncate-quotient", new LibBase.TruncateQuotientFn());

        putAFn(lib, "truncate-remainder", new LibBase.TruncateRemainderFn());

        putAFn(lib, "truncate/", new LibBase.TruncateSlashFn());

        putAFn(lib, "u8-ready?", new PortFns.U8ReadyFn());

        putAFn(lib, "unquote", new UnquoteFn());

        putAFn(lib, "utf8->string", new LibBase.Utf8StringFn());

        putAFn(lib, "values", new LibBase.ValuesFn());

        putAFn(lib, "vector", new LibBase.VectorFn());

        putAFn(lib, "vector->list", new LibBase.VectorListFn());

        putAFn(lib, "vector->string", new LibBase.VectorStringFn());

        putAFn(lib, "vector-append", new LibBase.VectorAppendFn());

        putAFn(lib, "vector-copy", new LibBase.VectorCopy());

        putAFn(lib, "vector-copy!", new LibBase.VectorCopyeFn());

        putAFn(lib, "vector-fill!", new LibBase.VectorFillFn());

        putAFn(lib, "vector-length", new LibBase.VectorLengthFn());

        //   putAFn(lib, "vector-map", new VectorMapFn());  // ********
        putAFn(lib, "vector-ref", new LibBase.VectorRefFn());

        putAFn(lib, "vector-set!", new LibBase.VectorSetFn());

        putAFn(lib, "vector?", new Func012.VectorqFn());

        putAFn(lib, "write-bytevector", new PortFns.WriteByteVectorFn());

        putAFn(lib, "write-char", new PortFns.WriteCharFn());

        putAFn(lib, "write-u8", new WriteU8Fn());

        putAFn(lib, "write-string", new PortFns.WriteStringFn());

        putAFn(lib, "zero?", new ZeroqFn());

        BASE = lib;
        return lib;
    }

    static Library CHAR;

    public static void initChar() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_CHAR);

        Library lib = new Library((Cell) name, export /*, internal*/);

        putAFn(lib, "char-alphabetic?", new CharAlphabetFn());
        putAFn(lib, "char-ci=?", new CharCi.CharEqqFn());
        putAFn(lib, "char-ci<?", new CharCi.CharLtFn());
        putAFn(lib, "char-ci<=?", new CharCi.CharLteFn());

        putAFn(lib, "char-ci>?", new CharCi.CharGtFn());
        putAFn(lib, "char-ci>=?", new CharCi.CharGteFn());

        putAFn(lib, "char-downcase", new CharDownCaseFn());
        putAFn(lib, "char-foldcase", new CharFoldcaseFn());

        putAFn(lib, "char-lower-case?", new CharLowerCaseFn());
        putAFn(lib, "char-numeric?", new CharNumericFn());

        putAFn(lib, "char-whitespace?", new CharWhiteSpaceFn());

        putAFn(lib, "char-upper-case?", new CharUpperCaseFn());

        putAFn(lib, "char->integer", new CharIntegerFn());

        putAFn(lib, "char-upcase", new CharUpCaseFn());

        putAFn(lib, "string-ci=?", new StringFn.StringCiEqual());

        putAFn(lib, "string-ci<?", new StringFn.StringCiLt());

        putAFn(lib, "string-ci<=?", new StringFn.StringCiLte());

        putAFn(lib, "string-ci>?", new StringFn.StringCiGt());

        putAFn(lib, "string-ci>=?", new StringFn.StringCiGte());

        putAFn(lib, "string-upcase", new StringUpcaseFn());

        putAFn(lib, "string-foldcase", new StringFoldcaseFn());
        putAFn(lib, "string-downcase", new StringDowncaseFn());

        CHAR = lib;
    }

    static Library COMPLEX;

    public static void initComplex() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_COMPLEX);

        Library lib = new Library((Cell) name, export /*, internal*/);

        putAFn(lib, "make-rectangular", new MakeRectanglerFn());

        putAFn(lib, "make-polar", new MakePolar());

        putAFn(lib, "magnitude", new MagnitudeFn());

        putAFn(lib, "real-part", new Complexs.RealPartFn());

        putAFn(lib, "imag-part", new Complexs.ImagPartFn());

        COMPLEX = lib;
    }

    static Library CXR;

    public static void initCxr() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_CXR);

        Library lib = new Library((Cell) name, export /*, internal*/);

        lib.add(SSymbol.CAAAR, new Func011.Caaar());
        lib.add(SSymbol.CAADR, new Func011.Caadr());

        lib.add(SSymbol.CADAR, new Func011.Cadar());
        lib.add(SSymbol.CADDR, new Func011.Caddr());

        lib.add(SSymbol.CDAAR, new Func011.Cdaar());
        lib.add(SSymbol.CDADR, new Func011.Cdadr());

        lib.add(SSymbol.CDDAR, new Func011.Cddar());
        lib.add(SSymbol.CDDDR, new Func011.Cdddr());

        CXR = lib;
    }

    static Library INEXACT;

    public static void initInexact() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_INEXACT);

        Library lib = new Library((Cell) name, export);

        putAFn(lib, "acos", new LibInexact.AcosFn());
        putAFn(lib, "asin", new LibInexact.AsinFn());
        putAFn(lib, "atan", new LibInexact.AtanFn());

        putAFn(lib, "cos", new LibInexact.CosFn());

        putAFn(lib, "exp", new ExpFn());

        putAFn(lib, "finite?", new LibInexact.FiniteqFn());
        putAFn(lib, "infinite?", new LibInexact.InfiniteqFn());

        putAFn(lib, "log", new LibInexact.LogFn());

        putAFn(lib, "nan?", new NanqFn());

        putAFn(lib, "sin", new LibInexact.SinFn());

        putAFn(lib, "sqrt", new SqrtFn());

        putAFn(lib, "tan", new LibInexact.TanFn());

        INEXACT = lib;
    }

    static void putAFn(Library lib, String name, AFn func) {

        SSymbol sym = new SSymbol(name);
        lib.add(sym, func);
    }

    static Library FILE;

    public static void initFile() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_FILE);

        Library lib = new Library((Cell) name, export);

        lib.add(SSymbol.OIF, new OpenInputFileFn());
        lib.add(SSymbol.OOF, new PortFns.OpenOutputFileFn());

        FILE = lib;
    }

    static Library PROCESS;

    public static void initProcessContext() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_PROC);

        Library lib = new Library((Cell) name, export /*, internal*/);

        putAFn(lib, "exit", new ExitFn()); //*******BASEじゃない?

        PROCESS = lib;
    }

    static Library TIME;

    public static void initTime() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_TIME);

        Library lib = new Library((Cell) name, export /*, internal*/);

        lib.add(SSymbol.CUR_SEC, new CurrentSecondFn());

        lib.add(SSymbol.CUR_JIF, new CurrentJiffyFn());

        lib.add(SSymbol.JIF_PER_SEC, new JiffiesPerSecFn());

        TIME = lib;
    }

    static Library WRITE;

    public static void initWrite() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_WRITE);

        Library lib = new Library((Cell) name, export);

        putAFn(lib, "display", new DisplaySmallFn());

        /* 以下の箇所、実装できていない。
        *  データラベルは，環状が 無ければ使ってはならない。
         */
        putAFn(lib, "write", new WriteSmallFn());

        putAFn(lib, "write-shared", new WriteSmallFn());

        putAFn(lib, "write-simple", new WriteSimpleFn());

        WRITE = lib;
    }

    static Library LOAD;

    public static void initLoad() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_LOAD);

        Library lib = new Library((Cell) name, export /*, internal*/);

        putAFn(lib, "load", new LoadFn());

        LOAD = lib;
    }

    static Library READ;

    public static void initRead() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_READ);

        Library lib = new Library((Cell) name, export /*, internal*/);

        putAFn(lib, "read", new ReadFn());

        LOAD = lib;
    }

    static Library EVAL;

    public static void initEval() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_EVAL);

        Library lib = new Library((Cell) name, export);

        putAFn(lib, "environment", new EnvironmentFn());

        putAFn(lib, "eval", new EvalFn());

        EVAL = lib;
    }

    static Library UIKA806_TEST;

    public static void initUika806Test() {

        Map<SSymbol, SSymbol> export = new HashMap<>();

        Object name = RT.list(SSymbol.LIB_UIKA806, SSymbol.LIB_TEST);

        Library lib = new Library((Cell) name, export /*, internal*/);

        lib.add(SSymbol.TEST_VALUES, new TestValuesMacro());

        lib.add(SSymbol.TEST_VALUES, new TestValuesMacro());

        lib.add(new SSymbol("test-begin"), new Tests.TestBegin());
        lib.add(new SSymbol("test-end"), new Tests.TestEnd());
        lib.add(new SSymbol("test"), new Tests.Test());

        lib.add(new SSymbol("*report-test"), new Tests.ReportTest());

        UIKA806_TEST = lib;
    }

}

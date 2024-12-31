/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

//import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.pico.fn.SimportFn;
import uika806.objects.SSymbol;
import uika806.lib.Library;
import uika806.port.CurrentPort;

import uika806.syntax.Environ;
import uika806.syntax.SchemeEnvironment;

/**
 *
 * @author hemmi
 */
public class EnvironFactory implements IEnvironFacory {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(EnvironFactory.class);

    SchemeEnvironment lexEnv;

    public EnvironFactory() {

    }

    public void loadBase() {
        loadBase("startup.scm");
    }

    public void loadBase(String fileName) {

        // ライブラリ baseを作成する
        Library schemeBase = InitLibs.initBase();

        // ▼▼▼▼   "startup.scm"を、読み込むための、環境を作成する
        Environ startEnv = schemeBase.getStartupEnvment();
        startEnv.define(SSymbol.SIMPORT, new SimportFn());

        final Object SCHEME_BASE = RT.list(RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_BASE));

        // ライブラリ base の内容を、sEnvにコピーする。
        new SimportFn().invokeWithEnv(SCHEME_BASE, startEnv);

        try {
            new Loader().load(fileName, startEnv);
        } catch (Exception ex) {
            throw new LispException("can't load " + fileName, ex);
        }
        // ▲▲▲▲   "startup.scm"を、読み込み・・・終わり

        InitLibs.initChar();

        InitLibs.initComplex();

        InitLibs.initCxr();
        InitLibs.initInexact();

        InitLibs.initFile();

        InitLibs.initProcessContext();
        InitLibs.initTime();

        InitLibs.initRead();

        InitLibs.initWrite();

        InitLibs.initLoad();

        InitLibs.initEval();

        InitLibs.initUika806Test();

        // 最初の環境を作成する
        lexEnv = new SchemeEnvironment(true);
        new SimportFn().invokeWithEnv(SCHEME_BASE, lexEnv);
    }
/*
    eprecated
    public void old_loadBase(String fileName) {

        lexEnv = new SchemeEnvironment(true);

        lexEnv.add(SSymbol.SIMPORT, new SimportFn());

        Library schemeBase = InitLibs.initBase();

        boolean bBase = true;
        if (bBase) {
            Object list = RT.list(RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_BASE));
            new SimportFn().invokeWithEnv(list, lexEnv);
        }

        try {

            new Loader().load(fileName, lexEnv);
        } catch (Exception ex) {
            throw new LispException("can't load " + fileName, ex);
            //  LOG.info("--------- Exception", ex);
        }

        InitLibs.initChar();

        InitLibs.initComplex();

        InitLibs.initCxr();
        InitLibs.initInexact();

        InitLibs.initFile();

        InitLibs.initProcessContext();
        InitLibs.initTime();

        InitLibs.initRead();

        InitLibs.initWrite();

        InitLibs.initLoad();

        InitLibs.initUika806Test();

    }
*/
    public Environ getFirstEnviron(boolean bProcess, boolean bWrite) {

        if (bProcess) {
            Object list = RT.list(RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_PROC));
            new SimportFn().invokeWithEnv(list, lexEnv);

        }
        if (bWrite) {
            Object list = RT.list(RT.list(SSymbol.LIB_SCHEME, SSymbol.LIB_WRITE));
            new SimportFn().invokeWithEnv(list, lexEnv);
        }

        return lexEnv;
    }

    @Override
    public Environ getEnviron(Cell list) {

        String str = CurrentPort.printString(list);
        LOG.info("153) {}", str);

        Environ scmEnv = new SchemeEnvironment(false);

        new SimportFn().invokeWithEnv(list, scmEnv);

        return scmEnv;
    }

}

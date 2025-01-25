/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.lib;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.syntax.SchemeEnvironment;

/**
 *
 * @author hemmi
 */
public class StartupEnviron implements Environ {

    private static final Logger LOG = LoggerFactory.getLogger(StartupEnviron.class);
    
    
    private Environ parent = null;

    ConcurrentHashMap<SSymbol, Object> bindings;

    public StartupEnviron(ConcurrentHashMap<SSymbol, Object> funcs) {
        this.bindings = funcs;
    }

    private StartupEnviron() {
        this.bindings = new ConcurrentHashMap<SSymbol, Object>();
    }

    @Override
    public Optional<Object> getOptional(SSymbol id) {

        Environ env = this;
        return env != null ? Optional.ofNullable(env.getSelfOptional(id)) : Optional.empty();

    }

    @Override
    public Object getSelfOptional(SSymbol id) {
        return bindings.get(id);
    }

    @Override
    public Environ getParent() {
        return parent;
    }
    
    
    @Override
    public Object get(SSymbol id) {

        return this.bindings.get(id);
    }

    @Override
    public boolean containsKey(SSymbol id) {
        return this.bindings.containsKey(id);
    }

    @Override
    public boolean isREPL() {
        return false; //2024-11-10
    }

    @Override
    public void set(SSymbol id, Object obj) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void add(SSymbol id, Object obj) {
        this.bindings.put(id, obj);
    }

    @Override
    public void define(SSymbol sym, Object val) {
        this.bindings.put(sym, val);
    }

    @Override
    public Environ extend(Object vars, Object vals) {
        String sVars = CurrentPort.printString(vars);
        String sVals = CurrentPort.printString(vals);
        LOG.debug("=======================********** extend {}, {}", sVars, sVals);

        SchemeEnvironment newEnv = new SchemeEnvironment(this);

        Object pointer = vars;
        Object expr2 = vals;
        while (pointer instanceof Cell) {

            SSymbol var0 = (SSymbol) ((Cell) pointer).getCar();
            //  LOG.info("53) env.add({}, {})", var0, "?");

            newEnv.add(var0, ((Cell) expr2).getCar());
            pointer = ((Cell) pointer).getCdr();
            expr2 = ((Cell) expr2).getCdr();
        }
        if (pointer instanceof SSymbol) {

//                LOG.info("61) env.add({}, {})", pointer, expr2);
            newEnv.add((SSymbol) pointer, expr2);
        }

        return newEnv;
    }

    @Override
    public Object lookupValue(SSymbol sym) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Map<SSymbol, Object> getUnmodifiableMap() {

        return Collections.unmodifiableMap(bindings);
    }

    @Override
    public String printEnv() {

        if (parent == null) {
            return " global";

        } else {
            return "SchemeEnvironment = " + bindings.toString();
        }
    }

    @Override
    public Environ clearBinds() {

        if (this.parent == null) {
            return new StartupEnviron();
        }

        return new StartupEnviron();

    }

}

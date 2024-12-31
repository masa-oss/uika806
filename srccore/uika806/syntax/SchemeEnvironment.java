/*
 * Copyright (C) 2016 Chan Chung Kwong <1m02math@126.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
// package com.github.chungkwong.jschememin;
package uika806.syntax;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;

/**
 * Standard environment
 * Author: Chan Chung Kwong <1m02math@126.com>
 * 
 * Modified: Masahito Hemmi
 * 
 */
public class SchemeEnvironment extends Environment implements Environ {


    private static final Logger LOG = LoggerFactory.getLogger(SchemeEnvironment.class);
    
	private final Environ parent;

        
        private final HashMap<SSymbol,Object> bindings=new HashMap<>();
	/**
	 * Create a top-level environment
	 * @param repl REPL mode or not
	 */
	public SchemeEnvironment(boolean repl){
		super(repl);
		this.parent=null;
                /*
		bindings.put(DefineLibrary.INSTANCE.getKeyword(),DefineLibrary.INSTANCE);
		bindings.put(Import.INSTANCE.getKeyword(),Import.INSTANCE);
		if(repl)
			Base.INSTANCE.getLibrary().exportTo(this);
                */
	}
        /**
	 * Create a envionment
	 * @param parent parent environment
	 */
	public SchemeEnvironment(Environ parent){
            
		super(parent.isREPL());
		this.parent=parent;
	}
        
        
    @Override
    public Environ clearBinds() {
        
        if (this.parent == null) {
            return new SchemeEnvironment(false);
        }
        
        return new SchemeEnvironment(this.parent);
    }
        
	@Override
	public Optional<Object> getOptional(SSymbol id){
		Environ env=getFirstEnvironmentContains(this,id);
		return env!=null?Optional.ofNullable(env.getSelfOptional(id)):Optional.empty();
	}
	@Override
	public Object getSelfOptional(SSymbol id){
		return bindings.get(id);
	}
	@Override
	public void set(SSymbol id,Object obj){
		Environ env= getFirstEnvironmentContains(this,id);
		if(env!=null){
			env.add(id,obj);
		}else if(isREPL())
			add(id,obj);
	}
	private static Environ getFirstEnvironmentContains(Environ env,SSymbol id){
		while(env instanceof SchemeEnvironment){
			if(((SchemeEnvironment)env).bindings.containsKey(id))
				break;
			//env=((SchemeEnvironment)env).parent;
			env=env.getParent();
		}
		return env;
	}
	@Override
	public void add(SSymbol id,Object obj){
		bindings.put(id,obj);
	}
	@Override
	public void remove(SSymbol id){
		bindings.remove(id);
	}
        
        //--------------------------------------------------------
        // add Hemmi
        
    @Override
    public Environ getParent() {
            return parent;
    }
        
    @Override
    public Map<SSymbol,Object> getUnmodifiableMap() {
            
            return Collections.unmodifiableMap(bindings);
    }
        
    @Override
    public boolean containsKey(SSymbol id) {
        return this.bindings.containsKey(id);
    }

    @Override
    public void define(SSymbol sym, Object val) {
        this.bindings.put(sym, val);
    }

    @Override
    public Environ extend(Object vars, Object vals) {

        if (LOG.isDebugEnabled()) {
            String sVars = CurrentPort.printString(vars);
            String sVals = CurrentPort.printString(vals);
            LOG.debug("=======================********** extend {}, {}", sVars, sVals);
        }
        
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
    public String printEnv() {

        int hashCode = super.hashCode();
        String hex = String.format("%09x", hashCode);
        
        if (parent == null) {
            return " global@" + hex;
            
        } else {
            return "SchemeEnvironment = " + bindings.toString() + "@" + hex  ;
        }

    }

}

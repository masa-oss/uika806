/*
 * Copyright (C) 2017 Chan Chung Kwong <1m02math@126.com>
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
 *
 *
 *
 * Masahito Hemmi modified this source file.
 */
// package com.github.chungkwong.jschememin;
package uika806.syntax;

import java.util.Optional;
import uika806.objects.SSymbol;
/**
 * Environment where variable is stored
 * @author Chan Chung Kwong <1m02math@126.com>
 */
public abstract class Environment extends Object{
	/**
	 * The value of unbound variable if in REPL mode
	 */
	public static final SSymbol UNBOUNDED=new SSymbol("unbound");
	private final boolean repl;
	/**
	 * Create a environment
	 * @param repl REPL mode or not
	 */
	public Environment(boolean repl){
		this.repl=repl;
	}
	/**
	 * Get the value of a variable
	 * @param id name
	 * @return value in Optional
	 */
	public abstract Optional<Object> getOptional(SSymbol id);
	/**
	 * Set a variable
	 * @param id name
	 * @param obj value
	 */
	public abstract void set(SSymbol id,Object obj);
	/**
	 * Add a variable
	 * @param id name
	 * @param obj value
	 */
	public abstract void add(SSymbol id,Object obj);
	/**
	 * Get the value of a variable in this environment, not its parent
	 * @param id
	 * @return value in Optional
	 */
	public abstract Object getSelfOptional(SSymbol id);
	/**
	 * Delete a variable
	 * @param id name
	 */
	public abstract void remove(SSymbol id);
	/**
	 * Get the value of a variable
	 * @param id the name of the variable
	 * @return
	 */
	public Object get(SSymbol id){
		Optional<Object> obj=getOptional(id);
		if(obj.isPresent())
			return obj.get();
		else
		//	return repl?UNBOUNDED:null;
			return null;
	}
	/**
	 * Check if a variable exists
	 * @param id the name of the variable
	 * @return
	 */
	public boolean containsKey(SSymbol id){
		return getOptional(id).isPresent();
	}
	/*
	 * Add a keyword
	 * @param keyword
	public void addPrimitiveType(BasicConstruct keyword){
		add(keyword.getKeyword(),keyword);
	}
	 */

        
        /**
	 * Check if in REPL mode
	 * @return
	 */
	public boolean isREPL(){
		return repl;
	}
        
	public String toExternalRepresentation(){
		return "'environment";
	}

        public boolean isSelfevaluating(){
		return false;
	}
}

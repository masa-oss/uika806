package uika806.objects;

/**
 * Created on 2008/07/19(SAT), 4:56
 * 
 * Rename CommonLispSymbol to EmptyList at 2024-08
 * 
 * Move uika806.kernel to uika806.small.objects
 *
 * @author masa
 */
public final class EmptyList  {
    
    public static final EmptyList NIL = new EmptyList("nil", "lisp");

 //   public static final EmptyList PKG = new EmptyList("*package*", "lisp");

    protected final String pkg;
    protected final String name;
    Object symbolValue;
    /** 
     * Creates a new instance
     * @param name
     * @param pkg
     */
    private EmptyList(String name, String pkg) {
     
        this.name = name;
        this.pkg = pkg;
/*     
        if ("lisp".equals(pkg)) {
            if ("nil".equals(name)) {
            }
        }
*/
    }
    
    // カレントパッケージを求めるのに使っている
    public Object getSymbolValue() {
        return symbolValue;
    }
    
    
    public final void setSymbolValue(Object symbolValue) {
        this.symbolValue = symbolValue;
    }
    
    public String getName() {
        return this.name;
    }
    
    
    public EmptyList(String name, String pkg, String className) {

        this.name = name;
        this.pkg = pkg;
    }

    @Override
    public String toString() {
        
        // 2024-08
        if ("lisp".equals(pkg) && "nil".equals(name) ) {
            return "()";
        }
        
        
        return "EmptyList[" + pkg + ", " +name + "]";
    }
}

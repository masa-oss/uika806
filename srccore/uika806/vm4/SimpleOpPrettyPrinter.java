/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.port.OutputPort;
import uika806.port.CurrentPort;

public class SimpleOpPrettyPrinter {
    
    private static final Logger LOG = LoggerFactory.getLogger(SimpleOpPrettyPrinter.class);

    public void printIndentTo(Op o, OutputPort outport) {
        printIndentList(o, outport, "");
    }

    void printIndentList(Op op, OutputPort outport, String space) {

        if (op == null) {
            return;
        }
        
        outport.write(space);

        String name = op.decodeOpCode();
        outport.write("(" + name );

        space = "  " + space;

        switch(op.code) {
            case 1:
            case 11:
            case 12:
               // 引数なし
                outport.write(")");
                break;
            case 7:
            case 10:
            case 14: // push
            case 15: // EXCEP_HN
            case 19: // VALS_LIST
                // nextのみ
                outport.write( "\n");
                printIndentList(op.nextOp, outport, space  );
                outport.write(")");
                break;
                
            case 2: // refer
            case 6: // assign
            case 13: // def
                outport.write(" ");
                outport.write( symbolToString(op.sym)  );
                
                outport.write( "\n");

                printIndentList(op.nextOp, outport, space  );
                outport.write(")");
                break;
                
            case 3: // constant
                outport.write( "\n");
                outport.write( space);
                
                outport.write( CurrentPort.printString( op.obj) ); // Value

                outport.write( "\n");

                printIndentList(op.nextOp, outport, space  );
                outport.write(")");
                break;

            case 4: // close
                outport.write( "\n");
                outport.write(space);

                outport.write( CurrentPort.printString( op.obj) );  // Object

                outport.write( "\n");

                printIndentList(op.operation1, outport, space  );
                outport.write( "\n");
                printIndentList(op.nextOp, outport, space  );
                break;
                
            case 5: // test
                outport.write( "\n");

                printIndentList(op.operation1, outport, space  );
                outport.write( "\n");
                printIndentList(op.nextOp, outport, space  );
                break;

            case 9: // frame
                outport.write( "\n");
                printIndentList(op.nextOp, outport, space  );

                outport.write( "\n");
                printIndentList(op.operation1, outport, space  );
                break;
                
            default:
                outport.write("  未実装・・・ )");
        }
    }    

    String symbolToString(SSymbol sym) {
        
        SSymbol a = sym;
        if (a instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol uni = (ScmUniqueSymbol) a;
          //  return uni.getOrigin().getName() + "#" + uni.getSeq();
            
          //  return "ScmUniqueSymbol " + uni.getOrigin().getName();
          
          // 2025-01-XX
          return uni.getReadableName();
        }
        
        return a.getName();
    }


}

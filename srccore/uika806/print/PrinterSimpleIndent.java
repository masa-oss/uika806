/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.print;

import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.port.OutputPort;
import uika806.port.CodepointOutputPortNop;
import uika806.err.FileException;

/**
 *
 * @author hemmi
 */
public class PrinterSimpleIndent extends PrinterSchemeEx {

    private static final Logger LOG = LoggerFactory.getLogger(PrinterSimpleIndent.class);

    public PrinterSimpleIndent() {

    }

    public void printIndentTo(Object o, OutputPort outport) {

        // phase-1
        CodepointOutputPortNop nopport = new CodepointOutputPortNop();
        super.prin1(o, nopport);

        // phase-2
        setSecondPhase();
        printInternal(o, outport, "");

    }

    void printInternal(Object o, OutputPort outport, String space) {

        if (o == null) {
            return;
        }

        if (o instanceof Cell) {

          //  outport.writeString( space);
            outport.write( space);
            secondPhaseList((Cell) o, outport, space);

        } else {
          //  outport.writeString( space);
            outport.write( space);
            super.printOtherThanCells(o, outport);
        }
    }

    private void write(OutputPort out, int codePoint) {
        try {
            out.write(codePoint);

        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }

    }

    void secondPhaseList(Cell cell, OutputPort outport, String space) {

        ConsInfo pair0 = findCell(cell);
        if (pair0 != null && pair0.refCount > 1) {
            if (pair0.first) {
                String work = "#" + pair0.id + "=";
                pair0.first = false;
             //   outport.writeString( work);
                outport.write( work);
            } else {
                String work = "#" + pair0.id + "#";
             //   outport.writeString( work);
                outport.write( work);
            }
        }

        write(outport, '(');

        boolean isSimple = isVerySimpleList(cell);
        if (isSimple) {
            printSimpleList(cell, outport, space);
        } else {
            printIndentList(cell, outport, space);
        }
    }

    void printIndentList(Cell cell, OutputPort outport, String space) {

        try {
            Object _car0 = cell.getCar();

            printInternal(_car0, outport, "");   // *** car を再帰呼び出し ****

            Object cdr0 = cell.getCdr();
            if (!(cdr0 instanceof Cell)) {

                write(outport, ')');
                return;
            }

            cell = (Cell) cdr0;
            space = "  " + space;

            for (;;) {
                write(outport, '\n');
              //  outport.writeString( space);
                outport.write( space);

                Object _car = cell.getCar();

                printInternal(_car, outport, space);   // *** car を再帰呼び出し ****

                Object next = cell.getCdr();
                if (next instanceof Cell) {

                    Cell cdr = (Cell) next;
                    ConsInfo pair = findCell(cdr);
                    if (pair != null && pair.refCount > 1) {
                        write(outport, ' ');
                        if (pair.first) {
                            pair.first = false;
                            write(outport, '.');
                            write(outport, ' ');

                            printInternal(cdr, outport, space);   // *** cdr を再帰呼び出し ****

                        } else {
                            outport.write('.');
                            outport.write(' ');
                            LOG.info("found={}", pair.id);
                            String work = "#" + pair.id + "#";
                         //   outport.writeString( work);
                            outport.write( work);
                        }
                        break;
                    } else {
                        outport.write(' ');
                        cell = cdr;
                    }
                } else if (next instanceof EmptyList) {
                    break;
                } else {
                    outport.write(' ');
                    outport.write('.');
                    outport.write(' ');
                    printInternal(next, outport, space);   // *** cdr を再帰呼び出し ****
                    break;
                }
            }
            outport.write(')');
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }

    }

    void printSimpleList(Cell cell, OutputPort outport, String space) {

        try {
            for (;;) {

                Object _car = cell.getCar();

                printInternal(_car, outport, "");   // *** car を再帰呼び出し ****

                Object next = cell.getCdr();
                if (next instanceof Cell) {

                    Cell cdr = (Cell) next;
                    ConsInfo pair = findCell(cdr);
                    if (pair != null && pair.refCount > 1) {
                        outport.write(' ');
                        if (pair.first) {
                            pair.first = false;
                            outport.write('.');
                            outport.write(' ');

                            printInternal(cdr, outport, space);   // *** cdr を再帰呼び出し ****

                        } else {

                            outport.write('.');
                            outport.write(' ');
                            LOG.info("found={}", pair.id);
                            String work = "#" + pair.id + "#";
                         //   outport.writeString( work);
                            outport.write( work);
                        }
                        break;
                    } else {
                        outport.write(' ');
                        cell = cdr;
                    }
                } else if (next instanceof EmptyList) {
                    break;
                } else {
                    outport.write(' ');
                    outport.write('.');
                    outport.write(' ');
                    printInternal(next, outport, space);   // *** cdr を再帰呼び出し ****
                    break;
                }
            }
            outport.write(')');
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
    }

    boolean isVerySimpleList(Cell list) {

        if (list == null) {
            throw new NullPointerException();
        }

        Object car = list.getCar();
        if (car instanceof Cell) {
            return false;
        }

        Object cdr = list.getCdr();
        if (cdr instanceof Cell) {
            return isVerySimpleList((Cell) cdr);
        } else if (cdr instanceof EmptyList) {
            return true;
        } else {
            return false;
        }

    }

}

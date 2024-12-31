package uika806.print;

import java.io.IOException;
import uika806.err.FileException;
import uika806.objects.SString;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public class PrintUtil {


    public static void printStringReadably(SString str, OutputPort outport) {

        outport.write("\"");

        int len = str.length();

        for (int i = 0; i < len; i++) {
            
            int c = str.getNth(i);

            switch (c) {
                case '\n':
                    outport.write("\\n");
                    break;
                case '\t':
                    outport.write("\\t");
                    break;
                case '\r':
                    outport.write("\\r");
                    break;
                case '"':
                    outport.write("\\\"");
                    break;
                case '\\':
                    outport.write("\\\\");
                    break;
                case '\f':
                    outport.write("\\f");
                    break;
                case '\b':
                    outport.write("\\b");
                    break;
                default:
                try {
                    outport.write(c);
                } catch (IOException ioe) {
                    throw new FileException("IOException", ioe);
                }
            }
        }

        outport.write("\"");
    }

    // disable construct
    private PrintUtil() {
        
    }
}

/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.kernel;

import uika806.port.OutputPort;

/**
 * Created on 2008/07/18(FRI), 21:22
 *
 * @author masa
 */
public interface SelfPrintable {

    public void prin1(OutputPort outport, PrintOption opt);
}

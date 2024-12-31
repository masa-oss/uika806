/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

import uika806.objects.Cell;
import uika806.syntax.Environ;

/**
 *
 * @author hemmi
 */
public interface IEnvironFacory {

    Environ getEnviron(Cell list);
    
}

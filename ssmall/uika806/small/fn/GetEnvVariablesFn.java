package uika806.small.fn;

import java.util.Map;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class GetEnvVariablesFn extends AFn {

    @Override
    public String getName() {
        return "get-env-variables";
    }

    @Override
    public Object invoke() {

        Object result = RT.EOL;

        Map<String, String> env = System.getenv();

        for (Map.Entry<String, String> ent : env.entrySet()) {

            SString k = SString.fromString(ent.getKey());

            SString v = SString.fromString(ent.getValue());

            result = new Cell(new Cell(k, v), result);
        }
        return result;
    }
}

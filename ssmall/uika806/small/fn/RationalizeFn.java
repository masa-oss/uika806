package uika806.small.fn;

import java.math.BigInteger;
import uika806.kernel.AFn;
import uika806.objects.Ratio;

/**
 *
 * https://qiita.com/Pseudonym/items/93d4e91828512a5ea6e3
 *
 * 浮動小数点数を分数に変換するアルゴリズムについて
 *
 * @author hemmi
 */
public class RationalizeFn extends AFn {

    @Override
    public String getName() {
        return "rationalize";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {

        double d1 = ((Number) arg1).doubleValue();
        double d2 = ((Number) arg2).doubleValue();
        long[]  arr = float2frac(d1, d2);

        
        BigInteger  nn = new BigInteger(  String.valueOf(arr[0]),10   );
        BigInteger  dd = new BigInteger(  String.valueOf(arr[1]),10   );
        
        Ratio ra = new Ratio(nn, dd);
        Ratio x = ra.getIrreducible();
        
        return x;
    }

    // https://qiita.com/Pseudonym/items/93d4e91828512a5ea6e3
    long[] float2frac(double f, double delta) {

        double ans = f;
        long n = 0;
        long m = 1;

        for (;;) {

            double d = ((double) n) / ((double) m);
            if (Math.abs(d - ans) < delta) {
                break;
            }

            /*  https://www.orchid.co.jp/computer/cschool/CREF/modf.html
          
          
          double modf(double x, double *ip);
          
          整数部は*ipに格納され、小数部は返り値として返されます。
          
             */
            double syousuu = f % 1.0;
            double i = f - syousuu;
            f = syousuu;

            n += i;
            n *= 10;
            m *= 10;
            f *= 10;

        }

        // frac_reduction
        long[] arr = new long[]{n, m};
        return arr;
    }

    public static void main(String[] argv) {

        double delta = 0.00001;
        double f = 0.0234;

        RationalizeFn logic = new RationalizeFn();

        long[] arr = logic.float2frac(f, delta);
        System.out.println("74) " + arr[0] + " / " + arr[1]);
    }

}

/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import uika806.err.FileException;
import java.io.IOException;
import uika806.kernel.PrintOption;
import uika806.kernel.SelfPrintable;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public final class Complex extends Number implements SelfPrintable {

    private final double re;
    private final double im;

    public Complex(double re, double im) {
        this.re = re;
        this.im = im;
    }

    public double re() {
        return re;
    }

    public double im() {
        return im;
    }

    public Complex add(Complex c) {
        return new Complex(re + c.re, im + c.im());
    }

    public Complex sub(Complex c) {
        return new Complex(re - c.re, im - c.im);
    }

    public Complex mul(Complex c) {
        return new Complex(
                re * c.re - im * c.im, re * c.im + im * c.re);
    }

    public Complex div(Complex c) {
        double denominator = c.re * c.re + c.im * c.im;
        return new Complex(
                (re * c.re + im * c.im) / denominator,
                (im * c.re - re * c.im) / denominator);
    }

    public Complex Con() {
        return new Complex(re, -im);
    }

    public double abs() {
        return Math.sqrt(re * re + im * im);
    }

    public double arg() {
        return Math.atan(im / re);
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append(schemeString(re));

        String ims = schemeString(im);
        if (ims.startsWith("+") || ims.startsWith("-")) {
            sb.append(ims);
        } else {
            sb.append("+");
            sb.append(ims);
        }

        return "Complex[" + sb.toString() + "i]";
    }

    @Override
    public int intValue() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public long longValue() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public float floatValue() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public double doubleValue() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void prin1(OutputPort outport, PrintOption opt) {

        String sre = schemeString(re);

        String sim = schemeString(im);

        if (sim.startsWith("+") || sim.startsWith("-")) {
            sim = sre + sim;
        } else {
            sim = sre + "+" + sim;
        }

        sim.codePoints().forEach((i) -> {
            try {
                outport.write(i);
            } catch (IOException ioe) {
                throw new FileException("IOException", ioe);
            }
        });
        try {
            outport.write('i');
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
    }

    private String schemeString(double d) {

        if (Double.isNaN(d)) {
            return "+nan.0";
        }
        if (Double.isInfinite(d)) {
            if (d > 0.0) {
                return "+inf.0";
            } else {
                return "-inf.0";
            }
        }

        return String.valueOf(d);
    }

}

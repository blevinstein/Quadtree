import org.apache.commons.math3.complex.Complex;

Complex E = new Complex(Math.E);
Complex I = Complex.I;

// return real part of the DFT of signal x
float[] rdft(float[] x) {
  int N = x.length;
  int N2 = ceil(x.length/2f); // input x is real, so output X is symmetrical; ignore second half
  Complex c[] = new Complex[N2];
  for (int i=0; i<N2; i++) {
    c[i] = new Complex(0);
    for (int j=0; j<x.length; j++) {
      Complex argument = I.multiply(-2*PI*i*j/N);
      Complex contrib = new Complex(x[j]).multiply(E.pow(argument));
      c[i] = c[i].add(contrib);
    }
  }
  // real parts => float array
  float X[] = new float[N2];
  for (int i=0; i<N2; i++)
    X[i] = (float)c[i].getReal();
  return X;
}

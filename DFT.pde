import org.apache.commons.math3.complex.Complex;

Complex E = new Complex(Math.E);
Complex I = Complex.I;

float[] dft(float[] x) {
  int N = x.length;
  Complex c[] = new Complex[N];
  for (int i=0; i<N; i++) {
    c[i] = new Complex(0);
    for (int j=0; j<N; j++) {
      Complex argument = I.multiply(-2*PI*i*j/N);
      Complex contrib = new Complex(x[j]).multiply(E.pow(argument));
      c[i] = c[i].add(contrib);
    }
  }
  // real parts => float array
  float X[] = new float[N];
  for (int i=0; i<N; i++)
    X[i] = (float)c[i].getReal();
  return X;
}

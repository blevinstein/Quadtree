/*
 * http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
 */

class CubicInterpolation {
  float p, q, r, s;
  CubicInterpolation(float v0, float v1, float v2, float v3) {
    p = (v3 - v2) - (v0 - v1);
    q = (v0 - v1) - p;
    r = v2 - v0;
    s = v1;
  }
  float get(float x) {
    return p*x*x*x + q*x*x + r*x + s;
  }
  String toString() {
    return "["+p+","+q+","+r+","+s+"]";
  }
}

// N = number of points
// A = amplitude of noise
// F = frequency
float[] perlinOne(int N, float A, int F, float left, float right) {
  assert(N % F == 0); // TODO: handle uneven frequencies
  int interval = N/F;
  // generate amplitudes
  float amps[] = new float[F+3];
  amps[0] = left;
  amps[F+2] = right;
  for(int i=1; i<F+2; i++)
    amps[i] = random(A);
  // generate noise
  float pNoise[] = new float[N];
  for(int i=0; i<F; i++) {
    CubicInterpolation interp = new CubicInterpolation(amps[i], amps[i+1], amps[i+2], amps[i+3]);
    for(int j=0; j<interval; j++)
      pNoise[i*interval + j] = interp.get(j*1f/interval);
    pNoise[i*interval] = 0;
  }
  return pNoise;
}

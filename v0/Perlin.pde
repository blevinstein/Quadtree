/*
 * For reference:
 * http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
 *
 * Now using builtins (noise, noiseSeed, noiseDetail) instead of writing my own.
 */

/*
float[] perlin(int N, int octaves, float falloff, long seed, float x1, float x2) {
  noiseSeed(seed);
  noiseDetail(octaves, falloff);
  float r[] = new float[N];
  for(int i=0; i<N; i++)
    r[i] = noise(lerp(x1, x2, i*1f/N));
  return r;
}
*/

float cosineInterp(float a, float b, float x) {
  float f = (1 - cos(PI * x)) / 2;
  return a*(1-f) + b*f;
}

// TODO: fix PerlinGenerator
class PerlinGenerator extends QuadGenerator {
  int rLevel;
  int octaves;
  float falloff;
  int frequency;
  float amplitude;
  HashMap boundaries;
  PerlinGenerator(int r, int o, float f, int fr, float a) {
    rLevel = r;
    octaves = o;
    falloff = f;
    frequency = fr;
    amplitude = a;
    boundaries = new HashMap();
  }
  float[] amps(int f, float amp) {
    float arr[] = new float[f];
    for(int i=0; i<f; i++)
      arr[i] = random(amp);
    return arr;
  }
  float eval(float[][] layers, float x) {
    float total = 0;
    for(int i=0; i<layers.length; i++) {
      int idx = (int)(x * (layers[i].length-1));
      float pos = x * (layers[i].length-1) % 1;
      total += cosineInterp(layers[i][idx], idx+1<layers[i].length ? layers[i][idx+1] : 0, pos);
    }
    return total;
  }
  Quad gen(int i, int j) {
    Quad q = new Quad(255);
    // generate noise functions
    float amps[][] = new float[octaves][];
    for(int c=0; c<octaves; c++)
      amps[c] = amps(frequency * (1<<c) + 1, amplitude / (1<<c));
    // find boundaries
    if(!boundaries.containsKey(s(i)))
      boundaries.put(s(i), random(amplitude));
    if(!boundaries.containsKey(s(i+1)))
      boundaries.put(s(i+1), random(amplitude));
    PVector b = new PVector((Float)boundaries.get(s(i)), (Float)boundaries.get(s(i+1)));
    // correct lowest frequency for boundary position
    // TODO: use multiple frequencies to guarantee all amplitudes between 0 and 1
    amps[0][0] += b.x - eval(amps, 0);
    amps[0][amps.length-1] += b.y - eval(amps, 1);
    int N = 1<<rLevel;
    for(int x=0; x<N; x++)
      for(int y=0; y<N; y++) {
        float u = x*1f/N;
        float v = y*1f/N;
        float h = eval(amps, u);
        if(v+j < h)
          q.set(new PVector(u, v), rLevel, 0);
      }
    return q;
  }
}

class NoiseGenerator extends QuadGenerator {
  int rLevel;
  int octaves;
  float falloff;
  long seed;
  float scalar;
  NoiseGenerator(int r, int o, float f, long s, float sc) {
    rLevel = r;
    octaves = o;
    falloff = f;
    seed = s;
    scalar = sc;
  }
  Quad gen(int i, int j) {
    Quad q = new Quad(255);
    noiseSeed(seed);
    noiseDetail(octaves, falloff);
    int N = 1<<rLevel;
    for(int x=0; x<N; x++)
      for(int y=0; y<N; y++) {
        // seed added because noise(x)==noise(-x)
        float u = (seed + i + x*1f/N)*BLOCK_SIZE;
        float v = (j + y*1f/N)*BLOCK_SIZE;
        float h = noise(u * scalar)*BLOCK_SIZE;
        if(v < h)
          q.set(new PVector(x*1f/N, y*1f/N), rLevel, 0);
      }
    return q;
  }
}

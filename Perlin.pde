/*
 * For reference:
 * http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
 *
 * Now using builtins (noise, noiseSeed, noiseDetail) instead of writing my own.
 */

float[] perlin(int N, int octaves, float falloff, long seed, float x1, float x2) {
  noiseSeed(seed);
  noiseDetail(octaves, falloff);
  float r[] = new float[N];
  for(int i=0; i<N; i++)
    r[i] = noise(lerp(x1, x2, i*1f/N));
  return r;
}

class PerlinGenerator extends QuadGenerator {
  int rLevel;
  int octaves;
  float falloff;
  long seed;
  float scalar;
  PerlinGenerator(int r, int o, float f, long s, float sc) {
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

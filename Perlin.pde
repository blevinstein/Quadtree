/*
 * For reference:
 * http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
 *
 * Now using builtins (noiseSeed, noiseDetail, noise) instead of writing my own.
 */

float[] perlin(int N, int octaves, float falloff, float x1, float x2) {
  float r[] = new float[N];
  noiseDetail(octaves, falloff);
  for(int i=0; i<N; i++)
    r[i] = noise(lerp(x1, x2, i*1f/N));
  return r;
}

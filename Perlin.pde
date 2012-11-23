/*
 * For reference:
 * http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
 *
 * Now using builtins (noiseSeed, noiseDetail, noise) instead of writing my own.
 */

float[] perlin(int N, int octaves, float falloff, float interval) {
  noiseSeed(millis());
  float r[] = new float[N];
  noiseDetail(octaves, falloff);
  for(int i=0; i<N; i++)
    r[i] = noise(i*interval);
  return r;
}

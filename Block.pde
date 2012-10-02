final float BLOCK_SIZE = 1000;

class Block {
  Quad q;
  PVector min, max;
  // TODO: add vertical component y
  Block(Quad quad, int x) {
    q = quad;
    min = new PVector(x * BLOCK_SIZE, 0);
    max = PVector.add(min, new PVector(BLOCK_SIZE, BLOCK_SIZE));
  }
}

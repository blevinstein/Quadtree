float BLOCK_SIZE = 1000;

class Grid {
  
  PVector min, max;
  HashMap<String, Quad> m;
  QuadGenerator generator;
  int seed;

  Grid() {
    min = new PVector(0, 0);
    max = new PVector(0, 0);
    m = new HashMap();
    //generator = new ConstantGenerator(255);
    generator = new NoiseGenerator(5, 4, 0.5, millis(), .001);
    //generator = new PerlinGenerator(5, 4, 0.5, 4, 0.5);
  }
  
  Quad get(int i, int j) {
    return (Quad)m.get(s(i,j));
  }
  
  boolean has(int i, int j) {
    return m.containsKey(s(i,j));
  }
  
  Quad get(PVector p) {
    int i = floor(p.x/BLOCK_SIZE);
    int j = floor(p.y/BLOCK_SIZE);
    Quad q = (Quad)m.get(s(i,j));
    if(q == null)
      return null;
    else
      return q.get(new PVector((p.x - BLOCK_SIZE*i)/BLOCK_SIZE, (p.y - BLOCK_SIZE*j)/BLOCK_SIZE));
  }
  
  void gen(int i, int j) {
    Quad q = generator.gen(i, j);
    load(i, j, q);
  }
  
  void load(int i, int j, Quad q) {
    assert(m != null);
    m.put(s(i,j), q);
    // TODO: increment min, max
  }
  
  void unload(int i, int j) {
    m.remove(s(i,j));
    // TODO: recalc min, max
  }
  
  boolean set(PVector p, int res, int val) {
    int i = (int)floor(p.x/BLOCK_SIZE);
    int j = (int)floor(p.y/BLOCK_SIZE);
    Quad q = get(i, j);
    if(q == null)
      return false;
    PVector relP = PVector.sub(p, new PVector(i * BLOCK_SIZE, j * BLOCK_SIZE));
    PVector propP = pdiv(relP, new PVector(BLOCK_SIZE, BLOCK_SIZE));
    q.set(propP, res, val);
    return true;
  }
  
  void iter(IterCallback cb) {
    for(Map.Entry e : m.entrySet()) {
      String str = (String)e.getKey();
      int i[] = rs(str);
      Quad q = (Quad)e.getValue();
      q.iter(new PVector(i[0] * BLOCK_SIZE, i[1] * BLOCK_SIZE),
             new PVector((i[0]+1) * BLOCK_SIZE, (i[1]+1) * BLOCK_SIZE), cb);
    }
  }
  
  ArrayList arccast(PVector source, PVector arc, IterCallback cb) {
    // TODO: implement
    return null;
  }
  
  void raycast(PVector source, PVector dir) {
    // TODO: test this
    PVector g = source;
    int i = (int)(g.x / BLOCK_SIZE);
    int j = (int)(g.y / BLOCK_SIZE);
    while(true) {
      PVector n = new PVector((dir.x > 0 ? floor(g.x/BLOCK_SIZE) + 1 : ceil(g.x/BLOCK_SIZE) - 1) * BLOCK_SIZE,
                              (dir.y > 0 ? floor(g.y/BLOCK_SIZE) + 1 : ceil(g.y/BLOCK_SIZE) - 1) * BLOCK_SIZE);
      PVector t = pdiv(PVector.sub(n, g), dir);
      if(t.x < t.y) {
        i++;
        g = PVector.add(g, PVector.mult(dir, t.x));
      } else {
        j++;
        g = PVector.add(g, PVector.mult(dir, t.y));
      }
    }
  }

}

abstract class QuadGenerator {
  abstract Quad gen(int i, int j);
}

class ConstantGenerator extends QuadGenerator {
  int value;
  ConstantGenerator(int v) {
    value = v;
  }
  Quad gen(int i, int j) {
    return new Quad(value);
  }
}

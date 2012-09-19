class Quad {

  Quad parent;
  Quad child[][];
  int material_id;
  
  Quad(int mid) {
    material_id = mid;
    child = null;
    parent = null;
  }
  
  Quad(Quad q0, Quad q1, Quad q2, Quad q3) {
    material_id = -1;
    setChildren(q0, q1, q2, q3);
    parent = null;
  }
  
  void setChildren(Quad q0, Quad q1, Quad q2, Quad q3) {
    child = new Quad[2][];
    child[0] = new Quad[2];
    child[1] = new Quad[2];
    child[0][0] = q0; child[0][0].parent = this;
    child[0][1] = q1; child[0][1].parent = this;
    child[1][0] = q2; child[1][0].parent = this;
    child[1][1] = q3; child[1][1].parent = this;
  }
  
  Quad get(PVector p) {
    if(material_id >= 0)
      return this;
    return child[floor(p.x*2)][floor(p.y*2)]
      .get(new PVector(p.x*2 % 1, p.y*2 % 1));
  }
  
  void set(PVector p, int r, int mid) {
    if(r <= 0) {
      material_id = mid;
      child = null;
    } else {
      if(material_id >= 0) { // leaf node
        setChildren(new Quad(material_id),new Quad(material_id),new Quad(material_id),new Quad(material_id));
        material_id = -1;
      }
      child[floor(p.x*2)][floor(p.y*2)]
        .set(new PVector(p.x*2 % 1, p.y*2 % 1), r-1, mid);
      // if all children have the same positive material_id, join together
      int child_mid = child[0][0].material_id;
      for(int i=0; i<2; i++)
        for(int j=0; j<2; j++)
          if(child[i][j].material_id != child_mid)
            child_mid = -1;
      if(child_mid >= 0) {
        child = null;
        material_id = child_mid;
      }
    }
  }
  
  void iter(PVector min, PVector max, IterCallback cb) {
    if(material_id >= 0) {
      cb.call(min, max, material_id);
    } else {
      PVector half = lerp(min, max, 0.5);
      child[0][0].iter(min, half, cb);
      child[0][1].iter(new PVector(min.x, half.y), new PVector(half.x, max.y), cb);
      child[1][0].iter(new PVector(half.x, min.y), new PVector(max.x, half.y), cb);
      child[1][1].iter(half, max, cb);
    }
  }
  
  void raycast(PVector min, PVector max, PVector a, PVector b, IterCallback cb) {
    if(contains(min, max, a) ||
       contains(min, max, b) ||
       intersectLineSeg(min, max, a, b) ||
       intersectLineSeg(new PVector(min.x, max.y), new PVector(max.x, min.y), a, b)) {
      if(material_id >= 0) {
        cb.call(min, max, material_id);
      } else {
        PVector half = lerp(min, max, 0.5);
        child[0][0].raycast(min, half, a, b, cb);
        child[0][1].raycast(new PVector(min.x, half.y), new PVector(half.x, max.y), a, b, cb);
        child[1][0].raycast(new PVector(half.x, min.y), new PVector(max.x, half.y), a, b, cb);
        child[1][1].raycast(half, max, a, b, cb);
      }
    }
  }
  
  ArrayList lightcast(PVector min, PVector max, PVector source, PVector a, PVector b, IterCallback cb) {
    if(material_id >= 0) {
      if(contains(min, max, source)) return new ArrayList();
    }
    PVector half = lerp(min, max, 0.5);
    // determine which quadrants to recurse on first
    int xs[2];
    int ys[2];
    if(source.x < half.x) {
      xs[0] = 0; xs[1] = 1;
    } else {
      xs[0] = 1; xs[1] = 0;
    }
    if(source.y < half.y) {
      ys[0] = 0; ys[1] = 1;
    } else {
      ys[0] = 1; ys[1] = 0;
    }
  }

}

abstract class IterCallback {
  abstract void call(PVector min, PVector max, Object ... data);
}

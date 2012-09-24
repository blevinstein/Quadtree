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
       intersectSegSeg(min, max, a, b) != null ||
       intersectSegSeg(new PVector(min.x, max.y), new PVector(max.x, min.y), a, b) != null) {
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
  
  boolean transparent(int mid) {
    // TODO: implement material registry, including transparency flag
    return mid >= 128;
  }
  
  /* Recursive Lightcasting
   * light is emitted from source along arc (angle arc.x to arc.y)
   * returns ArrayList of PVectors representing occluded arcs
   * invokes callbacks to denote blocks and edges hit by light
   */
  // TODO: add intensity based on initial intensity (arc.z?) and distance
  ArrayList lightcast(PVector min, PVector max, PVector source, PVector arc, IterCallback cb) {
    boolean trans = transparent(material_id);
    if(material_id >= 0) {
      ArrayList litSides = new ArrayList(); // PVector[2] sides
      /* TEMP
      if(contains(min, max, source) && !trans) { // source inside opaque block
        ArrayList occluded = new ArrayList(); // PVector arcs
        occluded.add(0, 2*PI);
        return occluded;
      }
      */
      PVector v[] = {new PVector(source.x + cos(arc.x), source.y + sin(arc.x)),
                     new PVector(source.x + cos(arc.y), source.y + sin(arc.y))};
      PVector corners[][] = {{min,                       new PVector(min.x, max.y)},
                             {new PVector(max.x, min.y), max                      }};
      ArrayList sides = new ArrayList();
      // if opaque, cull by choosing only light-facing sides to examine
      //if(trans || source.x < min.x)
        sides.add(new PVector[] { corners[0][0], corners[0][1] });
      //if(trans || source.y < min.y)
        sides.add(new PVector[] { corners[0][0], corners[1][0] });
      //if(trans || source.x > max.x )
        sides.add(new PVector[] { corners[1][0], corners[1][1] });
      //if(trans || source.y > max.y )
        sides.add(new PVector[] { corners[0][1], corners[1][1] });
      for(int i=0; i<sides.size(); i++) { // foreach side
        PVector side[] = (PVector[])sides.get(i);
        PVector c1 = side[0];
        PVector c2 = side[1];
        PVector ct = PVector.sub(c2, c1);
        boolean i1 = intersectRaySeg(source, c1, v[0], v[1]) != null; // true if corner c1 is inside arc
        boolean i2 = intersectRaySeg(source, c2, v[0], v[1]) != null; // true if corner c2 is inside arc
        if(i1 && i2) { litSides.add(new PVector[] {c1, c2}); } // corner-corner
        else if(i1 ^ i2) {
            PVector p1 = i1 ? c1 : c2, p2 = null;
            for(int j=0; j<2; j++) {
              PVector pt = intersectRaySeg(source, v[j], c1, c2);
              if(pt != null) p2 = pt;
            }
            if(p2 != null) { litSides.add(new PVector[] {p1, p2}); } // intersection-corner
        } else {
          PVector p1 = intersectRaySeg(source, v[0], c1, c2);
          PVector p2 = intersectRaySeg(source, v[1], c1, c2);
          // TODO: fix
          if(p1==null ^ p2==null) { println("shouldn't happen"); }
          else if(p1 != null) { litSides.add(new PVector[] {p1, p2}); } // intersection-intersection or none
        }
      }
      if(litSides.size() > 0)
        cb.call(min, max, material_id, litSides);
      ArrayList occluded = new ArrayList(); // PVector arcs
      /* TODO: fix
      for(int i=0; i<litSides.size(); i++) {
        PVector side[] = (PVector[])litSides.get(i);
        float s1 = atan(PVector.sub(side[0],source));
        float s2 = atan(PVector.sub(side[1],source));
        if(abs(s1-s2) > PI) {
          occluded.add(new PVector(-PI, min(s1, s2)));
          occluded.add(new PVector(max(s1, s2), PI));
        } else {
          occluded.add(new PVector(min(s1, s2), max(s1, s2)));
        }
      }
      // merge and return occluded arcs
      mergeArcs(occluded);
      */
      return occluded;
    } else { // recurse
      ArrayList occluded = new ArrayList();
      ArrayList arcs = new ArrayList();
      arcs.add(new PVector(arc.x, arc.y));
      // determine which quadrants to recurse on first
      PVector half = lerp(min, max, 0.5);
      int xs[] = new int[2];
      int ys[] = new int[2];
      if(source.x < half.x) { // determine x iteration
        xs[0] = 0; xs[1] = 1;
      } else {
        xs[0] = 1; xs[1] = 0;
      }
      if(source.y < half.y) { // determine y iteration 
        ys[0] = 0; ys[1] = 1;
      } else {
        ys[0] = 1; ys[1] = 0;
      }
      for(int i=0; i<2; i++) // foreach x
        for(int j=0; j<2; j++) { // foreach y
          int x = xs[i];
          int y = ys[j];
          for(int k=0; k<arcs.size(); k++) { // foreach arc
            PVector halfx = new PVector(half.x-min.x, 0);
            PVector halfy = new PVector(0, half.y-min.y);
            PVector offset = PVector.add(PVector.mult(halfx, i), PVector.mult(halfy, j));
            ArrayList childOccluded = child[x][y].lightcast(PVector.add(min, offset), PVector.add(half, offset), source, (PVector)arcs.get(k), cb);
            /* TODO: fix
            occludeArcs(arcs, childOccluded);
            occluded.addAll(childOccluded);
            */
          }
        }
    mergeArcs(occluded);
    return occluded;
    }
  }

}

void occludeArcs(ArrayList arcs, ArrayList occluded) {
  for(int i=0; i<occluded.size(); i++) {
    PVector o = (PVector)occluded.get(i);
    occludeArcs(arcs, o);
  }
}

void occludeArcs(ArrayList arcs, PVector o) {
  for(int i=0; i<arcs.size(); i++) {
    PVector a = (PVector)arcs.get(i);
    if(o.x < a.x && o.y > a.y) { // occlude allk
      arcs.remove(i);
      i--;
    } else if(o.x < a.y && o.y > a.y) { // occlude upper
      a.y = o.x;
      arcs.set(i, a);
    } else if(o.x < a.x && o.y > a.x) { // occlude lower
      a.x = o.y;
      arcs.set(i, a);
    } else if(o.x > a.x && o.y < a.y) { // occlude middle
      a.y = o.x;
      PVector n = new PVector(o.y, a.y);
      arcs.set(i, a);
      arcs.add(n);
    }
  }
}

void mergeArcs(ArrayList arcs) {
  for(int i=0; i<arcs.size(); i++) {
    for(int j=i+1; j<arcs.size(); j++) {
      PVector a1 = (PVector)arcs.get(i);
      PVector a2 = (PVector)arcs.get(j);
      if(a2.x < a1.y && a2.y > a1.x) { // overlap
        a1.x = min(a1.x, a2.x);
        a1.y = max(a1.y, a2.y);
        arcs.set(i, a1); // merge into first arc
        arcs.remove(j); // remove second arc
        j--;
      }
    }
  }
}

abstract class IterCallback {
  abstract void call(PVector min, PVector max, Object ... data);
}

import java.awt.event.*;

final float TOL = .0001;

boolean transparent(int mid) {
  // TODO: implement material registry, including transparency and other flags
  return mid >= 128;
}

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
    if(p.x < 0 || p.x >= 1 ||
       p.y < 0 || p.y >= 1)
       return;
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
      cb.call(min, max, this);
    } else {
      PVector half = lerp(min, max, 0.5);
      child[0][0].iter(min, half, cb);
      child[0][1].iter(new PVector(min.x, half.y), new PVector(half.x, max.y), cb);
      child[1][0].iter(new PVector(half.x, min.y), new PVector(max.x, half.y), cb);
      child[1][1].iter(half, max, cb);
    }
  }
  
  void raycast(PVector min, PVector max, PVector source, PVector direction, IterCallback cb) {
    if(contains(min, max, source) ||
       contains(min, max, direction) ||
       intersectRaySeg(source, direction, min, max) != null ||
       intersectRaySeg(source, direction, new PVector(min.x, max.y), new PVector(max.x, min.y)) != null) {
      if(material_id >= 0) {
        cb.call(min, max, this);
      } else {
        PVector half = lerp(min, max, 0.5);
        child[0][0].raycast(min, half, source, direction, cb);
        child[0][1].raycast(new PVector(min.x, half.y), new PVector(half.x, max.y), source, direction, cb);
        child[1][0].raycast(new PVector(half.x, min.y), new PVector(max.x, half.y), source, direction, cb);
        child[1][1].raycast(half, max, source, direction, cb);
      }
    }
  }
  
  /* Recursive Lightcasting
   * light is emitted from source along arc (angle arc.x to arc.y)
   * returns ArrayList of PVectors representing light after occlusion
   *         OR null to signify no occlusions
   * invokes callbacks to denote blocks and edges hit by light
   */
  // TODO: add intensity based on initial intensity (arc.z?) and distance
  ArrayList arccast(PVector min, PVector max, PVector source, PVector arc, IterCallback cb) {
    if(material_id >= 0) { // base case
      // determine transparency
      boolean trans = transparent(material_id);
      if(contains(min, max, source) && !trans) return new ArrayList(); // occlude all
      else if(!trans) { // find lit sides
      
        ArrayList litSides = new ArrayList(); // PVector[2] sides
        
        // specify ray by two vectors
        PVector v[] = {new PVector(source.x + cos(arc.x), source.y + sin(arc.x)),
                       new PVector(source.x + cos(arc.y), source.y + sin(arc.y))};

        // specify corner coordinates
        PVector corners[][] = {{min,                       new PVector(min.x, max.y)},
                               {new PVector(max.x, min.y), max                      }};
        ArrayList sides = new ArrayList();
        
        // if opaque, cull by choosing only light-facing sides to examine
        if(trans || source.x < min.x)
          sides.add(new PVector[] { corners[0][0], corners[0][1] });
        if(trans || source.y < min.y)
          sides.add(new PVector[] { corners[0][0], corners[1][0] });
        if(trans || source.x > max.x )
          sides.add(new PVector[] { corners[1][0], corners[1][1] });
        if(trans || source.y > max.y )
          sides.add(new PVector[] { corners[0][1], corners[1][1] });

        for(int i=0; i<sides.size(); i++) { // foreach side
          PVector side[] = (PVector[])sides.get(i);
          PVector c1 = side[0];
          PVector c2 = side[1];
          PVector ct = PVector.sub(c2, c1);
          boolean i1 = PVector.sub(source,c1).mag()<TOL || intersectRaySeg(source, c1, v[0], v[1]) != null; // true if corner c1 is inside arc
          boolean i2 = PVector.sub(source,c2).mag()<TOL || intersectRaySeg(source, c2, v[0], v[1]) != null; // true if corner c2 is inside arc
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
            assert(!(p1==null ^ p2==null));
            if(p1 != null) { litSides.add(new PVector[] {p1, p2}); } // intersection-intersection or none
          }
        }

        // callback lit sides
        if(litSides.size() > 0)
          cb.call(min, max, this, litSides);
        // convert into occlusion arcs
        if(!trans) {
          ArrayList occluded = new ArrayList(); // PVector arcs
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
          ArrayList arcs = new ArrayList();
          arcs.add(arc);
          occludeArcs(arcs, occluded);
          return arcs;
        }
      } else { // transparent
        return null; // occlude none
      }
    } else { // recurse
      // determine order to recurse over quadrants
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
      
      // determine whether any light enters this quad
      PVector v[] = {new PVector(source.x + cos(arc.x), source.y + sin(arc.x)),
                     new PVector(source.x + cos(arc.y), source.y + sin(arc.y))};
      PVector c1 = new PVector(xs[0] == 1 ? max.x : min.x, ys[1] == 1 ? max.y : min.y);
      PVector c2 = new PVector(xs[1] == 1 ? max.x : min.x, ys[0] == 1 ? max.y : min.y);
      PVector ct = PVector.sub(c2, c1);
      boolean i1 = PVector.sub(source,c1).mag()<TOL || intersectRaySeg(source, c1, v[0], v[1]) != null; // true if corner c1 is inside arc
      boolean i2 = PVector.sub(source,c2).mag()<TOL || intersectRaySeg(source, c2, v[0], v[1]) != null; // true if corner c2 is inside arc
      if(!(i1 || i2)) { // neither corner inside arc
        PVector p1 = intersectRaySeg(source, v[0], c1, c2);
        PVector p2 = intersectRaySeg(source, v[1], c1, c2);
        assert(!(p1==null ^ p2==null));
        if(p1 == null) { return null; } // no light enters this quadrant
      }

      ArrayList occluded = new ArrayList();
      
      // deal with malformed input arcs
      while(arc.x < -PI) arc.x += 2*PI;
      while(arc.y > PI) arc.y -= 2*PI;
      if(arc.x > arc.y) arc = new PVector(arc.y, arc.x);
      ArrayList arcs = new ArrayList();
      if(abs(arc.x-arc.y) < PI) {
        arcs.add(new PVector(arc.x, arc.y));
      } else {
        arcs.add(new PVector(-PI, arc.x));
        arcs.add(new PVector(arc.y, PI));
      }

      // recurse over quadrants
      PVector halfx = new PVector(half.x-min.x, 0);
      PVector halfy = new PVector(0, half.y-min.y);
      for(int i=0; i<2; i++) // foreach x
        for(int j=0; j<2; j++) { // foreach y
          int x = xs[i];
          int y = ys[j];
          ArrayList newArcs = new ArrayList();
          for(int k=0; k<arcs.size(); k++) { // foreach arc
            PVector offset = PVector.add(PVector.mult(halfx, x), PVector.mult(halfy, y));
            ArrayList childArcs = child[x][y].arccast(PVector.add(min, offset), PVector.add(half, offset), source, (PVector)arcs.get(k), cb);
            if(childArcs != null)
              newArcs.addAll(childArcs);
            else
              newArcs.add(arcs.get(k));
          }
          arcs = newArcs;
        }
      return arcs;
    }
    return null;
  }
  
  ArrayList bandcast(PVector min, PVector max, PVector dir, PVector band, IterCallback cb) {
    // TODO: implement directional lightcasting
    return null;
  }

}

void occludeArcs(ArrayList arcs, ArrayList occluded) {
  for(int i=0; i<occluded.size(); i++)
    occludeArcs(arcs, (PVector)occluded.get(i));
  for(int i=0; i<arcs.size(); i++) {
    PVector a = (PVector)arcs.get(i);
    if(abs(a.x-a.y) < TOL) {
      arcs.remove(i);
      i--;
    }
  }
}

void occludeArcs(ArrayList arcs, PVector o) {
  for(int i=0; i<arcs.size(); i++) {
    PVector a = (PVector)arcs.get(i);
    if(o.x <= a.x && o.y >= a.y) { // occlude all
      arcs.remove(i);
      i--;
    } else if(o.x <= a.y && o.y >= a.y) { // occlude upper
      a.y = o.x;
    } else if(o.x <= a.x && o.y >= a.x) { // occlude lower
      a.x = o.y;
    } else if(o.x >= a.x && o.y <= a.y) { // occlude middle
      PVector n = new PVector(o.y, a.y);
      a.y = o.x;
      arcs.add(n);
    }
  }
}

// TODO: add a return value to call() for shortcircuiting, etc?
abstract class IterCallback {
  abstract void call(PVector min, PVector max, Quad q, Object ... data);
}

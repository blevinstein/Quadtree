PVector lerp(PVector a, PVector b, float f) {
  return new PVector(lerp(a.x,b.x,f), lerp(a.y,b.y,f), lerp(a.z,b.z,f));
}

PVector invLerp(PVector a, PVector b, PVector c) {
  return new PVector(invLerp(a.x,b.x,c.x), invLerp(a.y,b.y,c.y), invLerp(a.z,b.z,c.z));
}

float invLerp(float a, float b, float c) {
  return (c-a)/(b-a);
}

boolean contains(PVector min, PVector max, PVector p) {
  return p.x >= min.x && p.x <= max.x &&
         p.y >= min.y && p.y <= max.y;
}

float atan(PVector p) {
  return atan2(p.y, p.x);
}

/*
 * http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
 * at = a2 - a1, bt = b2 - b1
 * a1 + t * at = b1 + s * bt
 * (a1 + t * at) X bt = b1 X bt
 * t * at X bt = (b1 - a1) X bt
 * t = (b1 - a1) X bt / (at X bt)
 *
 * u = point on A where B intersects, from a1 = 0 to a2 = 1
 * v = point on B where A intersects
 */
PVector intersect(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector at = PVector.sub(a2,a1);
  PVector bt = PVector.sub(b2,b1);
  
  float d = at.y*bt.x-at.x*bt.y;
  if(d == 0) return null;
  float u = (bt.x*(b1.y-a1.y)-bt.y*(b1.x-a1.x))/d;
  float v = (at.x*(b1.y-a1.y)-at.y*(b1.x-a1.x))/d;
  return new PVector(u, v);
}

// check whether segments intersect
PVector intersectSegSeg(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector i = intersect(a1, a2, b1, b2);
  if(i != null && i.x >= 0 && i.x <= 1 && i.y >= 0 && i.y <= 1) {
    PVector at = PVector.sub(a2, a1);
    return PVector.add(a1, PVector.mult(at, i.x));
  } else {
    return null;
  }
}

// check whether ray a intersects segment b
PVector intersectRaySeg(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector i = intersect(a1, a2, b1, b2);
  if(i != null && i.x >= 0 && i.y >= 0 && i.y <= 1) {
    PVector at = PVector.sub(a2, a1);
    return PVector.add(a1, PVector.mult(at, i.x));
  } else {
    return null;
  }
}


import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import java.awt.event.*; 
import java.util.*; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class Quadtree extends PApplet {




final float TOL = .0001f;

public boolean transparent(int mid) {
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
  
  public void setChildren(Quad q0, Quad q1, Quad q2, Quad q3) {
    child = new Quad[2][];
    child[0] = new Quad[2];
    child[1] = new Quad[2];
    child[0][0] = q0; child[0][0].parent = this;
    child[0][1] = q1; child[0][1].parent = this;
    child[1][0] = q2; child[1][0].parent = this;
    child[1][1] = q3; child[1][1].parent = this;
  }
  
  public Quad get(PVector p) {
    if(material_id >= 0)
      return this;
    return child[floor(p.x*2)][floor(p.y*2)]
      .get(new PVector(p.x*2 % 1, p.y*2 % 1));
  }
  
  public void set(PVector p, int r, int mid) {
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
  
  public void iter(PVector min, PVector max, IterCallback cb) {
    if(material_id >= 0) {
      cb.call(min, max, this);
    } else {
      PVector half = lerp(min, max, 0.5f);
      child[0][0].iter(min, half, cb);
      child[0][1].iter(new PVector(min.x, half.y), new PVector(half.x, max.y), cb);
      child[1][0].iter(new PVector(half.x, min.y), new PVector(max.x, half.y), cb);
      child[1][1].iter(half, max, cb);
    }
  }
  
  public void raycast(PVector min, PVector max, PVector source, PVector direction, IterCallback cb) {
    if(contains(min, max, source) ||
       contains(min, max, direction) ||
       intersectRaySeg(source, direction, min, max) != null ||
       intersectRaySeg(source, direction, new PVector(min.x, max.y), new PVector(max.x, min.y)) != null) {
      if(material_id >= 0) {
        cb.call(min, max, this);
      } else {
        PVector half = lerp(min, max, 0.5f);
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
  public ArrayList arccast(PVector min, PVector max, PVector source, PVector arc, IterCallback cb) {
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
      PVector half = lerp(min, max, 0.5f);
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
  
  public ArrayList bandcast(PVector min, PVector max, PVector dir, PVector band, IterCallback cb) {
    // TODO: implement directional lightcasting
    return null;
  }

}

public void occludeArcs(ArrayList arcs, ArrayList occluded) {
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

public void occludeArcs(ArrayList arcs, PVector o) {
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
  public abstract void call(PVector min, PVector max, Quad q, Object ... data);
}
Grid grid;

float max_zoom = 10;
float pan_frac = 0.5f;
float max_res = 8;
float wheel_sensitivity = .25f;
float speed = 5;
float gravity = 2;
float jump_accel = 15;
boolean grounded = false;

PVector player = new PVector(0, 1000);
PVector velocity = new PVector(0, 0);
PVector zoom = new PVector(1, -1);
float res = 0;

int tick_count = 0;

public void setup() {
  // Wheel mouse code adapted from http://wiki.processing.org/index.php/Wheel_mouse (@author Rick Companje)
  addMouseWheelListener(new MouseWheelListener() {
    public void mouseWheelMoved(MouseWheelEvent mwe) {
      mouseWheel(mwe.getWheelRotation());
  }});

  grid = new Grid();

  size(1200,800);
  //frame.setResizable(true);
}

public void mouseWheel(int delta) {
  res -= wheel_sensitivity * delta;
  if(res < 0) res = 0;
  if(res > max_res) res = max_res;
}

public void draw() {
  // handle input
  final PVector player_win = new PVector(width/2 + (width/2-mouseX)*pan_frac, height/2 + (height/2-mouseY)*pan_frac);
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, PVector.div(new PVector(1, 1), zoom));
  if(mousePressed && mouseButton == LEFT)
    grid.set(mouse, floor(res), 0);
  else if(mousePressed && mouseButton == RIGHT)
    grid.set(mouse, floor(res), 255);
  
  // adjust zoom
  //zoom = floor(res)/2f + 1;
  if(keys[KEY_Q]) zoom = PVector.mult(zoom, 1.05f);
  if(keys[KEY_E]) zoom = PVector.div(zoom, 1.05f);
  
  // move player
  if(grounded) {
    if(keys[KEY_SPACE]) {
      velocity.y = jump_accel;
      grounded = false;
    }
  } else {
    velocity.y -= gravity;
  }
  if(keys[KEY_A] ^ keys[KEY_D]) velocity.x = keys[KEY_A] ? -speed : speed;
  else velocity.x = 0;
  PVector new_player = PVector.add(player, velocity);
  /*
  Quad q = grid.get(new_player);
  if(q != null && q.material_id == 0) {
    velocity = new PVector(0, 0);
    grounded = true;
  } else {
    player = new_player;
    grounded = false;
  }
  */
  Quad q;
  if(null != (q = grid.get(new PVector(player.x+velocity.x, player.y+velocity.y))) && q.material_id != 0) {
    player = new_player;
    grounded = false;
  } else if(null != (q = grid.get(new PVector(player.x, player.y+velocity.y))) && q.material_id != 0) {
    player.y += velocity.y;
    velocity.x = 0;
    grounded = false;
  } else if(null != (q = grid.get(new PVector(player.x+velocity.x, player.y))) && q.material_id != 0) {
    player.x += velocity.x;
    if(velocity.y < 0) grounded = true;
    velocity.y = 0;
  } else {
    grounded = true;
  }
  
  // handle tick actions
  if(tick_count % 30 == 0) { // load new blocks every 30 ticks
    PVector win_min = transform(new PVector(0, 0), player_win, player, PVector.div(new PVector(1, 1), zoom));
    PVector win_max = transform(new PVector(width, height), player_win, player, PVector.div(new PVector(1, 1), zoom));
    int minx = floor(win_min.x / BLOCK_SIZE);
    int maxx = ceil(win_max.x / BLOCK_SIZE);
    int miny = floor(win_min.y / BLOCK_SIZE);
    int maxy = ceil(win_max.y / BLOCK_SIZE);
    for(int x=minx-1; x<=maxx+1; x++)
      for(int y=maxy-1; y<=miny+1; y++)
        if(!grid.has(x, y))
          grid.gen(x, y);
  }
  tick_count++;

  // draw background
  background(128);

  // draw quads
  stroke(128,128,128,64);
  strokeWeight(1);
  rectMode(CORNERS);
  grid.iter(new IterCallback() {
    public void call(PVector min, PVector max, Quad q, Object ... data) {
      int mid = q.material_id;
      fill(mid);
      PVector win_min = transform(min, player, player_win, zoom);
      PVector win_max = transform(max, player, player_win, zoom);
      rect(win_min.x, win_min.y, win_max.x, win_max.y);
    }
  });
  
  //draw player
  stroke(0,0,255);
  noFill();
  ellipseMode(CENTER);
  ellipse(player_win.x, player_win.y, zoom.x*10, zoom.y*10);

  // draw cursor
  int divs = (int)pow(2,floor(res));
  float cursor_size = BLOCK_SIZE * pow(0.5f, floor(res));
  PVector cursor_pos = new PVector( floor(mouse.x / cursor_size) * cursor_size, floor(mouse.y / cursor_size) * cursor_size );
  PVector cursor_win = transform(cursor_pos, player, player_win, zoom);
  noFill();
  stroke(128);
  strokeWeight(1);
  rectMode(CORNER);
  rect(cursor_win.x, cursor_win.y, zoom.x*cursor_size, zoom.y*cursor_size);
}

public void drawLight(PVector source, PVector arc) {
  float inf = width + height;
  PVector s1 = PVector.add(source, new PVector(inf*cos(arc.x), inf*sin(arc.x)));
  PVector s2 = PVector.add(source, new PVector(inf*cos(arc.y), inf*sin(arc.y)));
  drawLight(source, s1, s2);
}

public void drawLight(PVector source, PVector s1, PVector s2) {
  noStroke();
  fill(255,255,0,128);
  strokeWeight(1);
  triangle(source.x, source.y, s1.x, s1.y, s2.x, s2.y);
}

public PVector transform(PVector p, PVector real_coord, PVector win_coord, PVector zoom) {
  return PVector.add(PVector.mult(PVector.sub(p, real_coord), zoom), win_coord);
}
/*
int N = 1000;
float[] x;

void setup() {
  size(N,300);
  frame.setResizable(true);
  mouseClicked();
}

void draw() {
  barGraph(x, 0, 1);
}

void mouseClicked() {
  x = perlin(N, 4, 0.5, millis(), -5, 5);
}

void barGraph(float[] x, float min, float max) {
  int N = x.length;
  background(255);
  float bar_width = width*1f / N;
  float bar_min = min, bar_max = max;
  for(int i=0; i<N; i++) {
    if(x[i] > bar_max) bar_max = x[i];
    if(x[i] < bar_min) bar_min = x[i];
  }
  noStroke();
  fill(200);
  rectMode(CORNERS);
  for(int i=0; i<N; i++)
    rect(i*bar_width, height, (i+1)*bar_width, height - height*(x[i] - bar_min)/(bar_max-bar_min));
  fill(0);
  text(bar_max+"", 0, textAscent());
  text(bar_min+"", 0, height-textDescent());
}
*/
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
    generator = new NoiseGenerator(5, 4, 0.5f, millis(), .001f);
    //generator = new PerlinGenerator(5, 4, 0.5, 4, 0.5);
  }
  
  public Quad get(int i, int j) {
    return (Quad)m.get(s(i,j));
  }
  
  public boolean has(int i, int j) {
    return m.containsKey(s(i,j));
  }
  
  public Quad get(PVector p) {
    int i = floor(p.x/BLOCK_SIZE);
    int j = floor(p.y/BLOCK_SIZE);
    Quad q = (Quad)m.get(s(i,j));
    if(q == null)
      return null;
    else
      return q.get(new PVector((p.x - BLOCK_SIZE*i)/BLOCK_SIZE, (p.y - BLOCK_SIZE*j)/BLOCK_SIZE));
  }
  
  public void gen(int i, int j) {
    Quad q = generator.gen(i, j);
    load(i, j, q);
  }
  
  public void load(int i, int j, Quad q) {
    assert(m != null);
    m.put(s(i,j), q);
    // TODO: increment min, max
  }
  
  public void unload(int i, int j) {
    m.remove(s(i,j));
    // TODO: recalc min, max
  }
  
  public boolean set(PVector p, int res, int val) {
    int i = (int)floor(p.x/BLOCK_SIZE);
    int j = (int)floor(p.y/BLOCK_SIZE);
    Quad q = get(i, j);
    if(q == null)
      return false;
    PVector relP = PVector.sub(p, new PVector(i * BLOCK_SIZE, j * BLOCK_SIZE));
    PVector propP = PVector.div(relP, new PVector(BLOCK_SIZE, BLOCK_SIZE));
    q.set(propP, res, val);
    return true;
  }
  
  public void iter(IterCallback cb) {
    for(Map.Entry e : m.entrySet()) {
      String str = (String)e.getKey();
      int i[] = rs(str);
      Quad q = (Quad)e.getValue();
      q.iter(new PVector(i[0] * BLOCK_SIZE, i[1] * BLOCK_SIZE),
             new PVector((i[0]+1) * BLOCK_SIZE, (i[1]+1) * BLOCK_SIZE), cb);
    }
  }
  
  public ArrayList arccast(PVector source, PVector arc, IterCallback cb) {
    // TODO: implement
    return null;
  }
  
  public void raycast(PVector source, PVector dir) {
    // TODO: test this
    PVector g = source;
    int i = (int)(g.x / BLOCK_SIZE);
    int j = (int)(g.y / BLOCK_SIZE);
    while(true) {
      PVector n = new PVector((dir.x > 0 ? floor(g.x/BLOCK_SIZE) + 1 : ceil(g.x/BLOCK_SIZE) - 1) * BLOCK_SIZE,
                              (dir.y > 0 ? floor(g.y/BLOCK_SIZE) + 1 : ceil(g.y/BLOCK_SIZE) - 1) * BLOCK_SIZE);
      PVector t = PVector.div(PVector.sub(n, g), dir);
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
  public abstract Quad gen(int i, int j);
}

class ConstantGenerator extends QuadGenerator {
  int value;
  ConstantGenerator(int v) {
    value = v;
  }
  public Quad gen(int i, int j) {
    return new Quad(value);
  }
}
int KEY_NUM = 256;
boolean keys[] = new boolean[KEY_NUM];
int KEY_W = 87, KEY_A = 65, KEY_S = 83, KEY_D = 68, KEY_Q = 81, KEY_E = 69, KEY_R = 82;
int KEY_SHIFT = 16, KEY_SPACE = 32;
int KEY_LEFT = 37, KEY_RIGHT = 39;

public void keyPressed() {
  keys[keyCode] = true;
}

public void keyReleased() {
  keys[keyCode] = false;
}
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

public float cosineInterp(float a, float b, float x) {
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
  public float[] amps(int f, float amp) {
    float arr[] = new float[f];
    for(int i=0; i<f; i++)
      arr[i] = random(amp);
    return arr;
  }
  public float eval(float[][] layers, float x) {
    float total = 0;
    for(int i=0; i<layers.length; i++) {
      int idx = (int)(x * (layers[i].length-1));
      float pos = x * (layers[i].length-1) % 1;
      total += cosineInterp(layers[i][idx], idx+1<layers[i].length ? layers[i][idx+1] : 0, pos);
    }
    return total;
  }
  public Quad gen(int i, int j) {
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
  public Quad gen(int i, int j) {
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
public PVector lerp(PVector a, PVector b, float f) {
  return new PVector(lerp(a.x,b.x,f), lerp(a.y,b.y,f), lerp(a.z,b.z,f));
}

public PVector invLerp(PVector a, PVector b, PVector c) {
  return new PVector(invLerp(a.x,b.x,c.x), invLerp(a.y,b.y,c.y), invLerp(a.z,b.z,c.z));
}

public float invLerp(float a, float b, float c) {
  return (c-a)/(b-a);
}

public boolean contains(PVector min, PVector max, PVector p) {
  return p.x >= min.x && p.x <= max.x &&
         p.y >= min.y && p.y <= max.y;
}

public float atan(PVector p) {
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
public PVector intersect(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector at = PVector.sub(a2,a1);
  PVector bt = PVector.sub(b2,b1);
  
  float d = at.y*bt.x-at.x*bt.y;
  if(d == 0) return null; // TODO: account for 0/d when d==0
  float u = (bt.x*(b1.y-a1.y)-bt.y*(b1.x-a1.x))/d;
  float v = (at.x*(b1.y-a1.y)-at.y*(b1.x-a1.x))/d;
  return new PVector(u, v);
}

// check whether segments intersect
public PVector intersectSegSeg(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector i = intersect(a1, a2, b1, b2);
  if(i != null && i.x >= 0 && i.x <= 1 && i.y >= 0 && i.y <= 1) {
    PVector at = PVector.sub(a2, a1);
    return PVector.add(a1, PVector.mult(at, i.x));
  } else {
    return null;
  }
}

// check whether ray a intersects segment b
public PVector intersectRaySeg(PVector a1, PVector a2, PVector b1, PVector b2) {
  PVector i = intersect(a1, a2, b1, b2);
  if(i != null && i.x >= 0 && i.y >= 0 && i.y <= 1) {
    PVector at = PVector.sub(a2, a1);
    return PVector.add(a1, PVector.mult(at, i.x));
  } else {
    return null;
  }
}

// helpers to enable use of HashMap indexed on multiple ints
String HASH_DELIM = ":";
// int[] to String helper
public String s(int... is) {
  String str = "";
  for(int i=0; i<is.length; i++) {
    if(i > 0) str += HASH_DELIM;
    str += is[i];
  }
  return str;
}
// String to int[] helper
public int[] rs(String s) {
  String[] ss = s.split(HASH_DELIM);
  int r[] = new int[ss.length];
  for(int i=0; i<ss.length; i++)
    r[i] = Integer.parseInt(ss[i]);
  return r;
}
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "Quadtree" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}

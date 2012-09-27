Quad root = new Quad(255);
PVector min = new PVector(0, 0);
PVector max = new PVector(800, 800);

float lightFov = PI/8;
PVector lightSource = new PVector(400, 400);
PVector lightArc = new PVector(-lightFov, lightFov);
boolean lightAir = true;

float res = 0;
float maxRes = 7;
float wheelSensitivity = .25;
void mouseWheel(int delta) {
  res -= wheelSensitivity * delta;
  if(res < 0) res = 0;
  if(res > maxRes) res = maxRes;
}

void setBlock() {
  PVector p = invLerp(min, max, new PVector(mouseX, mouseY));
  root.set(p, floor(res), mouseButton == LEFT ? 0 : 255);
}

void mousePressed() {
  if(keys[KEY_SHIFT]) {
    if(mouseButton == LEFT) {
      lightSource = new PVector(mouseX, mouseY);
    } else {
      float mid = atan2(mouseY - lightSource.y, mouseX - lightSource.x);
      lightArc = new PVector(mid - lightFov, mid + lightFov);
    }
  } else {
    setBlock();
  }
}

void mouseDragged() {
  setBlock();
}

void setup() {
  setupMouseWheel();
  size(800,800);
}

void draw() {
  background(128);

  // draw quads
  stroke(128,128,128,64);
  strokeWeight(1);
  rectMode(CORNERS);
  root.iter(min, max, new IterCallback() {
    public void call(PVector min, PVector max, Object ... data) {
      int mid = (Integer)data[0];
      fill(mid);
      rect(min.x, min.y, max.x, max.y);
    }
  });

  // draw cursor
  int divs = (int) pow(2,floor(res));
  PVector cursorSize = PVector.div(PVector.sub(max, min), divs);
  PVector cursorPos = new PVector( floor(mouseX / cursorSize.x) * cursorSize.x, floor(mouseY / cursorSize.y) * cursorSize.y );
  noFill();
  stroke(128);
  strokeWeight(1);
  rectMode(CORNER);
  rect(cursorPos.x, cursorPos.y, cursorSize.x, cursorSize.y);

  // draw light
  ArrayList lightOut = root.lightcast(min, max, lightSource, lightArc, new IterCallback() {
    public void call(PVector min, PVector max, Object ... data) {
      int mid = (Integer)data[0];
      ArrayList segs = (ArrayList)data[1];
      if(mid == 0) {
        for(int i=0; i<segs.size(); i++) {
          PVector side[] = (PVector[]) segs.get(i);
          drawLight(lightSource, side[0], side[1]);
        }
      }
    }
  });
  if(lightOut == null) {
    drawLight(lightSource, lightArc);
  } else for(int i=0; i<lightOut.size(); i++) {
    PVector arc = (PVector)lightOut.get(i);
    drawLight(lightSource, arc);
  }
}

void drawLight(PVector source, PVector arc) {
  float inf = width + height;
  PVector s1 = PVector.add(source, new PVector(inf*cos(arc.x), inf*sin(arc.x)));
  PVector s2 = PVector.add(source, new PVector(inf*cos(arc.y), inf*sin(arc.y)));
  drawLight(source, s1, s2);
}

void drawLight(PVector source, PVector s1, PVector s2) {
  noStroke();
  fill(255,255,0,128);
  strokeWeight(1);
  triangle(source.x, source.y, s1.x, s1.y, s2.x, s2.y);
}

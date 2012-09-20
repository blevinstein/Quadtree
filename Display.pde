Quad root = new Quad(255);
PVector min = new PVector(0, 0);
PVector max = new PVector(800, 800);

float lightFov = PI/8;
PVector lightSource = new PVector(400, 400);
PVector lightArc = new PVector(-lightFov, lightFov);

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
  stroke(128);
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
  strokeWeight(3);
  rectMode(CORNER);
  rect(cursorPos.x, cursorPos.y, cursorSize.x, cursorSize.y);
  // draw light
  noFill();
  stroke(255,255,0);
  strokeWeight(3);
  line(lightSource.x, lightSource.y, lightSource.x + 1000*cos(lightArc.x), lightSource.y + 1000*sin(lightArc.x));
  line(lightSource.x, lightSource.y, lightSource.x + 1000*cos(lightArc.y), lightSource.y + 1000*sin(lightArc.y));
  root.lightcast(min, max, lightSource, lightArc, new IterCallback() {
    public void call(PVector min, PVector max, Object ... data) {
      int mid = (Integer)data[0];
      ArrayList segs = (ArrayList)data[1];
      if(mid == 0) {
        for(int i=0; i<segs.size(); i+=2) {
          PVector s1 = (PVector) segs.get(i);
          PVector s2 = (PVector) segs.get(i+1);
          line(s1.x, s1.y, s2.x, s2.y);
        }
      }
    }
  });
}

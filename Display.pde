Quad root = new Quad(0);
PVector min = new PVector(0, 0);
PVector max = new PVector(800, 800);
float res = 0;
float maxRes = 7;
float wheelSensitivity = .25;

void mouseWheel(int delta) {
  res -= wheelSensitivity * delta;
  if(res < 0) res = 0;
  if(res > maxRes) res = maxRes;
}

void mousePressed() {
  PVector p = invLerp(min, max, new PVector(mouseX, mouseY));
  int c = (mouseButton == LEFT ? 0 : 255);
  root.set(p, floor(res), c);
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
  root.iter(min, max, new IterCallback(){
    public void call(PVector min, PVector max, int mid) {
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
  rect(cursorPos.x, cursorPos.y, cursorSize.x, cursorSize.y);
}

PVector lerp(PVector a, PVector b, float f) {
  return new PVector(lerp(a.x,b.x,f), lerp(a.y,b.y,f), lerp(a.z,b.z,f));
}

PVector invLerp(PVector a, PVector b, PVector c) {
  return new PVector(invLerp(a.x,b.x,c.x), invLerp(a.y,b.y,c.y), invLerp(a.z,b.z,c.z));
}

float invLerp(float a, float b, float c) {
  return (c-a)/(b-a);
}

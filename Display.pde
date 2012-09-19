Quad root = new Quad(255);
PVector min = new PVector(0, 0);
PVector max = new PVector(800, 800);

float res = 0;
float maxRes = 7;
float wheelSensitivity = 1;
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
  rectMode(CORNERS);
  root.iter(min, max, new IterCallback(){
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
}

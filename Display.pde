Quad root = new Quad(255);
PVector min = new PVector(0, 0);
PVector max = new PVector(1200, 1200);

PVector player;
float zoom = 1;

float lightFov = PI/8;
PVector lightSource = new PVector(400, 400);
PVector lightArc = new PVector(-lightFov, lightFov);

void setup() {
  setupMouseWheel();
  size(1200,800);
  player = new PVector(width/2, height/2);
}

float res = 0;
float maxRes = 8;
float wheelSensitivity = .25;
void mouseWheel(int delta) {
  res -= wheelSensitivity * delta;
  if(res < 0) res = 0;
  if(res > maxRes) res = maxRes;
}

void setBlock(PVector mouse, int value) {
  PVector p = invLerp(min, max, mouse);
  root.set(p, floor(res), value);
}

void mousePressed() {
  PVector player_win = new PVector(width - mouseX, height - mouseY);
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, 1/zoom);
  if(keys[KEY_SHIFT]) {
    if(mouseButton == LEFT) {
      lightSource = new PVector(mouse.x, mouse.y);
    } else {
      float mid = atan2(mouse.y - lightSource.y, mouse.x - lightSource.x);
      lightArc = new PVector(mid - lightFov, mid + lightFov);
    }
  } else {
    setBlock(mouse, mouseButton == LEFT ? 0 : 255);
  }
}

void mouseDragged() {
  // TODO: draw continuous lines, not broken dots
  PVector player_win = new PVector(width - mouseX, height - mouseY);
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, 1/zoom);
  setBlock(mouse, mouseButton == LEFT ? 0 : 255);
}

void draw() {
  // move player
  float speed = 5;
  if(keys[KEY_W]) player.y -= speed;
  if(keys[KEY_S]) player.y += speed;
  if(keys[KEY_A]) player.x -= speed;
  if(keys[KEY_D]) player.x += speed;

  // background
  background(128);
  
  // calculate player and block positions
  PVector player_win = new PVector(width - mouseX, height - mouseY);
  PVector win_min = transform(min, player, player_win, zoom);
  PVector win_max = transform(max, player, player_win, zoom);

  // draw quads
  stroke(128,128,128,64);
  strokeWeight(1);
  rectMode(CORNERS);
  root.iter(win_min, win_max, new IterCallback() {
    public void call(PVector min, PVector max, Quad q, Object ... data) {
      int mid = q.material_id;
      fill(mid);
      rect(min.x, min.y, max.x, max.y);
    }
  });
  
  //draw player
  stroke(0,0,255);
  noFill();
  ellipseMode(CENTER);
  ellipse(player_win.x, player_win.y, 10, 10);

  // draw cursor
  int divs = (int)pow(2,floor(res));
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, 1/zoom);
  PVector cursor_size = PVector.div(PVector.sub(win_min, win_max), divs);
  PVector cursor_pos = new PVector( floor(mouse.x / cursor_size.x) * cursor_size.x, floor(mouse.y / cursor_size.y) * cursor_size.y );
  PVector cursor_win = transform(cursor_pos, player, player_win, zoom);
  noFill();
  stroke(128);
  strokeWeight(1);
  rectMode(CORNER);
  rect(cursor_win.x, cursor_win.y, cursor_size.x, cursor_size.y);

  // draw light
  final PVector light_win = transform(lightSource, player, player_win, zoom);
  ArrayList lightOut = root.lightcast(win_min, win_max, light_win, lightArc, new IterCallback() {
    public void call(PVector min, PVector max, Quad q, Object ... data) {
      int mid = q.material_id;
      ArrayList segs = (ArrayList)data[0];
      if(mid == 0) {
        for(int i=0; i<segs.size(); i++) {
          PVector side[] = (PVector[]) segs.get(i);
          drawLight(light_win, side[0], side[1]);
        }
      }
    }
  });
  if(lightOut == null) {
    drawLight(light_win, lightArc);
  } else for(int i=0; i<lightOut.size(); i++) {
    PVector arc = (PVector)lightOut.get(i);
    drawLight(light_win, arc);
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

PVector transform(PVector p, PVector real_coord, PVector win_coord, float zoom) {
  return PVector.add(PVector.mult(PVector.sub(p, real_coord), zoom), win_coord);
}

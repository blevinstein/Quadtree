Grid grid;

float max_zoom = 10;
float pan_frac = 0.5;
float max_res = 8;
float wheel_sensitivity = .25;
float speed = 5;

PVector player;
float zoom = 1;
float res = 0;

int tick_count = 0;

void setup() {
  setupMouseWheel();

  grid = new Grid();
  
  size(1200,800);
  player = new PVector(0,0);
  
  grid.load(0, 0, new Quad(255));
  grid.load(1, 0, new Quad(255));
  grid.load(-1, 0, new Quad(255));
}

void mouseWheel(int delta) {
  res -= wheel_sensitivity * delta;
  if(res < 0) res = 0;
  if(res > max_res) res = max_res;
}

void draw() {
  
  // handle input
  final PVector player_win = new PVector(width/2 + (width/2-mouseX)*pan_frac, height/2 + (height/2-mouseY)*pan_frac);
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, 1/zoom);
  if(mouseButton == LEFT)
    grid.set(mouse, floor(res), 0);
  else if(mouseButton == RIGHT)
    grid.set(mouse, floor(res), 255);
  
  // adjust zoom
  //zoom = floor(res)/2f + 1;
  if(keys[KEY_Q]) zoom += .5;
  if(keys[KEY_E]) zoom -= .5;
  if(zoom < 1) zoom = 1;
  if(zoom > max_zoom) zoom = max_zoom;
  
  // move player
  if(keys[KEY_W]) player.y -= speed;
  if(keys[KEY_S]) player.y += speed;
  if(keys[KEY_A]) player.x -= speed;
  if(keys[KEY_D]) player.x += speed;
  
  // handle tick actions
  if(tick_count % 30 == 0) { // load new blocks every 30 ticks
    PVector win_min = transform(new PVector(0, 0), player_win, player, 1/zoom);
    PVector win_max = transform(new PVector(width, height), player_win, player, 1/zoom);
    int minx = floor(win_min.x / BLOCK_SIZE);
    int maxx = ceil(win_max.x / BLOCK_SIZE);
    int miny = floor(win_min.y / BLOCK_SIZE);
    int maxy = ceil(win_max.y / BLOCK_SIZE);
    for(int x=minx-1; x<=maxx+1; x++)
      for(int y=miny-1; y<=maxy+1; y++)
        if(!grid.has(x, y))
          grid.load(x, y, new Quad(255));
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
  ellipse(player_win.x, player_win.y, 10*zoom, 10*zoom);

  // draw cursor
  int divs = (int)pow(2,floor(res));
  float cursor_size = BLOCK_SIZE * pow(0.5, floor(res));
  PVector cursor_pos = new PVector( floor(mouse.x / cursor_size) * cursor_size, floor(mouse.y / cursor_size) * cursor_size );
  PVector cursor_win = transform(cursor_pos, player, player_win, zoom);
  noFill();
  stroke(128);
  strokeWeight(1);
  rectMode(CORNER);
  rect(cursor_win.x, cursor_win.y, cursor_size*zoom, cursor_size*zoom);

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

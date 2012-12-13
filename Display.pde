Grid grid;

float max_zoom = 10;
float pan_frac = 0.5;
float max_res = 8;
float wheel_sensitivity = .25;
float speed = 5;
float gravity = 2;
float jump_accel = 15;
boolean grounded = false;

PVector player = new PVector(0, 1000);
PVector velocity = new PVector(0, 0);
PVector zoom = new PVector(1, -1);
float res = 0;

int tick_count = 0;

void setup() {
  // Wheel mouse code adapted from http://wiki.processing.org/index.php/Wheel_mouse (@author Rick Companje)
  addMouseWheelListener(new MouseWheelListener() {
    public void mouseWheelMoved(MouseWheelEvent mwe) {
      mouseWheel(mwe.getWheelRotation());
  }});

  grid = new Grid();

  size(1200,800);
  //frame.setResizable(true);
}

void mouseWheel(int delta) {
  res -= wheel_sensitivity * delta;
  if(res < 0) res = 0;
  if(res > max_res) res = max_res;
}

void draw() {
  // handle input
  final PVector player_win = new PVector(width/2 + (width/2-mouseX)*pan_frac, height/2 + (height/2-mouseY)*pan_frac);
  PVector mouse = transform(new PVector(mouseX, mouseY), player_win, player, PVector.div(new PVector(1, 1), zoom));
  if(mouseButton == LEFT)
    grid.set(mouse, floor(res), 0);
  else if(mouseButton == RIGHT)
    grid.set(mouse, floor(res), 255);
  
  // adjust zoom
  //zoom = floor(res)/2f + 1;
  if(keys[KEY_Q]) zoom = PVector.mult(zoom, 1.05);
  if(keys[KEY_E]) zoom = PVector.div(zoom, 1.05);
  
  // move player
  if(grounded) {
    if(keys[KEY_SPACE]) {
      velocity.y = jump_accel;
      grounded = false;
    }
  } else {
    velocity.y -= gravity;
  }
  if(keys[KEY_LEFT] ^ keys[KEY_RIGHT]) velocity.x = keys[KEY_LEFT] ? -speed : speed;
  PVector new_player = PVector.add(player, velocity);
  Quad q = grid.get(new_player);
  if(q != null && q.material_id == 0) {
    velocity = new PVector(0, 0);
    grounded = true;
  } else {
    player = new_player;
    grounded = false;
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
  float cursor_size = BLOCK_SIZE * pow(0.5, floor(res));
  PVector cursor_pos = new PVector( floor(mouse.x / cursor_size) * cursor_size, floor(mouse.y / cursor_size) * cursor_size );
  PVector cursor_win = transform(cursor_pos, player, player_win, zoom);
  noFill();
  stroke(128);
  strokeWeight(1);
  rectMode(CORNER);
  rect(cursor_win.x, cursor_win.y, zoom.x*cursor_size, zoom.y*cursor_size);
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

PVector transform(PVector p, PVector real_coord, PVector win_coord, PVector zoom) {
  return PVector.add(PVector.mult(PVector.sub(p, real_coord), zoom), win_coord);
}

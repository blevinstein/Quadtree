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
  x = perlin(N, 4, 0.5, 0.005);
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

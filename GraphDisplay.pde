int N = 500;
float[] x = new float[N];
int LIM = 10;

void setup() {
  size(N,400);
  frame.setResizable(true);
  
  for(int i=0; i<LIM; i++)
    x[i] = random(-1,1);
  for(int i=LIM; i<N; i++)
    x[i] = 0;
}

void draw() {
  barGraph(dft(x));
}

void barGraph(float[] x) {
  int N = x.length/2;
  background(255);
  float bar_width = width*1f / N;
  float bar_min = 0, bar_max = 0;
  for(int i=0; i<N; i++) {
    if(x[i] > bar_max) bar_max = x[i]*2;
    if(x[i] < bar_min) bar_min = x[i]*2;
  }
  noStroke();
  fill(200);
  rectMode(CORNERS);
  for(int i=0; i<N; i++)
    rect(i*bar_width, height, (i+1)*bar_width, height - height*(x[i] - bar_min)/(bar_max-bar_min));
    //rect(i*bar_width, height - height*(-bar_min)/(bar_max-bar_min), (i+1)*bar_width, height - height*(x[i] - bar_min)/(bar_max-bar_min));
  fill(0);
  text(bar_max+"", 0, textAscent());
  text(bar_min+"", 0, height-textDescent());
}

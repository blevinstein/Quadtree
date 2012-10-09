int N = 1000;
float[] x;

void setup() {
  size(N,300);
  frame.setResizable(true);
  
  //x = bandLimitedNoise(N, 60);
  x = exponentLimitedNoise(N, 80);
}

void draw() {
  barGraph(rdft(x));
  //barGraph(x);
}

float[] exponentLimitedNoise(int N, float k) {
  float x[] = new float[N];
  for(int i=0; i<N; i++)
    x[i] = random(-1, 1) * exp(-i*k/N);
  return x;
}

float[] bandLimitedNoise(int N, int LIM) {
  float x[] = new float[N];
  for(int i=0; i<LIM && i<N; i++)
    x[i] = random(-1, 1);
  for(int i=LIM; i<N; i++)
    x[i] = 0;
  return x;
}

void barGraph(float[] x) {
  int N = x.length;
  background(255);
  float bar_width = width*1f / N;
  float bar_min = -5, bar_max = 5;
  for(int i=0; i<N; i++) {
    if(x[i] > bar_max) bar_max = x[i];
    if(x[i] < bar_min) bar_min = x[i];
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

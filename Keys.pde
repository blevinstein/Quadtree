int KEY_NUM = 256;
boolean keys[] = new boolean[KEY_NUM];
int KEY_W = 87, KEY_A = 65, KEY_S = 83, KEY_D = 68, KEY_Q = 81, KEY_E = 69, KEY_R = 82, KEY_SHIFT = 16;

void keyPressed() {
  keys[keyCode] = true;
}

void keyReleased() {
  keys[keyCode] = false;
}

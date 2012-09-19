int keys[] = new int[256];
int KEY_NUM = 256, KEY_UP = 0, KEY_DOWN = 1;
int KEY_W = 87, KEY_A = 65, KEY_S = 83, KEY_D = 68;
int KEY_R = 82;
int KEY_SHIFT = 16;

void keyPressed() {
  keys[keyCode] = KEY_DOWN;
}

void keyReleased() {
  keys[keyCode] = KEY_UP;
}

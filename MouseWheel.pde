/*
 * Wheel mouse adapted from http://wiki.processing.org/index.php/Wheel_mouse
 * @author Rick Companje
*/
import java.awt.event.*;
 
void setupMouseWheel() {
  addMouseWheelListener(new MouseWheelListener() { 
    public void mouseWheelMoved(MouseWheelEvent mwe) { 
      mouseWheel(mwe.getWheelRotation());
  }}); 
}

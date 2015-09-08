package com.blevinstein.qt

import com.blevinstein.util.Throttle

import com.jogamp.opengl.GL.GL_COLOR_BUFFER_BIT
import com.jogamp.opengl.GL.GL_FRONT_AND_BACK
import com.jogamp.opengl.GL.GL_LINES
import com.jogamp.opengl.GL.GL_TRIANGLE_FAN
import com.jogamp.opengl.GL2
import com.jogamp.opengl.GLAutoDrawable
import com.jogamp.opengl.GLCapabilities
import com.jogamp.opengl.GLEventListener
import com.jogamp.opengl.GL2GL3.GL_FILL
import com.jogamp.opengl.GL2GL3.GL_LINE
import com.jogamp.opengl.GLProfile
import com.jogamp.opengl.awt.GLCanvas
import com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_MODELVIEW
import com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_PROJECTION
import com.jogamp.opengl.glu.GLU
import com.jogamp.opengl.util.awt.TextRenderer
import java.awt.Color
import java.awt.Font
import java.awt.Frame
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.MouseMotionAdapter
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

object Driver extends App {
  val FPS : Int = 60
  // dimensions of the screen
  var height : Int = 1
  var width : Int = 1
  // size and offset of the screen
  var size : Int = 1
  var offset : Point = Point.zero

  // setup OpenGL
  val glProfile = GLProfile.getDefault()
  GLProfile.initSingleton()
  val glCapabilities = new GLCapabilities(glProfile)
  glCapabilities setSampleBuffers true
  glCapabilities setDoubleBuffered true
  val glCanvas = new GLCanvas(glCapabilities)
  glCanvas.addGLEventListener(EventListener)
  // scalastyle:off magic.number
  val textRenderer = new TextRenderer(new Font("Arial", Font.PLAIN, 10))
  glCanvas.addMouseListener(MouseListener)
  glCanvas.addMouseMotionListener(MouseMotionListener)

  // setup window
  val frame = new Frame()
  frame.add(glCanvas)
  frame.addWindowListener(WindowListener)
  frame.setSize(640, 800 + 25) // scalastyle:off magic.number
  frame.setVisible(true)

  // setup game
  val root = QuadTree.approx(3, (p) =>
      if ((p - new Point(0.5f, 0.5f)).mag < 0.5f) {
        Material.Full
      } else {
        Material.Empty
      })

  run

  def run : Unit  = {
    val throttle = new Throttle(FPS)
    while (true) {
      glCanvas.display()
      throttle.sleep
    }
  }

  def getCanvas : GLCanvas = glCanvas

  def setup(gl : GL2) : Unit = {
    gl.glMatrixMode(GL_PROJECTION)
    gl.glLoadIdentity

    val glu = new GLU()
    glu.gluOrtho2D(0, width, 0, height)

    gl.glMatrixMode(GL_MODELVIEW)
    gl.glLoadIdentity

    gl.glViewport(0, 0, width, height)
  }

  def render(gl : GL2) : Unit = {
    // drawing subroutines
    def setColor(c : Color): Unit = {
      gl.glColor4d(c.getRed() / 255.0,
        c.getGreen() / 255.0,
        c.getBlue() / 255.0,
        c.getAlpha() / 255.0)
    }
    def setFill(fill : Boolean): Unit = {
      gl.glPolygonMode(GL_FRONT_AND_BACK, if (fill) GL_FILL else GL_LINE)
    }
    def drawRect(rect : Rectangle): Unit = {
      val screenRect = rect * size + offset
      gl.glRectf(screenRect.min.x, screenRect.min.y,
          screenRect.max.x, screenRect.max.y)
    }

    gl.glClear(GL_COLOR_BUFFER_BIT)

    // draw background
    setFill(true)
    setColor(Color.WHITE)
    gl.glRectf(0, 0, width, height)

    // draw quadtree
    setColor(Color.BLACK)
    root.iter((r, m) => {
      if (m == Material.Full) {
        drawRect(r)
      }
    })

    // end drawing
    gl.glFlush()
  }

  object MouseListener extends MouseAdapter {
    override def mouseClicked(e : MouseEvent): Unit = {}
  }

  object MouseMotionListener extends MouseMotionAdapter {
    override def mouseMoved(e : MouseEvent): Unit = {}
  }

  object EventListener extends GLEventListener {
    // Respond to changes in width or height
    def reshape(drawable : GLAutoDrawable,
        x : Int,
        y : Int,
        w : Int,
        h : Int) : Unit = {
      // update screen dimensions
      width = w
      height = h
      size = math.min(width, height)
      val offsetDist = math.abs(width - height) / 2
      offset = if (width > height) {
        new Point(offsetDist, 0)
      } else {
        new Point(0, offsetDist)
      }

      setup(drawable.getGL().getGL2())
    }

    def init(drawable : GLAutoDrawable) : Unit =
        setup(drawable.getGL().getGL2())

    def dispose(drawable : GLAutoDrawable) : Unit = ()

    def display(drawable : GLAutoDrawable) : Unit =
        render(drawable.getGL().getGL2())
  }

  object WindowListener extends WindowAdapter {
    // When window is closed, exit program
    override def windowClosing(e : WindowEvent) : Unit = {
      frame.remove(glCanvas)
      frame.dispose()
      System.exit(0)
    }
  }
}


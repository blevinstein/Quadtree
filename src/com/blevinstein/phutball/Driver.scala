package com.blevinstein.phutball

import com.blevinstein.util.Throttle

import com.jogamp.opengl.GL.GL_COLOR_BUFFER_BIT
import com.jogamp.opengl.GL.GL_LINES
import com.jogamp.opengl.GL2
import com.jogamp.opengl.GLAutoDrawable
import com.jogamp.opengl.GLCapabilities
import com.jogamp.opengl.GLEventListener
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
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

object Driver extends App {
  val FPS : Int = 60
  // dimensions of the screen
  var height : Int = 1
  var width : Int = 1
  // dimensions of the board
  val boardRatio = (Board.height + 1f) / (Board.width + 1f)
  var boardHeight : Int = 1
  var boardWidth : Int = 1

  // setup OpenGL
  val glProfile = GLProfile.getDefault()
  GLProfile.initSingleton()
  val glCapabilities = new GLCapabilities(glProfile)
  glCapabilities setSampleBuffers true
  glCapabilities setDoubleBuffered true
  val glCanvas = new GLCanvas(glCapabilities)
  glCanvas.addGLEventListener(EventListener)
  val textRenderer = new TextRenderer(new Font("Arial", Font.PLAIN, 10))
  glCanvas.addMouseListener(MouseListener)

  // setup window
  val frame = new Frame()
  frame.add(glCanvas)
  frame.addWindowListener(WindowListener)
  frame.setSize(640, 800 + 25) // scalastyle:off magic.number
  frame.setVisible(true)

  // setup game
  val board = Board.newBoard

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

  def setColor(gl : GL2, c : Color) {
    gl.glColor4d(c.getRed() / 255.0,
      c.getGreen() / 255.0,
      c.getBlue() / 255.0,
      c.getAlpha() / 255.0)
  }

  def render(gl : GL2) : Unit = {
    gl.glClear(GL_COLOR_BUFFER_BIT)

    // draw background
    setColor(gl, Color.DARK_GRAY)
    gl.glRectf(0, 0, boardWidth, boardHeight)
    // draw grid
    setColor(gl, Color.WHITE)
    gl.glColor4d(255, 255, 255, 255)
    gl.glBegin(GL_LINES)
    for (i <- 0 until Board.width + 1) {
      gl.glVertex2d(boardWidth * i / Board.width, 0)
      gl.glVertex2d(boardWidth * i / Board.width, boardHeight)
    }
    for (j <- 0 until Board.height + 1) {
      gl.glVertex2d(0, boardHeight * j / Board.height)
      gl.glVertex2d(boardWidth, boardHeight * j / Board.height)
    }
    gl.glEnd()
    // draw coords
    textRenderer.beginRendering(width, height)
    for (i <- 0 until Board.width) {
      for (j <- 0 until Board.height) {
        textRenderer.draw(new Position(i, j).toString,
          ((i + 0.5f) / Board.width * boardWidth).toInt,
          ((j + 0.5f) / Board.height * boardHeight).toInt)
      }
    }
    textRenderer.endRendering()

    gl.glFlush()
  }

  object MouseListener extends MouseAdapter {
    override def mouseClicked(e : MouseEvent) {
      val pos = new Position(
        math.floor(e.getX() * 1f / boardWidth * Board.width).toInt,
        math.floor((boardHeight - e.getY()) * 1f / boardHeight * Board.height).toInt)
      Console.println(pos.toString)
    }
  }

  object EventListener extends GLEventListener {
    // Respond to changes in width or height
    def reshape(drawable : GLAutoDrawable, x : Int, y : Int, w : Int, h : Int) : Unit = {
      // update screen dimensions
      width = w
      height = h
      // update board dimensions
      // TODO: add board offset
      boardWidth = math.round(math.min(width : Float, height / boardRatio))
      boardHeight = math.round(boardWidth * boardRatio)

      setup(drawable.getGL().getGL2())
    }

    def init(drawable : GLAutoDrawable) : Unit = setup(drawable.getGL().getGL2())

    def dispose(drawable : GLAutoDrawable) : Unit = ()

    def display(drawable : GLAutoDrawable) : Unit = render(drawable.getGL().getGL2())
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

package com.blevinstein.qt.sim

import com.blevinstein.ga.Population
import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.{QuadAddr,QuadOffset,Quadrant,QuadRectangle,QuadLen}
import com.blevinstein.qt.{QuadTree,QuadLeaf,QuadBranch}
import com.blevinstein.util.RateLimiter
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
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent.{VK_DOWN,VK_LEFT,VK_RIGHT,VK_UP}
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.MouseMotionAdapter
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

// TODO: Think about better ways of handling degredation when framerate < [FPS]
object Driver extends App {
  val FPS: Int = 60
  // Dimensions of the screen
  var height: Int = 1
  var width: Int = 1

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
  glCanvas.addKeyListener(KeyListener)
  glCanvas.addMouseListener(MouseListener)
  glCanvas.addMouseMotionListener(MouseMotionListener)

  // setup window
  val frame = new Frame()
  frame.add(glCanvas)
  frame.addWindowListener(WindowListener)
  frame.setSize(640, 800 + 25) // scalastyle:off magic.number
  frame.setVisible(true)

  // TODO: refactor game logic out of Driver
  def checkerboard(depth: Int): QuadTree[Option[Material]] = depth match {
    case 1 => new QuadBranch(new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Blue),
      new QuadLeaf(Material.Blue),
      new QuadLeaf(Material.Empty))
    case other: Int => new QuadBranch(checkerboard(other - 1),
      checkerboard(other - 1),
      checkerboard(other - 1),
      checkerboard(other - 1))
  }

  // setup game
  var world = new World[Material]
  val figureId = world.add(
    (QuadRectangle.unit >> 3) + QuadOffset.half,
    checkerboard(3))
  val containerId = world.add(
    QuadRectangle.unit,
    QuadTree.approx(6, (p) =>
        if (p.y < 0.1 || p.y < p.x - 0.5) {
          Material.Gray
        } else {
          Material.Empty
        }))

  def run: Unit = {
    val throttle = new Throttle(FPS)
    while (true) {
      mainLoop
      glCanvas.display()
      throttle.sleep
    }
  }

  // TODO: refactor movement out of Driver
  // TODO: move minimum resolution (-6) elsewhere?
  val moveLen = new QuadLen(1, -6)
  val down = new QuadOffset(QuadLen.zero, -moveLen)
  val left = new QuadOffset(-moveLen, QuadLen.zero)
  val right = new QuadOffset(moveLen, QuadLen.zero)
  val up = new QuadOffset(QuadLen.zero, moveLen)
  def mainLoop: Unit = {
    if (KeyListener.keyDown(VK_DOWN)) {
      world.moveIfPossible(figureId, down)
    }
    if (KeyListener.keyDown(VK_LEFT)) {
      world.moveIfPossible(figureId, left)
    }
    if (KeyListener.keyDown(VK_RIGHT)) {
      world.moveIfPossible(figureId, right)
    }
    if (KeyListener.keyDown(VK_UP)) {
      world.moveIfPossible(figureId, up)
    }
  }

  // DEBUGGING ROUTINES
  def printObjectPositions: Unit = {
    println("object positions:")
    for (obj <- world.allObjs) {
      println(s"${obj.position}")
    }
  }

  def getCanvas: GLCanvas = glCanvas

  def setup(gl: GL2): Unit = {
    gl.glMatrixMode(GL_PROJECTION)
    gl.glLoadIdentity

    val glu = new GLU()
    glu.gluOrtho2D(0, width, 0, height)

    gl.glMatrixMode(GL_MODELVIEW)
    gl.glLoadIdentity

    gl.glViewport(0, 0, width, height)
  }

  def render(gl: GL2): Unit = {
    // drawing subroutines
    def setColor(c: Color): Unit = {
      gl.glColor4d(c.getRed() / 255.0,
        c.getGreen() / 255.0,
        c.getBlue() / 255.0,
        c.getAlpha() / 255.0)
    }
    def setFill(fill: Boolean): Unit = {
      gl.glPolygonMode(GL_FRONT_AND_BACK, if (fill) GL_FILL else GL_LINE)
    }
    def drawRect(rect: Rectangle): Unit = {
      val screenRect =
          rect * LayoutManager.screen.size + LayoutManager.screen.min
      gl.glRectf(screenRect.min.x, screenRect.min.y,
          screenRect.max.x, screenRect.max.y)
    }
    def drawAll(rects: List[(Rectangle, Color)]) {
      // fill
      setFill(true)
      rects.foreach { case (rect, color) =>
        setColor(color)
        drawRect(rect)
      }
      // outline in black
      setFill(false)
      setColor(Color.BLACK)
      rects.foreach { case (rect, color) => drawRect(rect) }
    }

    gl.glClear(GL_COLOR_BUFFER_BIT)

    // Short-circuit if world is uninitialized
    if (world == null) {
      return
    }

    // draw background
    setFill(true)
    setColor(Color.WHITE)
    gl.glRectf(0, 0, width, height)

    // draw world
    var rects = List[(Rectangle,Color)]()
    world.view.iter((addr, m) => m match {
      case Some(mat) => rects = (addr.toRectangle, mat.color) :: rects
      case None => ()
    })
    drawAll(rects)

    // draw collisions
    val figure = world.getObj(figureId)
    val container = world.getObj(containerId)
    val contacts = figure.contacts(container)
    var collisions = List[(Rectangle,Color)]()
    contacts.foreach { case (a: QuadAddr, b: QuadAddr) =>
      collisions = (a.toRectangle, Color.YELLOW) ::
          (b.toRectangle, Color.RED) :: collisions
    }
    drawAll(collisions)

    // end drawing
    gl.glFlush()
  }

  object LayoutManager {
    // Position of the screen
    var screen = new Rectangle(Point.zero, new Point(1, 1))

    def reshape(x: Int, y: Int, w: Int, h: Int) {
      // Update view panel: center onscreen and maintain aspect ratio
      screen = centerSquare(new Rectangle(Point.zero, new Point(width, height)))
    }

    // Given a rectangle, finds the largest square that can be fit inside, and
    // centers it in the available space.
    def centerSquare(rect: Rectangle): Rectangle = {
      val size = math.min(rect.size.x, rect.size.y)
      val offsetDist = math.abs(rect.size.x - rect.size.y) / 2
      val offset = if(rect.size.x > rect.size.y) {
        new Point(offsetDist, 0)
      } else {
        new Point(0, offsetDist)
      }
      new Rectangle(rect.min + offset, rect.max - offset)
    }
  }

  object KeyListener extends KeyAdapter {
    var keysDown: Set[Integer] = Set()

    def keyDown(keyCode: Integer): Boolean = keysDown.contains(keyCode)
    override def keyPressed(e: KeyEvent): Unit = keysDown += e.getKeyCode()
    override def keyReleased(e: KeyEvent): Unit = keysDown -= e.getKeyCode()
  }

  object MouseListener extends MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {}
  }

  object MouseMotionListener extends MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {}
  }

  object EventListener extends GLEventListener {
    // Respond to changes in width or height
    def reshape(drawable: GLAutoDrawable,
        x: Int,
        y: Int,
        w: Int,
        h: Int): Unit = {
      // Update screen dimensions
      width = w
      height = h
      LayoutManager.reshape(x, y, w, h)

      setup(drawable.getGL().getGL2())
    }

    def init(drawable: GLAutoDrawable): Unit =
        setup(drawable.getGL().getGL2())

    def dispose(drawable: GLAutoDrawable): Unit = ()

    def display(drawable: GLAutoDrawable): Unit =
        render(drawable.getGL().getGL2())
  }

  object WindowListener extends WindowAdapter {
    // When window is closed, exit program
    override def windowClosing(e: WindowEvent): Unit = {
      frame.remove(glCanvas)
      frame.dispose()
      System.exit(0)
    }
  }

  run
}


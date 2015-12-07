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
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.MouseMotionAdapter
import java.awt.event.MouseWheelEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.util.control.Breaks._

// This is a simple driver for testing out a simulation running on the QuadTree
// engine.
//
// Much of the code here is experimental, and should eventually be
// refactored into other classes within the [sim] package, e.g. World or
// QuadObject.
//
// TODO: Think about better ways of handling degredation when framerate < [FPS]
//
// TODO: Debug freezing on osx (stops receiving [KeyEvent]s)
object Driver extends App {
  val FPS: Int = 30
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
  glCanvas.addMouseWheelListener(MouseListener)

  // TODO: refactor toolbelt -> List, instead of 'active tool' apply all tools
  var toolbelt: List[Tool] = List(
    DeleteTool
  )

  // setup window
  val frame = new Frame()
  frame.add(glCanvas)
  frame.addWindowListener(WindowListener)
  frame.setSize(1024, 1024 + 25) // scalastyle:off magic.number
  frame.setVisible(true)

  // setup game
  val physics = new PhysicsModule
  physics.gravity = new Point(0, -1f / (1 << 8))

  val reaper = new ReaperModule
  reaper.boundingRectangle = new Rectangle(new Point(-5, -5), new Point(5, 5))

  var world = (new World).
      install(physics).
      install(reaper)

  val figureShape =
      ImageHelper.createTreeFromImage(ImageIO.read(new File("data/figure.png")))
  val figureId = world.add(new QuadObject(
      (QuadRectangle.unit >> 3) + QuadOffset.half,
      figureShape)).get
  world.add(new QuadObject(
      QuadRectangle.unit,
      QuadTree.approx(6, (p) =>
          if (p.y < 0.05) {
            Material.Gray
          } else {
            Material.Empty
          }),
      Fixed)).get
  world.add(new QuadObject(
      QuadRectangle.unit + new QuadOffset(QuadLen.one, QuadLen.zero),
      QuadTree.approx(6, (p) =>
          if (p.y < p.x) {
            Material.Gray
          } else {
            Material.Empty
          }),
      Fixed)).get

  def run: Unit = {
    val throttle = new Throttle(FPS)
    while (true) {
      mainLoop
      glCanvas.display()
      throttle.sleep
    }
  }

  val accel = 1f / (1 << 6)

  val left = new Point(-accel, 0)
  val right = new Point(accel, 0)
  val up = new Point(0, accel * 2)

  def mainLoop: Unit = {
    val figure = world.getObj(figureId)
    val contactsEnvironment = !world.contactsWithAll(figureId).isEmpty

    val desiredVelocity = Point.zero +
        (if (KeyListener.keyDown(KeyEvent.VK_A)) left else Point.zero) +
        (if (KeyListener.keyDown(KeyEvent.VK_D)) right else Point.zero) +
        (if (KeyListener.keyDown(KeyEvent.VK_W)) up else Point.zero)

    if (contactsEnvironment) {
      world.setVelocity(figureId, desiredVelocity)
    }

    world.update
  }

  // DEBUGGING ROUTINES
  def printObjectPositions: Unit = {
    println("object positions:")
    for ((id, obj) <- world.objs) {
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

  var zoom = 1f
  var center = Point.zero
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
    // TODO: center on figure
    // TODO: add isOnScreen check
    def drawRect(rect: Rectangle): Unit = {
      val screenRect =
          (rect - center) * LayoutManager.screen.size / zoom +
              LayoutManager.screen.center
      gl.glRectf(screenRect.min.x, screenRect.min.y,
          screenRect.max.x, screenRect.max.y)
    }
    def drawAll(rects: Iterable[(Rectangle, Color)]) {
      // fill
      setFill(true)
      rects.foreach { case (rect, color) =>
        setColor(color)
        drawRect(rect)
      }
      // outline in black
      // TODO: implement outline only where material changes, use getRegions?
      // TODO: getBorders(region: List[QuadAddr/QuadRectangle]):
      //     List[(Quadffset, QuadOffset)]
      //setFill(false)
      //setColor(Color.BLACK)
      //rects.foreach { case (rect, color) => drawRect(rect) }
    }

    gl.glClear(GL_COLOR_BUFFER_BIT)

    // Short-circuit if world is uninitialized
    // TODO: refactor, this happens because App uses DelayedInit
    if (world == null || inputStack == null) {
      return
    }

    // Get world data
    val figure = world.getObj(figureId)

    // Focus camera
    center = figure.center.toPoint

    // draw background
    setFill(true)
    setColor(Color.WHITE)
    gl.glRectf(0, 0, width, height)

    // draw world
    var rects = List[(Rectangle,Color)]()
    world.iter((objId, quadRect, mat) => {
      rects = (quadRect.toRectangle, mat.color) :: rects
    })
    drawAll(rects)

    for (tool <- toolbelt) {
      if (tool.trigger(world, getInput)) {
        val cursorRects = tool.activate(world, getInput)
        // TODO: refactor to allow different colors for different tools
        drawAll(
            for (rect <- cursorRects) yield (rect.toRectangle, Color.YELLOW))
      }
      if (tool.clear(world, getInput)) {
        inputStack.clear()
      }
    }

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
    private var keysDown: Set[Int] = Set()

    val inputStackBlacklist = Set(
        KeyEvent.VK_A,
        KeyEvent.VK_D,
        KeyEvent.VK_S,
        KeyEvent.VK_W)

    def keyDown(keyCode: Int): Boolean = keysDown.contains(keyCode)

    override def keyPressed(e: KeyEvent): Unit = {
      // Update keysDown
      keysDown += e.getKeyCode()
      // Update inputStack
      e.getKeyCode() match {
        case KeyEvent.VK_ESCAPE => inputStack.clear()
        case code if inputStackBlacklist contains code => ()
        case code => inputStack.push(KeyInput(code))
      }
    }

    override def keyReleased(e: KeyEvent): Unit = {
      println(s"keyReleased: ${e.getKeyCode()}")
      keysDown -= e.getKeyCode()
    }
  }

  val zoomUnit = 1.05f;
  object MouseListener extends MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      // TODO: BROKEN fix y coordinate when width < height
      inputStack.push(MouseInput(
          screenToWorld(new Point(
              e.getX(),
              height - e.getY())),
          e.getButton()))
    }
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      if (e.getWheelRotation() > 0) {
        zoom *= zoomUnit
      } else if (e.getWheelRotation() < 0) {
        zoom /= zoomUnit
      }
    }
  }

  var inputStack: Stack[Input] = new Stack()

  def getInput: List[Input] = {
    inputStack.toList match {
      case MouseInput(point, button) :: _ => inputStack.toList
      case _ =>
          MouseInput(
              screenToWorld(MouseMotionListener.position),
              MouseInput.HOVER) ::
          inputStack.toList
    }
  }

  def screenToWorld(p: Point) = (p - LayoutManager.screen.center) /
      LayoutManager.screen.size * zoom + center

  object MouseMotionListener extends MouseMotionAdapter {
    var position: Point = Point.zero
    override def mouseMoved(e: MouseEvent): Unit = {
      position = new Point(e.getX(), LayoutManager.screen.size.y - e.getY())
    }
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


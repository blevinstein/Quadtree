package com.blevinstein.qt

import com.jogamp.opengl.GL.GL_COLOR_BUFFER_BIT
import com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_PROJECTION
import com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_MODELVIEW

import com.blevinstein.util.Throttle

import com.jogamp.opengl.GL2
import com.jogamp.opengl.GLAutoDrawable
import com.jogamp.opengl.GLCapabilities
import com.jogamp.opengl.GLEventListener
import com.jogamp.opengl.GLProfile
import com.jogamp.opengl.awt.GLCanvas
import com.jogamp.opengl.glu.GLU
import java.awt.Color
import java.awt.Frame
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

object Driver extends App {
  val FPS : Int = 60
  var width : Int = 1
  var height : Int = 1

  // setup OpenGL
  val glProfile = GLProfile.getDefault()
  GLProfile.initSingleton()
  val glCapabilities = new GLCapabilities(glProfile)
  glCapabilities setSampleBuffers true
  glCapabilities setDoubleBuffered true
  val glCanvas = new GLCanvas(glCapabilities)
  glCanvas.addGLEventListener(EventListener)

  // setup window
  val frame = new Frame()
  frame.add(glCanvas)
  frame.addWindowListener(WindowListener)
  frame.setSize(640, 480 + 25)
  frame.setVisible(true)
  // TODO: add input listeners

  run

  def run = {
    val throttle = new Throttle(FPS)
    while (true) {
      // TODO: update loop
      glCanvas.display()
      throttle.sleep
    }
  }

  def getCanvas = glCanvas

  def setup(gl : GL2) = {
    gl.glMatrixMode(GL_PROJECTION)
    gl.glLoadIdentity
    
    val glu = new GLU()
    glu.gluOrtho2D(0, width, 0, height)

    gl.glMatrixMode(GL_MODELVIEW)
    gl.glLoadIdentity

    gl.glViewport(0, 0, width, height)
  }

  def render(gl : GL2) = {
    gl.glClear(GL_COLOR_BUFFER_BIT)
    // TODO: draw
    gl.glFlush()
  }
  
  object EventListener extends GLEventListener {
    def reshape(drawable : GLAutoDrawable, x : Int, y : Int, w : Int, h : Int)  = {
      width = w
      height = h
      setup(drawable.getGL().getGL2())
    }

    def init(drawable : GLAutoDrawable) = setup(drawable.getGL().getGL2())

    def dispose(drawable : GLAutoDrawable) = ()

    def display(drawable : GLAutoDrawable) = render(drawable.getGL().getGL2())
  }

  object WindowListener extends WindowAdapter {
    override def windowClosing(e : WindowEvent) {
      frame.remove(glCanvas)
      frame.dispose()
      System.exit(0)
    }
  }
}

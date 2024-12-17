package com.fdilke.truchet

import java.awt._
import javax.swing.{JFrame, JPanel, WindowConstants}

object TruchetApp extends App:
  val device: GraphicsDevice =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice

  new TruchetFrame(device)

class TruchetFrame(
  device: GraphicsDevice
) extends JFrame("Truchet Frame", device.getDefaultConfiguration):

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  initComponents()
  begin()

  private def initComponents(): Unit =
    val contentPane = getContentPane
    contentPane.setLayout(new BorderLayout)
    val panel = new TruchetPanel
    contentPane.add(panel, BorderLayout.CENTER)

  private def begin(): Unit =
    val isFullScreen: Boolean = device.isFullScreenSupported
    setUndecorated(isFullScreen)
    setResizable(!isFullScreen)
    if (isFullScreen)     // Full-screen mode
      device.setFullScreenWindow(this)
      validate()
    else                  // Windowed mode
      pack()
      setVisible(true)

class TruchetPanel extends JPanel(new GridLayout(1, 2)):
  override def paint(graphics: Graphics): Unit =
    val dimension = getSize()
    graphics.setColor(Color.BLACK)
    graphics.drawLine(0, 0, dimension.width, dimension.height)
    graphics.drawLine(0, 0, dimension.width/2, dimension.height)
    graphics.drawLine(0, 0, dimension.width, dimension.height/2)

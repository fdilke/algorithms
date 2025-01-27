package com.fdilke.truchet

import com.fdilke.backtrack.node.coloring.{ColorGraphTweakedLoop, NoEffortColoring}

import java.awt.*
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.util.Random

object TruchetApp extends App:
  val device: GraphicsDevice =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  // good sizes include (50, 30),
  // too much: (75, 45)
  val grid = new TruchetGrid(
    width = 80,
    height = 50,
    toroidal = true, // false,
    boolStream = Random(System.currentTimeMillis()), // Random(0L),
    colorGenerator = TruchetGrid.colorGenerator,
    algo = ColorGraphTweakedLoop
  )
  new TruchetFrame(device, grid)

class TruchetFrame(
  device: GraphicsDevice,
  grid: SquareHolder & TileHolder
) extends JFrame("Truchet Frame", device.getDefaultConfiguration):

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  initComponents()
  begin()

  private def initComponents(): Unit =
    val contentPane = getContentPane
    contentPane.setLayout(new BorderLayout)
    val panel = new TruchetPanel(grid)
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

class TruchetPanel(
  grid: SquareHolder & TileHolder
) extends JPanel(new GridLayout(1, 2)):
  override def paint(graphics: Graphics): Unit =
    val dimension = getSize()
    grid.draw(graphics, dimension)

object ShowRandomApp extends App:
  val rnd = new Random(0)
  for (i <- 0 until 7)
    println(s"${rnd.nextInt()}")
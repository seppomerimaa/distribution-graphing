//import io.StdIn._
import javax.swing.{JFrame, JComponent, Timer}
import java.awt.{Color,Dimension,Graphics,Graphics2D, BasicStroke}
import java.awt.geom._

/**
 * Draw some dots on a grid. For no reason at all.
 */
object Dots extends App{
  val fr = new JFrame("Dots") // your enclosing app window

  def cx(x:Double) = 250+25*x
  def cy(y:Double) = 250-25*y

  def mkLine(slope:Double, intercept:Double) = {
    val y1 = slope * -10 + intercept
    val y2 = slope * 10 + intercept
    new Line2D.Double(cx(-10.0), cy(y1), cx(10.0), cy(y2))
  }

  val colors = Set(Color.blue, Color.red, Color.green)
  var bgColor = Color.blue

  val canvas = new JComponent { // JComponent is a blank space on which you can draw
    override def getPreferredSize = new Dimension(500,500)
    override def paintComponent(g:Graphics){
      super.paintComponent(g)

      val gg = g.asInstanceOf[Graphics2D]
      gg.setColor(bgColor)
      gg.fill( new Rectangle2D.Double(cx(-10.0),cy(10.0),cx(10.0),cy(-10.0)))

      gg.setColor(Color.lightGray)
      (-10.0 to 10.0 by 1.0).foreach{
        y =>
          val horiz = new Line2D.Double(cx(-10.0),cy(y),cx(10.0),cy(y))
          val vert = new Line2D.Double( cx(y),cy(10),cx(y),cy(-10))

          gg.draw(horiz)
          gg.draw(vert)
      }

      gg.setStroke(new BasicStroke(3.0F))
      gg.setColor(Color.blue)

      (1 to 20).foreach { y=>
        val x = -10 + util.Random.nextInt(20)
        val y = -10 + util.Random.nextInt(20)
        val dot = new Line2D.Double(cx(x), cy(y), cx(x), cy(y))

        gg.draw(dot)
      }
    }
  }

  //canvas.setSize(1000, 1000)

  fr.getContentPane.add(canvas)
  fr.pack()
  fr.setVisible(true)

  while (true) {
    Thread.sleep(1000)
    bgColor = util.Random.shuffle(colors).head
    canvas.repaint()
  }
}

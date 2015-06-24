import java.awt._
import java.awt.event.{ActionListener, ActionEvent}
import javax.swing._
import java.awt.geom._
import org.apache.commons.math3.distribution
import org.apache.commons.math3.distribution.{AbstractIntegerDistribution, AbstractRealDistribution}

import scala.collection.mutable

case class Coords(x: Double, y: Double)

class Chart(xPixels: Int, yPixels: Int, var xMin: Double, var xMax: Double, var yMin: Double, var yMax: Double) extends JComponent {
  //val colors = Set(Color.white, Color.lightGray, Color.green)
  var color = Color.white
  var points: Seq[Coords] = Array[Coords]()
  var continuousMode = true

  def mx(x: Double) = (x - xMin) * (xPixels / (xMax - xMin))
  def my(y: Double) = yPixels - ((y - yMin) * (yPixels / (yMax - yMin)))
  def barWidth = xPixels * (9.0/10.0) / points.length

  override def getPreferredSize = new Dimension(xPixels, yPixels)
  override def paintComponent(g:Graphics) = {
    super.paintComponent(g)
    val gg = g.asInstanceOf[Graphics2D]
    gg.setColor(color)
    gg.fill(new Rectangle2D.Double(0, 0, xPixels, yPixels))

    gg.setColor(Color.blue)
    gg.setStroke(new BasicStroke(2.0f))
    gg.draw(new Line2D.Double(0.0, my(0.0), xPixels, my(0.0)))
    gg.draw(new Line2D.Double(mx(0.0), my(yMin), mx(0.0), my(yMax)))

    gg.setColor(Color.black)
    if (continuousMode && points.length > 1) {
      val pairs = points zip points.tail
      pairs.foreach {
        pair => {
          val segment = new Line2D.Double(pair._1.x, pair._1.y, pair._2.x, pair._2.y)
          gg.draw(segment)
        }
      }
    } else {
      points.foreach {
        point => {
          //println(point)
          // ugh fuck swing. or awt. whatever.
          val bar = new Rectangle2D.Double(point.x, point.y, barWidth, yPixels - point.y)
          gg.fill(bar)
        }
      }
    }

    gg.setColor(Color.red)
    gg.drawString(xMin.toString, 5, my(0.0).toInt - 5)
    gg.drawString(xMax.toString, xPixels - 25, my(0.0).toInt - 5)
    gg.drawString(yMin.toString, mx(0.0).toInt + 5, yPixels - 5)
    gg.drawString(yMax.toString, mx(0.0).toInt + 5, 12)
  }

  def continuousPlot(points: Seq[Coords]): Unit = {
    continuousMode = true
    plot(points)
  }
  
  def discretePlot(points: Seq[Coords]): Unit = {
    continuousMode = false
    plot(points)
  }

  private def plot(points: Seq[Coords]): Unit = {
    xMin = points.map(p => p.x).min
    xMax = points.map(p => p.x).max
    yMin = points.map(p => p.y).min
    yMax = points.map(p => p.y).max
    this.points = points.map(pt => new Coords(mx(pt.x), my(pt.y)))
    //println(this.points)
    repaint()
  }
}

abstract class StateChange()
case class DistributionTypeChange(newType: String) extends StateChange
case class ParamValueChange(distributionType: String, paramName: String, newValue: Double) extends StateChange
case class EventChange()

class ModelInputBox(distributions: Array[Distribution], stateChangedFunc: (StateChange) => Unit) extends JPanel {
  this.setLayout(new BorderLayout())
  val distributionTypes = distributions.map(d => d.name)
  var currentDistributionType = distributionTypes.head

  val paramInputBoxes = distributions.map(d => (d.name, new ParamInputBox(d, stateChangedFunc))).toMap
  this.add(paramInputBoxes(distributionTypes.head), BorderLayout.SOUTH)

  val distributionPicker: JComboBox[String] = new JComboBox(distributionTypes) {
    val handler = new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val cb: JComboBox[String] = e.getSource.asInstanceOf[JComboBox[String]]
        val selected: String = cb.getSelectedItem.asInstanceOf[String]
        stateChangedFunc(new DistributionTypeChange(selected))
        distributionTypeChanged(selected)
      }
    }
    this.addActionListener(handler)
  }

  this.add(distributionPicker, BorderLayout.NORTH)

  private def distributionTypeChanged(newType: String) = {
    this.removeAll()
    paramInputBoxes(newType).reset()
    this.add(distributionPicker, BorderLayout.NORTH)
    this.add(paramInputBoxes(newType), BorderLayout.SOUTH)

    this.revalidate()
    this.repaint()
  }
}

class ParamInputBox(distribution: Distribution, stateChangedFunc: (StateChange) => Unit) extends JPanel {
  val paramNames = distribution.values.keys
  val inputBoxes = paramNames.map {
    paramName => new ParamInputField(distribution.name, paramName, distribution.defaults(paramName), distribution.isContinuous, stateChangedFunc)
  }
  inputBoxes.foreach(b => this.add(b))

  def reset() = {
    inputBoxes.foreach(box => box.reset())
  }

  val xBox = new ParamInputField("", "x", 0.0, true, xChanged)
  val xOut = new JTextField("0.0", 5)

  val xOutBox = new JPanel() {
    if (distribution.isContinuous) this.add(new JLabel("P(X<=x)"))
    else this.add(new JLabel("P(X=x)"))
    this.add(xOut)
  }
  this.add(xBox)
  this.add(xOutBox)

  def xChanged(state: StateChange): Unit = {
    state match {
      case s: ParamValueChange => {
        val p = distribution.prob(s.newValue).toString
        val decIndex = p.indexOf('.')
        xOut.setText(p.substring(0, decIndex + 4))
      }
      case s: DistributionTypeChange => Unit
    }

  }
}

class ParamInputField(distributionType: String, paramName: String, defaultValue: Double, isContinuous: Boolean, stateChangedFunc: (StateChange) => Unit) extends JPanel with ActionListener {
  this.add(new JLabel(paramName))
  val field = new JTextField(defaultValue.toString, 5)
  field.addActionListener(this)
  this.add(field)

  override def actionPerformed(e: ActionEvent): Unit = {
    val tf: JTextField = e.getSource.asInstanceOf[JTextField]
    try {
      val v = tf.getText.toDouble
      stateChangedFunc(new ParamValueChange(distributionType, paramName, v))
    } catch {
      case e: NumberFormatException => Unit // YOLO
    }
  }

  def reset() = {
    field.setText(defaultValue.toString)
  }
}

abstract class Distribution(xMin: Double, xMax: Double) {
  val name: String
  val defaults: Map[String, Double]
  lazy val values: mutable.Map[String, Double] = mutable.Map(defaults.toSeq: _*)

  def calcPoints: Seq[Coords]

  def reset() = {
    defaults.map { case (k,v) => values(k) = v }
  }

  val isContinuous: Boolean

  def prob(x: Double): Double
}

abstract class ContinuousDistribution(xMin: Double, xMax: Double) extends Distribution(xMin: Double, xMax: Double) {
  def distr: AbstractRealDistribution
  override val isContinuous = true
  val stepSize: Double
  override def calcPoints = (xMin to xMax by stepSize).map(x => new Coords(x, distr.density(x)))
  override def prob(x: Double) = distr.cumulativeProbability(x)
}

abstract class DiscreteDistribution(xMin: Double, xMax: Double) extends Distribution(xMin: Double, xMax: Double) {
  def distr: AbstractIntegerDistribution
  override val isContinuous = false
  override def calcPoints = (xMin to xMax by 1.0).map(x => new Coords(x, distr.probability(x.toInt)))
  override def prob(x: Double) = distr.probability(x.toInt)
}

class NormalDistribution(xMin: Double, xMax: Double) extends ContinuousDistribution(xMin: Double, xMax: Double) {
  val name = "normal"
  val defaults = Map[String, Double]("mean" -> 0, "sd" -> 1)
  override def distr = new distribution.NormalDistribution(values("mean"), values("sd"))
  override val stepSize = .1
}

class BinomialDistribution(xMin: Double, xMax: Double) extends DiscreteDistribution(xMin: Double, xMax: Double) {
  val name = "binomial"
  val defaults = Map[String, Double]("n" -> 10, "p" -> .5)
  override def distr = new distribution.BinomialDistribution(values("n").toInt, values("p"))
}

class BetaDistribution(xMin: Double, xMax: Double) extends ContinuousDistribution(xMin: Double, xMax: Double) {
  val name = "beta"
  val defaults = Map[String, Double]("alpha" -> 0.5, "beta" -> 0.5)
  override def distr = new distribution.BetaDistribution(values("alpha"), values("beta"))
  override val stepSize = 0.01
}

class PoissonDistribution(xMin: Double, xMax: Double) extends DiscreteDistribution(xMin: Double, xMax: Double) {
  val name = "poisson"
  val defaults = Map[String, Double]("lambda" -> 4)
  override def distr = new distribution.PoissonDistribution(values("lambda").toInt)
}

class ExponentialDistribution(xMin: Double, xMax: Double) extends ContinuousDistribution(xMin: Double, xMax: Double) {
  val name = "exponential"
  val defaults = Map[String, Double]("lambda" -> 1.5)
  override def distr = new distribution.ExponentialDistribution(values("lambda"))
  override val stepSize = 1.0
}

class PascalDistribution(xMin: Double, xMax: Double) extends DiscreteDistribution(xMin: Double, xMax: Double) {
  val name = "pascal"
  val defaults = Map[String, Double]("r" -> 2, "p" -> .5)
  override def distr = new distribution.PascalDistribution(values("r").toInt, values("p"))
}



class ProbabilityDistributionsApp extends JPanel {
  this.setLayout(new BorderLayout())

  val chart = new Chart(600, 750, -10, 10, 0, 1)
  val points = (-10.0 to 10.0 by .005).map(x => new Coords(x, x*x))
  val points2 = (-10.0 to 10.0 by .005).map(x => new Coords(x, -(x*x) + 10.0))
  val points3 = (-10.0 to 10.0 by .005).map(x => new Coords(x, 2*x + 2))

  val distributions = Array[Distribution](
    new BinomialDistribution(0, 20),
    new NormalDistribution(-10, 10),
    new BetaDistribution(0.00001, 1),
    new PoissonDistribution(0, 20),
    new ExponentialDistribution(0, 10),
    new PascalDistribution(0, 20)
  )
  val distributionTypes = distributions.map(d => d.name)
  val box = new ModelInputBox(distributions, onStateChanged)
  chart.discretePlot(distributions.head.calcPoints)

  this.add(box, BorderLayout.NORTH)
  this.add(chart, BorderLayout.SOUTH)

  def onStateChanged(msg: StateChange) = {
    msg match {
      case msg: DistributionTypeChange => {
        val distribution = distributions.find(d => d.name == msg.newType).get
        distribution.reset()
        if (distribution.isContinuous) chart.continuousPlot(distribution.calcPoints)
        else chart.discretePlot(distribution.calcPoints)

      }
      case msg: ParamValueChange => {
        val distribution = distributions.find(d => d.name == msg.distributionType).get
        distribution.values(msg.paramName) = msg.newValue
        if (distribution.isContinuous) chart.continuousPlot(distribution.calcPoints)
        else chart.discretePlot(distribution.calcPoints)
      }
    }
  }

}

object ChartApp extends App {
  val fr = new JFrame("Chart")
  val pdApp = new ProbabilityDistributionsApp
//  val box = new ModelInputBox(Map("binomial" -> Array("n", "p"), "normal" -> Array("n", "p", "x")), println)



  fr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  fr.getContentPane.add(pdApp)
  fr.pack()
  fr.setVisible(true)
}

import java.awt.Point
import java.beans.{PropertyChangeEvent, PropertyChangeListener, PropertyChangeSupport}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Chapter10 {

  /**
   * Task 1:
   *
   * The `java.awt.Rectangle` class has useful methods `translate` and `grow`
   * that are unfortunately absent from classes such as `java.awt.geom.Ellipse2D`.
   * In Scala, you can fix this problem.
   * Define a trait `RectangleLike` with methods `translate` and `grow`. Provide any abstract
   * methods that you need for the implementation so that you can mix in the trait like this:
   * {{{
   *    val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
   *    egg.translate(10, -10)
   *    egg.grow(10, 20)
   * }}}
   */
  trait RectangleLike {
    def getX: Double
    def getY: Double
    def getWidth: Double
    def getHeight: Double

    def setFrame(x: Double, y: Double, w: Double, h: Double): Unit

    def translate(dx: Double, dy: Double): Unit = {
      var x = getX
      var y = getY
      var width = getWidth
      var height = getHeight

      var oldv = x
      var newv = oldv + dx
      if (dx < 0) {
        if (newv > oldv) {
          if (width >= 0) {
            width += newv - Integer.MIN_VALUE
          }
          newv = Integer.MIN_VALUE
        }
      }
      else {
        if (newv < oldv) {
          if (width >= 0) {
            width += newv - Integer.MAX_VALUE
            if (width < 0) width = Integer.MAX_VALUE
          }
          newv = Integer.MAX_VALUE
        }
      }
      x = newv
      oldv = y
      newv = oldv + dy
      if (dy < 0) {
        if (newv > oldv) {
          if (height >= 0) {
            height += newv - Integer.MIN_VALUE
          }
          newv = Integer.MIN_VALUE
        }
      }
      else {
        if (newv < oldv) {
          if (height >= 0) {
            height += newv - Integer.MAX_VALUE
            if (height < 0) height = Integer.MAX_VALUE
          }
          newv = Integer.MAX_VALUE
        }
      }
      y = newv

      setFrame(x, y, width, height)
    }

    def grow(h: Double, v: Double): Unit = {
      var x0 = getX
      var y0 = getY
      var x1 = getWidth
      var y1 = getHeight
      x1 += x0
      y1 += y0
      x0 -= h
      y0 -= v
      x1 += h
      y1 += v
      if (x1 < x0) {
        x1 -= x0
        if (x1 < Integer.MIN_VALUE) x1 = Integer.MIN_VALUE
        if (x0 < Integer.MIN_VALUE) x0 = Integer.MIN_VALUE
        else if (x0 > Integer.MAX_VALUE) x0 = Integer.MAX_VALUE
      }
      else {
        if (x0 < Integer.MIN_VALUE) x0 = Integer.MIN_VALUE
        else if (x0 > Integer.MAX_VALUE) x0 = Integer.MAX_VALUE
        x1 -= x0
        if (x1 < Integer.MIN_VALUE) x1 = Integer.MIN_VALUE
        else if (x1 > Integer.MAX_VALUE) x1 = Integer.MAX_VALUE
      }
      if (y1 < y0) {
        y1 -= y0
        if (y1 < Integer.MIN_VALUE) y1 = Integer.MIN_VALUE
        if (y0 < Integer.MIN_VALUE) y0 = Integer.MIN_VALUE
        else if (y0 > Integer.MAX_VALUE) y0 = Integer.MAX_VALUE
      }
      else {
        if (y0 < Integer.MIN_VALUE) y0 = Integer.MIN_VALUE
        else if (y0 > Integer.MAX_VALUE) y0 = Integer.MAX_VALUE
        y1 -= y0
        if (y1 < Integer.MIN_VALUE) y1 = Integer.MIN_VALUE
        else if (y1 > Integer.MAX_VALUE) y1 = Integer.MAX_VALUE
      }

      setFrame(x0, y0, x1, y1)
    }
  }

  /**
   * Task 2:
   *
   * Define a class `OrderedPoint` by mixing `scala.math.Ordered[Point]` into `java.awt.Point`.
   * Use lexicographic ordering, i e. (x, y) < (x’, y’) if x < x’ or x = x’ and y < y’.
   */
  class OrderedPoint(x: Int, y: Int) extends Point(x, y) with scala.math.Ordered[Point] {

    override def compare(that: Point): Int = {
      if (x < that.x || (x == that.x && y < that.y)) -1
      else if (x == that.x && y == that.y) 0
      else 1
    }
  }

  /**
   * Task 3:
   *
   * Look at the BitSet class, and make a diagram of all its superclasses and traits.
   * Ignore the type parameters (everything inside the [...]).
   * Then give the linearization of the traits.
   */
  def bitSetLinearization: List[String] = {
    val diagram = Map(
      "scala.collection.BitSet" -> List("scala.AnyRef",
        "scala.collection.SortedSet",
        "scala.collection.BitSetLike"),

      "scala.collection.SortedSet" -> List("scala.AnyRef",
        "scala.collection.Set",
        "scala.collection.SortedSetLike"),

      "scala.collection.Set" -> List("scala.AnyRef",
        "scala.Function1",
        "scala.collection.Iterable",
        "scala.collection.GenSet",
        "scala.collection.generic.GenericSetTemplate",
        "scala.collection.SetLike"),

      "scala.collection.SetLike" -> List("scala.AnyRef",
        "scala.collection.IterableLike",
        "scala.collection.GenSetLike",
        "scala.collection.generic.Subtractable",
        "scala.collection.Parallelizable"),

      "scala.collection.Parallelizable" -> List("scala.Any"),

      "scala.collection.generic.Subtractable" -> List("scala.AnyRef"),

      "scala.collection.GenSetLike" -> List("scala.AnyRef",
        "scala.collection.GenIterableLike",
        "scala.Function1",
        "scala.Equals",
        "scala.collection.Parallelizable"),

      "scala.Equals" -> List("scala.Any"),

      "scala.collection.GenTraversableOnce" -> List("scala.Any"),

      "scala.collection.IterableLike" -> List("scala.Any",
        "scala.Equals",
        "scala.collection.TraversableLike",
        "scala.collection.GenIterableLike"),

      "scala.collection.GenIterableLike" -> List("scala.Any",
        "scala.collection.GenTraversableLike"),

      "scala.collection.GenTraversableLike" -> List("scala.Any",
        "scala.collection.GenTraversableOnce",
        "scala.collection.Parallelizable"),

      "scala.collection.TraversableLike" -> List("scala.Any",
        "scala.collection.generic.HasNewBuilder",
        "scala.collection.generic.FilterMonadic",
        "scala.collection.TraversableOnce",
        "scala.collection.GenTraversableLike",
        "scala.collection.Parallelizable"),

      "scala.collection.TraversableOnce" -> List("scala.Any",
        "scala.collection.GenTraversableOnce"),

      "scala.collection.generic.FilterMonadic" -> List("scala.Any"),

      "scala.collection.GenSet" -> List("scala.AnyRef",
        "scala.collection.GenSetLike",
        "scala.collection.GenIterable",
        "scala.collection.generic.GenericSetTemplate"),

      "scala.collection.GenIterable" -> List("scala.AnyRef",
        "scala.collection.GenIterableLike",
        "scala.collection.GenTraversable",
        "scala.collection.generic.GenericTraversableTemplate"),

      "scala.collection.GenTraversable" -> List("scala.AnyRef",
        "scala.collection.GenTraversableLike",
        "scala.collection.GenTraversableOnce",
        "scala.collection.generic.GenericTraversableTemplate"),

      "scala.collection.generic.GenericSetTemplate" -> List("scala.AnyRef",
        "scala.collection.generic.GenericTraversableTemplate"),

      "scala.collection.generic.GenericTraversableTemplate" -> List("scala.AnyRef",
        "scala.collection.generic.HasNewBuilder"),

      "scala.collection.generic.HasNewBuilder" -> List("scala.Any"),

      "scala.Function1" -> List("scala.AnyRef"),

      "scala.AnyRef" -> List("scala.Any", "java.lang.Object"),
      "scala.Any" -> Nil,
      "java.lang.Object" -> Nil,

      "scala.collection.Iterable" -> List("scala.AnyRef",
        "scala.collection.Traversable",
        "scala.collection.GenIterable",
        "scala.collection.generic.GenericTraversableTemplate",
        "scala.collection.IterableLike"),

      "scala.collection.Traversable" -> List("scala.AnyRef",
        "scala.collection.TraversableLike",
        "scala.collection.GenTraversable",
        "scala.collection.TraversableOnce",
        "scala.collection.generic.GenericTraversableTemplate"),


      "scala.collection.SortedSetLike" -> List("scala.AnyRef",
        "scala.collection.generic.Sorted",
        "scala.collection.SetLike"),

      "scala.collection.generic.Sorted" -> List("scala.AnyRef"),

      "scala.collection.BitSetLike" -> List("scala.AnyRef",
        "scala.collection.SortedSetLike")
    )

    val buffer = new ListBuffer[String]

    def lin(clazz: String): Unit = {
      buffer += clazz
      for (c <- diagram(clazz).reverse) {
        lin(c)
      }
    }

    lin("scala.collection.BitSet")

    // remove duplicates from the right
    val set = new mutable.LinkedHashSet[String]
    set ++= buffer.reverse

    // remove AnyRef since its represented as java.lang.Object
    set.toList.reverse.filter(_ != "scala.AnyRef")
   }

  /**
   * Task 4:
   *
   * Provide a `CryptoLogger` trait that encrypts the log messages with the Caesar cipher.
   * The key should be 3 by default, but it should be overridable by the user.
   * Provide usage examples with the default key and a key of -3.
   */
  trait Logger {
    def log(msg: String): Unit
  }

  trait CryptoLogger extends Logger {
    val key: Int = 3

    abstract override def log(msg: String): Unit = {
      super.log(msg.map(c => (c + key).toChar))
    }
  }

  /**
   * Task 5:
   *
   * The JavaBeans specification has the notion of a property change listener, a standardized
   * way for beans to communicate changes in their properties. The `PropertyChangeSupport` class
   * is provided as a convenience superclass for any bean that wishes to support property change
   * listeners. Unfortunately, a class that already has another superclass - such as JComponent -
   * must reimplement the methods. Reimplement `PropertyChangeSupport` as a trait,
   * and mix it into the `java.awt.Point` class.
   */
  trait PropertyChangeSupportLike {
    private val support = new PropertyChangeSupport(this)

    def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
      support.addPropertyChangeListener(listener)

    def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
      support.removePropertyChangeListener(listener)

    def getPropertyChangeListeners: Array[PropertyChangeListener] =
      support.getPropertyChangeListeners

    def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit =
      support.addPropertyChangeListener(propertyName, listener)

    def removePropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit =
      support.removePropertyChangeListener(propertyName, listener)

    def getPropertyChangeListeners(propertyName: String): Array[PropertyChangeListener] =
      support.getPropertyChangeListeners(propertyName)

    def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any): Unit =
      support.firePropertyChange(propertyName, oldValue, newValue)

    def firePropertyChange(propertyName: String, oldValue: Int, newValue: Int): Unit =
      support.firePropertyChange(propertyName, oldValue, newValue)

    def firePropertyChange(propertyName: String, oldValue: Boolean, newValue: Boolean): Unit =
      support.firePropertyChange(propertyName, oldValue, newValue)

    def firePropertyChange(event: PropertyChangeEvent): Unit =
      support.firePropertyChange(event)

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Any, newValue: Any): Unit =
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Int, newValue: Int): Unit =
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Boolean, newValue: Boolean): Unit =
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

    def hasListeners(propertyName: String): Boolean =
      support.hasListeners(propertyName)
  }

  class PointBean(x: Int = 0, y: Int = 0) extends Point(x, y) with PropertyChangeSupportLike
}

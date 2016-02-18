import java.awt.Point

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
   *
   * Solution:
   * {{{
   * Classes:
   * -------------------------------------------------------------------------
   * mutable.BitSet extends mutable.AbstractSet
   *                   with mutable.SortedSet
   *                   with BitSet
   *                   with BitSetLike
   *                   with mutable.SetLike
   *                   with scala.Serializable
   *
   * mutable.AbstractSet extends mutable.AbstractIterable
   *                        with mutable.Set
   *
   * mutable.AbstractIterable extends AbstractIterable
   *                             with mutable.Iterable
   *
   * AbstractIterable extends AbstractTraversable
   *                     with Iterable
   *
   * AbstractTraversable extends AnyRef
   *                        with Traversable
   *
   * Traits:
   * -------------------------------------------------------------------------
   * mutable.Set with mutable.Iterable
   *             with Set
   *             with generic.GenericSetTemplate
   *             with mutable.SetLike
   *
   * mutable.SetLike with SetLike
   *                 with script.Scriptable
   *                 with mutable.Builder
   *                 with generic.Growable
   *                 with generic.Shrinkable
   *                 with mutable.Cloneable
   *                 with Parallelizable
   *
   * mutable.Iterable with mutable.Traversable
   *                  with Iterable
   *                  with generic.GenericTraversableTemplate
   *                  with IterableLike
   *                  with Parallelizable
   *
   * mutable.SortedSet with SortedSet
   *                   with SortedSetLike
   *                   with mutable.Set
   *                   with mutable.SetLike
   *
   * Set with Function1
   *     with Iterable
   *     with GenSet
   *     with generic.GenericSetTemplate
   *     with SetLike
   *
   * Iterable with Traversable
   *          with GenIterable
   *          with generic.GenericTraversableTemplate
   *          with IterableLike
   *
   * Traversable with TraversableLike
   *             with GenTraversable
   *             with TraversableOnce
   *             with generic.GenericTraversableTemplate
   *
   * BitSet with SortedSet
   *        with BitSetLike
   *
   * BitSetLike with SortedSetLike
   *
   * scala.Serializable with java.io.Serializable
   * }}}
   */
}

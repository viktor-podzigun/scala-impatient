

object Chapter18 {

  /**
   * Task 1:
   *
   * Implement a `Bug` class modeling a bug that moves along a horizontal line.
   * The `move` method moves in the current direction,
   * the `turn` method makes the bug turn around,
   * and the `show` method prints the current position.
   * Make these methods chainable. For example,
   * {{{
   *   bugsy.move(4).show().move(6).show().turn().move(5).show()
   * }}}
   * should display `4 10 5`.
   */
  class Bug {

    private var position: Int = 0
    private var turnedAround: Boolean = false

    def move(steps: Int): this.type = {
      if (turnedAround) position -= steps
      else position += steps
      this
    }

    def turn(): this.type = {
      turnedAround = !turnedAround
      this
    }

    def show(): this.type = {
      print(" ")
      print(position)
      this
    }
  }

  /**
   * Task 2:
   *
   * Provide a fluent interface for the `Bug` class of the preceding exercise, so that one can
   * write
   * {{{
   *   bugsy move 4 and show and then move 6 and show turn around move 5 and show
   * }}}
   */
  trait FluentBug { this: Bug =>

    def and(obj: Show.type): this.type = this.show()

    def and(obj: Then.type): this.type = this

    def turn(obj: Around.type): this.type = this.turn()
  }

  object Show
  object Then
  object Around

  val show = Show
  val then = Then
  val around = Around
}

package main

import scala.math.abs

object Main extends App:

  println("Hello, Scala!")

  // Task 2a

  // value syntax assigned to function literal
  val positiveVal: Int => Boolean = _ match
    case n if n >= 0 => true
    case _ => false

  // method syntax
  def positiveMeth(n: Int): Boolean = n match
    case n if n >= 0 => true
    case _ => false

  println("Test - Task 2a")
  println("Expected true, Actual: " + positiveVal(5))
  println("Expected true, Actual: " + positiveMeth(5))

  // neg function
  val empty: String => Boolean = _ == "" // predicate on strings
  def negMeth(fun: String => Boolean): String => Boolean = (string: String) => !fun(string)
  // underscore version
  def negUnderscore(fun: String => Boolean): String => Boolean = !fun(_)
  // val version
  val negVal: (String => Boolean) => (String => Boolean) = f => string => !f(string)
  val notEmpty = negMeth(empty) // which type of notEmpty?

  println("Expected true, Actual: " + notEmpty("foo"))
  println("Expected false, Actual: " + notEmpty(""))
  println("Expected true, Actual: " + (notEmpty("foo") && !notEmpty("")))

  def negGenericMeth[A](fun: A => Boolean): A => Boolean = string => !fun(string)
  val isZero: Int => Boolean = _ == 0
  val notZero = negGenericMeth(isZero)

  println("Expected true, Actual: " + notZero(1))
  println("Expected false, Actual: " + notZero(0))

  // Task 2b
  // CurriedFunType
  val p1: Double => Double => Double => Boolean = x => y => z => x <= y && y == z
  // NonCurriedFunType
  val p2: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z
  // method with Currying
  def p3(x: Double)(y: Double)(z: Double): Boolean = x <= y && y == z
  // method without Currying
  def p4(x: Double, y: Double, z: Double): Boolean = x <= y && y == z

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  // val version
  val composeVal: (Int => Int, Int => Int) => Int => Int = (f, g) => x => f(g(x))

  println("Test - Task 2b")
  println("Expected 9, Actual: " + compose(_ - 1, _ * 2)(5))

  def composeGen[A](f: A => A, g: A => A): A => A = x => f(g(x))
  // there is a constraint, the method cannot infer the type
  println("Expected 9, Actual: " + composeGen[Int](_ - 1, _ * 2)(5))

  // Task 3
  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case _ if a % b == 0 => b
    case _ if a > b => gcd(a, a % b)
    case _ => gcd(b, b % a)

  println("Test - Task 3")
  println("Expected (4, 7), Actual: " + (gcd(12, 8), gcd(14, 7)))

  // Task 4
  enum Shape:
    case Rectangle(base: Double, height: Double)
    case Circle(radius: Double)
    case Square(edge: Double)

  object Shape:

    def perimeter(shape: Shape): Double = shape match
      case Rectangle(base, height) => base * height
      case Circle(radius) => 2 * radius * Math.PI
      case Square(edge) => edge * edge

    // Please note, it is assumed that the shape has (0,0) coordinates in its center
    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(base, height), (x, y)) => abs(x) <= abs(base / 2) && abs(y) <= abs(height / 2)
      case (Circle(radius), (x, y)) => abs(x) <= radius && abs(y) <= radius
      case (Square(edge), (x, y)) => abs(x) <= abs(edge / 2) && abs(y) <= abs(edge / 2)

  import Shape.*
  println("Test - Task 4")

  val rectangle = Rectangle(4, 5)
  val circle = Circle(10)
  val square = Square(5)
  println("Expected true, Actual: " + (20 == perimeter(rectangle)))
  println("Expected true, Actual: " + ((2 * 10 * Math.PI) == perimeter(circle)))
  println("Expected true, Actual: " + ((5.0 * 5.0) == perimeter(square)))

  println("Expected true, Actual: " + contains(rectangle, (2, 2)))
  println("Expected false, Actual: " + contains(rectangle, (-5, 2)))
  println("Expected true, Actual: " + contains(circle, (10, 2)))
  println("Expected false, Actual: " + contains(circle, (100, 30)))
  println("Expected true, Actual: " + contains(square, (1, 2.5)))
  println("Expected false, Actual: " + contains(square, (26, 25)))

  // Task 5
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(predicate: A => Boolean): Option[A] = opt match
      case Some(value) if predicate(value) => Some(value)
      case _ => None()

    def map[A](opt: Option[A])(predicate: A => Boolean): Option[Boolean] = opt match
      case Some(value) if predicate(value) => Some(true)
      case Some(value) if !predicate(value) => Some(false)
      case _ => None()

    def fold[A](opt: Option[A])(default: A)(operator: A => A): A = opt match
      case Some(value) => operator(value)
      case _ => default

  import Option.*

  println("Test - Task 5")

  println("Expected Some(5), Actual: " + filter(Some(5))(_ > 2))
  println("Expected None(), Actual: " + filter(Some(5))(_ > 8))
  println("Expected None(), Actual: " + filter(None[Int]())(_ > 2))

  println("Expected Some(true), Actual: " + map(Some(5))(_ > 2))
  println("Expected Some(false), Actual: " + map(Some(5))(_ > 8))
  println("Expected None(), Actual: " + map(None[Int]())(_ > 2))

  println("Expected 6, Actual: " + fold(Some(5))(1)(_ + 1)) // 6
  println("Expected 1, Actual: " + fold(None[Int]())(1)(_ + 1)) // 1

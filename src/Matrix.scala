import scala.reflect.ClassTag

trait Cursor[T] {
  def get(rel : (Int, Int)*) : Seq[T] = rel.map(a => get(a._1, a._2))

  def get(relX: Int, relY: Int): T

  def apply(): T = get(0, 0)
}

class Matrix[T: ClassTag](cells: Array[Array[T]], default: T) {
  val rows = cells.length
  require(rows > 0)

  val columns = cells(0).length
  require(columns > 0)
  require(cells.filter(_.length != columns).isEmpty)

  def this(columns: Int, rows: Int, default: T) = {
    this(Matrix.makeArray[T](columns, rows, default), default)
  }

  private def inBounds(x : Int, y : Int) = x >= 0 && x < columns && y >= 0 && y < rows

  def defaultValue = default

  def get(x: Int, y: Int): T = if (inBounds(x, y)) cells(y)(x) else default

  def map[U: ClassTag](fn: Cursor[T] => U, default: U): Matrix[U] = {
    val newCells = Array.ofDim[U](rows, columns)
    for {y <- 0 until cells.length
         row = cells(y)
         newRow = newCells(y)
         x <- 0 until row.length
    } {
      newRow(x) = fn(cursor(x, y))
    }
    new Matrix[U](newCells, default)
  }

  private def cursor(x: Int, y: Int) = new Matrix.CursorImp[T](this, x, y)

  override def toString() = cells.map(_.mkString(" ")).mkString("\n")
}

object Matrix {
  private def makeArray[T: ClassTag](columns: Int, rows: Int, initial: T): Array[Array[T]] =
    Array.fill[Array[T]](rows) {
      Array.fill[T](columns)(initial)
    }

  def zip[A, B](a: Matrix[A], b: Matrix[B]): Matrix[(A, B)] = {
    val newCells = Array.ofDim[(A, B)](a.rows, a.columns)
    for {y <- 0 until a.rows
      newRow = newCells(y)
      x <- 0 until a.columns
    } {
        newRow(x) = (a.get(x, y), b.get(x, y))
    }

    new Matrix[(A, B)](newCells, (a.defaultValue, b.defaultValue))
  }

  def combine[A, B, C: ClassTag](a: Matrix[A], b: Matrix[B], fn: Cursor[(A, B)] => C, default: C): Matrix[C] = {
    val newCells = Array.ofDim[C](a.rows, a.columns)
    for {y <- 0 until a.rows
         newRow = newCells(y)
         x <- 0 until a.columns
    } {
      newRow(x) = fn(new ZipCursorImp[A, B](a, b, x, y))
    }

    new Matrix[C](newCells, default)
  }

  class CursorImp[A : ClassTag](matrix: Matrix[A], x: Int, y: Int) extends Cursor[A] {
    def get(relX: Int, relY: Int) = matrix.get(x + relX, y + relY)
  }

  class ZipCursorImp[A, B](matrix1: Matrix[A], matrix2: Matrix[B], x: Int, y: Int) extends Cursor[(A, B)] {
    def get(relX: Int, relY: Int) = (matrix1.get(x + relX, y + relY), matrix2.get(x + relX, y + relY))
  }
}


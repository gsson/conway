package se.fnord.conway

import scala.reflect.ClassTag

trait Cursor[T] {
  def get(rel : (Int, Int)*) : Seq[T] = rel.map(a => get(a._1, a._2))

  def get(relX: Int, relY: Int): T

  def apply(): T = get(0, 0)
}

abstract class Matrix[T : ClassTag] {
  val rows : Int
  val columns : Int

  def rowIndices = Stream.range(0, rows)
  def columnIndices = Stream.range(0, columns)

  def get(x : Int, y : Int) : T

  def map[U: ClassTag](fn: Cursor[T] => U, default: U): Matrix[U]

  def view(x : Int, y : Int, w : Int, h : Int) : Matrix[T]

  def cursor(x: Int, y: Int) : Cursor[T] = new CursorImp[T](this, x, y)
}

object Matrix {
  def combine[A, B, C: ClassTag](a: Matrix[A], b: Matrix[B], fn: Cursor[(A, B)] => C, default: C): Matrix[C] = {
    require(a.rows == b.rows)
    require(a.columns == b.columns)

    val newCells = for {
      y <- a.rowIndices
    } yield for {
        x <- a.columnIndices
      } yield fn(new ZipCursorImp[A, B](a, b, x, y))

    Matrix[C](newCells, default)
  }

  def apply[T : ClassTag](cells: Seq[Seq[T]], default: T): Matrix[T] = new MatrixImpl[T](cells, default)

  def apply[T : ClassTag](columns: Int, rows: Int, default: T): Matrix[T] = new MatrixImpl[T](columns, rows, default)
}

private class CursorImp[A : ClassTag](matrix: Matrix[A], x: Int, y: Int) extends Cursor[A] {
  def get(relX: Int, relY: Int) = matrix.get(x + relX, y + relY)
}

private class ZipCursorImp[A, B](matrix1: Matrix[A], matrix2: Matrix[B], x: Int, y: Int) extends Cursor[(A, B)] {
  def get(relX: Int, relY: Int) = (matrix1.get(x + relX, y + relY), matrix2.get(x + relX, y + relY))
}

private class MatrixViewImpl[T : ClassTag](parent : Matrix[T], offsetX : Int, offsetY : Int, override val rows : Int, override val columns : Int) extends Matrix[T] {
  def get(x: Int, y: Int): T = parent.get(x + offsetX, y + offsetY)

  def map[U: ClassTag](fn: (Cursor[T]) => U, default: U): Matrix[U] = new MatrixViewImpl(parent.map(fn, default), offsetX, offsetY, rows, columns)

  def view(x: Int, y: Int, w: Int, h: Int): Matrix[T] = parent.view(x + offsetX, y + offsetY, w, h)
}


private class MappingMatrixView[T, U : ClassTag](parent : Matrix[T], fn: (Cursor[T]) => U, default : U) extends Matrix[U] {
  val rows: Int = parent.rows

  val columns: Int = parent.columns

  def get(x: Int, y: Int): U = fn(parent.cursor(x, y))

  def view(x: Int, y: Int, w: Int, h: Int): Matrix[U] = new MatrixViewImpl(this, x, y, w, h)

  def map[V: ClassTag](mappingFunction: (Cursor[U]) => V, default: V): Matrix[V] = new MappingMatrixView[U, V](this, mappingFunction, default)

}


private class MatrixImpl[T : ClassTag](cells: Seq[Seq[T]], default: T) extends Matrix[T]{
  val rows = cells.length
  require(rows > 0)

  val columns = cells(0).length
  require(columns > 0)
  require(!cells.exists(_.length != columns))

  def this(columns: Int, rows: Int, default: T) = {
    this(MatrixImpl.makeArray[T](columns, rows, default), default)
  }

  private def inBounds(x : Int, y : Int) = x >= 0 && x < columns && y >= 0 && y < rows

  def get(x: Int, y: Int): T = if (inBounds(x, y)) cells(y)(x) else default

  def map[U: ClassTag](fn: Cursor[T] => U, default: U): Matrix[U] = new MappingMatrixView[T, U](this, fn, default)

  def view(x: Int, y: Int, w: Int, h: Int): Matrix[T] = new MatrixViewImpl[T](this, x, y, w, h);

  override def toString() = cells.map(_.mkString(" ")).mkString("\n")

}

private object MatrixImpl {
  private def makeArray[T: ClassTag](columns: Int, rows: Int, initial: T): Seq[Seq[T]] =
    Array.fill(rows, columns)(initial).asInstanceOf
}


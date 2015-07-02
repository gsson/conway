package se.fnord.conway

trait Cell {
  def nextState(liveNeighbors : Int) : Cell
}

object DeadCell extends Cell {
  private def canReproduce(liveNeighbors : Int) = liveNeighbors == 3

  def nextState(liveNeighbors: Int) =
    if (canReproduce(liveNeighbors)) LiveCell
    else DeadCell

  override def toString = "."
}

object LiveCell extends Cell {
  private def underPopulation(liveNeighbors : Int) = liveNeighbors < 2
  private def overcrowding(liveNeighbors : Int) = liveNeighbors > 3
  private def equilibrium(liveNeighbors : Int) = (liveNeighbors == 2) || (liveNeighbors == 3)

  def nextState(liveNeighbors: Int) =
    if (underPopulation(liveNeighbors)) DeadCell
    else if (equilibrium(liveNeighbors)) LiveCell
    else if (overcrowding(liveNeighbors)) DeadCell
    else LiveCell

  override def toString = "*"
}

object Engine {
  type Board = Matrix[Cell]

  private def countAlive(c: Cell) = if (c == LiveCell) 1 else 0

  private def checkNeighbors[T, U](cursor: Cursor[T], mapper: (T) => U, reducer: (U, U) => U) =
    cursor.get(
      (-1, -1), ( 0, -1), ( 1, -1),
      (-1,  0),           ( 1,  0),
      (-1,  1), ( 0,  1), ( 1,  1))
      .map(mapper)
      .reduce(reducer)

  private def countLivingNeighbors(c: Cursor[Cell]) = checkNeighbors[Cell, Int](c, countAlive, (a, b) => a + b)

  private def neighborMap(board: Board) = board.map[Int](countLivingNeighbors, 0)

  def evolve(board: Board) = Matrix.combine[Cell, Int, Cell](board, neighborMap(board), c => c()._1.nextState(c()._2), DeadCell)
}

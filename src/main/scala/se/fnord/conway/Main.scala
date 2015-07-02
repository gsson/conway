package se.fnord.conway

object Main {
  def run(board : Engine.Board) = {
    var c = board
    while (true) {
      println(c)
      println()
      Thread.sleep(1000)
      c = Engine.evolve(c)
    }
  }


  def main(args: Array[String]) {
    val cells = Matrix(Array[Seq[Cell]](
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, LiveCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, LiveCell, LiveCell, LiveCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, LiveCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
      Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell)
    ), DeadCell)

    run(cells)
  }
}

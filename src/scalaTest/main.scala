package scalaTest

/**
  * Created by Stefan Adam on 7/22/2016.
  */
object main {
  def main(args: Array[String]): Unit = {

    val sudokuGrid = new SudokuGrid(2)
    val initList = List[GridCoordinate](new GridCoordinate(1, 1, 2), new GridCoordinate(1, 2, 5), new GridCoordinate(1, 2, 4))
    sudokuGrid.initializeGrid(initList)
    sudokuGrid.showData(sudokuGrid.tableInitValues)
    val solutions = sudokuGrid.computeSolution()
    sudokuGrid.showData(solutions.head)
    sudokuGrid.showData(solutions.tail.head)
    
    println(solutions.length)
  }
}





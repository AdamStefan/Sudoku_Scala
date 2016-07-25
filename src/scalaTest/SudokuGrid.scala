package scalaTest

/**
  * Created by Stefan Adam on 7/25/2016.
  */
class SudokuGrid(unitSize:Int) {
  val width: Int = unitSize * unitSize
  val height: Int = unitSize * unitSize
  val tableInitValues = Array.ofDim[Int](height, width)


  def initializeGrid(list: List[GridCoordinate]): Unit = {
    //val cellValues: Array.ofDim[Int](width,height)
    val ttt = new GridCoordinate(1, 2, 3)

    //    for( a <- list){
    //      a match {
    //        case  dd: GridCoordinate=>{
    //          tableInitValues(dd.X)(dd.Y) = dd.Value
    //          println(dd.Value)
    //        }
    //      }
    //    }

    for (a <- list) {
      tableInitValues(a.X)(a.Y) = a.Value
    }
  }

  def showData(grid: Array[Array[Int]]): Unit = {
    println("*****************************************")
    for (i <- 0 to this.height - 1) {
      var line = ""
      for (j <- 0 to this.width - 1) {
        line = line + " " + (grid(i)(j)).toString()
      }
      println(line)
    }
    println("*****************************************")
  }


  def copy(source: Array[Array[Int]]):Array[Array[Int]] = {
    val result = Array.ofDim[Array[Int]](source.length)
    for(x <- 0 until source.length) {
      result(x) = Array.ofDim[Int](source(x).length)
      for(y <- 0 until source(x).length) {
        result(x)(y) = source(x)(y)
      }
    }
    return result
  }

  def computeSolution(): List[Array[Array[Int]]] = {
    var solution =  copy(tableInitValues)
    var itemsToSet = List[GridCoordinate]()
    var solutions = List[Array[Array[Int]]]()

    for (i <- 0 to this.height - 1) {
      for (j <- 0 to this.width - 1) {
        if (tableInitValues(i)(j) == 0) {
          itemsToSet = new GridCoordinate(j, i, tableInitValues(i)(j)) :: itemsToSet
        }
      }
    }

    var itemsToResolve = itemsToSet.toArray

    var continue = true
    var currentItemIndex = 0

    while (currentItemIndex >= 0) {
      var currentGridItem = itemsToResolve(currentItemIndex) // get the current coordinate which needs to be updated

      if (solution(currentGridItem.Y)(currentGridItem.X) < (this.unitSize* this.unitSize)) {
        // in case the next value is in the valid range

        val nextValue = solution(currentGridItem.Y)(currentGridItem.X) + 1
        currentGridItem.Value = nextValue
        solution(currentGridItem.Y)(currentGridItem.X) = nextValue

        if (isValidValue(currentGridItem, solution)) {
          // in case the value is valid in the grid
          if (currentItemIndex == itemsToResolve.length - 1) {
            solutions = solution :: solutions
            solution = copy(solution)
            solution(itemsToResolve(currentItemIndex).Y)(itemsToResolve(currentItemIndex).X) = 0
            currentItemIndex = currentItemIndex - 1
          }
          else {
            currentItemIndex = currentItemIndex + 1
          }
        }
      }
      else {
        solution(itemsToResolve(currentItemIndex).Y)(itemsToResolve(currentItemIndex).X) = 0
        currentItemIndex = currentItemIndex - 1
      }
    }

    return solutions
  }

  def isValidValue(gridCoordinate: GridCoordinate, grid: Array[Array[Int]]): Boolean = {
    for (i <- 0 to this.height - 1) {
      val currentRowValue = grid(i)(1)
      if (i != gridCoordinate.Y && grid(i)(gridCoordinate.X) > 0 && grid(i)(gridCoordinate.X) == gridCoordinate.Value) {
        return false
      }
    }

    for (i <- 0 to this.width - 1) {
      val currentRowValue = grid(i)(1)
      if (i != gridCoordinate.X && grid(gridCoordinate.Y)(i) > 0 & grid(gridCoordinate.Y)(i) == gridCoordinate.Value) {
        return false
      }
    }

    val startRowSquare = unitSize * (gridCoordinate.Y / unitSize)
    val startColumnSquare = unitSize * (gridCoordinate.X / unitSize)

    for (i <- startRowSquare to startRowSquare + unitSize-1) {
      for (j <- startColumnSquare to startColumnSquare + unitSize-1) {
        if (grid(i)(j) > 0 && i != gridCoordinate.Y && j != gridCoordinate.X && grid(i)(j) == gridCoordinate.Value) {
          return false
        }
      }
    }

    return true
  }

}

case class GridCoordinate(X:Int, Y:Int, var Value:Int)

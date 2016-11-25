package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      println("terrain = " + terrain)
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      println("testing: optimal solution for level 1")
      assert(solve(solution) == Block(goal, goal))
    }
  }

//  test("state space 2") {
//    new Level1 {
//      val tp = pathsFromStart.take(150).toList
//      println("testing: length = " + tp.length)
//      println("testing: tp = " + tp)
//
//      //assert(solve(solution) == Block(goal, goal))
//    }
//  }

  test("pos equality") {
    new Level1 {
      val p1 = Pos(1,2)
      val p2 = Pos(1,2)
      val p3 = Pos(2,2)
      assert(p1 == p2, "1")
      assert(p1 equals  p2, "2")
      assert(p3 != p2, "3")
    }
  }

  test("Block equality") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(1, 2))
      val b2 = Block(Pos(1,2), Pos(1, 2))
      val b3 = Block(Pos(2,1), Pos(2, 1))
      assert(b1 == b2)
      assert(b1 equals  b2)
      assert(b3 != b2)
    }
  }

  test("Block contains") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(1, 2))
      val b2 = Block(Pos(1,2), Pos(1, 2))
      val b3 = Block(Pos(2,1), Pos(2, 1))
      val b4 = Block(Pos(3,1), Pos(3, 1))

      val list = List[Block](b1, b2, b3)
      assert(list contains b1)
      assert( !(list contains b4))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      println("testing: optimal solution length for level 1")
      assert(solution.length == optsolution.length)
    }
  }
//
  test("neighbours with history for level 1") {
    new Level1 {
      val res = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(res.toSet.size == expected.size)
      assert(res.toSet == expected)
    }
  }

  test("legal neighbours ") {
    new Level1 {
      val b1 = Block(Pos(1,1),Pos(1,1))
      val ln = b1.legalNeighbors map(b => b._1)

      assert(2 == ln.length)
    }
  }

  test("new neighbours only with level 1") {
    new Level1 {
      val res = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )

      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(res == expected)
    }
  }




}

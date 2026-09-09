package tetris

import infrastructure.ScoreCounter
import org.junit.runner.RunWith
import org.scalatest.{Args, Status, Suites}
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TetrisTestSuite3_2 extends TetrisTestSuitesBase(
  new Test01_Placement,
  new Test02_Rotation,
  new Test03_RotateBackToStart,
  new Test04_Movement,
  new Test05_Drop,
  new Test06_Blocked,
  new Test07_Spawn,
  new Test08_ClearLines,
  new Test09_GameOver,
  new Test10_FullGame)
  {

    val MaxPoints = 5.5

    override def run(testName: Option[String], args: Args): Status = {
      val (scoreCounter, res) = runWithScoreCounter(testName,args)
      printf("You got %d/%d points!\n", scoreCounter.points, scoreCounter.maxPoints)
      printf("Your base grade for the tetris exercise will be : %.2f\n",scoreCounter.fraction() * MaxPoints)
      res
    }

  }

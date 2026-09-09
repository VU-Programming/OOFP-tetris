package tetris

import infrastructure.ScoreCounter
import org.scalatest.events.{Event, SuiteAborted, TestCanceled, TestFailed, TestIgnored, TestPending, TestStarting, TestSucceeded}
import org.scalatest.{Args, Reporter, Status, Suites}

import scala.collection.immutable.ListSet

abstract class TetrisTestSuitesBase(suites: TetrisTestSuiteBase*) extends Suites() {
  private case class FlatTest(
    suite: TetrisTestSuiteBase,
    originalName: String,
    reportedName: String
  )

  private val flatTests: Seq[FlatTest] =
    suites.flatMap { suite =>
      val testNumberWidth = suite.testNames.size.toString.length
      suite.testNames.toSeq.zipWithIndex.map { case (testName, index) =>
        val suiteName = suite.getClass.getSimpleName
        val testNumber = s"%0${testNumberWidth}d".format(index + 1)
        FlatTest(suite, testName, s"$suiteName${testNumber}_$testName")
      }
    }

  private val flatTestsByName: Map[String, FlatTest] =
    flatTests.map(test => test.reportedName -> test).toMap

  override def testNames: Set[String] =
    ListSet.from(flatTests.map(_.reportedName))

  override protected def runTest(testName: String, args: Args): Status = {
    val test = flatTestsByName.getOrElse(
      testName,
      throw new IllegalArgumentException(s"Unknown test: $testName")
    )
    val reporter = new FlatTestReporter(args.reporter, test)
    test.suite.run(Some(test.originalName), args.copy(reporter = reporter))
  }

  def runWithScoreCounter(testName: Option[String], args: Args): (ScoreCounter,Status) = {
    val scoreCounter = new ScoreCounter()
    val newArgs =
      args.copy(configMap = args.configMap.updated("scoreCounter",scoreCounter))
    val res = runDirect(testName,newArgs)
    (scoreCounter,res)
  }

  // run without making a new scorecounter
  def runDirect(testName: Option[String], args: Args): Status = {
    super.run(testName, args)
  }

  private class FlatTestReporter(delegate: Reporter, test: FlatTest) extends Reporter {
    private val outerSuiteName = TetrisTestSuitesBase.this.suiteName
    private val outerSuiteId = TetrisTestSuitesBase.this.suiteId
    private val outerSuiteClassName = Some(TetrisTestSuitesBase.this.getClass.getName)
    private val reportedName = test.reportedName

    private def trimFrameworkFrames(throwable: Throwable): Throwable = {
      val stackTrace = throwable.getStackTrace
      val firstTestFrame = stackTrace.indexWhere(_.getClassName == test.suite.getClass.getName)
      if (firstTestFrame >= 0) throwable.setStackTrace(stackTrace.drop(firstTestFrame))
      throwable
    }

    override def apply(event: Event): Unit = delegate(event match {
      case e: TestStarting =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName)
      case e: TestSucceeded =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName)
      case e: TestFailed =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName,
          throwable = e.throwable.map(trimFrameworkFrames))
      case e: TestIgnored =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName)
      case e: TestCanceled =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName,
          throwable = e.throwable.map(trimFrameworkFrames))
      case e: TestPending =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName, testName = reportedName, testText = reportedName)
      case e: SuiteAborted =>
        e.copy(suiteName = outerSuiteName, suiteId = outerSuiteId,
          suiteClassName = outerSuiteClassName)
      case other => other
    })
  }
}

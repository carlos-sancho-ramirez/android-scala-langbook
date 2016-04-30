package sword.langbook.android.test

import java.util.Locale

import android.app.{Activity, Instrumentation}
import android.os.Bundle
import android.test.InstrumentationTestRunner
import android.util.Log

object PerformanceTestInstrumentation {

  /**
   * List of all recognized arguments that can be entered through the command line as key-value
   * string pairs by using the '-e' parameter.
   */
  object ExpectedArguments {

    /**
     * Enables performance tests.
     * Performance tests are desabled by default to avoid taking more time than required,
     * as these tests assume that all works fine and are here to measure how much time it takes
     * perform some actions.
     *
     * "true" or any insensitive alternative for true, false otherwise
     */
    val enablePerformanceTests = "enablePerformanceTests"
  }

  /**
   * Status codes recognised by the Android system through the InstrumentationResultParser.
   *
   * This list is extracted from InstrumentationResultParser.StatusCodes so the values should not be
   * changed. They are copied as they are private
   */
  object ResultCodes {
    val assumptionFailure = -4
    val ignored = -3
    val failure = -2
    val error = -1
    val success = 0
    val start = 1
    val inProgress = 2
  }
}

/**
 * Instrumentation to run performance tests.
 *
 * This instrumentation is not included in the manifest by default. To include it just ensure that
 * the following line is included and not commented in the build.sbt file.
 *   instrumentTestRunner in Android := "sword.langbook.android.test.PerformanceTestInstrumentation"
 *
 * This instrumentation can be run from the command line using the following command
 * $ adb shell am instrument -w sword.langbook.android/.test.PerformanceTestInstrumentation
 */
class PerformanceTestInstrumentation extends Instrumentation {

  private def extractBoolean(bundle: Bundle, key: String): Boolean = {
    Option(bundle).flatMap(b => Option(b.getString(key)))
      .exists("true" == _.toLowerCase(Locale.ENGLISH))
  }

  private var _performanceTestsEnabled = false
  private var _currentMessageIndex = 0

  override def onCreate(args: Bundle): Unit = {
    _performanceTestsEnabled = extractBoolean(args,
        PerformanceTestInstrumentation.ExpectedArguments.enablePerformanceTests)

    val str = new StringBuilder("onCreate executed with arguments:")
    import scala.collection.JavaConversions.asScalaSet
    for (key <- args.keySet()) {
      val value = Option(args.get(key)).map(_.toString).getOrElse("<null>")
      str append s" $key => $value "
    }
    Log.i(getClass.getSimpleName, str.toString)

    start()
  }

  private def report(resultCode: Int, className: String, methodName: String, message: String) = {
    val testResult = new Bundle()
    testResult.putString(InstrumentationTestRunner.REPORT_KEY_NAME_CLASS, className)
    testResult.putString(InstrumentationTestRunner.REPORT_KEY_NAME_TEST, methodName)
    _currentMessageIndex += 1
    testResult.putInt(InstrumentationTestRunner.REPORT_KEY_NUM_CURRENT, _currentMessageIndex)
    testResult.putString(Instrumentation.REPORT_KEY_STREAMRESULT, message)
    sendStatus(resultCode, testResult)
  }

  private def runPerformanceTest(className: String, methodName: String, method: () => String) = {
    report(PerformanceTestInstrumentation.ResultCodes.start, className, methodName, s"  - $methodName...")
    val msg = measureTimeFor(method) + "\n"
    report(PerformanceTestInstrumentation.ResultCodes.success, className, methodName, msg)
  }

  override def onStart(): Unit = {
    if (_performanceTestsEnabled) {
      val testsInstance = LinkedStorageManagerTests(this)
      val suiteName = testsInstance.getClass.getName
      runPerformanceTest(suiteName, "allWordTests", testsInstance.allWordTexts _)
    }

    val results = new Bundle()
    results.putString(Instrumentation.REPORT_KEY_STREAMRESULT, "All tests run\n")
    finish(Activity.RESULT_OK, results)
  }

  private def measureTimeFor(f: () => String): String = {
    val startTime = System.currentTimeMillis
    val result = f()
    val totalTime = System.currentTimeMillis - startTime
    s" $totalTime milliseconds\n$result"
  }
}

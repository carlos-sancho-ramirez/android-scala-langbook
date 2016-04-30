package sword.langbook.android.test

import android.app.{Activity, Instrumentation}
import android.os.Bundle
import android.test.InstrumentationTestRunner
import android.util.Log

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

  /**
   * Extracted from InstrumentationResultParser.StatusCodes...
   * We need to copy them because they are private
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

  override def onCreate(args: Bundle): Unit = {
    val str = new StringBuilder("onCreate executed with arguments:")
    import scala.collection.JavaConversions.asScalaSet
    for (key <- args.keySet()) {
      val value = Option(args.get(key)).map(_.toString).getOrElse("<null>")
      str append s" $key => $value "
    }
    Log.i(getClass.getSimpleName, str.toString)

    start()
  }

  private var _currentMessageIndex = 0

  private def report(resultCode: Int, className: String, methodName: String) = {
    val testResult = new Bundle()
    testResult.putString(InstrumentationTestRunner.REPORT_KEY_NAME_CLASS, className)
    testResult.putString(InstrumentationTestRunner.REPORT_KEY_NAME_TEST, methodName)
    _currentMessageIndex += 1
    testResult.putInt(InstrumentationTestRunner.REPORT_KEY_NUM_CURRENT, _currentMessageIndex)
    val message = s"Code $resultCode on $className#$methodName\n"
    testResult.putString(Instrumentation.REPORT_KEY_STREAMRESULT, message)
    sendStatus(resultCode, testResult)
  }

  override def onStart(): Unit = {
    report(ResultCodes.start, getClass.getName, "testNameForOK")
    report(ResultCodes.success, getClass.getName, "testNameForOK")

    report(ResultCodes.start, getClass.getName, "testNameForOK2")
    report(ResultCodes.success, getClass.getName, "testNameForOK2")

    report(ResultCodes.start, getClass.getName, "testNameForKO")
    report(ResultCodes.failure, getClass.getName, "testNameForKO")

    report(ResultCodes.start, getClass.getName, "testNameForKO2")
    report(ResultCodes.failure, getClass.getName, "testNameForKO2")

    val results = new Bundle()
    results.putString(Instrumentation.REPORT_KEY_STREAMRESULT, "All OK, thanks for asking ;)\n")
    finish(Activity.RESULT_OK, results)
  }
}

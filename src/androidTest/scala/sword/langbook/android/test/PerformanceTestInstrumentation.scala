package sword.langbook.android.test

import android.app.{Activity, Instrumentation}
import android.os.Bundle

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

  override def onCreate(args: Bundle): Unit = {
    start()
  }

  override def onStart(): Unit = {
    val results = new Bundle()
    results.putString(Instrumentation.REPORT_KEY_STREAMRESULT, "All OK, thanks for asking ;)")
    finish(Activity.RESULT_OK, results)
  }
}

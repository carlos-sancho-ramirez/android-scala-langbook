package sword.langbook.android

import android.os.Build

/**
  * Class where all code that depends on a given version should refer to.
  *
  * This should help to find all app pieces of code related to a given OS
  * version and make more maintainable when upgrading versions.
  */
object VersionUtils {

  object Android {
    val isAtLeast17 = Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1;
  }

  object SQLite {
    val isAtLeast3_7_11 = Android.isAtLeast17
  }
}

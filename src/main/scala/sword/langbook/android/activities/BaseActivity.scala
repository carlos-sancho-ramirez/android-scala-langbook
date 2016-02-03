package sword.langbook.android.activities

import android.app.Activity
import sword.langbook.android.{LangbookApplication, TypedFindView}

/**
  * Base for all activities within the Langbook project.
  * This class is mainly intended to centralise common code.
  */
class BaseActivity extends Activity with TypedFindView {
  def linkedDb = getApplication.asInstanceOf[LangbookApplication].linkedDb
}

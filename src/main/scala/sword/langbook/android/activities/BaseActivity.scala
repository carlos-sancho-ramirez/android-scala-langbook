package sword.langbook.android.activities

import android.support.v7.app.AppCompatActivity
import sword.langbook.android.{LangbookApplication, TypedFindView}

/**
  * Base for all activities within the Langbook project.
  * This class is mainly intended to centralise common code.
  */
class BaseActivity extends AppCompatActivity with TypedFindView {
  def linkedDb = getApplication.asInstanceOf[LangbookApplication].linkedDb
}

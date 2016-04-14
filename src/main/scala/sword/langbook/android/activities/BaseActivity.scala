package sword.langbook.android.activities

import java.util.Locale

import android.content.Intent
import android.support.v7.app.AppCompatActivity
import sword.langbook.android.{LangbookApplication, TypedFindView}
import sword.langbook.db.LinkedStorageManager

/**
  * Base for all activities within the Langbook project.
  * This class is mainly intended to centralise common code.
  */
class BaseActivity extends AppCompatActivity with TypedFindView {
  def linkedDb = getApplication.asInstanceOf[LangbookApplication].linkedDb

  def questionBuilder = getApplication.asInstanceOf[LangbookApplication].questionBuilder
  def questionBuilder_=(builder: LinkedStorageManager => Option[sword.langbook.Question]): Unit = {
    getApplication.asInstanceOf[LangbookApplication].questionBuilder = builder
  }

  /**
   * Returns the language preferred by the user.
   * It should be configurable within the app to display the language the user want to learn or its
   * native language.
   * Right now it is collecting the current language set through the global Android settings.
   */
  lazy val preferredLanguage = {
    val languages = linkedDb.languages.values
    val settingsLanguage = languages.find(_.code == Locale.getDefault.getLanguage)
    settingsLanguage.getOrElse {
      val english = languages.find(_.code == Locale.ENGLISH.getLanguage)
      english.getOrElse(languages.head)
    }
  }

  /**
   * This is just here to force onActivityResult to be public. Then fragments can call it when
   * finishing.
   */
  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    super.onActivityResult(requestCode, resultCode, data)
  }
}

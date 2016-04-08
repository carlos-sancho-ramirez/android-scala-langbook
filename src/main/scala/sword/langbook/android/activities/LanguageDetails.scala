package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.LinearLayoutManager
import sword.langbook.android.{TR, R}
import sword.langbook.db.{Language, Word}

object LanguageDetails {
  private val className = "sword.langbook.android.activities.LanguageDetails"

  def openWith(activity :Activity, requestCode :Int = 0, languageEncodedKey: String = null) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.languageKey, languageEncodedKey)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class LanguageDetails extends BaseActivity {

  lazy val languageKeyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.languageKey))
  lazy val languageOption = languageKeyOption.map(Language(_))

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)

    val toolBar = findView(TR.toolBar)
    val recyclerView = findView(TR.recyclerView)
    for (language <- languageOption) {
      for (title <- language.suitableTextForLanguage(preferredLanguage)) {
        toolBar.setTitle(title)
      }

      recyclerView.setLayoutManager(new LinearLayoutManager(this))
      recyclerView.setAdapter(new LanguageDetailsAdapter(preferredLanguage, "", language.alphabets.toVector))
    }
  }
}

package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.LinearLayoutManager
import sword.langbook.android.{TR, R}
import sword.langbook.db.Language

object LanguageDetails {
  private val className = "sword.langbook.android.activities.LanguageDetails"

  def openWith(activity :Activity, requestCode :Int = 0, language: Language = null, languageEncodedKey: String = null) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    val languageEncoded = {
      if (language != null) language.key.encoded
      else languageEncodedKey
    }
    intent.putExtra(BundleKeys.languageKey, languageEncoded)

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

      val wordCount = language.wordCount
      val statistics = Vector(("Number of words", wordCount.toString))
      recyclerView.setLayoutManager(new LinearLayoutManager(this))
      recyclerView.setAdapter(new LanguageDetailsAdapter(this, preferredLanguage, "", language.alphabets.toVector, statistics))
    }
  }
}

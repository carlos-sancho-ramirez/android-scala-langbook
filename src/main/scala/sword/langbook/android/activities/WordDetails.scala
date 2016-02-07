package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.text.TextUtils
import android.view.View
import sword.langbook.android.{TR, R}
import sword.langbook.db.Word

object WordDetails {
  private val className = "sword.langbook.android.activities.WordDetails"

  def openWith(activity :Activity, requestCode :Int, word :Word) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.wordKey, word.key.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordDetails extends BaseActivity {

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)
    //findView(TR.toolBar).setTitle(getIntent.getStringExtra(BundleKeys.wordKey))

    val keyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.wordKey))
    for {
      key <- keyOption
      word <- linkedDb.words.get(key)
    } {
      // Assumed for now that all words have the first alphabet and language and are the ones to be
      // displayed
      // TODO: Remove this assumption
      val alphabet = linkedDb.alphabets.values.head
      val text = word.pieces.flatMap(_.get(alphabet)).flatMap(x => x)
        .map(_.unicode.toChar).mkString("")
      findView(TR.toolBar).setTitle(text)

      // TODO: Display the word attached instead of the concept hint
      findView(TR.languageText).setText(word.language.concept.hint)
    }
  }
}

package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.{GridLayoutManager, LinearLayoutManager}
import sword.langbook.android.{TR, R}
import sword.langbook.db.Alphabet

object AlphabetDetails {
  private val className = "sword.langbook.android.activities.AlphabetDetails"

  def openWith(activity :Activity, requestCode :Int = 0, alphabetEncodedKey: String = null) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.alphabetKey, alphabetEncodedKey)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class AlphabetDetails extends BaseActivity {

  lazy val alphabetKeyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.alphabetKey))
  lazy val alphabetOption = alphabetKeyOption.map(Alphabet(_))

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)

    val toolBar = findView(TR.toolBar)
    val recyclerView = findView(TR.recyclerView)
    for (alphabet <- alphabetOption) {
      for (title <- alphabet.suitableTextForLanguage(preferredLanguage)) {
        toolBar.setTitle(title)
      }

      val adapter = AlphabetDetailsAdapter(this, alphabet)
      recyclerView.setLayoutManager(new GridLayoutManager(this, adapter.spanCount))
      recyclerView.setAdapter(adapter)
    }
  }
}

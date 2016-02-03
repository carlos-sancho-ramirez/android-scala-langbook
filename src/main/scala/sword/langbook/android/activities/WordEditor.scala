package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.text.TextUtils
import android.view.View
import sword.langbook.android.{TR, R}

object WordEditor {
  private val className = "sword.langbook.android.activities.WordEditor"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordEditor extends BaseActivity with View.OnClickListener {

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_editor)

    findViewById(R.id.saveButton).setOnClickListener(this)
  }

  override def onClick(v: View): Unit = {
    val word = findView(TR.wordField).getText.toString

    // TODO: This has to be changed to include words instead of concepts
    if (!TextUtils.isEmpty(word)) {
      linkedDb.storageManager.insert(sword.langbook.db.registers.Concept(word))
    }

    setResult(Activity.RESULT_OK)
    finish()
  }
}

package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.LinearLayoutManager
import sword.langbook.android.{TR, R}
import sword.langbook.db.Symbol

object SymbolDetails {
  private val className = "sword.langbook.android.activities.SymbolDetails"

  def openWith(activity :Activity, symbol :Symbol, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.symbolKey, symbol.key.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class SymbolDetails extends BaseActivity {
  lazy val symbolKeyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.symbolKey))
  lazy val symbolOption = symbolKeyOption.map(Symbol(_))

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)
    findView(TR.recyclerView).setLayoutManager(new LinearLayoutManager(this))
    updateUi()
  }

  def updateUi(): Unit = {
    val title = symbolOption.map(_.text).getOrElse("")
    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(title)

    for (symbol <- symbolOption) {
      findView(TR.recyclerView).setAdapter(new SymbolDetailsAdapter(this, symbol))
    }
  }
}

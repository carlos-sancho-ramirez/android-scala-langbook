package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.LinearLayoutManager
import android.view.{MenuItem, Menu}
import android.widget.Toast
import sword.langbook.android.{TR, R}
import sword.langbook.db.{Concept, Word}

object ConceptPicker {
  private val className = "sword.langbook.android.activities.ConceptPicker"

  def openWith(activity :Activity, requestCode :Int, concepts :Iterable[Concept]) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    val keys: Array[String] = concepts.map(_.key.encoded).toArray
    intent.putExtra(BundleKeys.conceptKeys, keys)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class ConceptPicker extends BaseActivity {

  lazy val concepts = {
    val encodedKeys = getIntent.getStringArrayExtra(BundleKeys.conceptKeys)
    val storageManager = linkedDb.storageManager
    encodedKeys.flatMap(encoded => storageManager.decode(encoded).map(Concept(_))).toVector
  }

  lazy val adapter = new ConceptPickerAdapter(this, concepts.flatMap(c => c.hintOpt).toList)

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.concept_picker)
    setSupportActionBar(findView(TR.toolBar))

    // TODO: Using hints as temporal solution until we implement a proper way to distinguish among concepts
    val recyclerView = findView(TR.recyclerView)
    recyclerView.setAdapter(adapter)
    recyclerView.setLayoutManager(new LinearLayoutManager(this))
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.concept_picker, menu)
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.continueOption =>
        val conceptKeys = adapter.selected.map(index => concepts(index).key.encoded).toArray
        val intent = new Intent()
        intent.putExtra(BundleKeys.conceptKeys, conceptKeys)
        setResult(Activity.RESULT_OK, intent)
        finish()
        true
      case _ => false
    }
  }
}

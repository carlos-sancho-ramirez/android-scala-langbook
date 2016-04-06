package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.LinearLayoutManager
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

  def concepts: Set[Concept] = {
    val encodedKeys = getIntent.getStringArrayExtra(BundleKeys.conceptKeys)
    val storageManager = linkedDb.storageManager
    encodedKeys.flatMap(encoded => storageManager.decode(encoded).map(Concept(_))).toSet
  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.concept_picker)
    setSupportActionBar(findView(TR.toolBar))

    // TODO: Using hints as temporal solution until we implement a proper way to distinguish among concepts
    val recyclerView = findView(TR.recyclerView)
    recyclerView.setAdapter(new ConceptPickerAdapter(this, concepts.flatMap(c => c.hintOpt).toList))
    recyclerView.setLayoutManager(new LinearLayoutManager(this))
  }
}

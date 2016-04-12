package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view._
import android.widget.{AbsListView, BaseAdapter, AdapterView}
import sword.db.ForeignKeyField
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._
import sword.langbook.db.registers

import scala.collection.Set

object Selector {
  private val className = "sword.langbook.android.activities.Selector"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Selector extends BaseActivity with AdapterView.OnItemClickListener with SelectorChoiceModeCallback {

  lazy val listView = findView(TR.listView)

  class Adapter extends BaseAdapter {
    lazy val items = linkedDb.words.values.toList

    override def getItemId(position: Int) = position
    override def getCount = items.size
    override def getItem(position: Int) = items(position)

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = {
        if (convertView != null) convertView
        else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)
      }
      val text = items(position).suitableText.getOrElse("")
      view.findView(TR.selectorEntry).setText(text)
      view
    }
  }

  def invalidateAdapter() :Unit = {
    listView.setAdapter(new Adapter)
  }

  override def onCreate(savedInstanceState :Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.selector)

    listView.setOnItemClickListener(this)
    listView.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE_MODAL)
    listView.setMultiChoiceModeListener(SelectorChoiceModeListener(this))

    invalidateAdapter()

    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(R.string.appName)
    setSupportActionBar(toolBar)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.selector, menu)
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.newWordOption =>
        WordEditor.openWith(this, RequestCodes.addNewWord)
        true
      case _ => false
    }
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    val word = parent.getAdapter.asInstanceOf[Adapter].getItem(position)
    WordDetails.openWith(this, RequestCodes.checkWordDetails, word)
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.`addNewWord` => if (resultCode == Activity.RESULT_OK) invalidateAdapter()
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }

  override def delete(positions: Set[Int]): Unit = {
    val adapter = listView.getAdapter.asInstanceOf[Adapter]
    val linked = linkedDb
    val manager = linked.storageManager
    for {
      position <- positions
    } {
      val wordKey = adapter.getItem(position).key

      // Delete the WordConcept relation
      manager.getMapFor(registers.WordConcept).filter(_._2.fields.collectFirst {
        case field: ForeignKeyField if field.definition.target == registers.Word && field.key == wordKey =>
          true
      }.isDefined).keys.foreach(manager.delete)

      // Delete the word
      if (!manager.delete(wordKey)) {
        throw new AssertionError("Unable to remove word")
      }
    }

    if (positions.nonEmpty) {
      invalidateAdapter()
    }
  }
}

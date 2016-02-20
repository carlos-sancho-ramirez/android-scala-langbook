package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.Toolbar
import android.view._
import android.widget.AbsListView.MultiChoiceModeListener
import android.widget.{Toast, AbsListView, BaseAdapter, AdapterView}
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._

object Selector {
  private val className = "sword.langbook.android.activities.Selector"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Selector extends BaseActivity with AdapterView.OnItemClickListener {

  lazy val listView = findView(TR.listView)

  class Adapter extends BaseAdapter {
    lazy val items = linkedDb.words.values.toList

    override def getItemId(position: Int) = position
    override def getCount = items.size
    override def getItem(position: Int) = items(position)

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = if (convertView != null) convertView
      else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)

      // Assumed for now that all words have the first alphabet and language and are the ones to be
      // displayed
      // TODO: Remove this assumption
      val text = items(position).pieces.flatMap(_.values.headOption).flatMap(x => x)
          .map(_.unicode.toChar).mkString("")
      view.findView(TR.entryCaption).setText(text)
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
    listView.setMultiChoiceModeListener( new MultiChoiceModeListener {

      private val selected = scala.collection.mutable.BitSet()

      override def onItemCheckedStateChanged(mode: ActionMode, position: Int, id: Long, checked: Boolean): Unit = {
        // Nothing to be done here
      }

      override def onDestroyActionMode(mode: ActionMode): Unit = {
        selected.clear()
      }

      override def onCreateActionMode(mode: ActionMode, menu: Menu): Boolean = {
        selected.clear()
        mode.getMenuInflater.inflate(R.menu.selector_multichoice_mode, menu)
        true
      }

      override def onActionItemClicked(mode: ActionMode, item: MenuItem): Boolean = {
        Toast.makeText(Selector.this, "Delete action not implemented yet", Toast.LENGTH_SHORT).show()
        true
      }

      override def onPrepareActionMode(mode: ActionMode, menu: Menu): Boolean = {
        // Nothing to be done
        false
      }
    })

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
}

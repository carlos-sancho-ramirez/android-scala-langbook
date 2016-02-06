package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.Toolbar
import android.view._
import android.widget.{TextView, BaseAdapter, AdapterView}
import sword.langbook.android.{TR, R}

object Selector {
  private val className = "sword.langbook.android.activities.Selector"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Selector extends BaseActivity with Toolbar.OnMenuItemClickListener with AdapterView.OnItemClickListener {

  lazy val listView = findView(TR.listView)

  class Adapter extends BaseAdapter {
    lazy val items = linkedDb.words.values.toList

    // Assumed for now that all words have the first alphabet and language and are the ones to be
    // displayed
    // TODO: Remove this assumption
    lazy val alphabet = linkedDb.alphabets.values.head
    lazy val language = linkedDb.languages.values.head

    override def getItemId(position: Int) = position
    override def getCount = items.size
    override def getItem(position: Int) = {
      items(position).pieces.flatMap(_.get(alphabet)).flatMap(x => x).map(_.unicode.toChar).mkString("")
    }

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = if (convertView != null) convertView
      else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)
      view.findViewById(R.id.entryCaption).asInstanceOf[TextView].setText(getItem(position))
      view
    }
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.selector, menu)
  }

  def invalidateAdapter() :Unit = {
    listView.setAdapter(new Adapter)
  }

  override def onCreate(savedInstanceState :Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.selector)

    listView.setOnItemClickListener(this)
    invalidateAdapter()

    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(R.string.appName)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    item.getItemId match {
      case R.id.newWordOption =>
        WordEditor.openWith(this, RequestCodes.newWord)
        true
      case _ => false
    }
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    // TODO: To be implemented to open the word details
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.newWord => if (resultCode == Activity.RESULT_OK) invalidateAdapter()
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }
}

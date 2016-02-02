package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.Toolbar
import android.view._
import android.widget.{TextView, BaseAdapter, ListView, AdapterView}
import sword.db._
import sword.langbook.android.R
import sword.langbook.android.db.SQLiteStorageManager
import sword.langbook.db.LinkedStorageManager

object Selector {
  private val className = "sword.langbook.android.activities.Selector"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Selector extends Activity with Toolbar.OnMenuItemClickListener with AdapterView.OnItemClickListener {

  lazy val linkedDb = new LinkedStorageManager(defs => new SQLiteStorageManager(Selector.this, defs))
  lazy val listView = findViewById(R.id.listView).asInstanceOf[ListView]

  class Adapter extends BaseAdapter {
    lazy val items = linkedDb.concepts.values.toList

    override def getItemId(position: Int) = position
    override def getCount = items.size
    override def getItem(position: Int) = items(position)

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = if (convertView != null) convertView
      else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)

      val item = getItem(position)
      val textView = view.findViewById(R.id.entryCaption).asInstanceOf[TextView]
      textView.setText(item.hint)
      view
    }
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.selector, menu)
  }

  override def onCreate(savedInstanceState :Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.selector)

    listView.setOnItemClickListener(this)
    listView.setAdapter(new Adapter)

    val toolBar = findViewById(R.id.toolBar).asInstanceOf[Toolbar]
    toolBar.setTitle(R.string.appName)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    item.getItemId match {
      case R.id.newWordOption =>
        // TODO: Including the WordEditor and open it from here
        true
      case _ => false
    }
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    // TODO: To be implemented to open the word details
  }
}

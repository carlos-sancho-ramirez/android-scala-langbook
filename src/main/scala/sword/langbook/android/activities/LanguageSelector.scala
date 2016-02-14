package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.Toolbar
import android.view._
import android.widget.{Toast, BaseAdapter, AdapterView}
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._

object LanguageSelector {
  private val className = "sword.langbook.android.activities.LanguageSelector"

  def openWith(activity: Activity, requestCode: Int = 0, excludedLanguageEncodedKey: String = null) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (excludedLanguageEncodedKey != null) {
      intent.putExtra(BundleKeys.excludedLanguageKey, excludedLanguageEncodedKey)
    }

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class LanguageSelector extends BaseActivity with Toolbar.OnMenuItemClickListener with AdapterView.OnItemClickListener {

  lazy val listView = findView(TR.listView)

  class Adapter extends BaseAdapter {
    lazy val items = linkedDb.languages.values.toList.filter(_.key.encoded !=
      getIntent.getStringExtra(BundleKeys.excludedLanguageKey))

    override def getItemId(position: Int) = position
    override def getCount = items.size
    override def getItem(position: Int) = items(position)

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = if (convertView != null) convertView
      else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)

      // Right now each language is displayed with the first alphabet and its same language.
      // But it would be valuable that the user could see the languages listed in its preferred language.
      // However, when this code was written, there was no way to select the preferred language.
      // TODO: Display the preferred language and its suitable alphabet
      val language = items(position)
      val text = language.concept.wordsForLanguage(language).headOption
          .flatMap(_.text.values.headOption).getOrElse("")
      view.findView(TR.entryCaption).setText(text)
      view
    }
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.language_selector, menu)
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
    toolBar.setTitle(R.string.languageSelectorTitle)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    item.getItemId match {
      case R.id.newLanguageOption =>
        // TODO: Implement a way to add languages
        Toast.makeText(this, "Unable to add languages yet", Toast.LENGTH_SHORT).show()
        true
      case _ => false
    }
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    val language = parent.getAdapter.asInstanceOf[Adapter].getItem(position)
    val intent = new Intent()
    intent.putExtra(BundleKeys.languageKey, language.key.encoded)
    setResult(Activity.RESULT_OK, intent)
    finish()
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.`addNewWord` => if (resultCode == Activity.RESULT_OK) invalidateAdapter()
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }
}

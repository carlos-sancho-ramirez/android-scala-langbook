package sword.langbook.android.activities

import java.util.Locale

import android.app.{Activity, SearchManager}
import android.content.{Context, Intent}
import android.os.Bundle
import android.support.v7.widget.SearchView
import android.view._
import android.widget.{AbsListView, AdapterView, BaseAdapter}
import sword.db.ForeignKeyField
import sword.langbook.android.{R, TR}
import sword.langbook.android.TypedResource._
import sword.langbook.db.{Bunch, Selectable, Word, registers}

import scala.collection.Set

object Selector {
  private val className = "sword.langbook.android.activities.Selector"

  sealed class VisibilityFlags private[Selector] (val intValue: Int) {
    def showWords = (intValue & 1) != 0
    def showBunches = (intValue & 2) != 0

    override def hashCode = intValue
    override def equals(other: Any) =
        other != null && other.isInstanceOf[VisibilityFlags] &&
        other.asInstanceOf[VisibilityFlags].intValue == intValue
  }

  val onlyWords = new VisibilityFlags(1)
  val onlyBunches = new VisibilityFlags(2)
  val wordsAndBunches = new VisibilityFlags(3)

  def openWith(activity :Activity, visibilityFlags: VisibilityFlags, requestCode: Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    intent.putExtra(BundleKeys.visibilityFlags, visibilityFlags.intValue)
    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Selector extends BaseActivity with AdapterView.OnItemClickListener with SelectorChoiceModeCallback with SearchView.OnQueryTextListener {

  lazy val listView = findView(TR.listView)
  lazy val visibilityFlags = new Selector.VisibilityFlags(getIntent.getIntExtra(
      BundleKeys.visibilityFlags, Selector.wordsAndBunches.intValue))

  class Adapter extends BaseAdapter {
    def foundWordTexts = linkedDb.storageManager.allStringArray.map { case (x,y) => (Word(x), y)}
    lazy val allWordTexts = foundWordTexts.map { case (word, texts) =>
      val newTexts = texts.flatMap(Word.normalisedText)
      (word, texts ++ newTexts)
    }

    lazy val allBunchNames: Map[Bunch, String] = linkedDb.storageManager.getMapFor(registers.Bunch).map { case (key, reg) =>
      (Bunch(key), reg.name)
    }.toMap

    private var _query = ""
    private var _items: IndexedSeq[Selectable] = evaluateItems(_query)

    override def getItemId(position: Int) = position
    override def getCount = _items.size
    override def getItem(position: Int) = _items(position)

    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      android.util.Log.i(getClass.getSimpleName, "getView called for position " + position)

      val view = {
        if (convertView != null) convertView
        else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)
      }

      val text = _items(position).suitableText.getOrElse("")
      view.findView(TR.selectorEntry).setText(text)
      view
    }

    private def evaluateWords(query: String): IndexedSeq[Word] = {
      if (query != null && query.nonEmpty) {
        val normalisedQuery = query.toLowerCase(Locale.ENGLISH)
        allWordTexts.flatMap {
          case (word, strings) =>
            if (strings.exists(_.contains(normalisedQuery))) Some(word)
            else None
        }.toVector
      }
      else allWordTexts.keySet.toVector
    }

    private def evaluateBunches(query: String): IndexedSeq[Bunch] = {
      if (query != null && query.nonEmpty) {
        val normalisedQuery = query.toLowerCase(Locale.ENGLISH)
        allBunchNames.collect {
          case (bunch, name) if name.contains(normalisedQuery) => bunch
        }.toVector
      }
      else allBunchNames.keySet.toVector
    }

    private def evaluateItems(query: String): IndexedSeq[Selectable] = {
      visibilityFlags match {
        case Selector.onlyWords => evaluateWords(query)
        case Selector.onlyBunches => evaluateBunches(query)
        case Selector.wordsAndBunches => evaluateWords(query) ++ evaluateBunches(query)
      }
    }

    private def updateItems(): Unit = {
      _items = evaluateItems(_query)
      notifyDataSetChanged()
    }

    def setQuery(query: String) = {
      val q = {
        if (query == null) ""
        else query
      }

      if (_query != q) {
        _query = q
        updateItems()
      }
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

    val searchManager = getSystemService(Context.SEARCH_SERVICE).asInstanceOf[SearchManager]
    val searchView = menu.findItem(R.id.searchOption).getActionView.asInstanceOf[SearchView]
    searchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName))
    searchView.setIconifiedByDefault(false)
    searchView.setOnQueryTextListener(this)
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
    val item = parent.getAdapter.asInstanceOf[Adapter].getItem(position)
    item match {
      case word: Word => WordDetails.openWith(this, RequestCodes.checkWordDetails, word)
      case _ =>
    }
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
      manager.getMapFor(registers.Acceptation).filter(_._2.fields.collectFirst {
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

  override def onQueryTextSubmit(query: String): Boolean = {
    // Nothing to be done
    false
  }

  override def onQueryTextChange(query: String): Boolean = {
    listView.getAdapter.asInstanceOf[Adapter].setQuery(query)
    true
  }
}

package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{Toast, AdapterView, BaseAdapter}
import sword.langbook.InterAlphabetQuestion
import sword.langbook.android.db.SQLiteStorageManager
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._

object QuizSelector {
  private val className = "sword.langbook.android.activities.QuizSelector"

  def openWith(activity: Activity, requestCode: Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class QuizSelector extends BaseActivity with AdapterView.OnItemClickListener {

  object quizTypes {
    val interAlphabetKanaKanji = "inter-alphabet (kana -> kanji)"
    val interAlphabetKanjiKana = "inter-alphabet (kanji -> kana)"
    val synonym = "synonym"
    val translation = "translation"
  }

  val quizNames = Vector(quizTypes.interAlphabetKanaKanji, quizTypes.interAlphabetKanjiKana,
    quizTypes.synonym, quizTypes.translation)

  class Adapter extends BaseAdapter {

    override def getItemId(position: Int) = position
    override val getCount = quizNames.size

    override def getView(position: Int, convertView: View, parent: ViewGroup): View = {
      val view = if (convertView != null) convertView
      else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)

      view.findView(TR.selectorEntryCaption).setText(quizNames(position))
      view
    }

    override def getItem(position: Int) = Vector(position)
  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.selector)

    findView(TR.toolBar).setTitle(R.string.quizSelectorTitle)

    val listView = findView(TR.listView)
    listView.setOnItemClickListener(this)
    listView.setAdapter(new Adapter())
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    quizNames(position) match {
      case quizTypes.interAlphabetKanaKanji =>
        val sources = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanaAlphabetHint).toSet
        val targets = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanjiAlphabetHint).toSet
        val questionOption = InterAlphabetQuestion.newAleatoryQuestion(linkedDb, sources, targets)
        questionOption.foreach(question => Question.openWith(this, question))
      case _ =>
        Toast.makeText(this, s"Clicked on ${quizNames(position)}", Toast.LENGTH_SHORT).show()
    }
  }
}

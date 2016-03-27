package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{Toast, AdapterView, BaseAdapter}
import sword.langbook.{TranslationQuestion, SynonymQuestion, InterAlphabetQuestion}
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

  // This list should be dynamic and appear more or less depending on the current database
  // TODO: Make this list dynamic and stop referencing concrete alphabets
  object quizTypes {
    val interAlphabetKanaKanji = "inter-alphabet (kana -> kanji)"
    val interAlphabetKanjiKana = "inter-alphabet (kanji -> kana)"
    val synonymEnglish = "English synonym"
    val synonymSpanish = "Spanish synonym"
    val synonymKana = "Japanese Kana synonym"
    val synonymKanji = "Japanese Kanji synonym"
    val translationEnSp = "translation (English -> Spanish)"
    val translationSpEn = "translation (Spanish -> English)"
    val translationEnJp = "translation (English -> Japanese)"
    val translationJpEn = "translation (Japanese -> English)"
    val translationJpSp = "translation (Japanese -> Spanish)"
    val translationSpJp = "translation (Spanish -> Japanese)"
  }

  val quizNames = Vector(quizTypes.interAlphabetKanaKanji, quizTypes.interAlphabetKanjiKana,
    quizTypes.synonymEnglish, quizTypes.synonymSpanish, quizTypes.synonymKana,
    quizTypes.synonymKanji, quizTypes.translationEnSp, quizTypes.translationSpEn,
    quizTypes.translationEnJp, quizTypes.translationJpEn, quizTypes.translationJpSp,
    quizTypes.translationSpJp)

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

  private def openSynonymQuestion(alphabetHint: String): Unit = {
    val alphabetOption = linkedDb.alphabets.values.find(_.concept.hint == alphabetHint)
    val questionOption = alphabetOption.flatMap(alphabet => SynonymQuestion.newAleatoryQuestion(linkedDb, alphabet))

    if (questionOption.isDefined) {
      questionOption.foreach(question => Question.openWith(this, question))
    }
    else {
      Toast.makeText(this, s"No question can be created for the current database", Toast.LENGTH_SHORT).show()
    }
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    quizNames(position) match {
      case quizTypes.interAlphabetKanaKanji =>
        val sources = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanaAlphabetHint).toSet
        val targets = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanjiAlphabetHint).toSet
        val questionOption = InterAlphabetQuestion.newAleatoryQuestion(linkedDb, sources, targets)
        questionOption.foreach(question => Question.openWith(this, question))
      case quizTypes.interAlphabetKanjiKana =>
        val sources = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanjiAlphabetHint).toSet
        val targets = linkedDb.alphabets.values.find(_.concept.hint == SQLiteStorageManager.kanaAlphabetHint).toSet
        val questionOption = InterAlphabetQuestion.newAleatoryQuestion(linkedDb, sources, targets)
        questionOption.foreach(question => Question.openWith(this, question))
      case quizTypes.synonymEnglish =>
        openSynonymQuestion(SQLiteStorageManager.englishAlphabetHint)
      case quizTypes.synonymSpanish =>
        openSynonymQuestion(SQLiteStorageManager.spanishAlphabetHint)
      case quizTypes.synonymKana =>
        openSynonymQuestion(SQLiteStorageManager.kanaAlphabetHint)
      case quizTypes.synonymKanji =>
        openSynonymQuestion(SQLiteStorageManager.kanjiAlphabetHint)
      case quizTypes.translationEnSp =>
        val questionOption = for {
          sourceLanguage <- linkedDb.languages.values
            .find(_.concept.hint == SQLiteStorageManager.englishLanguageHint)
          targetLanguage <- linkedDb.languages.values
            .find(_.concept.hint == SQLiteStorageManager.spanishLanguageHint)
          question <- TranslationQuestion.newAleatoryQuestion(linkedDb, sourceLanguage,
            targetLanguage, sourceLanguage.alphabets.toSet, targetLanguage.alphabets.toSet)
        } yield {
          question
        }

        if (questionOption.isDefined) Question.openWith(this, questionOption.get)
        else Toast.makeText(this, s"No question can be created for the current database", Toast.LENGTH_SHORT).show()
      case _ =>
        Toast.makeText(this, s"Clicked on ${quizNames(position)}", Toast.LENGTH_SHORT).show()
    }
  }
}

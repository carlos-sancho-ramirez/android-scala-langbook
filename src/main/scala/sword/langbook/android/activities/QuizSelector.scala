package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{Toast, AdapterView, BaseAdapter}
import sword.db.Register
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

  lazy val possibleInterAlphabetQuestions = InterAlphabetQuestion.findPossibleQuestionTypes(linkedDb).toList
  def interAlphabetQuestionNames = for {
    (sources, targets) <- possibleInterAlphabetQuestions
  } yield {
    // TODO: Improve way to retrieve a suitable text to name an alphabet
    val sourceText = sources.flatMap(source => source.concept.words.headOption.flatMap(w => w.text.get(w.language.preferredAlphabet))).mkString(", ")
    val targetText = targets.flatMap(target => target.concept.words.headOption.flatMap(w => w.text.get(w.language.preferredAlphabet))).mkString(", ")
    s"Inter-alphabet from $sourceText to $targetText"
  }

  // This list should be dynamic and appear more or less depending on the current database
  // TODO: Make this list dynamic and stop referencing concrete alphabets
  object quizTypes {
    val synonymEnglish = "English synonym"
    val synonymSpanish = "Spanish synonym"
    val synonymKana = "Japanese Kana synonym"
    val synonymKanji = "Japanese Kanji synonym"
    val translationEnSp = "Translation (English -> Spanish)"
    val translationSpEn = "Translation (Spanish -> English)"
    val translationEnJp = "Translation (English -> Japanese)"
    val translationJpEn = "Translation (Japanese -> English)"
    val translationJpSp = "Translation (Japanese -> Spanish)"
    val translationSpJp = "Translation (Spanish -> Japanese)"
  }

  lazy val quizNames = interAlphabetQuestionNames.toVector ++ Vector(
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

  private def openTranslationQuestion(sourceLanguageCode: Register.LanguageCode, targetLanguageCode: Register.LanguageCode) = {
    val questionOption = for {
      sourceLanguage <- linkedDb.languages.values.find(_.code == sourceLanguageCode)
      targetLanguage <- linkedDb.languages.values.find(_.code == targetLanguageCode)
      question <- TranslationQuestion.newAleatoryQuestion(linkedDb, sourceLanguage,
        targetLanguage, sourceLanguage.alphabets.toSet, targetLanguage.alphabets.toSet)
    } yield {
      question
    }

    if (questionOption.isDefined) Question.openWith(this, questionOption.get)
    else Toast.makeText(this, s"No question can be created for the current database", Toast.LENGTH_SHORT).show()
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit = {
    if (position < possibleInterAlphabetQuestions.size) {
      val (sources, targets) = possibleInterAlphabetQuestions(position)
      val questionOption = InterAlphabetQuestion.newAleatoryQuestion(linkedDb, sources, targets)
      questionOption.foreach(question => Question.openWith(this, question))
    }
    else {
      quizNames(position) match {
        case quizTypes.synonymEnglish =>
          openSynonymQuestion(SQLiteStorageManager.englishAlphabetHint)
        case quizTypes.synonymSpanish =>
          openSynonymQuestion(SQLiteStorageManager.spanishAlphabetHint)
        case quizTypes.synonymKana =>
          openSynonymQuestion(SQLiteStorageManager.kanaAlphabetHint)
        case quizTypes.synonymKanji =>
          openSynonymQuestion(SQLiteStorageManager.kanjiAlphabetHint)
        case quizTypes.translationEnSp =>
          openTranslationQuestion(SQLiteStorageManager.englishCode,
            SQLiteStorageManager.spanishCode)
        case quizTypes.translationEnJp =>
          openTranslationQuestion(SQLiteStorageManager.englishCode,
            SQLiteStorageManager.japaneseCode)
        case quizTypes.translationSpEn =>
          openTranslationQuestion(SQLiteStorageManager.spanishCode,
            SQLiteStorageManager.englishCode)
        case quizTypes.translationSpJp =>
          openTranslationQuestion(SQLiteStorageManager.spanishCode,
            SQLiteStorageManager.japaneseCode)
        case quizTypes.translationJpEn =>
          openTranslationQuestion(SQLiteStorageManager.japaneseCode,
            SQLiteStorageManager.englishCode)
        case quizTypes.translationJpSp =>
          openTranslationQuestion(SQLiteStorageManager.japaneseCode,
            SQLiteStorageManager.spanishCode)
        case _ =>
          Toast.makeText(this, s"Clicked on ${quizNames(position)}", Toast.LENGTH_SHORT).show()
      }
    }
  }
}

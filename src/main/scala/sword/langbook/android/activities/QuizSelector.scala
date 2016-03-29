package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{Toast, AdapterView, BaseAdapter}
import sword.db.Register
import sword.langbook.db.{Word, Alphabet}
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
  lazy val possibleSynonymQuestions = SynonymQuestion.findPossibleQuestionTypes(linkedDb).toList

  private def preferredAlphabetTitle(alphabet: Alphabet): Option[String] = {
    val preferredWordOption = alphabet.concept.wordsForLanguage(preferredLanguage).headOption
    val wordOption = {
      if (preferredWordOption.isDefined) preferredWordOption
      else alphabet.concept.words.headOption
    }
    wordOption.flatMap(w => w.text.get(w.language.preferredAlphabet))
  }

  def interAlphabetQuestionNames = for {
    (sources, targets) <- possibleInterAlphabetQuestions
  } yield {
    val sourceText = sources.flatMap(preferredAlphabetTitle).mkString(", ")
    val targetText = targets.flatMap(preferredAlphabetTitle).mkString(", ")
    s"Inter-alphabet from $sourceText to $targetText"
  }

  def synonymQuestionNames = for {
    alphabet <- possibleSynonymQuestions
  } yield {
    val text = preferredAlphabetTitle(alphabet).getOrElse("?")
    s"Synonym for $text alphabet"
  }

  // This list should be dynamic and appear more or less depending on the current database
  // TODO: Make this list dynamic and stop referencing concrete alphabets
  object quizTypes {
    val translationEnSp = "Translation (English -> Spanish)"
    val translationSpEn = "Translation (Spanish -> English)"
    val translationEnJp = "Translation (English -> Japanese)"
    val translationJpEn = "Translation (Japanese -> English)"
    val translationJpSp = "Translation (Japanese -> Spanish)"
    val translationSpJp = "Translation (Spanish -> Japanese)"
  }

  lazy val quizNames = interAlphabetQuestionNames.toVector ++ synonymQuestionNames.toVector ++ Vector(
    quizTypes.translationEnSp, quizTypes.translationSpEn, quizTypes.translationEnJp,
    quizTypes.translationJpEn, quizTypes.translationJpSp, quizTypes.translationSpJp)

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
    val interAlphabetQuestionTypeCount = possibleInterAlphabetQuestions.size

    if (position < interAlphabetQuestionTypeCount) {
      val (sources, targets) = possibleInterAlphabetQuestions(position)
      val questionOption = InterAlphabetQuestion.newAleatoryQuestion(linkedDb, sources, targets)
      questionOption.foreach(question => Question.openWith(this, question))
    }
    else if (position < interAlphabetQuestionTypeCount + possibleSynonymQuestions.size) {
      val alphabet = possibleSynonymQuestions(position - interAlphabetQuestionTypeCount)
      val questionOption = SynonymQuestion.newAleatoryQuestion(linkedDb, alphabet)
      questionOption.foreach(question => Question.openWith(this, question))
    }
    else {
      quizNames(position) match {
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

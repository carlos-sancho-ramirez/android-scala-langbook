package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{LayoutInflater, ViewGroup, View}
import android.widget.{AdapterView, BaseAdapter}
import sword.langbook.db.{Language, Alphabet}
import sword.langbook.{TranslationQuestion, SynonymQuestion, InterAlphabetQuestion}
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
  lazy val possibleTranslationQuestions = TranslationQuestion.findPossibleQuestionTypes(linkedDb).toList

  def interAlphabetQuestionNames = {
    val prefLang = preferredLanguage
    for {
      (sources, targets) <- possibleInterAlphabetQuestions
    } yield {
      val sourceText = sources.flatMap(_.suitableTextForLanguage(prefLang)).mkString(", ")
      val targetText = targets.flatMap(_.suitableTextForLanguage(prefLang)).mkString(", ")
      getString(R.string.interAlphabetQuizName, sourceText, targetText)
    }
  }

  def synonymQuestionNames = for {
    alphabet <- possibleSynonymQuestions
  } yield {
    val text = alphabet.suitableTextForLanguage(preferredLanguage).getOrElse("?")
    getString(R.string.synonymQuizName, text)
  }

  private def translationQuestionAlphabetName(language: Language, alphabets: Set[Alphabet]) = {
    val languageText = language.suitableTextForLanguage(preferredLanguage).getOrElse("?")
    if (language.alphabets.size != 1) {
      val subText = alphabets.flatMap(_.suitableTextForLanguage(preferredLanguage)).mkString(", ")
      s"$languageText ($subText)"
    }
    else languageText
  }

  def translationQuestionNames = for {
    (sourceLanguage, targetLanguage, sourceAlphabets, targetAlphabets) <- possibleTranslationQuestions
  } yield {
    val source = translationQuestionAlphabetName(sourceLanguage, sourceAlphabets)
    val target = translationQuestionAlphabetName(targetLanguage, targetAlphabets)
    getString(R.string.translationQuizName, source, target)
  }

  lazy val quizNames =
    interAlphabetQuestionNames.toVector ++
    synonymQuestionNames.toVector ++
    translationQuestionNames.toVector

  class Adapter extends BaseAdapter {

    override def getItemId(position: Int) = position
    override val getCount = quizNames.size

    override def getView(position: Int, convertView: View, parent: ViewGroup): View = {
      val view = {
        if (convertView != null) convertView
        else LayoutInflater.from(parent.getContext).inflate(R.layout.selector_entry, parent, false)
      }

      view.findView(TR.selectorEntry).setText(quizNames(position))
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
    val firstSynonymQuestionPosition = possibleInterAlphabetQuestions.size
    val firstTranslationQuestionPosition = firstSynonymQuestionPosition + possibleSynonymQuestions.size

    questionBuilder = {
      if (position < firstSynonymQuestionPosition) {
        val (sources, targets) = possibleInterAlphabetQuestions(position)
        InterAlphabetQuestion.newAleatoryQuestion(sources, targets)
      }
      else if (position < firstTranslationQuestionPosition) {
        val alphabet = possibleSynonymQuestions(position - firstSynonymQuestionPosition)
        SynonymQuestion.newAleatoryQuestion(alphabet)
      }
      else {
        val (sourceLanguage, targetLanguage, sourceAlphabets, targetAlphabets) =
          possibleTranslationQuestions(position - firstTranslationQuestionPosition)
        TranslationQuestion.newAleatoryQuestion(sourceLanguage, targetLanguage, sourceAlphabets, targetAlphabets)
      }
    }

    Question.openWith(this)
  }
}

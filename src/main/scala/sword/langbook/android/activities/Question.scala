package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.view.{LayoutInflater, View}
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._
import sword.langbook.db.{Language, Concept}

object Question {
  private val className = "sword.langbook.android.activities.Question"

  def openWith(activity: Activity, question: sword.langbook.Question, requestCode: Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.question, question.encodedQuestion)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Question extends BaseActivity with View.OnClickListener {

  object StateBundleKeys {
    val displayAnswer = "da"
  }

  lazy val question = {
    val encoded = getIntent.getStringExtra(BundleKeys.question)
    sword.langbook.Question.decode(linkedDb, encoded).get
  }

  private var _displayingAnswer: Boolean = false

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.question)

    if (savedInstanceState != null) {
      _displayingAnswer = savedInstanceState.getBoolean(StateBundleKeys.displayAnswer)
    }

    updateView()
  }

  def updateView(): Unit = {
    val button = findViewById(R.id.checkAnswerButton)
    if (_displayingAnswer) {
      button.setVisibility(View.GONE)
    }
    else {
      button.setOnClickListener(this)
      button.setVisibility(View.VISIBLE)
    }

    val container = findView(TR.entryContainer)
    container.removeAllViews()

    val inflater = LayoutInflater.from(this)
    for {
      (alphabet, hint) <- question.clues
      // TODO: This is getting the first word associated to the Alphabet, this should be checking the language
      alphabetWord <- alphabet.concept.words.headOption
      label <- alphabetWord.text.get(alphabetWord.language.preferredAlphabet)
    } {
      val entry = inflater.inflate(TR.layout.question_entry, container)
      entry.findView(TR.questionTitle).setText(label)
      entry.findView(TR.questionHint).setText(hint)
    }

    for {
      (alphabet, hint) <- question.possibleAnswers.head
      // TODO: This is getting the first word associated to the Alphabet, this should be checking the language
      alphabetWord <- alphabet.concept.words.headOption
      label <- alphabetWord.text.get(alphabetWord.language.preferredAlphabet)
    } {
      val entry = inflater.inflate(TR.layout.question_entry, container)
      entry.findView(TR.questionTitle).setText(label)

      val text = if (_displayingAnswer) hint else "?"
      entry.findView(TR.questionHint).setText(text)
    }
  }

  override def onClick(v: View): Unit = {
    _displayingAnswer = true
    updateView()
  }

  override def onSaveInstanceState(state: Bundle): Unit = {
    state.putBoolean(StateBundleKeys.displayAnswer, _displayingAnswer)
  }
}
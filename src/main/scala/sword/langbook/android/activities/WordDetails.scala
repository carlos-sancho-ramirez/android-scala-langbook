package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.{LinearLayoutManager, Toolbar}
import android.view.{MenuItem, Menu}
import sword.langbook.android.{TR, R}
import sword.langbook.db.Word

object WordDetails {
  private val className = "sword.langbook.android.activities.WordDetails"

  def openWith(activity :Activity, requestCode :Int, word :Word) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.wordKey, word.key.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordDetails extends BaseActivity with Toolbar.OnMenuItemClickListener {

  lazy val wordKeyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.wordKey))
  lazy val wordOption = wordKeyOption.flatMap(linkedDb.words.get)

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)
    findView(TR.recyclerView).setLayoutManager(new LinearLayoutManager(this))
    updateUi()
  }

  def updateUi(): Unit = {
    val title = wordOption.map {
      word =>
        word.text.getOrElse(word.language.preferredAlphabet, "")
    }.getOrElse(getString(R.string.appName))

    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(title)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)

    updateAdapter()
  }

  def updateAdapter(): Unit = {
    var alphabetStrings = Vector[String]()
    var languageText = ""
    var synonyms = Vector[Word]()
    var translations = Vector[Word]()

    for {
      word <- wordOption
    } {
      val language = word.language
      val preferredAlphabet = language.preferredAlphabet

      alphabetStrings = word.text.flatMap {
        case (alphabet, thisText) =>
          val alphabetText = alphabet.concept.words.headOption.flatMap(_.text.values.headOption)
          alphabetText.map(alphabetText => s"$alphabetText: $thisText")
      }.toVector

      for {
        word <- language.concept.wordsForLanguage(language).headOption
        text <- word.text.get(preferredAlphabet)
      } {
        languageText = text
      }

      synonyms = word.synonyms.toVector
      translations = word.translations.toVector
    }

    findView(TR.recyclerView).setAdapter(new WordDetailsAdapter(alphabetStrings, languageText, synonyms, translations))
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.word_details, menu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    val result = {
      for (word <- wordOption) yield {
        val concepts = word.concepts

        // Currently the user only can create a synonym or translation for a word with only one concept linked.
        // This limitation should be removed whenever there is a suitable option available to
        // distinguish among concepts in a human readable way.
        // TODO: Change this when possible to allow the user selecting to which concept should be linked
        if (concepts.size == 1) {
          item.getItemId match {
            case R.id.newSynonymOption =>
              WordEditor.openWith(this, RequestCodes.addNewWord, concept = concepts.head, language = word.language)
              true

            case R.id.newTranslationOption =>
              WordEditor.openWith(this, RequestCodes.addNewWord, concept = concepts.head, excludedLanguage = word.language)
              true

            case _ =>
              false
          }
        }
        else false
      }
    }

    result.isDefined && result.get
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.`addNewWord` => if (resultCode == Activity.RESULT_OK) updateUi()
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }
}

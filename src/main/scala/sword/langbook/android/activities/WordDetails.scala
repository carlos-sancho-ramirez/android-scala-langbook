package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.{LinearLayoutManager, Toolbar}
import android.util.Log
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
    val title = wordOption.flatMap(_.suitableText).getOrElse(getString(R.string.appName))
    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(title)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)

    updateAdapter()
  }

  def updateAdapter(): Unit = {
    for (word <- wordOption) {
      val language = word.language

      val alphabetStrings = word.text.flatMap {
        case (alphabet, thisText) =>
          alphabet.suitableTextForLanguage(preferredLanguage).map(alphabetText => s"$alphabetText: $thisText")
      }.toVector

      val acceptations = word.concepts.flatMap(_.isTypeOf)
          .flatMap(_.wordsForLanguage(preferredLanguage).headOption)
          .flatMap(_.suitableText).toVector

      val synonyms = word.synonyms.toVector
      val translations = word.translations.toVector

      findView(TR.recyclerView).setAdapter(new WordDetailsAdapter(this, alphabetStrings, language,
        acceptations, synonyms, translations))
    }
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.word_details, menu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    val result = {
      for (word <- wordOption) yield {
        val concepts = word.concepts
        val concrete = concepts.size == 1

        item.getItemId match {
          case R.id.newSynonymOption =>
            if (concrete) WordEditor.openWith(this, RequestCodes.addNewWord, concept = concepts.head, language = word.language)
            else ConceptPicker.openWith(this, RequestCodes.pickConceptsForSynonym, concepts)
            true

          case R.id.newTranslationOption =>
            if (concrete) WordEditor.openWith(this, RequestCodes.addNewWord, concept = concepts.head, excludedLanguage = word.language)
            else ConceptPicker.openWith(this, RequestCodes.pickConceptsForTranslation, concepts)
            true

          case _ =>
            false
        }
      }
    }

    result.isDefined && result.get
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    super.onActivityResult(requestCode, resultCode, data)

    if (resultCode == Activity.RESULT_OK && wordOption.isEmpty) {
      finish()
    }
    else {
      requestCode match {
        case RequestCodes.`addNewWord` => updateUi()
        case RequestCodes.`pickConceptsForSynonym` =>
          WordEditor.openWith(this, RequestCodes.addNewWord,
            conceptEncodedKeys = data.getStringArrayExtra(BundleKeys.conceptKeys),
            language = wordOption.get.language)
        case RequestCodes.`pickConceptsForTranslation` =>
          WordEditor.openWith(this, RequestCodes.addNewWord,
            conceptEncodedKeys = data.getStringArrayExtra(BundleKeys.conceptKeys),
            excludedLanguage = wordOption.get.language)
        case _ => super.onActivityResult(requestCode, resultCode, data)
      }
    }
  }
}

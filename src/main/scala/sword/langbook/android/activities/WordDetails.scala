package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.{LinearLayoutManager, Toolbar}
import android.util.Log
import android.view.{Menu, MenuItem}
import sword.db.StorageManager
import sword.langbook.android.{R, TR}
import sword.langbook.db.{Concept, Word, redundant, registers}

object WordDetails {
  private val className = "sword.langbook.android.activities.WordDetails"

  def openWith(activity :Activity, requestCode :Int, word: Word) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.wordKey, word.key.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }

  def openWith(activity :Activity, requestCode :Int, acceptationKey: StorageManager.Key) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.acceptationKey, acceptationKey.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordDetails extends BaseActivity with Toolbar.OnMenuItemClickListener {

  lazy val storageManager = linkedDb.storageManager
  lazy val acceptationKeyOption = storageManager.decode(getIntent.getStringExtra(BundleKeys.acceptationKey))
  lazy val wordKeyOption = {
    val fromAcceptation = acceptationKeyOption.flatMap(storageManager.get).collect {
      case reg: registers.Acceptation => reg.word
    }

    if (fromAcceptation.isDefined) fromAcceptation
    else storageManager.decode(getIntent.getStringExtra(BundleKeys.wordKey))
  }
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

      val alternatives :IndexedSeq[String] = if (wordKeyOption.isDefined) {
        val regDef = registers.WordRepresentation
        val field = regDef.WordReferenceField(wordKeyOption.get)
        storageManager.getJointSet(regDef, redundant.Text, field, regDef.SymbolArrayReferenceField, redundant.Text.SymbolArrayReferenceField)
          .map(_.text).toVector
      }
      else IndexedSeq()

      val givenAcceptation = acceptationKeyOption.flatMap(key => storageManager.get(key).map(reg => (key, reg.asInstanceOf[registers.Acceptation])))
      val acceptations = {
        if (givenAcceptation.isDefined) Vector(givenAcceptation.get)
        else {
          val regDef = registers.Acceptation
          val field = regDef.WordReferenceField(wordKeyOption.get)
          storageManager.getMapFor(regDef, field).toVector
        }
      }

      val definitions = acceptations.map { case (accKey, acc) =>
        Concept(acc.concept).isTypeOf.headOption.flatMap(_.wordsForLanguage(preferredLanguage).headOption)
          .flatMap(_.suitableText).getOrElse("")
      }

      val accRepr = for {
        (accKey, acc) <- acceptations
        text <- {
          val regDef = registers.AcceptationRepresentation
          val field = regDef.AcceptationReferenceField(accKey)
          storageManager.getJointSet(regDef, redundant.Text, field, regDef.SymbolArrayReferenceField, redundant.Text.SymbolArrayReferenceField)
            .map(_.text).headOption
        }
      } yield text

      Log.i(getClass.getSimpleName, s" ---> definitions: $definitions")
      Log.i(getClass.getSimpleName, s" ---> accRepr: $accRepr")
      val defs = definitions zip accRepr map { case (definition, repr) => s"[$repr] $definition"}

      val bunches = word.bunches.map(_.name).toVector
      val synonyms = word.synonyms.toVector
      val translations = word.translations.toVector
      val morphologies = word.morphologies

      findView(TR.recyclerView).setAdapter(new WordDetailsAdapter(this, defs, language,
        alternatives, synonyms, translations, bunches, morphologies))
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

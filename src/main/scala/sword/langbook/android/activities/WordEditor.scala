package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.text.TextUtils
import android.view.View
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._
import sword.langbook.db.{Language, Concept}

object WordEditor {
  private val className = "sword.langbook.android.activities.WordEditor"

  def openWith(activity: Activity, requestCode: Int = 0, concept: Concept = null,
               language: Language = null, excludedLanguage: Language = null) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (concept != null) {
      intent.putExtra(BundleKeys.conceptKey, concept.key.encoded)
    }

    if (language != null) {
      intent.putExtra(BundleKeys.languageKey, language.key.encoded)
    }

    if (excludedLanguage != null) {
      intent.putExtra(BundleKeys.excludedLanguageKey, excludedLanguage.key.encoded)
    }

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordEditor extends BaseActivity with View.OnClickListener {

  private var _languageEncodedKey: String = null

  def languageKeyOpt = {
    val encoded = Option(_languageEncodedKey) getOrElse getIntent.getStringExtra(BundleKeys.languageKey)
    if (encoded != null) linkedDb.storageManager.decode(encoded)
    else None
  }

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_editor)

    findViewById(R.id.saveButton).setOnClickListener(this)

    if (savedInstanceState != null) {
      _languageEncodedKey = savedInstanceState.getString(BundleKeys.languageKey)
      updateUi()
    }
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.`pickLanguage` => {
        if (resultCode == Activity.RESULT_OK) {
          _languageEncodedKey = data.getStringExtra(BundleKeys.languageKey)
          updateUi()
        }
        else {
          finish()
        }
      }
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }

  private def updateUi(): Unit = {
    val container = findView(TR.entryContainer)
    container.removeAllViews()
    val inflater = getLayoutInflater

    for {
      languageKey <- languageKeyOpt
      alphabet <- Language(languageKey).alphabets
      // TODO: This must be changed to pick the proper alphabet
      alphabetWord <- alphabet.concept.words.headOption
      alphabetText <- alphabetWord.text.values.headOption
    } {
      val entry = inflater.inflate(TR.layout.word_editor_entry, container, true)
      entry.findView(TR.fieldTitle).setText(alphabetText)
    }
  }

  override def onResume() :Unit = {
    super.onResume()

    if (languageKeyOpt.isEmpty) {
      LanguageSelector.openWith(this, requestCode = RequestCodes.pickLanguage,
        excludedLanguageEncodedKey = getIntent.getStringExtra(BundleKeys.excludedLanguageKey))
    }
  }

  // TODO: This should save all the alphabets entered, and not just the first one
  override def onClick(v: View): Unit = {
    val text = findView(TR.wordField).getText.toString

    // TODO: This has to be changed to include words instead of concepts
    for {
      languageKey <- languageKeyOpt
    } {
      if (!TextUtils.isEmpty(text)) {
        import sword.langbook.db.registers
        val manager = linkedDb.storageManager

        // First we need to add the each missing symbol
        val originalSymbols = manager.getMapFor(registers.Symbol).values.map(_.fields(0)
          .asInstanceOf[sword.db.UnicodeField].value).toSet
        val symbolsToAdd = text.toSet[Char].map(_.toInt).diff(originalSymbols)
        for (symbol <- symbolsToAdd) {
          // Currently if there is an error inserting any of the symbols we will be unable to register
          // the word properly, so none of them should be added in that case to avoid orfan symbols
          // TODO: Find a strategy to include all only if the word can be added or nothing at all if not
          manager.insert(registers.Symbol(symbol))
        }

        // We collect back all symbol keys and create the array of symbols
        val currentSymbols = manager.getMapFor(registers.Symbol).map { case (k, v) =>
          (v.fields(0).asInstanceOf[sword.db.UnicodeField].value, k)
        }
        val symbolArrayCollection = manager.insert(text.map(currentSymbols(_)).map(registers.SymbolPosition(_)))

        // Right now we are assuming that all words are for the first alphabet in the database
        // TODO: Make this alphabet not hardcoded
        val alphabetKey = manager.getKeysFor(registers.Alphabet).head
        val piece = symbolArrayCollection.flatMap(array => manager.insert(List(registers.Piece(alphabetKey, array))))

        // Right now we are assuming that there is only one piece per word
        // TODO: Make possible to add more than one piece for word
        val pieceArray = piece.flatMap(piece => manager.insert(List(registers.PiecePosition(piece))))

        val word = pieceArray.flatMap(array => manager.insert(registers.Word(languageKey, array)))

        val argsConceptKey = manager.decode(getIntent.getStringExtra(BundleKeys.conceptKey))
        val conceptKeyOpt = {
          if (argsConceptKey.nonEmpty) argsConceptKey
          else manager.insert(registers.Concept(text))
        }

        for {
          wordKey <- word
          conceptKey <- conceptKeyOpt
        } {
          manager.insert(registers.WordConcept(wordKey, conceptKey))
        }
      }
    }

    setResult(Activity.RESULT_OK)
    finish()
  }

  override def onSaveInstanceState(bundle: Bundle): Unit = {
    super.onSaveInstanceState(bundle)
    bundle.putString(BundleKeys.languageKey, _languageEncodedKey)
  }
}

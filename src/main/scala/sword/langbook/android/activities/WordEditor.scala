package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.text.TextUtils
import android.view.View
import sword.langbook.android.{TR, R}

object WordEditor {
  private val className = "sword.langbook.android.activities.WordEditor"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordEditor extends BaseActivity with View.OnClickListener {

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_editor)

    findViewById(R.id.saveButton).setOnClickListener(this)
  }

  override def onClick(v: View): Unit = {
    val word = findView(TR.wordField).getText.toString

    // TODO: This has to be changed to include words instead of concepts
    if (!TextUtils.isEmpty(word)) {
      import sword.langbook.db.registers
      val manager = linkedDb.storageManager

      // First we need to add the each missing symbol
      val originalSymbols = manager.getMapFor(registers.Symbol).values.map(_.fields(0)
          .asInstanceOf[sword.db.UnicodeField].value).toSet
      val symbolsToAdd = word.toSet[Char].map(_.toInt).diff(originalSymbols)
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
      val symbolArrayCollection = manager.insert(word.map(currentSymbols(_)).map(registers.SymbolPosition(_)))

      // Right now we are assuming that all words are for the first alphabet in the database
      // TODO: Make this alphabet not hardcoded
      val alphabetKey = manager.getKeysFor(registers.Alphabet).head
      val piece = symbolArrayCollection.flatMap(array => manager.insert(List(registers.Piece(alphabetKey,array))))

      // Right now we are assuming that there is only one piece per word
      // TODO: Make possible to add more than one piece for word
      val pieceArray = piece.flatMap(piece => manager.insert(List(registers.PiecePosition(piece))))

      // Right now we are assuming that all words are for the first language in the database
      // TODO: Make this alphabet not hardcoded
      val languageKey = manager.getKeysFor(registers.Language).head
      for (array <- pieceArray) {
        manager.insert(registers.Word(languageKey, array))
      }
    }

    setResult(Activity.RESULT_OK)
    finish()
  }
}

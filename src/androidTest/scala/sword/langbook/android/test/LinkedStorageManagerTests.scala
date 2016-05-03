package sword.langbook.android.test

import android.app.Instrumentation
import sword.langbook.{TranslationQuestion, SynonymQuestion, InterAlphabetQuestion}
import sword.langbook.android.db.SQLiteStorageManager
import sword.langbook.db.LinkedStorageManager

case class LinkedStorageManagerTests(instrumentation: Instrumentation) {

  private def createManager = {
    val context = instrumentation.getTargetContext
    new LinkedStorageManager(defs => new SQLiteStorageManager(context, SQLiteStorageManager.dbName, defs))
  }

  def allWordTexts: String = {
    val manager = createManager
    val allWords = manager.words.values.toVector
    val allTexts = allWords.map(word => (word, word.text.values.toList)).toMap

    s"Found ${allWords.size} words in the database. Retrieved ${allTexts.size} strings"
  }

  def possibleInterAlphabetQuestions: String = {
    val manager = createManager
    val possibilities = InterAlphabetQuestion.findPossibleQuestionTypes(manager)
    s"Found ${possibilities.size} possibilities"
  }

  def possibleSynonymQuestions: String = {
    val manager = createManager
    val possibilities = SynonymQuestion.findPossibleQuestionTypes(manager)
    s"Found ${possibilities.size} possibilities"
  }

  def possibleTranslationQuestions: String = {
    val manager = createManager
    val possibilities = TranslationQuestion.findPossibleQuestionTypes(manager)
    s"Found ${possibilities.size} possibilities"
  }
}
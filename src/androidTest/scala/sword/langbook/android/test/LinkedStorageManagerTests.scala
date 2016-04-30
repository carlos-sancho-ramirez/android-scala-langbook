package sword.langbook.android.test

import android.app.Instrumentation
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
}
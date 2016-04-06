package sword.langbook.android

import android.app.Application
import sword.langbook.android.activities.Question
import sword.langbook.android.db.SQLiteStorageManager
import sword.langbook.db.LinkedStorageManager

class LangbookApplication extends Application {
  lazy val linkedDb = new LinkedStorageManager(defs => new SQLiteStorageManager(this, SQLiteStorageManager.dbName, defs))

  /**
   * Used by Question activity to generate questions. This avoid having to encode the whole builder
   * to pass it across activities.
   */
  var questionBuilder: LinkedStorageManager => Option[sword.langbook.Question] = null
}

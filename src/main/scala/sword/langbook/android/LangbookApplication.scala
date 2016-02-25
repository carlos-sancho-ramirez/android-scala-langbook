package sword.langbook.android

import android.app.Application
import sword.langbook.android.db.SQLiteStorageManager
import sword.langbook.db.LinkedStorageManager

class LangbookApplication extends Application {
  lazy val linkedDb = new LinkedStorageManager(defs => new SQLiteStorageManager(this, SQLiteStorageManager.dbName, defs))
}

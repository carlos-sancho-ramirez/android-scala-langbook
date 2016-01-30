package sword.langbook.android

import android.app.Activity
import android.os.Bundle
import android.widget.TextView
import sword.db.MemoryStorageManager
import sword.langbook.db.LinkedStorageManager

class MainActivity extends Activity {
  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    val manager = new LinkedStorageManager(defs => new MemoryStorageManager(defs))
    findViewById(R.id.text).asInstanceOf[TextView].setText(manager.toString)
  }
}

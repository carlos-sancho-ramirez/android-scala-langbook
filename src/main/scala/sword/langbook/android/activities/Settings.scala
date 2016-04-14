package sword.langbook.android.activities

import java.io._

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.view.View
import android.view.View.OnClickListener
import android.widget.Toast
import sword.langbook.android.R
import sword.langbook.android.db.SQLiteStorageManager

object Settings {
  private val className = "sword.langbook.android.activities.Settings"

  def openWith(activity :Activity, requestCode :Int = 0) = {
    val intent = new Intent()
    intent.setClassName(activity, className)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class Settings extends BaseActivity {

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.settings)

    findViewById(R.id.importDatabaseButton).setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = {
        FilePathDialog.openWith(Settings.this, RequestCodes.pickFile)
      }
    })

    findViewById(R.id.exportDatabaseButton).setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = {
        Toast.makeText(Settings.this, getString(R.string.unsupportedFeature), Toast.LENGTH_SHORT).show()
      }
    })
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    if (resultCode == Activity.RESULT_OK && requestCode == RequestCodes.pickFile) {
      val path = data.getStringExtra(BundleKeys.filePath)
      val message = {
        if (importDatabaseFromPath(path)) R.string.databaseImportSuccess
        else R.string.databaseImportFailure
      }
      Toast.makeText(this, message, Toast.LENGTH_SHORT).show()
    }
  }

  private def importDatabaseFromPath(path: String): Boolean = {
    val inFile = new File(path)
    val outFile = getDatabasePath(SQLiteStorageManager.dbName)

    try {
      val inStream = new FileInputStream(inFile)

      try {
        val outStream = new FileOutputStream(outFile)

        try {
          val buffer = new Array[Byte](4096)
          var count = 0

          do {
            count = inStream.read(buffer)
            if (count > 0) {
              outStream.write(buffer, 0, count)
            }
          }
          while (count >= 0)
          true
        }
        catch {
          case _: IOException =>
            // Nothing can be done
            false
        }
        finally {
          outStream.close()
        }
      }
      finally {
        inStream.close()
      }
    }
    catch {
      case _: FileNotFoundException =>
        // Nothing can be done
        false
    }
  }
}

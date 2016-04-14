package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.view.View.OnClickListener
import android.widget.Toast
import sword.langbook.android.R

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
      Toast.makeText(this, s"Should import from $path", Toast.LENGTH_SHORT).show()
    }
  }
}

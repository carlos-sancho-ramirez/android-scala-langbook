package sword.langbook.android.activities

import android.app.Activity
import android.content.{Intent, DialogInterface}
import android.os.Bundle
import android.support.v4.app.DialogFragment
import android.support.v7.app.AlertDialog
import android.view.LayoutInflater
import android.widget.EditText
import sword.langbook.android.{TR, R}
import sword.langbook.android.TypedResource._

object FilePathDialog {

  private[FilePathDialog] object SaveBundleKeys {
    val path = "a"
    val requestCode = "b"
  }

  def openWith(activity: BaseActivity, requestCode: Int) = {
    val fragment = new FilePathDialog()
    fragment.setRequestCode(requestCode)
    fragment.show(activity.getSupportFragmentManager, "filePathDialog")
  }
}

class FilePathDialog extends DialogFragment with DialogInterface.OnClickListener {

  private var _editText: EditText = null
  private var _requestCode: Int = 0
  private var _path: String = null

  def setRequestCode(requestCode: Int) = {
    _requestCode = requestCode
  }

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    if (savedInstanceState != null) {
      _requestCode = savedInstanceState.getInt(FilePathDialog.SaveBundleKeys.requestCode)
      _path = savedInstanceState.getString(FilePathDialog.SaveBundleKeys.path)
    }
  }

  override def onCreateDialog(savedInstanceState: Bundle) = {
    val alertDialog = new AlertDialog.Builder(getActivity)
        .setTitle(R.string.filePathDialogTitle)
        .setPositiveButton(R.string.filePathDialogOkButton, this)
        .setCancelable(true)
        .create()

    _editText = LayoutInflater.from(alertDialog.getContext).inflate(TR.layout.file_path_dialog)
    if (savedInstanceState != null) {
      val text = savedInstanceState.getString(FilePathDialog.SaveBundleKeys.path)
      if (text != null) {
        _editText.setText(text)
      }
    }

    alertDialog.setView(_editText)
    alertDialog
  }

  private def updatePathVariable():Unit = {
    if (_editText != null) {
      _path = _editText.getText.toString
      if (_path != null && _path.isEmpty) _path = null
    }
  }

  override def onClick(dialog: DialogInterface, which: Int): Unit = {
    getActivity match {
      case activity: BaseActivity =>
        updatePathVariable()
        if (_path != null) {
          val data = new Intent()
          data.putExtra(BundleKeys.filePath, _path)
          activity.onActivityResult(_requestCode, Activity.RESULT_OK, data)
        }
      case _ =>
        // Nothing to be done
    }
  }

  override def onSaveInstanceState(outState: Bundle): Unit = {
    outState.putInt(FilePathDialog.SaveBundleKeys.requestCode, _requestCode)

    updatePathVariable()
    if (_path != null) {
      outState.putString(FilePathDialog.SaveBundleKeys.path, _path)
    }
  }
}

package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import sword.langbook.android.{R, TR}

class Main extends BaseActivity {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    findView(TR.checkLanguagesButton).setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = {
        LanguageSelector.openWith(Main.this, RequestCodes.pickLanguage)
      }
    })

    findView(TR.checkWordsButton).setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = Selector.openWith(Main.this)
    })

    findView(TR.startQuizButton).setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = QuizSelector.openWith(Main.this)
    })
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    super.onActivityResult(requestCode, resultCode, data)

    if (requestCode == RequestCodes.pickLanguage && resultCode == Activity.RESULT_OK) {
      LanguageDetails.openWith(Main.this, languageEncodedKey = data.getStringExtra(BundleKeys.languageKey))
    }
  }
}

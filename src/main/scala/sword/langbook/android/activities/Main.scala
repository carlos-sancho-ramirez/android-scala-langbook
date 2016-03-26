package sword.langbook.android.activities

import android.os.Bundle
import android.view.View
import sword.langbook.android.{R, TR}

class Main extends BaseActivity {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    findView(TR.checkWordsButton).setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = Selector.openWith(Main.this)
    })
  }
}

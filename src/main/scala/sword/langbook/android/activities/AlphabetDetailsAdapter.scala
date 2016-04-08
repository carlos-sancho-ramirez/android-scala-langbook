package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.util.DisplayMetrics
import android.view.ViewGroup
import sword.langbook.android.R
import sword.langbook.db.Alphabet

object AlphabetDetailsAdapter {

  def apply(activity: Activity, alphabet: Alphabet) = {
    val metrics = new DisplayMetrics()
    activity.getWindowManager.getDefaultDisplay.getMetrics(metrics)

    val columns = metrics.widthPixels / activity.getResources.getDimensionPixelSize(R.dimen.alphabetDetailsSymbolSide)
    new AlphabetDetailsAdapter(alphabet, columns)
  }
}

class AlphabetDetailsAdapter(val alphabet: Alphabet, val spanCount: Int) extends RecyclerView.Adapter[SymbolViewHolder] {
  val symbols = alphabet.symbols.map(_.unicode.toChar).toVector
  override val getItemCount = symbols.size

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int) = {
    SymbolViewHolder.newInstance(viewGroup)
  }

  override def onBindViewHolder(vh: SymbolViewHolder, position: Int): Unit = {
    vh.textView.setText("" + symbols(position))
  }
}

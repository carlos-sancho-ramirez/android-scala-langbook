package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.util.DisplayMetrics
import android.view.View.OnClickListener
import android.view.{View, ViewGroup}
import sword.langbook.android.R
import sword.langbook.android.viewholders.SymbolViewHolder
import sword.langbook.db.Alphabet

object AlphabetDetailsAdapter {

  def apply(activity: Activity, alphabet: Alphabet) = {
    val metrics = new DisplayMetrics()
    activity.getWindowManager.getDefaultDisplay.getMetrics(metrics)

    val columns = metrics.widthPixels / activity.getResources.getDimensionPixelSize(R.dimen.alphabetDetailsSymbolSide)
    new AlphabetDetailsAdapter(activity, alphabet, columns)
  }
}

case class AlphabetDetailsAdapter(activity: Activity, alphabet: Alphabet, spanCount: Int) extends RecyclerView.Adapter[SymbolViewHolder] {
  val symbols = alphabet.symbols.toSeq.sortWith(_.unicode < _.unicode)
  val chars = symbols.map(_.unicode.toChar)
  override val getItemCount = symbols.size

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int) = {
    SymbolViewHolder.newInstance(viewGroup)
  }

  override def onBindViewHolder(vh: SymbolViewHolder, position: Int): Unit = {
    vh.textView.setText("" + chars(position))
    vh.textView.setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = {
        SymbolDetails.openWith(activity, symbol = symbols(position))
      }
    })
  }
}

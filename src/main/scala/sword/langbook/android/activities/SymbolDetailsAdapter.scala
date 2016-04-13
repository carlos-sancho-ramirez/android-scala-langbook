package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup
import sword.langbook.db.Symbol

case class SymbolDetailsAdapter(symbol: Symbol) extends RecyclerView.Adapter[WordDetailsSectionEntryViewHolder] {
  override val getItemCount = 1
  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int) = {
    WordDetailsSectionEntryViewHolder.newInstance(viewGroup)
  }

  override def onBindViewHolder(vh: WordDetailsSectionEntryViewHolder, position: Int): Unit = {
    vh.textView.setText(s"Unicode: ${symbol.unicode.toHexString}")
  }
}

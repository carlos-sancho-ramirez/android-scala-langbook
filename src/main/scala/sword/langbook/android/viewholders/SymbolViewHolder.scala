package sword.langbook.android.viewholders

import android.support.v7.widget.RecyclerView
import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class SymbolViewHolder(textView: TextView) extends RecyclerView.ViewHolder(textView)

object SymbolViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.symbol_view_holder, parent, false)
    new SymbolViewHolder(view)
  }
}

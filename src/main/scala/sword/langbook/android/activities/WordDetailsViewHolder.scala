package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.{LayoutInflater, ViewGroup}
import android.widget.LinearLayout
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class WordDetailsViewHolder(linearLayout: LinearLayout) extends RecyclerView.ViewHolder(linearLayout) {
  val textView = linearLayout.findView(TR.selectorEntryCaption)
}

object WordDetailsViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.word_details_entry, parent, false)
    new WordDetailsViewHolder(view)
  }
}

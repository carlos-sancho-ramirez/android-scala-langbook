package sword.langbook.android.activities

import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class WordDetailsSectionHeaderViewHolder(textView: TextView) extends WordDetailsViewHolder(textView)

object WordDetailsSectionHeaderViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.word_details_section_header, parent, false)
    new WordDetailsSectionHeaderViewHolder(view)
  }
}

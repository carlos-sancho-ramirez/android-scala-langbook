package sword.langbook.android.activities

import android.view.{LayoutInflater, ViewGroup}
import android.widget.{TextView, LinearLayout}
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class WordDetailsSectionEntryViewHolder(textView: TextView) extends WordDetailsViewHolder(textView)

object WordDetailsSectionEntryViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.word_details_section_entry, parent, false)
    new WordDetailsSectionEntryViewHolder(view)
  }
}

package sword.langbook.android.viewholders

import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class SectionEntryViewHolder(textView: TextView) extends BaseViewHolder(textView)

object SectionEntryViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.section_entry, parent, false)
    new SectionEntryViewHolder(view)
  }
}

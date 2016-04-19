package sword.langbook.android.viewholders

import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class SectionHeaderViewHolder(textView: TextView) extends BaseViewHolder(textView)

object SectionHeaderViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.section_header, parent, false)
    new SectionHeaderViewHolder(view)
  }
}

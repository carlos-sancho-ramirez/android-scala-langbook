package sword.langbook.android.viewholders

import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class DescriptionHeaderViewHolder(textView: TextView) extends BaseViewHolder(textView)

object DescriptionHeaderViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.description_header, parent, false)
    new DescriptionHeaderViewHolder(view)
  }
}

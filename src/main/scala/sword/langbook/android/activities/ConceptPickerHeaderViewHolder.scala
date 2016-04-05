package sword.langbook.android.activities

import android.view.{LayoutInflater, ViewGroup}
import android.widget.TextView
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class ConceptPickerHeaderViewHolder(textView: TextView) extends ConceptPickerViewHolder(textView)

object ConceptPickerHeaderViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.description_header, parent, false)
    new ConceptPickerHeaderViewHolder(view)
  }
}

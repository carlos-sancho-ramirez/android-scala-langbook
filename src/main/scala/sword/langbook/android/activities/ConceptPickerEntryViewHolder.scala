package sword.langbook.android.activities

import android.view.{LayoutInflater, ViewGroup}
import android.widget.LinearLayout
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class ConceptPickerEntryViewHolder(linearLayout: LinearLayout) extends ConceptPickerViewHolder(linearLayout) {
  val textView = linearLayout.findView(TR.conceptPickerEntryCaption)
  val checkBox = linearLayout.findView(TR.conceptPickerSwitch)
}

object ConceptPickerEntryViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.concept_picker_entry, parent, false)
    new ConceptPickerEntryViewHolder(view)
  }
}

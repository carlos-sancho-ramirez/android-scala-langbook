package sword.langbook.android.viewholders

import android.view.{LayoutInflater, ViewGroup}
import android.widget.LinearLayout
import sword.langbook.android.TR
import sword.langbook.android.TypedResource._

case class CheckableEntryViewHolder(linearLayout: LinearLayout) extends BaseViewHolder(linearLayout) {
  val textView = linearLayout.findView(TR.conceptPickerEntryCaption)
  val checkBox = linearLayout.findView(TR.conceptPickerSwitch)
}

object CheckableEntryViewHolder {
  def newInstance(parent: ViewGroup) = {
    val inflater = LayoutInflater.from(parent.getContext)
    val view = inflater.inflate(TR.layout.checkable_entry, parent, false)
    new CheckableEntryViewHolder(view)
  }
}

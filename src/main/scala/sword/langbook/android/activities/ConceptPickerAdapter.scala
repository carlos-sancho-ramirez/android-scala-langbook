package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.ViewGroup
import android.widget.{Toast, CompoundButton}
import sword.langbook.android.R
import sword.langbook.android.viewholders._

import scala.collection.mutable

case class ConceptPickerAdapter(activity: Activity, concepts: Seq[String]) extends RecyclerView.Adapter[BaseViewHolder] with CompoundButton.OnCheckedChangeListener {
  val descriptionHeaderText = activity.getString(R.string.conceptPickerDescriptionHeader)
  override val getItemCount = concepts.size + 1

  private val _selected = {
    val result = mutable.BitSet()
    for (index <- concepts.indices) result += index
    result
  }

  def selected = _selected.toSet

  override def getItemViewType(position: Int) = {
    if (position == 0) BaseViewHolder.types.descriptionHeader
    else BaseViewHolder.types.checkableEntry
  }

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): BaseViewHolder = {
    viewType match {
      case BaseViewHolder.types.`descriptionHeader` => DescriptionHeaderViewHolder.newInstance(viewGroup)
      case BaseViewHolder.types.`checkableEntry` => CheckableEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: BaseViewHolder, position: Int): Unit = {
    vh match {
      case holder: DescriptionHeaderViewHolder =>
        holder.textView.setText(descriptionHeaderText)

      case holder: CheckableEntryViewHolder =>
        holder.textView.setText(concepts(position - 1))

        val checkBox = holder.checkBox
        checkBox.setTag(position - 1)
        checkBox.setChecked(_selected(position - 1))
        checkBox.setOnCheckedChangeListener(this)
    }
  }

  override def onCheckedChanged(buttonView: CompoundButton, isChecked: Boolean): Unit = {
    val index = buttonView.getTag.asInstanceOf[Int]
    if (isChecked) _selected += index
    else _selected -= index
  }
}

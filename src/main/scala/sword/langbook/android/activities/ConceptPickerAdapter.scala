package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.ViewGroup
import android.widget.{Toast, CompoundButton}
import sword.langbook.android.R

import scala.collection.mutable

case class ConceptPickerAdapter(activity: Activity, concepts: Seq[String]) extends RecyclerView.Adapter[ConceptPickerViewHolder] with CompoundButton.OnCheckedChangeListener {
  val descriptionHeaderText = activity.getString(R.string.conceptPickerDescriptionHeader)
  override val getItemCount = concepts.size + 1

  private val _selected = {
    val result = mutable.BitSet()
    for (index <- concepts.indices) result += index
    result
  }

  def selected = _selected.toSet

  object ViewTypes {
    val header = 0
    val entry = 1
  }

  override def getItemViewType(position: Int) = {
    if (position == 0) ViewTypes.header
    else ViewTypes.entry
  }

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): ConceptPickerViewHolder = {
    viewType match {
      case ViewTypes.header => ConceptPickerHeaderViewHolder.newInstance(viewGroup)
      case ViewTypes.entry => ConceptPickerEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: ConceptPickerViewHolder, position: Int): Unit = {
    vh match {
      case holder: ConceptPickerHeaderViewHolder =>
        holder.textView.setText(descriptionHeaderText)
        Toast.makeText(activity, s"Description Header text: $descriptionHeaderText", Toast.LENGTH_SHORT).show()

      case holder: ConceptPickerEntryViewHolder =>
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

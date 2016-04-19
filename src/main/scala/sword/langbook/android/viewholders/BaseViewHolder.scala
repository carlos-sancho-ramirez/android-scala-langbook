package sword.langbook.android.viewholders

import android.support.v7.widget.RecyclerView
import android.view.View

abstract class BaseViewHolder(view: View) extends RecyclerView.ViewHolder(view)

object BaseViewHolder {
  object types {
    val checkableEntry = 0
    val descriptionHeader = 1
    val sectionEntry = 2
    val sectionHeader = 3
    val symbol = 4
  }
}
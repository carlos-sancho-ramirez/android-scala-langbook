package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.{View, ViewGroup}
import sword.langbook.android.viewholders._
import sword.langbook.db.{Language, Alphabet}

case class LanguageDetailsAdapter(activity: Activity, preferredLanguage: Language, topText: String,
    alphabets: Vector[Alphabet], statistics: Seq[(String, String)]) extends RecyclerView.Adapter[BaseViewHolder] with View.OnClickListener {

  override def getItemViewType(position: Int) = {
    if (position == alphabetPositionOffset - 1 || position == statisticsPositionOffset - 1) BaseViewHolder.types.sectionHeader
    else BaseViewHolder.types.sectionEntry
  }

  val alphabetPositionOffset = 2
  val statisticsPositionOffset = alphabetPositionOffset + alphabets.size + 1

  override val getItemCount = statistics.size + statisticsPositionOffset

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): BaseViewHolder = {
    viewType match {
      case BaseViewHolder.types.`sectionHeader` => SectionHeaderViewHolder.newInstance(viewGroup)
      case BaseViewHolder.types.`sectionEntry` => SectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: BaseViewHolder, position: Int): Unit = {
    vh match {
      case holder: SectionEntryViewHolder =>
        val text = {
          if (position == 0) {
            topText
          }
          else if (position < statisticsPositionOffset) {
            alphabets(position - alphabetPositionOffset).suitableTextForLanguage(preferredLanguage).getOrElse("")
          }
          else {
            val tuple = statistics(position - statisticsPositionOffset)
            tuple._1 + ": " + tuple._2
          }
        }

        val textView = holder.textView
        textView.setText(text)
        textView.setOnClickListener(this)
        textView.setTag(position)

      case holder: SectionHeaderViewHolder =>
        val text = {
          if (position == alphabetPositionOffset - 1) "Alphabets"
          else "Statistics"
        }
        holder.textView.setText(text)
    }
  }

  override def onClick(v: View): Unit = {
    val alphabetPosition = v.getTag.asInstanceOf[Int] - alphabetPositionOffset
    if (alphabetPosition >= 0 && alphabetPosition < alphabets.size) {
      AlphabetDetails.openWith(activity, alphabetEncodedKey = alphabets(alphabetPosition).key.encoded)
    }
  }
}

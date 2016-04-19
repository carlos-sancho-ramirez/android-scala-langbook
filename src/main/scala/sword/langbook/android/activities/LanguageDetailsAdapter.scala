package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.{View, ViewGroup}
import sword.langbook.android.viewholders._
import sword.langbook.db.{Language, Alphabet}

case class LanguageDetailsAdapter(activity: Activity, preferredLanguage: Language, topText: String,
    alphabets: Vector[Alphabet]) extends RecyclerView.Adapter[BaseViewHolder] with View.OnClickListener {

  override def getItemViewType(position: Int) = {
    if (position == 1) BaseViewHolder.types.sectionHeader
    else BaseViewHolder.types.sectionEntry
  }

  val alphabetPositionOffset = 2
  override val getItemCount = alphabets.size + alphabetPositionOffset
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
          if (position == 0) topText
          else alphabets(position - alphabetPositionOffset).suitableTextForLanguage(preferredLanguage).getOrElse("")
        }
        val textView = holder.textView
        textView.setText(text)
        textView.setOnClickListener(this)
        textView.setTag(position)
      case holder: SectionHeaderViewHolder =>
        holder.textView.setText("Alphabets")
    }
  }

  override def onClick(v: View): Unit = {
    val alphabetPosition = v.getTag.asInstanceOf[Int] - alphabetPositionOffset
    if (alphabetPosition >= 0) {
      AlphabetDetails.openWith(activity, alphabetEncodedKey = alphabets(alphabetPosition).key.encoded)
    }
  }
}

package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup
import sword.langbook.db.{Language, Alphabet}

case class LanguageDetailsAdapter(preferredLanguage: Language, topText: String,
    alphabets: Vector[Alphabet]) extends RecyclerView.Adapter[WordDetailsViewHolder] {

  object ViewTypes {
    val header = 0
    val entry = 1
  }

  override def getItemViewType(position: Int) = {
    if (position == 1) ViewTypes.header
    else ViewTypes.entry
  }

  override val getItemCount = alphabets.size + 2
  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): WordDetailsViewHolder = {
    viewType match {
      case ViewTypes.header => WordDetailsSectionHeaderViewHolder.newInstance(viewGroup)
      case ViewTypes.entry => WordDetailsSectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: WordDetailsViewHolder, position: Int): Unit = {
    vh match {
      case holder: WordDetailsSectionEntryViewHolder =>
        val text = {
          if (position == 0) topText
          else alphabets(position - 2).suitableTextForLanguage(preferredLanguage).getOrElse("")
        }
        holder.textView.setText(text)
      case holder: WordDetailsSectionHeaderViewHolder =>
        holder.textView.setText("Alphabets")
    }
  }
}

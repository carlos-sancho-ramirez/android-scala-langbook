package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup

case class WordDetailsAdapter(alphabets: String, language: String, synonyms: String, translations: String) extends RecyclerView.Adapter[WordDetailsViewHolder] {

  // TODO: This should not be using 4 strings, but a dynamical growing structure

  val sectionTitles = List("Alphabets", "Language", "Synonyms", "Translations")

  val sectionPositions = Map(0 -> 0, 2 -> 1, 4 -> 2, 6 -> 3)

  override val getItemCount = 8

  override def getItemViewType(position: Int) = position % 2

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): WordDetailsViewHolder = {
    viewType match {
      case 0 => WordDetailsSectionHeaderViewHolder.newInstance(viewGroup)
      case 1 => WordDetailsSectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: WordDetailsViewHolder, position: Int): Unit = {
    vh match {
      case holder: WordDetailsSectionHeaderViewHolder =>
        holder.textView.setText(sectionTitles(sectionPositions(position)))
      case holder: WordDetailsSectionEntryViewHolder =>
        val text = position match {
          case 1 => alphabets
          case 3 => language
          case 5 => synonyms
          case 7 => translations
        }

        holder.textView.setText(text)
    }
  }
}

package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup

case class WordDetailsAdapter(alphabets: IndexedSeq[String], language: String,
    synonyms: IndexedSeq[String], translations: IndexedSeq[String])
    extends RecyclerView.Adapter[WordDetailsViewHolder] {

  val alphabetsSectionCount = 1 + alphabets.size
  val languageSectionCount = 2
  val synonymsSectionCount = {
    val size = synonyms.size
    if (size > 0) 1 + synonyms.size
    else 0
  }

  val translationsSectionCount = {
    val size = translations.size
    if (size > 0) 1 + translations.size
    else 0
  }

  // TODO: This titles should not be hardcoded
  object sectionTitles {
    val alphabets = "Alphabets"
    val language = "Language"
    val synonyms = "Synonyms"
    val translations = "Translations"
  }

  val sectionHeaderPositions = {
    val map = scala.collection.mutable.Map(0 -> sectionTitles.alphabets,
      alphabetsSectionCount -> sectionTitles.language)
    var currentPos = alphabetsSectionCount + languageSectionCount

    if (synonymsSectionCount > 0) {
      map += currentPos -> sectionTitles.synonyms
      currentPos += synonymsSectionCount
    }

    if (translationsSectionCount > 0) {
      map += currentPos -> sectionTitles.translations
      currentPos += translationsSectionCount
    }

    map.toMap
  }

  override val getItemCount = {
    alphabetsSectionCount + languageSectionCount + synonymsSectionCount + translationsSectionCount
  }

  object ViewTypes {
    val header = 0
    val entry = 1
  }

  override def getItemViewType(position: Int) = {
    if (sectionHeaderPositions.contains(position)) ViewTypes.header
    else ViewTypes.entry
  }

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): WordDetailsViewHolder = {
    viewType match {
      case ViewTypes.header => WordDetailsSectionHeaderViewHolder.newInstance(viewGroup)
      case ViewTypes.entry => WordDetailsSectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: WordDetailsViewHolder, position: Int): Unit = {
    vh match {
      case holder: WordDetailsSectionHeaderViewHolder =>
        holder.textView.setText(sectionHeaderPositions(position))
      case holder: WordDetailsSectionEntryViewHolder =>
        val currentHeaderPosition = sectionHeaderPositions.keys.filter(_ < position).max
        val currentSection = sectionHeaderPositions(currentHeaderPosition)
        val relPosition = position - currentHeaderPosition - 1

        val text = currentSection match {
          case sectionTitles.alphabets => alphabets(relPosition)
          case sectionTitles.language => language
          case sectionTitles.synonyms => synonyms(relPosition)
          case sectionTitles.translations => translations(relPosition)
        }

        holder.textView.setText(text)
    }
  }
}

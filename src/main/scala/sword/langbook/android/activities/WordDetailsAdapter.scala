package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.{View, ViewGroup}
import sword.langbook.android.viewholders._
import sword.langbook.db.{Language, Word}

case class WordDetailsAdapter(activity: BaseActivity, alphabets: IndexedSeq[String], language: Language,
    acceptations: IndexedSeq[String], bunches: IndexedSeq[String], synonyms: IndexedSeq[Word],
    translations: IndexedSeq[Word]) extends RecyclerView.Adapter[BaseViewHolder] {

  val alphabetsSectionCount = {
    val size = alphabets.size
    if (size > 1) 1 + size else 0
  }

  val languageSectionCount = 2

  val acceptationsSectionCount = {
    val size = acceptations.size
    if (size > 0) 1 + size
    else 0
  }

  val bunchesSectionCount = {
    val size = bunches.size
    if (size > 0) 1 + size
    else 0
  }

  val synonymsSectionCount = {
    val size = synonyms.size
    if (size > 0) 1 + size
    else 0
  }

  val translationsSectionCount = {
    val size = translations.size
    if (size > 0) 1 + size
    else 0
  }

  // TODO: This titles should not be hardcoded
  object sectionTitles {
    val alphabets = "Alphabets"
    val language = "Language"
    val acceptations = "Acceptations"
    val bunches = "Bunches"
    val synonyms = "Synonyms"
    val translations = "Translations"
  }

  val sectionHeaderPositions = {
    var currentPos = 0
    val map = scala.collection.mutable.Map[Int, String]()

    if (alphabetsSectionCount > 0) {
      map += currentPos -> sectionTitles.alphabets
      currentPos += alphabetsSectionCount
    }

    if (languageSectionCount > 0) {
      map += currentPos -> sectionTitles.language
      currentPos += languageSectionCount
    }

    if (acceptationsSectionCount > 0) {
      map += currentPos -> sectionTitles.acceptations
      currentPos += acceptationsSectionCount
    }

    if (bunchesSectionCount > 0) {
      map += currentPos -> sectionTitles.bunches
      currentPos += bunchesSectionCount
    }

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
    alphabetsSectionCount + languageSectionCount + synonymsSectionCount + translationsSectionCount +
      acceptationsSectionCount + bunchesSectionCount
  }

  override def getItemViewType(position: Int) = {
    if (sectionHeaderPositions.contains(position)) BaseViewHolder.types.sectionHeader
    else BaseViewHolder.types.sectionEntry
  }

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): BaseViewHolder = {
    viewType match {
      case BaseViewHolder.types.`sectionHeader` => SectionHeaderViewHolder.newInstance(viewGroup)
      case BaseViewHolder.types.`sectionEntry` => SectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: BaseViewHolder, position: Int): Unit = {
    vh match {
      case holder: SectionHeaderViewHolder =>
        holder.textView.setText(sectionHeaderPositions(position))
      case holder: SectionEntryViewHolder =>
        val currentHeaderPosition = sectionHeaderPositions.keys.filter(_ < position).max
        val currentSection = sectionHeaderPositions(currentHeaderPosition)
        val relPosition = position - currentHeaderPosition - 1

        val text = currentSection match {
          case sectionTitles.alphabets =>
            holder.textView.setClickable(false)
            alphabets(relPosition)
          case sectionTitles.language =>
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                LanguageDetails.openWith(activity, language = language)
              }
            })
            language.suitableTextForLanguage(activity.preferredLanguage).getOrElse("")
          case sectionTitles.acceptations =>
            holder.textView.setClickable(false)
            acceptations(relPosition)
          case sectionTitles.bunches =>
            holder.textView.setClickable(false)
            bunches(relPosition)
          case sectionTitles.synonyms =>
            val synonym = synonyms(relPosition)
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                WordDetails.openWith(activity, RequestCodes.checkWordDetails, synonym)
              }
            })
            synonym.suitableText.getOrElse("")
          case sectionTitles.translations =>
            val translation = translations(relPosition)
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                WordDetails.openWith(activity, RequestCodes.checkWordDetails, translation)
              }
            })
            translation.suitableText.getOrElse("")
        }

        holder.textView.setText(text)
    }
  }
}

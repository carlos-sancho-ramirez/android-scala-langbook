package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.View.OnClickListener
import android.view.{View, ViewGroup}
import sword.db.StorageManager
import sword.langbook.android.viewholders._
import sword.langbook.db.{Acceptation, Language}

case class WordDetailsAdapter(
    activity: BaseActivity,
    acceptations: IndexedSeq[(StorageManager.Key, String)],
    language: Language,
    representations: IndexedSeq[String],
    synonyms: IndexedSeq[Acceptation],
    translations: IndexedSeq[Acceptation],
    bunches: IndexedSeq[String],
    morphologies: Map[String, String]) extends RecyclerView.Adapter[BaseViewHolder] {

  val acceptationsSectionCount = acceptations.size

  val languageSectionCount = 1

  val representationSectionCount = {
    val size = representations.size
    if (size > 0) 1 + size else 0
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

  val bunchesSectionCount = {
    val size = bunches.size
    if (size > 0) 1 + size
    else 0
  }

  val morphologiesSectionCount = {
    val size = morphologies.size
    if (size > 0) 1 + size
    else 0
  }

  // TODO: This titles should not be hardcoded
  object sectionTitles {
    val acceptations = "Acceptations"
    val language = "Language"
    val representations = "Alternatives"
    val synonyms = "Synonyms"
    val translations = "Translations"
    val bunches = "Bunches"
    val morphologies = "Morphologies"
  }

  val (sectionHeaderPositions, sectionContentStartPositions) = {
    var currentPos = 0
    val headerMap = scala.collection.mutable.Map[Int, String]()
    val contentMap = scala.collection.mutable.Map[Int, String]()

    if (acceptationsSectionCount > 0) {
      contentMap += currentPos -> sectionTitles.acceptations
      currentPos += acceptationsSectionCount
    }

    if (languageSectionCount > 0) {
      contentMap += currentPos -> sectionTitles.language
      currentPos += languageSectionCount
    }

    if (representationSectionCount > 0) {
      headerMap += currentPos -> sectionTitles.representations
      contentMap += (currentPos + 1) -> sectionTitles.representations
      currentPos += representationSectionCount
    }

    if (synonymsSectionCount > 0) {
      headerMap += currentPos -> sectionTitles.synonyms
      contentMap += (currentPos + 1) -> sectionTitles.synonyms
      currentPos += synonymsSectionCount
    }

    if (translationsSectionCount > 0) {
      headerMap += currentPos -> sectionTitles.translations
      contentMap += (currentPos + 1) -> sectionTitles.translations
      currentPos += translationsSectionCount
    }

    if (bunchesSectionCount > 0) {
      headerMap += currentPos -> sectionTitles.bunches
      contentMap += (currentPos + 1) -> sectionTitles.bunches
      currentPos += bunchesSectionCount
    }

    if (morphologiesSectionCount > 0) {
      headerMap += currentPos -> sectionTitles.morphologies
      contentMap += (currentPos + 1) -> sectionTitles.morphologies
      currentPos += morphologiesSectionCount
    }

    (headerMap.toMap, contentMap.toMap)
  }

  override val getItemCount = {
    acceptationsSectionCount + languageSectionCount + representationSectionCount +
      synonymsSectionCount + translationsSectionCount +
      bunchesSectionCount + morphologiesSectionCount
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
        val sectionStartPosition = sectionContentStartPositions.keys.filter(_ <= position).max
        val currentSection = sectionContentStartPositions(sectionStartPosition)
        val relPosition = position - sectionStartPosition

        val text = currentSection match {
          case sectionTitles.acceptations =>
            val acc = acceptations(relPosition)

            val textView = holder.textView
            if (acc._1 != null) {
              textView.setOnClickListener(new OnClickListener {
                override def onClick(v: View): Unit = {
                  WordDetails.openWith(activity, RequestCodes.checkWordDetails, acc._1)
                }
              })
            }
            else textView.setClickable(false)

            acceptations(relPosition)._2
          case sectionTitles.language =>
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                LanguageDetails.openWith(activity, language = language)
              }
            })
            language.suitableTextForLanguage(activity.preferredLanguage).getOrElse("")
          case sectionTitles.representations =>
            holder.textView.setClickable(false)
            representations(relPosition)
          case sectionTitles.synonyms =>
            val synonym = synonyms(relPosition)
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                WordDetails.openWith(activity, RequestCodes.checkWordDetails, synonym.key)
              }
            })
            synonym.suitableText.getOrElse("")
          case sectionTitles.translations =>
            val translation = translations(relPosition)
            holder.textView.setClickable(true)
            holder.textView.setOnClickListener(new View.OnClickListener() {
              override def onClick(v: View): Unit = {
                WordDetails.openWith(activity, RequestCodes.checkWordDetails, translation.key)
              }
            })
            translation.suitableText.getOrElse("")
          case sectionTitles.bunches =>
            holder.textView.setClickable(false)
            bunches(relPosition)
          case sectionTitles.morphologies =>
            holder.textView.setClickable(false)
            val pair = morphologies.toList(relPosition)
            pair._1 + ": " + pair._2
        }

        holder.textView.setText(text)
    }
  }
}

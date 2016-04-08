package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.{View, ViewGroup}
import sword.langbook.db.{Language, Alphabet}

case class LanguageDetailsAdapter(activity: Activity, preferredLanguage: Language, topText: String,
    alphabets: Vector[Alphabet]) extends RecyclerView.Adapter[WordDetailsViewHolder] with View.OnClickListener {

  object ViewTypes {
    val header = 0
    val entry = 1
  }

  override def getItemViewType(position: Int) = {
    if (position == 1) ViewTypes.header
    else ViewTypes.entry
  }

  val alphabetPositionOffset = 2
  override val getItemCount = alphabets.size + alphabetPositionOffset
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
          else alphabets(position - alphabetPositionOffset).suitableTextForLanguage(preferredLanguage).getOrElse("")
        }
        val textView = holder.textView
        textView.setText(text)
        textView.setOnClickListener(this)
        textView.setTag(position)
      case holder: WordDetailsSectionHeaderViewHolder =>
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

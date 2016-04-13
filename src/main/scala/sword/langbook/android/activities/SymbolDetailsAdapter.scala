package sword.langbook.android.activities

import android.app.Activity
import android.support.v7.widget.RecyclerView
import android.view.View.OnClickListener
import android.view.{View, ViewGroup}
import sword.langbook.android.R
import sword.langbook.db.Symbol

case class SymbolDetailsAdapter(activity: BaseActivity, symbol: Symbol) extends RecyclerView.Adapter[WordDetailsViewHolder] {
  val alphabets = symbol.alphabetsWhereIncluded.toVector

  val alphabetSectionSize = if (alphabets.nonEmpty) alphabets.size + 1 else 0

  object ViewTypes {
    val header = 0
    val entry = 1
  }

  override val getItemCount = 1 + alphabetSectionSize

  override def getItemViewType(position: Int) = {
    if (position == 1) ViewTypes.header
    else ViewTypes.entry
  }

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int) = {
    viewType match {
      case ViewTypes.`header` => WordDetailsSectionHeaderViewHolder.newInstance(viewGroup)
      case ViewTypes.`entry` => WordDetailsSectionEntryViewHolder.newInstance(viewGroup)
    }
  }

  override def onBindViewHolder(vh: WordDetailsViewHolder, position: Int): Unit = {
    vh match {
      case holder: WordDetailsSectionHeaderViewHolder =>
        holder.textView.setText(activity.getString(R.string.alphabetsWhereIncluded))
      case holder: WordDetailsSectionEntryViewHolder =>
        val textView = holder.textView
        if (position == 0) {
          textView.setText(s"Unicode: ${symbol.unicode.toHexString}")
          textView.setClickable(false)
        }
        else {
          val alphabet = alphabets(position - 2)
          textView.setText(alphabet.suitableTextForLanguage(activity.preferredLanguage).getOrElse(""))
          textView.setOnClickListener(new OnClickListener {
            override def onClick(v: View): Unit = {
              AlphabetDetails.openWith(activity, alphabetEncodedKey = alphabet.key.encoded)
            }
          })
          textView.setClickable(true)
        }
    }
  }
}

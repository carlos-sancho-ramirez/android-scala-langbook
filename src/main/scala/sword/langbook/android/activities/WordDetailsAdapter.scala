package sword.langbook.android.activities

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup

case class WordDetailsAdapter(alphabets: String, language: String, synonyms: String, translations: String) extends RecyclerView.Adapter[WordDetailsViewHolder] {

  // TODO: This should not be using 4 strings, but a dynamical growing structure

  override val getItemCount = 4

  override def onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): WordDetailsViewHolder = {
    WordDetailsViewHolder.newInstance(viewGroup)
  }

  override def onBindViewHolder(vh: WordDetailsViewHolder, position: Int): Unit = {
    val text = position match {
      case 0 => alphabets
      case 1 => language
      case 2 => synonyms
      case 3 => translations
    }

    vh.textView.setText(text)
  }
}

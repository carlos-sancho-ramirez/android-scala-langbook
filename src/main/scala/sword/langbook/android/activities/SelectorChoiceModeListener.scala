package sword.langbook.android.activities

import android.view.{MenuItem, Menu, ActionMode}
import android.widget.AbsListView.MultiChoiceModeListener
import sword.langbook.android.R

trait SelectorChoiceModeCallback {
  /**
    * Notifies that the delete action has been triggered with the given positions selected
    * @param positions Set including all selected positions for the current adapter
    */
  def delete(positions: scala.collection.Set[Int]): Unit
}

case class SelectorChoiceModeListener(callback: SelectorChoiceModeCallback) extends MultiChoiceModeListener {

  private val selected = scala.collection.mutable.BitSet()

  override def onItemCheckedStateChanged(mode: ActionMode, position: Int, id: Long, checked: Boolean): Unit = {
    if (checked) {
      selected.add(position)
    }
    else {
      selected.remove(position)
    }

    // TODO: Move this hardcoded string to the XML resources handling placeholders and plurals
    mode.setTitle(s"${selected.size} word(s) selected")
  }

  override def onDestroyActionMode(mode: ActionMode): Unit = {
    selected.clear()
  }

  override def onCreateActionMode(mode: ActionMode, menu: Menu): Boolean = {
    selected.clear()
    mode.getMenuInflater.inflate(R.menu.selector_multichoice_mode, menu)
    true
  }

  override def onActionItemClicked(mode: ActionMode, item: MenuItem): Boolean = {
    callback.delete(selected)
    mode.finish()
    true
  }

  override def onPrepareActionMode(mode: ActionMode, menu: Menu): Boolean = {
    // Nothing to be done
    false
  }
}

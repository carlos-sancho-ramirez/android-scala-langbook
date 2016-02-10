package sword.langbook.android.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.Toolbar
import android.view.{MenuItem, Menu}
import sword.langbook.android.{TR, R}
import sword.langbook.db.Word

object WordDetails {
  private val className = "sword.langbook.android.activities.WordDetails"

  def openWith(activity :Activity, requestCode :Int, word :Word) = {
    val intent = new Intent()
    intent.setClassName(activity, className)
    intent.putExtra(BundleKeys.wordKey, word.key.encoded)

    if (requestCode > 0) activity.startActivityForResult(intent, requestCode)
    else activity.startActivity(intent)
  }
}

class WordDetails extends BaseActivity with Toolbar.OnMenuItemClickListener {

  lazy val wordKeyOption = linkedDb.storageManager.decode(getIntent.getStringExtra(BundleKeys.wordKey))
  lazy val wordOption = wordKeyOption.flatMap(linkedDb.words.get)

  override def onCreate(savedInstanceState :Bundle) :Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.word_details)
    updateUi()
  }

  def updateUi(): Unit = {
    val toolBar = findView(TR.toolBar)
    toolBar.setTitle(R.string.appName)
    toolBar.setOnMenuItemClickListener(this)
    updateMenu(toolBar.getMenu)

    for {
      key <- wordKeyOption
      word <- linkedDb.words.get(key)
    } {
      // Assumed for now that all words have the first alphabet and language and are the ones to be
      // displayed
      // TODO: Remove this assumption
      val alphabet = linkedDb.alphabets.values.head
      val text = word.pieces.flatMap(_.get(alphabet)).flatMap(x => x)
        .map(_.unicode.toChar).mkString("")
      toolBar.setTitle(text)

      val language = word.language
      for {
        word <- language.concept.wordsForLanguage(language).headOption
        // TODO: Select the most suitable alphabet
        text <- word.text(alphabet)
      } {
        findView(TR.languageText).setText(text)
      }

      findView(TR.synonymsText).setText(word.synonyms.flatMap(_.text(alphabet)).mkString(", "))
    }
  }

  def updateMenu(menu :Menu) = {
    menu.clear()
    getMenuInflater.inflate(R.menu.word_details, menu)
  }

  override def onMenuItemClick(item: MenuItem) = {
    println("onMenuItemClick called")
    item.getItemId match {
      case R.id.newSynonymOption =>
        println("  with newSynonymOption")
        for (word <- wordOption) {
          val concepts = word.concepts

          // Currently the user only can create a synonym for a word with only one concept linked.
          // This limitation should be removed whenever there is a suitable option available to
          // distinguish among concepts in a human readable way.
          // TODO: Change this when possible to allow the user selecting to which concept should be linked
          println(s"  with concepts.size ${concepts.size}")
          if (concepts.size == 1) {
            WordEditor.openWith(this, RequestCodes.addNewWord, concept = concepts.head)
          }
        }
        true
      case _ => false
    }
  }

  override def onActivityResult(requestCode :Int, resultCode :Int, data :Intent) :Unit = {
    requestCode match {
      case RequestCodes.`addNewWord` => if (resultCode == Activity.RESULT_OK) updateUi()
      case _ => super.onActivityResult(requestCode, resultCode, data)
    }
  }
}

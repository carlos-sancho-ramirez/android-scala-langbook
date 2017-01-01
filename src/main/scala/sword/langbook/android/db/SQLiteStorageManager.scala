package sword.langbook.android.db

import android.content.Context
import android.database.Cursor
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import android.util.Log
import sword.db.Register.CollectionId
import sword.db.StorageManager.LanguageCodes
import sword.db._
import sword.langbook.android.VersionUtils
import sword.langbook.db.redundant
import sword.langbook.db.registers
import sword.langbook.db.registers._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SQLiteStorageManager {
  val dbName = "LanguageDb"
  val currentDbVersion = 4
  val idKey = "id"
  val collKey = "coll"

  // This is required in order that app can know which alphabet is the kana, and which one the Kanji.
  // This should not be required if the app was generic enough.
  // TODO: Remove this hints when the app do not require them
  val roumajiAlphabetHint = "Roumaji alphabet"
  val kanaAlphabetHint = "Kana alphabet"
  val kanjiAlphabetHint = "Kanji alphabet"
  val englishAlphabetHint = "English alphabet"
  val spanishAlphabetHint = "Spanish alphabet"
}

class SQLiteStorageManager(context :Context, dbName: String, override val registerDefinitions :Seq[RegisterDefinition[Register]])
    extends SQLiteOpenHelper(context, dbName, null, SQLiteStorageManager.currentDbVersion)
    with StorageManager {

  // Just to measure time taken
  var _lastQueryMillis: Long = 0

  // The following code is copied from AbstractStorageManager, and it should be centralised
  // TODO: Centralise this code
  if (registerDefinitions.toSet.size < registerDefinitions.size) {
    throw new IllegalArgumentException("Duplicated register definitions are not allowed")
  }

  val singleReferences :Seq[(RegisterDefinition[Register], RegisterDefinition[Register])] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[ForeignKeyFieldDefinition]
  } yield {
      (regDef, fieldDef.asInstanceOf[ForeignKeyFieldDefinition].target)
    }

  if (singleReferences.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a foreign key" +
      " field must have as target one of the definitions given")
  }

  val groupReferences :Seq[(RegisterDefinition[Register], CollectibleRegisterDefinition[Register])] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[CollectionReferenceFieldDefinition]
  } yield {
    (regDef, fieldDef.asInstanceOf[CollectionReferenceFieldDefinition].target)
  }

  if (groupReferences.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a collection" +
      " reference field must have as target one of the definitions given")
  }
  // End of duplicated code...

  private def logi(message :String) = android.util.Log.i("DB", message)

  private def query(db: SQLiteDatabase, tableName: String, columns: Array[String], whereClause: String, orderByClause: String) = {

    val logWhere = {
      if (whereClause != null) s" WHERE $whereClause"
      else ""
    }

    val logOrder = {
      if (orderByClause != null) s" ORDER BY $orderByClause"
      else ""
    }

    logi(s"Executing query: SELECT ${columns.mkString(", ")} FROM $tableName$logWhere$logOrder")
    val startQueryTime = System.currentTimeMillis
    try {
      db.query(tableName, columns, whereClause, null, null, null, orderByClause, null)
    }
    finally {
      _lastQueryMillis = System.currentTimeMillis - startQueryTime
    }
  }

  private def exec(db: SQLiteDatabase, query: String): Unit = {
    logi(s"Executing query: $query")
    val startQueryTime = System.currentTimeMillis
    try {
      db.execSQL(query)
    }
    finally {
      _lastQueryMillis = System.currentTimeMillis - startQueryTime
    }
  }

  private def tableName(regDef :RegisterDefinition[Register]) :String = s"R${registerDefinitions.indexOf(regDef)}"
  private def tableName(reg :Register) :String = tableName(reg.definition)

  private def fieldName(regDef :RegisterDefinition[Register], fieldDef :FieldDefinition) :String = s"C${regDef.fields.indexOf(fieldDef)}"
  private def fieldName(regDef :RegisterDefinition[Register], field :Field) :String = fieldName(regDef, field.definition)

  private def createTables(db :SQLiteDatabase) = {
    for (regDef <- registerDefinitions) {
      val idKey = SQLiteStorageManager.idKey
      val fields = regDef.fields.map { fieldDef =>
        val sqlType = fieldDef match {
          case CharSequenceFieldDefinition => "TEXT"
          case LanguageCodeFieldDefinition => "TEXT"
          case _ => "INTEGER"
        }

        s"${fieldName(regDef, fieldDef)} $sqlType"
      }
      val columns = (regDef match {
        case _: CollectibleRegisterDefinition[_] => Seq(s"${SQLiteStorageManager.collKey} INTEGER") ++ fields
        case _ => fields
      }).mkString(", ")

      val tName = tableName(regDef)
      exec(db, s"CREATE TABLE IF NOT EXISTS $tName ($idKey INTEGER PRIMARY KEY AUTOINCREMENT, $columns)")

      regDef match {
        case _: CollectibleRegisterDefinition[_] =>
          exec(db, s"CREATE INDEX IF NOT EXISTS ${tName}_${SQLiteStorageManager.collKey} ON $tName(${SQLiteStorageManager.collKey})")
        case _ =>
      }

      regDef.fields.collect {
        case f: ForeignKeyFieldDefinition => f
      }.foreach { f =>
        val fName = fieldName(regDef, f)
        exec(db, s"CREATE INDEX IF NOT EXISTS ${tName}_$fName ON $tName($fName)")
      }
    }
  }

  private def removeAllResolvedBunches(db: SQLiteDatabase): Unit = {
    val keys = keysFor(db, redundant.ResolvedBunch)
    for (key <- keys) {
      delete(db, key)
    }
  }

  /**
   * As some agents targets bunches that are sources of other agents, they must be sorted before
   * trigger them.
   */
  private def sortedAgents(db: SQLiteDatabase): Seq[Agent] = {
    var agents = getMapFor(db, registers.Agent).values.toSet
    if (agents.isEmpty) Nil
    else {
      val result = ListBuffer[registers.Agent]()
      var count = Integer.MAX_VALUE
      while (count > agents.size) {
        count = agents.size

        val currentAgents = agents
        val targets = agents.map(_.targetBunch)
        for (agent <- currentAgents) {
          if (!targets(agent.sourceBunch) && !targets(agent.sourceBunch)) {
            agents -= agent
            result += agent
          }
        }
      }

      result.toList
    }
  }

  /**
   * Fill the resolvedBunch redundant register for the given agent
   */
  private def processAgent(db: SQLiteDatabase, agent: registers.Agent, texts: scala.collection.Map[Register.CollectionId, String]): Unit = {
    val nullBunchKey = obtainKey(registers.Bunch, 0, 0)
    val sourceWords = {
      if (agent.sourceBunch == nullBunchKey) {
        keysFor(db, registers.Word)
      }
      else {
        getMapFor(db, redundant.ResolvedBunch, BunchReferenceField(agent.sourceBunch)).map(_._2.word).toSet
      }
    }

    val filteredWords = {
      if (Agent.Flags.shouldFilterFromSource(agent.flags)) {
        val result = ListBuffer[Key]()

        // TODO: Another redundant table is required to keep resolved kanji representations
        // from kana or roumaji from kana
        val correlation = getCollection(db, registers.Correlation, agent.correlation)
            .map(e => (e.alphabet, texts(e.symbolArray))).toMap

        for (word <- sourceWords) {
          // TODO: AcceptationRepresentation should be taken into account as well
          val wordRepresentations = getMapFor(db, registers.WordRepresentation, WordReferenceField(word)).values
          val alphabets = wordRepresentations.map(_.alphabet).toSet
          if (correlation.keySet.forall(alphabets)) {
            val f = {
              if (Agent.Flags.startSide(agent.flags)) (a: String, b: String) => a.startsWith(b)
              else (a: String, b: String) => a.endsWith(b)
            }

            if (correlation.forall { case (alphabet, correlationText) =>
              wordRepresentations.filter(_.alphabet == alphabet).exists(repr => f(texts(repr.symbolArray), correlationText))
            }) {
              result += word
            }
          }
        }

        result.toSet
      }
      else sourceWords
    }

    val diffWords = {
      if (agent.diffBunch == nullBunchKey) {
        Set[Key]()
      }
      else {
        getMapFor(db, redundant.ResolvedBunch, BunchReferenceField(agent.diffBunch)).map(_._2.word).toSet
      }
    }

    val resultSet = filteredWords diff diffWords

    // TODO: Add logic to 'add' the correlation if append or prepend flags are active

    for (result <- resultSet) {
      insert(db, redundant.ResolvedBunch(agent.targetBunch, result))
    }
  }

  private def updateResolvedBunches(db: SQLiteDatabase, texts: scala.collection.Map[Register.CollectionId, String]): Unit = {
    removeAllResolvedBunches(db)
    val agents = sortedAgents(db)
    for (agent <- agents) {
      processAgent(db, agent, texts)
    }
  }

  private def removeAllWordTexts(db: SQLiteDatabase): Unit = {
    val keys = keysFor(db, redundant.WordText)
    for (key <- keys) {
      delete(db, key)
    }
  }

  private def copySymbolArraysToWordTexts(db: SQLiteDatabase): Unit = {
    val wordReprs = getMapFor(db, registers.WordRepresentation).values
    for (wordRepr <- wordReprs) {
      val texts = getMapFor(db, redundant.Text, NullableSymbolArrayReferenceField(wordRepr.symbolArray))
      if (texts.size != 1) {
        throw new AssertionError(s"A single text was expected to be found for symbol array ${wordRepr.symbolArray} but ${texts.size} were found")
      }

      insert(db, redundant.WordText(wordRepr.word, wordRepr.alphabet, texts.head._1))
    }
  }

  private def convertTextIteration(text: String, pairs: Seq[(String, String)], acc: String): Option[String] = {
    val matching = pairs.find { case (t,_) => text.startsWith(t) }
    matching.flatMap { case (source, target) =>
      val newText = text.substring(source.length)
      val newAcc = acc + target
      if (newText == "") Some(newAcc)
      else convertTextIteration(newText, pairs, newAcc)
    }
  }

  private def convertText(text: String, pairs: Seq[(String, String)]): Option[String] = {
    convertTextIteration(text, pairs, "")
  }

  private def convertAlphabets(db: SQLiteDatabase, texts: scala.collection.Map[Register.CollectionId, String]): Unit = {
    val nullSymbolArray: Register.CollectionId = 0
    val conversions = getMapFor(db, registers.Conversion).values
    for (conversion <- conversions) {
      // This is ignoring the AcceptationRepresentations assuming that the only conversion registers
      // are coming from kana, which is not expected to be as AcceptationRepresentation.
      // TODO: This should also include the AcceptationRepresentations to be generic
      val sources = getMapFor(db, registers.WordRepresentation, AlphabetReferenceField(conversion.sourceAlphabet)).values
      val targetedWords = getMapFor(db, registers.WordRepresentation, AlphabetReferenceField(conversion.targetAlphabet)).map(_._2.word).toSet

      val toProcess = sources.filterNot(source => targetedWords(source.word))
      if (toProcess.nonEmpty) {
        val pairs = getArray(db, registers.ConversionPair, conversion.conversionArray)
        val conversionList = pairs.map(pair => texts(pair.sourceSymbolArray) -> texts(pair.targetSymbolArray))
        for {
          wordRepr <- toProcess
          newText <- convertText(texts(wordRepr.symbolArray), conversionList)
        } {
          val textReg = redundant.Text(nullSymbolArray, newText)
          val textKey = find(db, textReg).headOption.getOrElse {
            insert(db, redundant.Text(nullSymbolArray, newText)).get
          }
          insert(db, redundant.WordText(wordRepr.word, conversion.targetAlphabet, textKey))
        }
      }
    }
  }

  private def updateWordTexts(db: SQLiteDatabase, texts: scala.collection.Map[Register.CollectionId, String]): Unit = {
    removeAllWordTexts(db)
    copySymbolArraysToWordTexts(db)
    convertAlphabets(db, texts)
  }

  def initializeDatabase(db: SQLiteDatabase): Unit = {
    // As a temporal solution, we add some data to the data base
    import sword.langbook.db.registers

    def insertConcept(hint :String) = {
      insert(db, new Register {
        override val definition = registers.Concept
        override val fields = List(CharSequenceField(hint))
      })
    }

    // Adding the English language
    // TODO: Find a way to add languages dynamically and not hardcode them on creating the database
    val enLanguageText = "Language"
    val spLanguageText = "Idioma"
    val kanjiLanguageText = "言語"
    val kanaLanguageText = "げんご"

    val englishEnText = "English"
    val englishSpText = "Inglés"
    val englishJpText = "英語"
    val englishKanaText = "えいご"
    val spanishEnText = "Spanish"
    val spanishSpText = "Español"
    val spanishJpText = "スペイン語"
    val spanishKanaText = "スペインご"
    val japaneseEnText = "Japanese"
    val japaneseSpText = "Japonés"
    val japaneseJpText = "日本語"
    val japaneseKanaText = "にほんご"

    val kanjiJpText = "漢字"
    val kanjiKanaText = "かんじ"
    val kanaJpText = "仮名"
    val kanaKanaText = "かな"

    val uKanaText = "う"
    val kuKanaText = "く"
    val ruKanaText = "る"

    val aruRoumajiText = "aru"
    val oruRoumajiText = "oru"
    val uruRoumajiText = "uru"

    val ttaKanaText = "った"

    val string =
        enLanguageText + spLanguageText + kanjiLanguageText + kanaLanguageText +
        englishEnText + englishSpText + englishJpText + englishKanaText +
        spanishEnText + spanishSpText + spanishJpText + spanishKanaText +
        japaneseEnText + japaneseSpText + japaneseJpText + japaneseKanaText +
        kanjiJpText + kanjiKanaText + kanaJpText + kanaKanaText + uKanaText + kuKanaText +
        ruKanaText + aruRoumajiText + oruRoumajiText + uruRoumajiText + ttaKanaText

    val kana2RoumajiConversionList = sword.langbook.db.Word.hiraganaConversions
    val conversionCharacters = kana2RoumajiConversionList.map { case (a,b) => a + b }.mkString("")
    val symbols = {
      for {
        symbol <- (conversionCharacters + string).toSet[Char]
        key <- insert(db, registers.Symbol(symbol.toInt))
      } yield (symbol, key)
    }.toMap

    val enAlphabetConceptKey = insertConcept(SQLiteStorageManager.englishAlphabetHint).get
    val enAlphabetKey = insert(db, registers.Alphabet(enAlphabetConceptKey)).get

    val spAlphabetConceptKey = insertConcept(SQLiteStorageManager.spanishAlphabetHint).get
    val spAlphabetKey = insert(db, registers.Alphabet(spAlphabetConceptKey)).get

    val kanjiAlphabetConceptKey = insertConcept(SQLiteStorageManager.kanjiAlphabetHint).get
    val kanjiAlphabetKey = insert(db, registers.Alphabet(kanjiAlphabetConceptKey)).get
    val kanaAlphabetConceptKey = insertConcept(SQLiteStorageManager.kanaAlphabetHint).get
    val kanaAlphabetKey = insert(db, registers.Alphabet(kanaAlphabetConceptKey)).get
    val roumajiAlphabetConceptKey = insertConcept(SQLiteStorageManager.roumajiAlphabetHint).get
    val roumajiAlphabetKey = insert(db, registers.Alphabet(roumajiAlphabetConceptKey)).get

    val englishConceptKey = insertConcept("English").get
    val spanishConceptKey = insertConcept("Spanish").get
    val japaneseConceptKey = insertConcept("Japanese").get

    val languageConceptKey = insertConcept("language").get
    insert(db, registers.ConceptTypeRelation(englishConceptKey, languageConceptKey))
    insert(db, registers.ConceptTypeRelation(spanishConceptKey, languageConceptKey))
    insert(db, registers.ConceptTypeRelation(japaneseConceptKey, languageConceptKey))

    val englishKey = insert(db, registers.Language(englishConceptKey, LanguageCodes.english, enAlphabetKey)).get
    val spanishKey = insert(db, registers.Language(spanishConceptKey, LanguageCodes.spanish, spAlphabetKey)).get
    val japaneseKey = insert(db, registers.Language(japaneseConceptKey, LanguageCodes.japanese, kanjiAlphabetKey)).get

    val texts = scala.collection.mutable.Map[String, Register.CollectionId]()
    val wordTexts = List(
      enLanguageText,
      spLanguageText,
      kanjiLanguageText,
      kanaLanguageText,
      englishEnText,
      englishSpText,
      englishJpText,
      englishKanaText,
      spanishEnText,
      spanishSpText,
      spanishJpText,
      spanishKanaText,
      japaneseEnText,
      japaneseSpText,
      japaneseJpText,
      japaneseKanaText,
      kanjiJpText,
      kanjiKanaText,
      kanaJpText,
      kanaKanaText
    )
    val arrayIds = insertSymbolArrays(db, wordTexts, symbols, texts).iterator
    val languageEnSymbolArrayCollection = arrayIds.next
    val languageSpSymbolArrayCollection = arrayIds.next
    val languageKanjiSymbolArrayCollection = arrayIds.next
    val languageKanaSymbolArrayCollection = arrayIds.next
    val englishEnSymbolArrayCollection = arrayIds.next
    val englishSpSymbolArrayCollection = arrayIds.next
    val englishJpSymbolArrayCollection = arrayIds.next
    val englishKanaSymbolArrayCollection = arrayIds.next
    val spanishEnSymbolArrayCollection = arrayIds.next
    val spanishSpSymbolArrayCollection = arrayIds.next
    val spanishJpSymbolArrayCollection = arrayIds.next
    val spanishKanaSymbolArrayCollection = arrayIds.next
    val japaneseEnSymbolArrayCollection = arrayIds.next
    val japaneseSpSymbolArrayCollection = arrayIds.next
    val japaneseJpSymbolArrayCollection = arrayIds.next
    val japaneseKanaSymbolArrayCollection = arrayIds.next
    val kanjiJpSymbolArrayCollection = arrayIds.next
    val kanjiKanaSymbolArrayCollection = arrayIds.next
    val kanaJpSymbolArrayCollection = arrayIds.next
    val kanaKanaSymbolArrayCollection = arrayIds.next

    val languageEnWord = insert(db, registers.Word(englishKey)).get
    val languageSpWord = insert(db, registers.Word(spanishKey)).get
    val languageJpWord = insert(db, registers.Word(japaneseKey)).get
    val englishEnWord = insert(db, registers.Word(englishKey)).get
    val englishSpWord = insert(db, registers.Word(spanishKey)).get
    val englishJpWord = insert(db, registers.Word(japaneseKey)).get
    val spanishEnWord = insert(db, registers.Word(englishKey)).get
    val spanishSpWord = insert(db, registers.Word(spanishKey)).get
    val spanishJpWord = insert(db, registers.Word(japaneseKey)).get
    val japaneseEnWord = insert(db, registers.Word(englishKey)).get
    val japaneseSpWord = insert(db, registers.Word(spanishKey)).get
    val japaneseJpWord = insert(db, registers.Word(japaneseKey)).get
    val kanjiJpWord = insert(db, registers.Word(japaneseKey)).get
    val kanaJpWord = insert(db, registers.Word(japaneseKey)).get

    insert(db, registers.WordRepresentation(languageEnWord, enAlphabetKey, languageEnSymbolArrayCollection))
    insert(db, registers.WordRepresentation(languageSpWord, spAlphabetKey, languageSpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(languageJpWord, kanjiAlphabetKey, languageKanjiSymbolArrayCollection))
    insert(db, registers.WordRepresentation(languageJpWord, kanaAlphabetKey, languageKanaSymbolArrayCollection))
    insert(db, registers.WordRepresentation(englishEnWord, enAlphabetKey, englishEnSymbolArrayCollection))
    insert(db, registers.WordRepresentation(englishSpWord, spAlphabetKey, englishSpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(englishJpWord, kanjiAlphabetKey, englishJpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(englishJpWord, kanaAlphabetKey, englishKanaSymbolArrayCollection))
    insert(db, registers.WordRepresentation(spanishEnWord, enAlphabetKey, spanishEnSymbolArrayCollection))
    insert(db, registers.WordRepresentation(spanishSpWord, spAlphabetKey, spanishSpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(spanishJpWord, kanjiAlphabetKey, spanishJpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(spanishJpWord, kanaAlphabetKey, spanishKanaSymbolArrayCollection))
    insert(db, registers.WordRepresentation(japaneseEnWord, enAlphabetKey, japaneseEnSymbolArrayCollection))
    insert(db, registers.WordRepresentation(japaneseSpWord, spAlphabetKey, japaneseSpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(japaneseJpWord, kanjiAlphabetKey, japaneseJpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(japaneseJpWord, kanaAlphabetKey, japaneseKanaSymbolArrayCollection))
    insert(db, registers.WordRepresentation(kanjiJpWord, kanjiAlphabetKey, kanjiJpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(kanjiJpWord, kanaAlphabetKey, kanjiKanaSymbolArrayCollection))
    insert(db, registers.WordRepresentation(kanaJpWord, kanjiAlphabetKey, kanaJpSymbolArrayCollection))
    insert(db, registers.WordRepresentation(kanaJpWord, kanaAlphabetKey, kanaKanaSymbolArrayCollection))

    insert(db, registers.Acceptation(languageEnWord, languageConceptKey))
    insert(db, registers.Acceptation(languageSpWord, languageConceptKey))
    insert(db, registers.Acceptation(languageJpWord, languageConceptKey))
    insert(db, registers.Acceptation(englishEnWord, englishConceptKey))
    insert(db, registers.Acceptation(englishSpWord, englishConceptKey))
    insert(db, registers.Acceptation(englishJpWord, englishConceptKey))
    insert(db, registers.Acceptation(spanishEnWord, spanishConceptKey))
    insert(db, registers.Acceptation(spanishSpWord, spanishConceptKey))
    insert(db, registers.Acceptation(spanishJpWord, spanishConceptKey))
    insert(db, registers.Acceptation(japaneseEnWord, japaneseConceptKey))
    insert(db, registers.Acceptation(japaneseSpWord, japaneseConceptKey))
    insert(db, registers.Acceptation(japaneseJpWord, japaneseConceptKey))
    insert(db, registers.Acceptation(englishEnWord, enAlphabetConceptKey))
    insert(db, registers.Acceptation(englishSpWord, enAlphabetConceptKey))
    insert(db, registers.Acceptation(englishJpWord, enAlphabetConceptKey))
    insert(db, registers.Acceptation(spanishEnWord, spAlphabetConceptKey))
    insert(db, registers.Acceptation(spanishSpWord, spAlphabetConceptKey))
    insert(db, registers.Acceptation(spanishJpWord, spAlphabetConceptKey))
    insert(db, registers.Acceptation(kanjiJpWord, kanjiAlphabetConceptKey))
    insert(db, registers.Acceptation(kanaJpWord, kanaAlphabetConceptKey))

    // Conversions
    val kana2RoumajiPairs = {
      for ((sourceText, targetText) <- kana2RoumajiConversionList) yield {
        val arrayIds = insertSymbolArrays(db, List(sourceText, targetText), symbols, texts).iterator
        val sourceSymbolArray = arrayIds.next()
        val targetSymbolArray = arrayIds.next()
        registers.ConversionPair(sourceSymbolArray, targetSymbolArray)
      }
    }
    val kana2RoumajiConversionArray = insert(db, kana2RoumajiPairs).get
    insert(db, registers.Conversion(kanaAlphabetKey, roumajiAlphabetKey, kana2RoumajiConversionArray))

    // Just here in case the autoincrement id algorithm starts giving the id 0. As id 0 is used for the whole database.
    insert(db, registers.Bunch("Unused"))

    // Temporal trial to test Agents
    val nullBunchKey = obtainKey(registers.Bunch, 0, 0)
    val nullCorrelationId: Register.CollectionId = 0

    val uGodanBunchKey = insert(db, registers.Bunch("Godan verb finishing in う")).get
    val kuGodanBunchKey = insert(db, registers.Bunch("Godan verb finishing in く")).get
    val ruVerbsBunchKey = insert(db, registers.Bunch("Verbs ending with る")).get
    val ichidanBunchKey = insert(db, registers.Bunch("Ichidan verb")).get

    // This is the target for some agents, but should include statically any -eru and -iru godan
    // verbs as rule exception
    val ruGodanBunchKey = insert(db, registers.Bunch("Godan verb ending with る")).get

    val preInformalPastBunchKey = insert(db, registers.Bunch("Pre informal past")).get
    val informalPastBunchKey = insert(db, registers.Bunch("informal past")).get

    val correlationTexts = List(
      uKanaText, kuKanaText, ruKanaText,
      aruRoumajiText, oruRoumajiText, uruRoumajiText,
      ttaKanaText
    )
    val arrayCorrelationIds = insertSymbolArrays(db, correlationTexts, symbols, texts).iterator
    val uKanaSymbolArrayCollection = arrayCorrelationIds.next()
    val uKanaCorrelation = insert(db, List(
        registers.Correlation(kanjiAlphabetKey, uKanaSymbolArrayCollection),
        registers.Correlation(kanaAlphabetKey, uKanaSymbolArrayCollection))).get

    val kuKanaSymbolArrayCollection = arrayCorrelationIds.next()
    val kuKanaCorrelation = insert(db, List(
      registers.Correlation(kanjiAlphabetKey, kuKanaSymbolArrayCollection),
      registers.Correlation(kanaAlphabetKey, kuKanaSymbolArrayCollection))).get

    val ruKanaSymbolArrayCollection = arrayCorrelationIds.next()
    val ruKanjiCorrelation = insert(db, List(
      registers.Correlation(kanjiAlphabetKey, ruKanaSymbolArrayCollection)
    )).get

    val aruRoumajiSymbolArrayCollection = arrayCorrelationIds.next()
    val aruRoumajiCorrelation = insert(db, List(
      registers.Correlation(roumajiAlphabetKey, aruRoumajiSymbolArrayCollection)
    )).get

    val oruRoumajiSymbolArrayCollection = arrayCorrelationIds.next()
    val oruRoumajiCorrelation = insert(db, List(
      registers.Correlation(roumajiAlphabetKey, oruRoumajiSymbolArrayCollection)
    )).get

    val uruRoumajiSymbolArrayCollection = arrayCorrelationIds.next()
    val uruRoumajiCorrelation = insert(db, List(
      registers.Correlation(roumajiAlphabetKey, uruRoumajiSymbolArrayCollection)
    )).get

    val ttaKanaSymbolArrayCollection = arrayCorrelationIds.next()
    val ttaKanaCorrelation = insert(db, List(
      registers.Correlation(kanjiAlphabetKey, ttaKanaSymbolArrayCollection),
      registers.Correlation(kanaAlphabetKey, ttaKanaSymbolArrayCollection)
    )).get

    val matchEndFlags = Agent.Flags.matchEnd
    insert(db, registers.Agent(nullBunchKey, uGodanBunchKey, nullBunchKey, uKanaCorrelation, matchEndFlags))
    insert(db, registers.Agent(nullBunchKey, kuGodanBunchKey, nullBunchKey, kuKanaCorrelation, matchEndFlags))

    insert(db, registers.Agent(nullBunchKey, ruVerbsBunchKey, nullBunchKey, ruKanjiCorrelation, matchEndFlags))
    insert(db, registers.Agent(ruVerbsBunchKey, ruGodanBunchKey, nullBunchKey, aruRoumajiCorrelation, matchEndFlags))
    insert(db, registers.Agent(ruVerbsBunchKey, ruGodanBunchKey, nullBunchKey, oruRoumajiCorrelation, matchEndFlags))
    insert(db, registers.Agent(ruVerbsBunchKey, ruGodanBunchKey, nullBunchKey, uruRoumajiCorrelation, matchEndFlags))
    insert(db, registers.Agent(ruVerbsBunchKey, ichidanBunchKey, ruGodanBunchKey, nullCorrelationId, matchEndFlags))

    val removeEndFlags = Agent.Flags.removeEnd
    insert(db, registers.Agent(uGodanBunchKey, preInformalPastBunchKey, nullBunchKey, uKanaCorrelation, removeEndFlags))
    insert(db, registers.Agent(ruGodanBunchKey, preInformalPastBunchKey, nullBunchKey, uKanaCorrelation, removeEndFlags))

    val appendFlags = Agent.Flags.append
    insert(db, registers.Agent(preInformalPastBunchKey, informalPastBunchKey, nullBunchKey, ttaKanaCorrelation, appendFlags))

    val reversedTexts = texts.map(_.swap)
    updateWordTexts(db, reversedTexts)
    updateResolvedBunches(db, reversedTexts)
  }

  override def onCreate(db: SQLiteDatabase): Unit = {
    createTables(db)
    initializeDatabase(db)
  }

  override def onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int): Unit = {
    if (newVersion == SQLiteStorageManager.currentDbVersion) {
      createTables(db)
      initializeDatabase(db)
    }

    if (oldVersion == 3 && newVersion == SQLiteStorageManager.currentDbVersion) {
      fromDbVersion3(db)
    }
  }

  private def currentSymbolsInDatabase(db: SQLiteDatabase): Map[Register.UnicodeType, Key] = {
    getMapFor(db, registers.Symbol).map { case (a,b) => (b.unicode, a) }.toMap
  }

  private def insertNewSymbolsAndRetrieveSymbolMap(db: SQLiteDatabase, cursor: Cursor): Map[Register.UnicodeType, Key] = {
    val newSymbols = scala.collection.mutable.Set[Char]()
    do {
      newSymbols ++= cursor.getString(0) ++= cursor.getString(1) ++= cursor.getString(2)
    } while(cursor.moveToNext())

    val currentSymbols = currentSymbolsInDatabase(db)

    currentSymbols ++ (for {
      missingSymbol <- newSymbols.map(_.toInt).toSet.diff(currentSymbols.keys.toSet)
    } yield {
      (missingSymbol, insert(db, registers.Symbol(missingSymbol)).get)
    })
  }

  private def insertSymbolArraysAndReturnIds(
      db: SQLiteDatabase, wordTexts: Seq[String],
      allSymbols: scala.collection.Map[Register.UnicodeType, Key],
      texts: scala.collection.mutable.Map[String, Register.CollectionId]): Seq[Register.CollectionId] = {

    val newWordTexts = wordTexts.filterNot(texts.keySet)
    if (newWordTexts.nonEmpty) {
      val newSymbolArrays = newWordTexts.map(_.map(c => registers.SymbolPosition(allSymbols(c.toInt))))
      val newIds = insertCollections(db, newSymbolArrays)
      for (pair <- newWordTexts zip newIds) {
        insert(db, redundant.Text(pair._2, pair._1))
        texts += pair
      }
    }

    wordTexts.map(texts)
  }

  private def insertSymbolArrays(
      db: SQLiteDatabase, wordTexts: Seq[String],
      allSymbols: scala.collection.Map[Char, Key],
      texts: scala.collection.mutable.Map[String, Register.CollectionId]): Seq[Register.CollectionId] = {
    val symbols = allSymbols.map { case (key, value) => (key.toInt, value)}
    insertSymbolArraysAndReturnIds(db, wordTexts, symbols, texts)
  }

  private def insertNewRepresentationForEachAcceptation(db: SQLiteDatabase, jaWord: Key,
      currentAcceptations: scala.collection.Map[Key, registers.Acceptation], inputConcepts: Iterable[Key], representation: Register.CollectionId): Unit = {

    val acceptations = for (inputConcept <- inputConcepts) yield {
      currentAcceptations.find(_._2.concept == inputConcept).map(_._1).getOrElse {
        insert(db, registers.Acceptation(jaWord, inputConcept)).get
      }
    }

    for (acceptation <- acceptations) {
      insert(db, registers.AcceptationRepresentation(acceptation, representation))
    }
  }

  private def insertNewKanjiRepresentation(db: SQLiteDatabase, word: Key, kanjiAlphabet: Key, representation: Register.CollectionId): Unit = {
    insert(db, registers.WordRepresentation(word, kanjiAlphabet, representation))
  }

  private def moveRepresentationFromWordToItsAcceptations(db: SQLiteDatabase, jaWord: Key, kanjiAlphabet: Key, acceptations: Iterable[Key]): Unit = {
    val wordRepresentations = getMapFor(db, registers.WordRepresentation, WordReferenceField(jaWord))
        .filter(_._2.alphabet == kanjiAlphabet)
    for {
      representation <- wordRepresentations.values.map(_.symbolArray)
      acceptation <- acceptations
    } {
      insert(db, registers.AcceptationRepresentation(acceptation, representation))
    }

    for (key <- wordRepresentations.keySet) {
      delete(db, key)
    }
  }

  private def insertKanjiRepresentationAndPossibleAcceptations(db: SQLiteDatabase, jaWord: Key,
      inputConcepts: Set[Key], kanjiAlphabet: Key, representation: Register.CollectionId): Unit = {

    val jaWordAcceptations = getMapFor(db, registers.Acceptation, WordReferenceField(jaWord))
    val jaWordAcceptationConcepts = jaWordAcceptations.mapValues(_.concept)
    val jaWordConcepts = jaWordAcceptationConcepts.values.toSet

    val sameConcepts = jaWordConcepts == inputConcepts
    def jaWordConceptsContainedInInput = jaWordConcepts.forall(inputConcepts)
    def inputConceptsContainedInJaWord = inputConcepts.forall(jaWordConcepts)

    if (sameConcepts) {
      insertNewKanjiRepresentation(db, jaWord, kanjiAlphabet, representation)
    }
    else if (inputConceptsContainedInJaWord) {
      insertNewRepresentationForEachAcceptation(db, jaWord, jaWordAcceptations, inputConcepts, representation)
    }
    else {
      moveRepresentationFromWordToItsAcceptations(db, jaWord, kanjiAlphabet, jaWordAcceptationConcepts.keySet)

      if (jaWordConceptsContainedInInput) {
        insertNewKanjiRepresentation(db, jaWord, kanjiAlphabet, representation)

        for (inputConcept <- inputConcepts) {
          insert(db, registers.Acceptation(jaWord, inputConcept))
        }
      }
      else { // Both sets contains elements that are not contained in the other
        insertNewRepresentationForEachAcceptation(db, jaWord, jaWordAcceptations, inputConcepts, representation)
      }
    }
  }

  private def fromDbVersion3(db: SQLiteDatabase): Unit = {
    import sword.langbook.db.registers

    val cursor = query(db, "WordRegister", Array("mWrittenWord", "mPronunciation", "meaning"),
      null, null)

    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        val allSymbols = insertNewSymbolsAndRetrieveSymbolMap(db, cursor)

        if (!cursor.moveToFirst()) return

        val languages = getMapFor(db, registers.Language)
        val (spanishKey, spanishAlphabetKey) = languages.collectFirst {
          case (key, reg) if reg.code == "es" => (key, reg.preferredAlphabet)
        }.get

        val (japaneseKey, kanjiKey) = languages.collectFirst {
          case (key, reg) if reg.code == "ja" => (key, reg.preferredAlphabet)
        }.get

        Log.i("kana extraction", "japaneseKey is " + japaneseKey.encoded)
        Log.i("kana extraction", "kanjiKey is " + kanjiKey.encoded)

        val jpWords = keysFor(db, registers.Word, LanguageReferenceField(japaneseKey))
        val jpAlphabetKeys = getMapFor(db, registers.WordRepresentation)
            .foldLeft(Set[Key]()) { (set, repr) =>
              if (jpWords(repr._2.word)) set + repr._2.alphabet
              else set
            }

        assert(jpAlphabetKeys.size == 2)
        val kanaKey = (jpAlphabetKeys - kanjiKey).head

        val representations = scala.collection.mutable.Map[String, registers.WordRepresentation]()
        for (repr <- getMapFor(db, registers.WordRepresentation, AlphabetReferenceField(spanishAlphabetKey)).values) {
          val str = getMapFor(db, redundant.Text, SymbolArrayReferenceField(repr.symbolArray)).values.head.text
          representations(str) = repr
        }

        val texts = scala.collection.mutable.Map[String, Register.CollectionId]()
        for (textReg <- getMapFor(db, redundant.Text).values) {
          texts(textReg.text) = textReg.symbolArray
        }

        // TODO: This map should be prefilled with the existing words in the database
        val spanishWords = scala.collection.mutable.Map[String, Register.Index]()

        do {
          val writtenText = cursor.getString(0)
          val kanaText = cursor.getString(1)
          val givenMeaning = cursor.getString(2)

          // From Db version 3 it is possible to find in the meaning field semicolon ';' characters.
          // semicolons are used here to separate different acceptations for the same word.
          // Within each acceptation, it is possible to find commas ',' to separate synonyms as well.
          val meaningTexts: Array[Array[String]] = {
            val acceptationTexts = {
              if (givenMeaning.indexOf(";") >= 0) {
                givenMeaning.split(";").map(_.trim).filter(s => s != null && s.length() > 0)
              }
              else {
                Array(givenMeaning)
              }
            }

            for (acceptationText <- acceptationTexts) yield {
              if (acceptationText.indexOf("(") >= 0 || acceptationText.indexOf(")") >= 0 || acceptationText.indexOf("/") >= 0 || acceptationText.indexOf(",") < 0) {
                Array(acceptationText)
              }
              else {
                acceptationText.split(",").map(_.trim)
              }
            }
          }

          // Before creating a new word, the kana has to be checked. If there is another word
          // with the same kana, we will assume that they are the same word, then no new word is
          // required, but the other one has to be reused.
          val jaWordOption = texts.get(kanaText).flatMap { kanaCollId =>
            val values = getMapFor(db, WordRepresentation, SymbolArrayReferenceField(kanaCollId))
                .filter(_._2.alphabet == kanaKey).map(_._2.word)
            if (values.size >= 2) throw new AssertionError(s"Found more than one word with the same kana '$kanaText'")
            values.headOption
          }

          // Check if the text is already in the database and reuses it if possible
          val wordTexts = writtenText :: kanaText :: meaningTexts.toList.flatten
          val ids = insertSymbolArraysAndReturnIds(db, wordTexts, allSymbols, texts)

          val jaWord = jaWordOption.getOrElse {
            val wordKey = insert(db, registers.Word(japaneseKey)).get
            insert(db, registers.WordRepresentation(wordKey, kanaKey, ids(1)))
            wordKey
          }

          // Add all missing Spanish words
          for (text <- meaningTexts.toList.flatten) {
            if (!spanishWords.contains(text)) {
              val key = insert(db, registers.Word(spanishKey)).get
              spanishWords(text) = key.index
              insert(db, registers.WordRepresentation(key, spanishAlphabetKey, texts(text)))
            }
          }

          // Check if there is another word that includes the same meanings and that
          // has only a concept assigned to it. If so, it is understood that both words
          // are synonym and the same concept should be reused. If not, a new concept should be
          // created
          val concepts = for (acceptationTexts <- meaningTexts) yield {
            val wordKeys = acceptationTexts.map(text => obtainKey(registers.Word, 0, spanishWords(text)))
            val commonConcepts = wordKeys.map { key =>
              getMapFor(db, registers.Acceptation, WordReferenceField(key)).values.map(_.concept).toSet }.reduce(_.intersect(_))
            val conceptOption = commonConcepts.find(concept => getMapFor(db, registers.Acceptation, ConceptReferenceField(concept)).size == wordKeys.length)
            conceptOption.getOrElse {
              val concept = insert(db, registers.Concept(acceptationTexts.mkString(", "))).get
              for (wordKey <- wordKeys) {
                insert(db, registers.Acceptation(wordKey, concept))
              }
              concept
            }
          }

          insertKanjiRepresentationAndPossibleAcceptations(db, jaWord, concepts.toSet, kanjiKey, ids.head)
        } while(cursor.moveToNext())

        val reversedTexts = texts.map(_.swap)
        updateWordTexts(db, reversedTexts)
        updateResolvedBunches(db, reversedTexts)
      }
    } finally {
      cursor.close()
    }
  }

  private def mapFor[R <: Register](regDef: RegisterDefinition[R],
      f: (String, Array[String]) => Cursor): scala.collection.Map[Key, R] = {

    val array = (regDef match {
      case _: CollectibleRegisterDefinition[_] =>
        Array(SQLiteStorageManager.idKey, SQLiteStorageManager.collKey)
      case _ =>
        Array(SQLiteStorageManager.idKey)
    }) ++ regDef.fields.map(fieldName(regDef,_))

    val cursor = f(tableName(regDef), array)

    if (cursor == null) Map()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Map()
      else {
        val buffer = new ListBuffer[(Key, R)]()
        do {
          val group = regDef match {
            case _: CollectibleRegisterDefinition[_] => cursor.getInt(1)
            case _ => 0
          }
          val key = obtainKey(regDef, group, cursor.getInt(0))
          val reg = fromCursor(regDef, cursor)
          buffer += ((key, reg))
        } while(cursor.moveToNext())
        buffer.result().toMap
      }
    } finally {
      cursor.close()
    }
  }

  private def getMapFor[R <: Register](db: SQLiteDatabase,
      regDef: RegisterDefinition[R]): scala.collection.Map[Key, R] = {
    mapFor(regDef, query(db, _, _, null, null))
  }

  private def getMapFor[R <: Register](db: SQLiteDatabase, regDef: RegisterDefinition[R], filter: ForeignKeyField): scala.collection.Map[Key, R] = {
    val whereClause = s"${fieldName(regDef, filter)}=${filter.key.index}"
    mapFor(regDef, query(db, _, _, whereClause, null))
  }

  private def getMapFor[R <: Register](db: SQLiteDatabase, regDef: RegisterDefinition[R], filter: CollectionReferenceField): scala.collection.Map[Key, R] = {
    val whereClause = s"${fieldName(regDef, filter)}=${filter.collectionId}"
    mapFor(regDef, query(db, _, _, whereClause, null))
  }

  private def getMapForCollection[R <: Register](db: SQLiteDatabase,
      registerDefinition: CollectibleRegisterDefinition[R], id: CollectionId): Map[Key, R] = {
    val keys = Seq(SQLiteStorageManager.idKey) ++ registerDefinition.fields.map(fieldName(registerDefinition,_))
    val cursor = query(db, tableName(registerDefinition), keys.toArray,
      s"${SQLiteStorageManager.collKey}=${id.toString}", null)

    val result = scala.collection.mutable.Map[Key, R]()
    if (cursor != null) {
      try {
        if (cursor.getCount > 0 && cursor.moveToFirst()) {
          do {
            val thisKey = obtainKey(registerDefinition, id, cursor.getInt(0))
            result += ((thisKey, fromCursor(registerDefinition, cursor)))
          } while(cursor.moveToNext())
        }
        else Some(fromCursor(registerDefinition, cursor))
      } finally {
        cursor.close()
      }
    }

    result.toMap
  }

  private def keyExtractor(fieldDef: FieldDefinition)(value: String): Option[Key] = {
    try {
      fieldDef match {
        case f: ForeignKeyFieldDefinition => Some(obtainKey(f.target, 0, value.toInt))
        case f: NullableForeignKeyFieldDefinition => Some(obtainKey(f.target, 0, value.toInt))
        case _ => None
      }
    }
    catch {
      case _: NumberFormatException => None
    }
  }

  private def fromCursor[R <: Register](regDef :RegisterDefinition[R], cursor :Cursor) :R = {
    val fieldValues = for (fieldDef <- regDef.fields) yield {
      cursor.getString(cursor.getColumnIndex(fieldName(regDef, fieldDef)))
    }

    regDef.from(fieldValues, keyExtractor).get
  }

  private case class GetParams(regDef: RegisterDefinition[Register]) {
    val table = tableName(regDef)
    val columns = regDef.fields.map(fieldName(regDef,_)).toArray
  }

  private val getParams = registerDefinitions.map(regDef => (regDef, GetParams(regDef))).toMap

  private def get(db :SQLiteDatabase, key: Key): Option[Register] = {
    val regDef = key.registerDefinition
    val params = getParams(regDef)
    val columns = params.columns
    val cursor = query(db, params.table, columns, s"${SQLiteStorageManager.idKey}=${key.index}", null)

    if (cursor == null) None
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) None
      else {
        val fieldValues = columns.indices.map(cursor.getString)
        regDef.from(fieldValues, keyExtractor)
      }
    } finally {
      cursor.close()
    }
  }

  /**
    * Extracts the value for a given field into a string that can be added to a SQL query
    */
  private def sqlValue(field :Field) :String = {
    field match {
      case f: UnicodeField => f.value.toString
      case f: IntField => f.value.toString
      case f: LanguageCodeField => s"'${f.code.toString}'"
      case f: CharSequenceField => s"'${f.value}'"
      case f: ForeignKeyField => f.key.index.toString
      case f: NullableForeignKeyField => f.key.index.toString
      case f: CollectionReferenceField => f.collectionId.toString
      case f: NullableCollectionReferenceField => f.collectionId.toString
      case f => throw new UnsupportedOperationException(s"Undefined field definition $f")
    }
  }

  private def find(db :SQLiteDatabase, reg :Register) :Seq[Key] = {
    val regDef = reg.definition
    val whereClause = reg.fields.map { field =>
      s"${fieldName(regDef, field)}=${sqlValue(field)}"
    }.mkString(" AND ")
    val cursor = query(db, tableName(regDef), Array(SQLiteStorageManager.idKey), whereClause, null)

    if (cursor == null) Nil
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Nil
      else {
        val buffer = new ListBuffer[Key]()
        do {
          buffer += obtainKey(regDef, 0, cursor.getInt(0))
        } while(cursor.moveToNext())
        buffer.result()
      }
    } finally {
      cursor.close()
    }
  }

  private def hasValidReference(db :SQLiteDatabase, register :Register) :Boolean = {
    val fields = register.fields.collect { case field :ForeignKeyField => field }
    fields.isEmpty || fields.forall { field =>
      get(db, field.key).isDefined
    }
  }

  private def insert(db :SQLiteDatabase, register: Register): Option[Key] = {
    if (hasValidReference(db, register)) {
      val regDef = register.definition
      val keys = regDef.fields.map(fieldName(regDef, _)).mkString(", ")
      val values = register.fields.map(sqlValue).mkString(", ")
      exec(db, s"INSERT INTO ${tableName(register)} ($keys) VALUES ($values)")
      find(db, register).lastOption
    }
    else None
  }

  private def insert(db :SQLiteDatabase, coll :Register.CollectionId, register: Register): Unit = {
    val regDef = register.definition
    val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")
    val values = register.fields.map(sqlValue).mkString(", ")
    exec(db, s"INSERT INTO ${tableName(register)} (${SQLiteStorageManager.collKey}, $keys) VALUES ($coll, $values)")
  }

  private def insert(db: SQLiteDatabase, collId: Register.CollectionId, registers: Traversable[Register]): Unit = {
    if (VersionUtils.SQLite.isAtLeast3_7_11) {
      val regDef = registers.head.definition
      val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")
      val allValues = {
        (for (reg <- registers) yield {
          val values = reg.fields.map(sqlValue).mkString(", ")
          s"($collId, $values)"
        }) mkString ", "
      }

      exec(db, s"INSERT INTO ${tableName(regDef)} (${SQLiteStorageManager.collKey}, $keys) VALUES $allValues")
    }
    else {
      for (register <- registers) {
        insert(db, collId, register)
      }
    }
  }

  private def maxCollectionId(db: SQLiteDatabase, regDef: RegisterDefinition[Register]): Register.CollectionId = {
    var collId = 0
    val cursor = query(db, tableName(regDef), Array(SQLiteStorageManager.collKey), null, null)

    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        do {
          val thisCollId = cursor.getInt(0)
          if (thisCollId > collId) collId = thisCollId
        } while(cursor.moveToNext())
      }
    } finally {
      cursor.close()
    }

    collId
  }

  private def insert(db: SQLiteDatabase, registers: Traversable[Register]): Option[CollectionId] = {
    val collId = maxCollectionId(db, registers.head.definition) + 1
    insert(db, collId, registers)
    Some(collId)
  }

  private def insertCollections(db: SQLiteDatabase, collections: Traversable[Traversable[Register]]): Seq[Register.CollectionId] = {
    val maxId = maxCollectionId(db, collections.head.head.definition)
    var id = maxId
    if (VersionUtils.SQLite.isAtLeast3_7_11) {
      val regDef = collections.head.head.definition
      val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")

      val allValues = (for (registers <- collections) yield {
        id += 1
        (for (reg <- registers) yield {
          val values = reg.fields.map(sqlValue).mkString(", ")
          s"($id, $values)"
        }) mkString ", "
      }).mkString(", ")

      exec(db, s"INSERT INTO ${tableName(regDef)} (${SQLiteStorageManager.collKey}, $keys) VALUES $allValues")

      (maxId + 1) to id
    }
    else {
      (for (registers <- collections) yield {
        id += 1
        insert(db, id, registers)
        id
      }).toList
    }
  }

  private def keysFor(db :SQLiteDatabase, regDef :RegisterDefinition[Register], whereClause: String) :Set[Key] = {
    val array = regDef match {
      case _: CollectibleRegisterDefinition[_] =>
        Array(SQLiteStorageManager.idKey, SQLiteStorageManager.collKey)
      case _ =>
        Array(SQLiteStorageManager.idKey)
    }

    val cursor = query(db, tableName(regDef), array, whereClause, null)

    if (cursor == null) Set()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Set()
      else {
        val buffer = new ListBuffer[Key]()
        do {
          val group = regDef match {
            case _: CollectibleRegisterDefinition[_] => cursor.getInt(1)
            case _ => 0
          }
          buffer += obtainKey(regDef, group, cursor.getInt(0))
        } while(cursor.moveToNext())
        buffer.result().toSet
      }
    } finally {
      cursor.close()
    }
  }

  private def keysFor(db :SQLiteDatabase, regDef :RegisterDefinition[Register]) :Set[Key] = {
    keysFor(db, regDef, null: String)
  }

  private def keysFor(db :SQLiteDatabase, regDef :RegisterDefinition[Register], filter: ForeignKeyField) :Set[Key] = {
    keysFor(db, regDef, s"${fieldName(regDef, filter)}=${filter.key.index}")
  }

  private def replace(db: SQLiteDatabase, register: Register, key: Key): Boolean = {
    val currentOption = get(db, key)
    if (currentOption.isDefined) {
      val expr = register.fields.map(f => s"${fieldName(register.definition, f)}=${sqlValue(f)}")
        .mkString(", ")
      exec(db, s"UPDATE ${tableName(key.registerDefinition)} SET $expr WHERE ${
        SQLiteStorageManager.idKey
      }=${key.index}")

      true
    }
    else false
  }

  private def existReference(db :SQLiteDatabase, key: Key, referencerRegDef: RegisterDefinition[Register], referencerFieldDef: ForeignKeyFieldDefinition): Boolean = {
    val whereClause = s"${fieldName(referencerRegDef, referencerFieldDef)}=${key.index}"
    val cursor = query(db, tableName(referencerRegDef), Array(SQLiteStorageManager.idKey), whereClause, null)

    if (cursor == null) false
    else try {
      cursor.getCount > 0
    } finally {
      cursor.close()
    }
  }

  private def isReferenced(db: SQLiteDatabase, key: Key): Boolean = {
    registerDefinitions.exists { regDef =>
      regDef.fields.collect {
        case x: ForeignKeyFieldDefinition if x.target == key.registerDefinition => x
      } exists { fieldDef =>
        existReference(db, key, regDef, fieldDef)
      }
    }
  }

  private def delete(db: SQLiteDatabase, key: Key): Boolean = {
    if (get(db, key).isDefined && !isReferenced(db, key)) {
      exec(db, s"DELETE FROM ${tableName(key.registerDefinition)} WHERE ${
        SQLiteStorageManager.idKey
      }=${key.index}")

      true
    }
    else false
  }

  private def getKeysForCollection(db: SQLiteDatabase, registerDefinition: CollectibleRegisterDefinition[Register], id: CollectionId): Set[Key] = {
    val whereClause = s"${SQLiteStorageManager.collKey}=$id"
    val cursor = query(db, tableName(registerDefinition), Array(SQLiteStorageManager.idKey), whereClause, null)

    if (cursor == null) Set()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Set()
      else {
        val set = scala.collection.mutable.Set[Key]()
        do {
          set += obtainKey(registerDefinition, id, cursor.getInt(0))
        } while(cursor.moveToNext())
        set.toSet
      }
    } finally {
      cursor.close()
    }
  }

  private def getKeysForArray(db: SQLiteDatabase, registerDefinition: ArrayableRegisterDefinition[Register], id: Register.CollectionId) :Seq[Key] = {
    val whereClause = s"${SQLiteStorageManager.collKey}=$id"
    val orderClause = s"${SQLiteStorageManager.idKey} ASC"
    val cursor = query(db, tableName(registerDefinition), Array(SQLiteStorageManager.idKey), whereClause, orderClause)

    if (cursor == null) Seq()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Seq()
      else {
        val buffer = scala.collection.mutable.ListBuffer[Key]()
        do {
          buffer += obtainKey(registerDefinition, id, cursor.getInt(0))
        } while(cursor.moveToNext())
        buffer.toList
      }
    } finally {
      cursor.close()
    }
  }

  private case class GetCollectionParams(regDef: RegisterDefinition[Register]) {
    val table = tableName(regDef)
    val columns = regDef.fields.map(fieldName(regDef, _)).toArray
    val columnIndices = columns.indices
  }

  private val getCollectionParams = registerDefinitions.collect {
    case regDef: CollectibleRegisterDefinition[Register] => (regDef, GetCollectionParams(regDef))
  }.toMap

  private def getCollection[R <: Register](db: SQLiteDatabase, regDef: CollectibleRegisterDefinition[R], id: Register.CollectionId): Set[R] = {
    val params = getCollectionParams(regDef)
    val cursor = query(db, params.table, params.columns, s"${SQLiteStorageManager.collKey}=$id", null)

    if (cursor == null) Set()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Set()
      else {
        val buffer = scala.collection.mutable.ListBuffer[R]()
        val indices = params.columnIndices
        do {
          val fieldValues = indices.map(cursor.getString)
          buffer += regDef.from(fieldValues, keyExtractor).get
        } while(cursor.moveToNext())
        buffer.toSet
      }
    } finally {
      cursor.close()
    }
  }

  private case class GetArrayParams(regDef: ArrayableRegisterDefinition[Register]) {
    val table = tableName(regDef)
    val columns = regDef.fields.map(fieldName(regDef, _)).toArray
    val columnIndices = columns.indices
  }

  private val getArrayParams = registerDefinitions.collect {
    case regDef: ArrayableRegisterDefinition[Register] => (regDef, GetArrayParams(regDef))
  }.toMap

  val getArrayOrderClause = s"${SQLiteStorageManager.idKey} ASC"

  private def getArray[R <: Register](db: SQLiteDatabase, regDef: ArrayableRegisterDefinition[R], id: Register.CollectionId) :Seq[R] = {

    val params = getArrayParams(regDef)
    val cursor = query(db, params.table, params.columns, s"${SQLiteStorageManager.collKey}=$id",
        getArrayOrderClause)

    if (cursor == null) Seq()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Seq()
      else {
        val buffer = scala.collection.mutable.ListBuffer[R]()
        val indices = params.columnIndices
        do {
          val fieldValues = indices.map(cursor.getString)
          buffer += regDef.from(fieldValues, keyExtractor).get
        } while(cursor.moveToNext())
        buffer.toList
      }
    } finally {
      cursor.close()
    }
  }

  private def withReadableDatabase[T](f: SQLiteDatabase => T): T = {
    val db = getReadableDatabase
    val startingTimeMillis = System.currentTimeMillis

    try {
      f(db)
    } finally {
      db.close()

      val timeMillis = System.currentTimeMillis - startingTimeMillis
      logi(s"Database query took ${_lastQueryMillis}. Whole operation took $timeMillis milliseconds")
    }
  }

  private def withWritableDatabase[T](f: SQLiteDatabase => T): T = {
    val db = getWritableDatabase
    val startingTimeMillis = System.currentTimeMillis

    try {
      f(db)
    } finally {
      db.close()

      val timeMillis = System.currentTimeMillis - startingTimeMillis
      logi(s"Database query took ${_lastQueryMillis}. Whole operation took $timeMillis milliseconds")
    }
  }

  override def get(key :Key) = {
    if (key.storageManager != this) {
      throw new IllegalArgumentException("This key do not belong to this storage manager")
    }

    withReadableDatabase(get(_, key))
  }

  override def insert(register: Register): Option[Key] = {
    withWritableDatabase(insert(_, register))
  }

  override def insert(collectionId :Register.CollectionId, register :Register) = {
    withWritableDatabase { db =>
      insert(db, collectionId, register)
      find(db, register).lastOption
    }
  }

  override def insert(registers: Traversable[Register]): Option[CollectionId] = {
    val definitions = registers.map(_.definition).toSet
    if (definitions.size != 1) {
      throw new UnsupportedOperationException("Unable to insert collections for registers with different definitions")
    }

    if (!definitions.head.isInstanceOf[CollectibleRegisterDefinition[_]]) {
      throw new UnsupportedOperationException("Unable to insert collections for non-collectible registers")
    }

    withWritableDatabase(insert(_, registers))
  }

  override def delete(key: Key): Boolean = {
    withWritableDatabase(delete(_, key))
  }

  override def replace(register: Register, key: Key): Boolean = {
    withWritableDatabase(replace(_, register, key))
  }

  override def getKeysFor(registerDefinition: RegisterDefinition[Register]): Set[Key] = {
    withReadableDatabase(keysFor(_, registerDefinition))
  }

  override def getKeysFor(registerDefinition: RegisterDefinition[Register], filter: ForeignKeyField): Set[Key] = {
    withReadableDatabase(keysFor(_, registerDefinition, filter))
  }

  override def getKeysForCollection(registerDefinition: CollectibleRegisterDefinition[Register], id: CollectionId): Set[Key] = {
    withReadableDatabase(getKeysForCollection(_, registerDefinition, id))
  }

  override def getKeysForArray(registerDefinition: ArrayableRegisterDefinition[Register], id: Register.CollectionId) :Seq[Key] = {
    withReadableDatabase(getKeysForArray(_, registerDefinition, id))
  }

  override def getMapFor[R <: Register](registerDefinition: RegisterDefinition[R], filter: ForeignKeyField): scala.collection.Map[Key, R] = {
    withReadableDatabase(getMapFor(_, registerDefinition, filter))
  }

  override def getMapForCollection[R <: Register](registerDefinition: CollectibleRegisterDefinition[R], id: CollectionId): Map[Key, R] = {
    withReadableDatabase(getMapForCollection(_, registerDefinition, id))
  }

  override def getCollection[R <: Register](registerDefinition: CollectibleRegisterDefinition[R], id: Register.CollectionId): Set[R] = {
    withReadableDatabase(getCollection(_, registerDefinition, id))
  }

  override def getArray[R <: Register](registerDefinition: ArrayableRegisterDefinition[R], id: Register.CollectionId) :Seq[R] = {
    withReadableDatabase(getArray(_, registerDefinition, id))
  }

  override def getAlphabetSet(language: ForeignKeyField, wordRefFieldDef: ForeignKeyFieldDefinition): Set[Key] = {

    val wordTable = sword.langbook.db.registers.Word
    val reprTable = sword.langbook.db.registers.WordRepresentation

    val wordTableName = tableName(wordTable)
    val reprTableName = tableName(reprTable)

    val alphabetFieldName = fieldName(reprTable, sword.langbook.db.registers.AlphabetReferenceFieldDefinition)
    val languageFieldName = fieldName(wordTable, language.definition)

    val wordRefFieldName = fieldName(reprTable, wordRefFieldDef)
    val languageKey = language.key.index

    val query = s"SELECT $reprTableName.$alphabetFieldName FROM $reprTableName JOIN $wordTableName ON $wordTableName.${SQLiteStorageManager.idKey} = $reprTableName.$wordRefFieldName WHERE $wordTableName.$languageFieldName = $languageKey"
    val set = scala.collection.mutable.Set[Key]()

    withReadableDatabase { db =>
      val cursor = db.rawQuery(query, null)

      var count = 0
      if (cursor != null) try {
        count = cursor.getCount
        if (cursor.getCount > 0 && cursor.moveToFirst()) {
          do {
            set += obtainKey(sword.langbook.db.registers.Alphabet, 0, cursor.getInt(0))
          } while(cursor.moveToNext())
        }
      } finally {
        cursor.close()
      }
    }

    set.toSet
  }

  override def getJointSet[R <: Register](sourceRegDef: RegisterDefinition[Register], targetRegDef: RegisterDefinition[R], filter: ForeignKeyField, join: ForeignKeyFieldDefinition): Set[R] = {

    val sourceTableName = tableName(sourceRegDef)
    val targetTableName = tableName(targetRegDef)

    val allColumns = targetRegDef.fields.map(f => targetTableName + '.' + fieldName(targetRegDef, f)).mkString(", ")

    val sourceJoinFieldName = sourceTableName + '.' + fieldName(sourceRegDef, join)
    val targetJoinFieldName = targetTableName + '.' + fieldName(targetRegDef, join)

    val filterFieldName = sourceTableName + '.' + fieldName(sourceRegDef, filter.definition)
    val filterKey = filter.key.index

    val query = s"SELECT $allColumns FROM $sourceTableName JOIN $targetTableName ON $sourceJoinFieldName = $targetJoinFieldName WHERE $filterFieldName = $filterKey"
    val set = scala.collection.mutable.Set[R]()

    withReadableDatabase { db =>
      val cursor = db.rawQuery(query, null)

      if (cursor != null) try {
        if (cursor.getCount > 0 && cursor.moveToFirst()) {
          do {
            val fieldValues = targetRegDef.fields.indices.map(cursor.getString)
            targetRegDef.from(fieldValues, keyExtractor).map(set += _)
          } while(cursor.moveToNext())
        }

      } finally {
        cursor.close()
      }
    }

    set.toSet
  }

  override def allStringArray: Map[Key, List[String]] = {
    withReadableDatabase(allStringArray)
  }

  private def allStringArray(db: SQLiteDatabase): Map[Key, List[String]] = {
    val reprTable = registers.WordRepresentation
    val reprTableName = tableName(reprTable)
    val textTable = redundant.Text
    val textTableName = tableName(textTable)

    val wordRefFieldName = fieldName(reprTable, WordReferenceFieldDefinition)
    val charSequenceFieldName = fieldName(textTable, CharSequenceFieldDefinition)
    val symbolArrayFieldName = fieldName(textTable, SymbolArrayReferenceFieldDefinition)
    val arrayRefFieldName = fieldName(reprTable, SymbolArrayReferenceFieldDefinition)

    val query = s"SELECT $reprTableName.$wordRefFieldName,$textTableName.$charSequenceFieldName FROM $reprTableName JOIN $textTableName ON $reprTableName.$arrayRefFieldName = $textTableName.$symbolArrayFieldName"
    val cursor = db.rawQuery(query, null)

    val result = scala.collection.mutable.Map[Int, List[String]]()
    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        do {
          val wordId = cursor.getInt(0)
          val str = cursor.getString(1)
          val list = str :: result.getOrElse(wordId, List())
          result(wordId) = list
        } while(cursor.moveToNext())
      }
    } finally {
      cursor.close()
    }

    result.map { case (x,y) => (obtainKey(sword.langbook.db.registers.Word, 0, x),y) }.toMap
  }

  override def isConceptDuplicated(alphabet: Key): Boolean = {

    val conceptTable = sword.langbook.db.registers.Acceptation
    val reprTable = sword.langbook.db.registers.WordRepresentation
    val joinFieldDef = sword.langbook.db.registers.WordReferenceFieldDefinition

    val conceptTableName = tableName(conceptTable)
    val reprTableName = tableName(reprTable)

    val conceptWordFieldName = fieldName(conceptTable, joinFieldDef)
    val reprWordFieldName = fieldName(reprTable, joinFieldDef)

    val conceptFieldName = fieldName(conceptTable, sword.langbook.db.registers.ConceptReferenceFieldDefinition)
    val alphabetFieldName = fieldName(reprTable, sword.langbook.db.registers.AlphabetReferenceFieldDefinition)

    val alphabetKey = alphabet.index

    var repeated = false
    withReadableDatabase { db =>
      val query = s"SELECT $conceptTableName.$conceptFieldName FROM $conceptTableName JOIN $reprTableName ON $conceptTableName.$conceptWordFieldName = $reprTableName.$reprWordFieldName WHERE $reprTableName.$alphabetFieldName = $alphabetKey"
      val cursor = db.rawQuery(query, null)

      val set = scala.collection.mutable.Set[Register.Index]()
      var count = 0
      if (cursor != null) try {
        count = cursor.getCount
        if (cursor.getCount > 0 && cursor.moveToFirst()) {
          do {
            val conceptKey = cursor.getInt(0)
            if (set(conceptKey)) repeated = true
            else set += conceptKey
          } while(cursor.moveToNext() && !repeated)
        }
      } finally {
        cursor.close()
      }
    }

    repeated
  }

  private def keySetForAlphabetSymbolMatching(
      db: SQLiteDatabase,
      targetRegDef: RegisterDefinition[Register],
      targetFieldDef: ForeignKeyFieldDefinition,
      filterRegDef: RegisterDefinition[Register],
      filter: ForeignKeyField): Set[Key] = {

    val symbolPositionTable = sword.langbook.db.registers.SymbolPosition
    val reprTable = sword.langbook.db.registers.WordRepresentation

    val reprTableName = tableName(reprTable)
    val symbolPositionTableName = tableName(symbolPositionTable)

    val arrayRefFieldName = fieldName(reprTable, SymbolArrayReferenceFieldDefinition)

    val targetTableName = tableName(targetRegDef)
    val targetFieldName = fieldName(targetRegDef, targetFieldDef)
    val target = s"$targetTableName.$targetFieldName"

    val filterTableName = tableName(filterRegDef)
    val filterFieldName = fieldName(filterRegDef, filter.definition)
    val filterId = filter.key.index
    val filterClause = s"$filterTableName.$filterFieldName = $filterId"

    val query = s"SELECT $target FROM $reprTableName JOIN $symbolPositionTableName ON $reprTableName.$arrayRefFieldName = $symbolPositionTableName.${SQLiteStorageManager.collKey} WHERE $filterClause"
    val cursor = db.rawQuery(query, null)

    val result = scala.collection.mutable.Set[Int]()
    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        do {
          result += cursor.getInt(0)
        } while(cursor.moveToNext())
      }
    } finally {
      cursor.close()
    }

    result.map(obtainKey(targetFieldDef.target, 0, _)).toSet
  }

  override def alphabetsWhereSymbolIncluded(symbol: Key): Set[Key] = {
    withReadableDatabase(alphabetsWhereSymbolIncluded(_, symbol))
  }

  private def alphabetsWhereSymbolIncluded(db: SQLiteDatabase, symbol: Key): Set[Key] = {
    keySetForAlphabetSymbolMatching(db,
        registers.WordRepresentation,
        registers.AlphabetReferenceFieldDefinition,
        registers.SymbolPosition,
        registers.SymbolReferenceField(symbol))
  }

  override def allSymbolsInAlphabet(alphabet: Key): Set[Key] = {
    withReadableDatabase(allSymbolsInAlphabet(_, alphabet))
  }

  private def allSymbolsInAlphabet(db: SQLiteDatabase, alphabet: Key): Set[Key] = {
    keySetForAlphabetSymbolMatching(db,
      registers.SymbolPosition,
      registers.SymbolReferenceFieldDefinition,
      registers.WordRepresentation,
      registers.AlphabetReferenceField(alphabet))
  }
}

package sword.langbook.android.db

import android.content.Context
import android.database.Cursor
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import android.util.Log
import sword.db.Register.CollectionId
import sword.db._
import sword.langbook.db.registers
import sword.langbook.db.registers.{SymbolArrayReferenceFieldDefinition, AlphabetReferenceField, WordReferenceFieldDefinition, LanguageReferenceField}

import scala.collection.mutable.ListBuffer

object SQLiteStorageManager {
  val dbName = "LanguageDb"
  val currentDbVersion = 4
  val idKey = "id"
  val collKey = "coll"

  // This is required in order that app can know which alphabet is the kana, and which one the Kanji.
  // This should not be required if the app was generic enough.
  // TODO: Remove this hints when the app do not require them
  val kanaAlphabetHint = "Kana alphabet"
  val kanjiAlphabetHint = "Kanji alphabet"
  val englishAlphabetHint = "English alphabet"
  val spanishAlphabetHint = "Spanish alphabet"

  // This is required in order that app can identify languages when selecting quizzes.
  // It should not be required if the app was generic enough.
  // TODO: Remove these codes when the app does not require them
  val englishCode = "en"
  val spanishCode = "es"
  val japaneseCode = "ja"
}

class SQLiteStorageManager(context :Context, dbName: String, override val registerDefinitions :Seq[RegisterDefinition[Register]])
    extends SQLiteOpenHelper(context, dbName, null, SQLiteStorageManager.currentDbVersion)
    with StorageManager {

  // Just to measure time taken
  var _lastQueryMillis: Long = 0

  // The following code is copied from AbstractStorageManager, and it should be centralised
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

    val string =
        enLanguageText + spLanguageText + kanjiLanguageText + kanaLanguageText +
        englishEnText + englishSpText + englishJpText + englishKanaText +
        spanishEnText + spanishSpText + spanishJpText + spanishKanaText +
        japaneseEnText + japaneseSpText + japaneseJpText + japaneseKanaText +
        kanjiJpText + kanjiKanaText + kanaJpText + kanaKanaText

    val symbols = {
      for {
        symbol <- string.toSet[Char]
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

    val englishConceptKey = insertConcept("English").get
    val spanishConceptKey = insertConcept("Spanish").get
    val japaneseConceptKey = insertConcept("Japanese").get

    val languageConceptKey = insertConcept("language").get
    insert(db, registers.ConceptTypeRelation(englishConceptKey, languageConceptKey))
    insert(db, registers.ConceptTypeRelation(spanishConceptKey, languageConceptKey))
    insert(db, registers.ConceptTypeRelation(japaneseConceptKey, languageConceptKey))

    val englishKey = insert(db, registers.Language(englishConceptKey, SQLiteStorageManager.englishCode, enAlphabetKey)).get
    val spanishKey = insert(db, registers.Language(spanishConceptKey, SQLiteStorageManager.spanishCode, spAlphabetKey)).get
    val japaneseKey = insert(db, registers.Language(japaneseConceptKey, SQLiteStorageManager.japaneseCode, kanjiAlphabetKey)).get

    val languageEnSymbolArrayCollection = insert(db, enLanguageText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val languageSpSymbolArrayCollection = insert(db, spLanguageText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val languageKanjiSymbolArrayCollection = insert(db, kanjiLanguageText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val languageKanaSymbolArrayCollection = insert(db, kanaLanguageText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val englishEnSymbolArrayCollection = insert(db, englishEnText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val englishSpSymbolArrayCollection = insert(db, englishSpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val englishJpSymbolArrayCollection = insert(db, englishJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val englishKanaSymbolArrayCollection = insert(db, englishKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val spanishEnSymbolArrayCollection = insert(db, spanishEnText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val spanishSpSymbolArrayCollection = insert(db, spanishSpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val spanishJpSymbolArrayCollection = insert(db, spanishJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val spanishKanaSymbolArrayCollection = insert(db, spanishKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val japaneseEnSymbolArrayCollection = insert(db, japaneseEnText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val japaneseSpSymbolArrayCollection = insert(db, japaneseSpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val japaneseJpSymbolArrayCollection = insert(db, japaneseJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val japaneseKanaSymbolArrayCollection = insert(db, japaneseKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanjiJpSymbolArrayCollection = insert(db, kanjiJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanjiKanaSymbolArrayCollection = insert(db, kanjiKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanaJpSymbolArrayCollection = insert(db, kanaJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanaKanaSymbolArrayCollection = insert(db, kanaKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get

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
    insert(db, registers.WordRepresentation(kanaJpWord, kanaAlphabetKey, kanjiKanaSymbolArrayCollection))

    insert(db, registers.WordConcept(languageEnWord, languageConceptKey))
    insert(db, registers.WordConcept(languageSpWord, languageConceptKey))
    insert(db, registers.WordConcept(languageJpWord, languageConceptKey))
    insert(db, registers.WordConcept(englishEnWord, englishConceptKey))
    insert(db, registers.WordConcept(englishSpWord, englishConceptKey))
    insert(db, registers.WordConcept(englishJpWord, englishConceptKey))
    insert(db, registers.WordConcept(spanishEnWord, spanishConceptKey))
    insert(db, registers.WordConcept(spanishSpWord, spanishConceptKey))
    insert(db, registers.WordConcept(spanishJpWord, spanishConceptKey))
    insert(db, registers.WordConcept(japaneseEnWord, japaneseConceptKey))
    insert(db, registers.WordConcept(japaneseSpWord, japaneseConceptKey))
    insert(db, registers.WordConcept(japaneseJpWord, japaneseConceptKey))
    insert(db, registers.WordConcept(englishEnWord, enAlphabetConceptKey))
    insert(db, registers.WordConcept(englishSpWord, enAlphabetConceptKey))
    insert(db, registers.WordConcept(englishJpWord, enAlphabetConceptKey))
    insert(db, registers.WordConcept(spanishEnWord, spAlphabetConceptKey))
    insert(db, registers.WordConcept(spanishSpWord, spAlphabetConceptKey))
    insert(db, registers.WordConcept(spanishJpWord, spAlphabetConceptKey))
    insert(db, registers.WordConcept(kanjiJpWord, kanjiAlphabetConceptKey))
    insert(db, registers.WordConcept(kanaJpWord, kanaAlphabetConceptKey))
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

  private def fromDbVersion3(db: SQLiteDatabase): Unit = {
    import sword.langbook.db.registers

    val cursor = query(db, "WordRegister", Array("mWrittenWord", "mPronunciation", "meaning"),
      null, null)

    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        val symbols = scala.collection.mutable.Set[Char]()
        do {
          symbols ++= cursor.getString(0) ++= cursor.getString(1) ++= cursor.getString(2)
        } while(cursor.moveToNext())

        val currentSymbols = keysFor(db, registers.Symbol).flatMap(k => get(db, k).map((k,_))).toMap.flatMap { case (key, reg) =>
          reg.fields.collectFirst { case f: UnicodeField => f.value }.map((key, _))
        }

        val allSymbols = currentSymbols ++ (for {
          missingSymbol <- symbols.map(_.toInt).toSet.diff(currentSymbols.values.toSet)
        } yield {
          val keyOpt = insert(db, registers.Symbol(missingSymbol))
          (keyOpt.get, missingSymbol)
        })

        if (!cursor.moveToFirst()) return

        val allSymbolsReverse = allSymbols.map { case (a,b) => (b, a) }

        val (spanishKey, spanishAlphabetKey) = keysFor(db, registers.Language).flatMap(k => get(db, k).map((k,_))).find {
          case (_,reg) =>
            reg.fields.collectFirst {
              case f: LanguageCodeField if f.code == "es" => true
            }.isDefined
        }.flatMap {
          case (languageKey, reg) =>
            reg.fields.collectFirst {
              case f: ForeignKeyField if f.definition.target == registers.Alphabet => f.key
            }.map((languageKey, _))
        }.get

        val (japaneseKey, kanjiKey) = keysFor(db, registers.Language).flatMap(k => get(db, k).map((k,_))).find {
          case (_,reg) =>
            reg.fields.collectFirst {
              case f: LanguageCodeField if f.code == "ja" => true
            }.isDefined
        }.flatMap {
          case (languageKey, reg) =>
            reg.fields.collectFirst {
              case f: ForeignKeyField if f.definition.target == registers.Alphabet => f.key
            }.map((languageKey, _))
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
          val str = getStringArray(db,
            registers.SymbolPosition, repr.symbolArray, registers.SymbolReferenceFieldDefinition)
          representations(str) = repr
        }

        do {
          val written = insert(db, cursor.getString(0).map(c => registers.SymbolPosition(allSymbolsReverse(c.toInt)))).get
          val kana = insert(db, cursor.getString(1).map(c => registers.SymbolPosition(allSymbolsReverse(c.toInt)))).get
          val jaWord = insert(db, registers.Word(japaneseKey)).get
          insert(db, registers.WordRepresentation(jaWord, kanjiKey, written))
          insert(db, registers.WordRepresentation(jaWord, kanaKey, kana))

          val concept = insert(db, registers.Concept(cursor.getString(0))).get
          insert(db, registers.WordConcept(jaWord, concept))

          val givenMeaning = cursor.getString(2)
          val meanings = {
            if (givenMeaning.indexOf("(") >= 0 || givenMeaning.indexOf(")") >= 0 || givenMeaning.indexOf("/") >= 0 || givenMeaning.indexOf(",") < 0) {
              Array(givenMeaning)
            }
            else {
              givenMeaning.split(",").map(_.trim)
            }
          }

          for (meaning <- meanings) {
            val reprOpt = representations.get(meaning)
            val esWord = if (reprOpt.isEmpty) {
              val symbolArray = insert(db, meaning.map(c => registers.SymbolPosition(allSymbolsReverse(c.toInt)))).get
              val word = insert(db, registers.Word(spanishKey)).get
              val repr = registers.WordRepresentation(word, spanishAlphabetKey, symbolArray)
              representations(meaning) = repr
              insert(db, repr)
              word
            }
            else {
              reprOpt.get.word
            }

            insert(db, registers.WordConcept(esWord, concept))
          }
        } while(cursor.moveToNext())
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
      case f: LanguageCodeField => s"'${f.code.toString}'"
      case f: CharSequenceField => s"'${f.value}'"
      case f: ForeignKeyField => f.key.index.toString
      case f: CollectionReferenceField => f.collectionId.toString
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

  private def insert(db: SQLiteDatabase, registers: Traversable[Register]): Option[CollectionId] = {
    var collId = 1
    val cursor = query(db, tableName(registers.head.definition), Array(SQLiteStorageManager.collKey),
        null, null)

    if (cursor != null) try {
      if (cursor.getCount > 0 && cursor.moveToFirst()) {
        do {
          val thisCollId = cursor.getInt(0)
          if (thisCollId >= collId) collId = thisCollId + 1
        } while(cursor.moveToNext())
      }
    } finally {
      cursor.close()
    }

    for (register <- registers) {
      insert(db, collId, register)
    }
    Some(collId)
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

  override def getStringArray[R <: Register](registerDefinition: ArrayableRegisterDefinition[R],
      id: Register.CollectionId, matcher: ForeignKeyFieldDefinition) :String = {
    withReadableDatabase(getStringArray(_, registerDefinition, id, matcher))
  }

  private def getStringArray[R <: Register](
      db: SQLiteDatabase,
      registerDefinition: ArrayableRegisterDefinition[R],
      id: Register.CollectionId,
      matcher: ForeignKeyFieldDefinition) :String = {

    val params = getArrayParams(registerDefinition)

    val symbolTableName = tableName(matcher.target)
    val symbolPositionTableName = params.table

    val symbolRefFieldName = fieldName(registerDefinition, matcher)

    val symbolUnicodeFieldDef = sword.db.UnicodeFieldDefinition
    val symbolUnicodeFieldName = fieldName(matcher.target, symbolUnicodeFieldDef)

    val symbolCharFieldDef = sword.db.CharSequenceFieldDefinition
    val symbolCharFieldName = fieldName(matcher.target, symbolCharFieldDef)

    val query = s"SELECT group_concat($symbolTableName.$symbolCharFieldName,'') FROM $symbolPositionTableName JOIN $symbolTableName ON $symbolTableName.${SQLiteStorageManager.idKey} = $symbolPositionTableName.$symbolRefFieldName WHERE $symbolPositionTableName.${SQLiteStorageManager.collKey} = $id GROUP BY $symbolPositionTableName.${SQLiteStorageManager.collKey} ORDER BY $symbolPositionTableName.${SQLiteStorageManager.idKey} ASC"
    val cursor = db.rawQuery(query, null)

    if (cursor != null) {
      try {
        if (cursor.getCount == 1 && cursor.moveToFirst()) cursor.getString(0)
        else ""
      } finally {
        cursor.close()
      }
    }
    else ""
  }

  override def allStringArray: Map[Key, List[String]] = {
    withReadableDatabase(allStringArray)
  }

  private def allStringArray(db: SQLiteDatabase): Map[Key, List[String]] = {

    val registerDefinition = registers.SymbolPosition
    val params = getArrayParams(registerDefinition)
    val matcher = registers.SymbolReferenceFieldDefinition

    val reprTable = sword.langbook.db.registers.WordRepresentation
    val reprTableName = tableName(reprTable)
    val symbolTableName = tableName(matcher.target)
    val symbolPositionTableName = params.table

    val symbolRefFieldName = fieldName(registerDefinition, matcher)
    val wordRefFieldName = fieldName(reprTable, WordReferenceFieldDefinition)
    val arrayRefFieldName = fieldName(reprTable, SymbolArrayReferenceFieldDefinition)

    val symbolCharFieldDef = sword.db.CharSequenceFieldDefinition
    val symbolCharFieldName = fieldName(matcher.target, symbolCharFieldDef)

    val query = s"SELECT $reprTableName.$wordRefFieldName,group_concat($symbolTableName.$symbolCharFieldName,'') FROM $reprTableName JOIN $symbolPositionTableName ON $reprTableName.$arrayRefFieldName = $symbolPositionTableName.${SQLiteStorageManager.collKey} JOIN $symbolTableName ON $symbolTableName.${SQLiteStorageManager.idKey} = $symbolPositionTableName.$symbolRefFieldName GROUP BY $symbolPositionTableName.${SQLiteStorageManager.collKey} ORDER BY $symbolPositionTableName.${SQLiteStorageManager.idKey} ASC"
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

    val conceptTable = sword.langbook.db.registers.WordConcept
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
}

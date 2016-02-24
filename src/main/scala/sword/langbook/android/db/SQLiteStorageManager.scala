package sword.langbook.android.db

import android.content.Context
import android.database.Cursor
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import sword.db.Register.CollectionId
import sword.db._

import scala.collection.mutable.ListBuffer

object SQLiteStorageManager {
  val dbName = "LanguageDb"
  val currentDbVersion = 4
  val idKey = "id"
  val collKey = "coll"
}

class SQLiteStorageManager(context :Context, override val registerDefinitions :Seq[RegisterDefinition])
    extends SQLiteOpenHelper(context, SQLiteStorageManager.dbName, null, SQLiteStorageManager.currentDbVersion)
    with StorageManager {

  private def logi(message :String) = android.util.Log.i("DB", message)

  private def tableName(regDef :RegisterDefinition) :String = s"R${registerDefinitions.indexOf(regDef)}"
  private def tableName(reg :Register) :String = tableName(reg.definition)

  private def fieldName(regDef :RegisterDefinition, fieldDef :FieldDefinition) :String = s"C${regDef.fields.indexOf(fieldDef)}"
  private def fieldName(regDef :RegisterDefinition, field :Field) :String = fieldName(regDef, field.definition)

  private def createTables(db :SQLiteDatabase) = {
    for (regDef <- registerDefinitions) {
      val idKey = SQLiteStorageManager.idKey
      val fields = regDef.fields.map { fieldDef =>
        val sqlType = fieldDef match {
          case _: CharSequenceField => "TEXT"
          case _ => "INTEGER"
        }

        s"${fieldName(regDef, fieldDef)} $sqlType"
      }
      val columns = (regDef match {
        case _:CollectibleRegisterDefinition => Seq(s"${SQLiteStorageManager.collKey} INTEGER") ++ fields
        case _ => fields
      }).mkString(", ")

      val query = s"CREATE TABLE IF NOT EXISTS ${tableName(regDef)} ($idKey INTEGER PRIMARY KEY AUTOINCREMENT, $columns)"
      logi(s"Executing SQLite query: $query")
      db.execSQL(query)
    }
  }

  override def onCreate(db: SQLiteDatabase): Unit = {
    createTables(db)

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

    // Temporally they are different to distinguish them
    val enAlphabetText = "latin"
    val spAlphabetText = "Latino"

    val kanjiJpText = "漢字"
    val kanjiKanaText = "かんじ"
    val kanaJpText = "仮名"
    val kanaKanaText = "かな"

    val string =
        englishEnText + englishSpText + englishJpText + englishKanaText +
        spanishEnText + spanishSpText + spanishJpText + spanishKanaText +
        japaneseEnText + japaneseSpText + japaneseJpText + japaneseKanaText +
        enAlphabetText + spAlphabetText + kanjiJpText + kanjiKanaText + kanaJpText + kanaKanaText

    val symbols = {
      for {
        symbol <- string.toSet[Char]
        key <- insert(db, registers.Symbol(symbol.toInt))
      } yield (symbol, key)
    }.toMap

    val englishConceptKey = insertConcept("English").get
    val spanishConceptKey = insertConcept("Spanish").get
    val japaneseConceptKey = insertConcept("Japanese").get

    val englishKey = insert(db, registers.Language(englishConceptKey)).get
    val enAlphabetConceptKey = insertConcept("English alphabet").get
    val enAlphabetKey = insert(db, registers.Alphabet(enAlphabetConceptKey)).get

    val spanishKey = insert(db, registers.Language(spanishConceptKey)).get
    val spAlphabetConceptKey = insertConcept("Spanish alphabet").get
    val spAlphabetKey = insert(db, registers.Alphabet(spAlphabetConceptKey)).get

    val japaneseKey = insert(db, registers.Language(japaneseConceptKey)).get
    val kanjiAlphabetConceptKey = insertConcept("Kanji alphabet").get
    val kanjiAlphabetKey = insert(db, registers.Alphabet(kanjiAlphabetConceptKey)).get
    val kanaAlphabetConceptKey = insertConcept("Kana alphabet").get
    val kanaAlphabetKey = insert(db, registers.Alphabet(kanaAlphabetConceptKey)).get

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
    val enAlphabetEnSymbolArrayCollection = insert(db, enAlphabetText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val spAlphabetSpSymbolArrayCollection = insert(db, spAlphabetText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanjiJpSymbolArrayCollection = insert(db, kanjiJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanjiKanaSymbolArrayCollection = insert(db, kanjiKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanaJpSymbolArrayCollection = insert(db, kanaJpText.toList.map(c => registers.SymbolPosition(symbols(c)))).get
    val kanaKanaSymbolArrayCollection = insert(db, kanaKanaText.toList.map(c => registers.SymbolPosition(symbols(c)))).get

    val englishEnPiece = insert(db, List(registers.Piece(enAlphabetKey, englishEnSymbolArrayCollection))).get
    val englishSpPiece = insert(db, List(registers.Piece(spAlphabetKey, englishSpSymbolArrayCollection))).get
    val englishJpPiece = insert(db, List(
        registers.Piece(kanjiAlphabetKey, englishJpSymbolArrayCollection),
        registers.Piece(kanaAlphabetKey, englishKanaSymbolArrayCollection)
    )).get
    val spanishEnPiece = insert(db, List(registers.Piece(enAlphabetKey, spanishEnSymbolArrayCollection))).get
    val spanishSpPiece = insert(db, List(registers.Piece(spAlphabetKey, spanishSpSymbolArrayCollection))).get
    val spanishJpPiece = insert(db, List(
        registers.Piece(kanjiAlphabetKey, spanishJpSymbolArrayCollection),
        registers.Piece(kanaAlphabetKey, spanishKanaSymbolArrayCollection)
    )).get
    val japaneseEnPiece = insert(db, List(registers.Piece(enAlphabetKey, japaneseEnSymbolArrayCollection))).get
    val japaneseSpPiece = insert(db, List(registers.Piece(spAlphabetKey, japaneseSpSymbolArrayCollection))).get
    val japaneseJpPiece = insert(db, List(
        registers.Piece(kanjiAlphabetKey, japaneseJpSymbolArrayCollection),
        registers.Piece(kanaAlphabetKey, japaneseKanaSymbolArrayCollection)
    )).get
    val enAlphabetEnPiece = insert(db, List(registers.Piece(enAlphabetKey, enAlphabetEnSymbolArrayCollection))).get
    val spAlphabetSpPiece = insert(db, List(registers.Piece(spAlphabetKey, spAlphabetSpSymbolArrayCollection))).get
    val kanjiJpPiece = insert(db, List(
      registers.Piece(kanjiAlphabetKey, kanjiJpSymbolArrayCollection),
      registers.Piece(kanaAlphabetKey, kanjiKanaSymbolArrayCollection)
    )).get
    val kanaJpPiece = insert(db, List(
      registers.Piece(kanjiAlphabetKey, kanaJpSymbolArrayCollection),
      registers.Piece(kanaAlphabetKey, kanaKanaSymbolArrayCollection)
    )).get

    val englishEnPieceArray = insert(db, List(registers.PiecePosition(englishEnPiece))).get
    val englishSpPieceArray = insert(db, List(registers.PiecePosition(englishSpPiece))).get
    val englishJpPieceArray = insert(db, List(registers.PiecePosition(englishJpPiece))).get
    val spanishEnPieceArray = insert(db, List(registers.PiecePosition(spanishEnPiece))).get
    val spanishSpPieceArray = insert(db, List(registers.PiecePosition(spanishSpPiece))).get
    val spanishJpPieceArray = insert(db, List(registers.PiecePosition(spanishJpPiece))).get
    val japaneseEnPieceArray = insert(db, List(registers.PiecePosition(japaneseEnPiece))).get
    val japaneseSpPieceArray = insert(db, List(registers.PiecePosition(japaneseSpPiece))).get
    val japaneseJpPieceArray = insert(db, List(registers.PiecePosition(japaneseJpPiece))).get
    val enAlphabetEnPieceArray = insert(db, List(registers.PiecePosition(enAlphabetEnPiece))).get
    val spAlphabetSpPieceArray = insert(db, List(registers.PiecePosition(spAlphabetSpPiece))).get
    val kanjiJpPieceArray = insert(db, List(registers.PiecePosition(kanjiJpPiece))).get
    val kanaJpPieceArray = insert(db, List(registers.PiecePosition(kanaJpPiece))).get

    val englishEnWord = insert(db, registers.Word(englishKey, englishEnPieceArray)).get
    val englishSpWord = insert(db, registers.Word(spanishKey, englishSpPieceArray)).get
    val englishJpWord = insert(db, registers.Word(japaneseKey, englishJpPieceArray)).get
    val spanishEnWord = insert(db, registers.Word(englishKey, spanishEnPieceArray)).get
    val spanishSpWord = insert(db, registers.Word(spanishKey, spanishSpPieceArray)).get
    val spanishJpWord = insert(db, registers.Word(japaneseKey, spanishJpPieceArray)).get
    val japaneseEnWord = insert(db, registers.Word(englishKey, japaneseEnPieceArray)).get
    val japaneseSpWord = insert(db, registers.Word(spanishKey, japaneseSpPieceArray)).get
    val japaneseJpWord = insert(db, registers.Word(japaneseKey, japaneseJpPieceArray)).get
    val enAlphabetEnWord = insert(db, registers.Word(englishKey, enAlphabetEnPieceArray)).get
    val spAlphabetSpWord = insert(db, registers.Word(spanishKey, spAlphabetSpPieceArray)).get
    val kanjiJpWord = insert(db, registers.Word(japaneseKey, kanjiJpPieceArray)).get
    val kanaJpWord = insert(db, registers.Word(japaneseKey, kanaJpPieceArray)).get

    insert(db, registers.WordConcept(englishEnWord, englishConceptKey))
    insert(db, registers.WordConcept(englishSpWord, englishConceptKey))
    insert(db, registers.WordConcept(englishJpWord, englishConceptKey))
    insert(db, registers.WordConcept(spanishEnWord, spanishConceptKey))
    insert(db, registers.WordConcept(spanishSpWord, spanishConceptKey))
    insert(db, registers.WordConcept(spanishJpWord, spanishConceptKey))
    insert(db, registers.WordConcept(japaneseEnWord, japaneseConceptKey))
    insert(db, registers.WordConcept(japaneseSpWord, japaneseConceptKey))
    insert(db, registers.WordConcept(japaneseJpWord, japaneseConceptKey))
    insert(db, registers.WordConcept(enAlphabetEnWord, enAlphabetConceptKey))
    insert(db, registers.WordConcept(spAlphabetSpWord, spAlphabetConceptKey))
    insert(db, registers.WordConcept(kanjiJpWord, kanjiAlphabetConceptKey))
    insert(db, registers.WordConcept(kanaJpWord, kanaAlphabetConceptKey))
  }

  override def onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int): Unit = {
    if (newVersion == SQLiteStorageManager.currentDbVersion) {
      createTables(db)
    }
  }

  override def getMapForCollection(registerDefinition: CollectibleRegisterDefinition, id: CollectionId): Map[Key, Register] = ???

  private def fromCursor(regDef :RegisterDefinition, cursor :Cursor) :Register = {
    val resultFields = for (fieldDef <- regDef.fields) yield {
      val value = cursor.getString(cursor.getColumnIndex(fieldName(regDef, fieldDef)))
      fieldDef match {
        case UnicodeFieldDefinition => UnicodeField(value.toInt)
        case CharSequenceFieldDefinition => CharSequenceField(value)
        case f:ForeignKeyFieldDefinition => new ForeignKeyField {
          override val key = obtainKey(f.target, 0, value.toInt)
          override val definition = f
        }
        case f:CollectionReferenceFieldDefinition => new CollectionReferenceField {
          override val collectionId = value.toInt
          override val definition = f
        }
        case _ => throw new UnsupportedOperationException("Undefined field definition")
      }
    }

    new Register {
      override val definition = regDef
      override val fields = resultFields
    }
  }

  private def get(db :SQLiteDatabase, key: Key): Option[Register] = {
    val regDef = key.registerDefinition
    val cursor = db.query(tableName(regDef), regDef.fields.map(fieldName(regDef,_)).toArray,
      s"${SQLiteStorageManager.idKey}=?", Array(key.index.toString), null, null, null, null)

    if (cursor == null) None
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) None
      else Some(fromCursor(regDef, cursor))
    } finally {
      cursor.close()
    }
  }

  override def get(key :Key) = {
    val db = getReadableDatabase
    try {
      get(db, key)
    } finally {
      db.close()
    }
  }

  /**
    * Extracts the value for a given field into a string that can be added to a SQL query
    */
  private def sqlValue(field :Field) :String = {
    field match {
      case f:UnicodeField => f.value.toString
      case f:CharSequenceField => s"'${f.value}'"
      case f:ForeignKeyField => f.key.index.toString
      case f:CollectionReferenceField => f.collectionId.toString
      case f => throw new UnsupportedOperationException(s"Undefined field definition $f")
    }
  }

  private def find(db :SQLiteDatabase, reg :Register) :Seq[Key] = {
    val regDef = reg.definition
    val whereClause = reg.fields.map { field =>
      s"${fieldName(regDef, field)}=${sqlValue(field)}"
    }.mkString(" AND ")
    val cursor = db.query(tableName(regDef), Array(SQLiteStorageManager.idKey), whereClause, null, null, null, null, null)

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

  private def insert(db :SQLiteDatabase, register: Register): Option[Key] = {
    val regDef = register.definition
    val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")
    val values = register.fields.map(sqlValue).mkString(", ")
    val query = s"INSERT INTO ${tableName(register)} ($keys) VALUES ($values)"
    logi(s"Executing query: $query")
    db.execSQL(query)
    find(db, register).lastOption
  }

  private def insert(db :SQLiteDatabase, coll :Register.CollectionId, register: Register): Unit = {
    val regDef = register.definition
    val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")
    val values = register.fields.map(sqlValue).mkString(", ")
    val query = s"INSERT INTO ${tableName(register)} (${SQLiteStorageManager.collKey}, $keys) VALUES ($coll, $values)"
    logi(s"Executing query: $query")
    db.execSQL(query)
  }

  override def insert(register: Register): Option[Key] = {
    val db = getWritableDatabase
    try {
      insert(db, register)
    } finally {
      db.close()
    }
  }

  private def insert(db: SQLiteDatabase, registers: Traversable[Register]): Option[CollectionId] = {
    var collId = 1
    val cursor = db.query(tableName(registers.head.definition), Array(SQLiteStorageManager.collKey),
        null, null, null, null, null, null)

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

  override def insert(registers: Traversable[Register]): Option[CollectionId] = {
    val definitions = registers.map(_.definition).toSet
    if (definitions.size != 1) {
      throw new UnsupportedOperationException("Unable to insert collections for registers with different definitions")
    }

    if (!definitions.head.isInstanceOf[CollectibleRegisterDefinition]) {
      throw new UnsupportedOperationException("Unable to insert collections for non-collectible registers")
    }

    val db = getWritableDatabase
    try {
      insert(db, registers)
    } finally {
      db.close()
    }
  }

  private def keysFor(db :SQLiteDatabase, regDef :RegisterDefinition) :Set[Key] = {
    val cursor = db.query(tableName(regDef), Array(SQLiteStorageManager.idKey), null, null,
        null, null, null, null)

    if (cursor == null) Set()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Set()
      else {
        val buffer = new ListBuffer[Key]()
        do {
          buffer += obtainKey(regDef, 0, cursor.getInt(0))
        } while(cursor.moveToNext())
        val result = buffer.result().toSet
        logi(s"Called keysFor and returning $result")
        result
      }
    } finally {
      cursor.close()
    }
  }

  override def getKeysFor(registerDefinition: RegisterDefinition): Set[Key] = {
    val db = getReadableDatabase
    try {
      keysFor(db, registerDefinition)
    } finally {
      db.close()
    }
  }

  override def replace(register: Register, key: Key): Boolean = ???

  // TODO: This should check if it is referenced before removing
  private def delete(db: SQLiteDatabase, key: Key): Boolean = {
    val query = s"DELETE FROM ${tableName(key.registerDefinition)} WHERE ${SQLiteStorageManager.idKey}=${key.index}"
    logi(s"Executing query: $query")
    db.execSQL(query)
    true
  }

  override def delete(key: Key): Boolean = {
    val db = getWritableDatabase
    try {
      delete(db, key)
    } finally {
      db.close()
    }
  }

  override def getKeysForCollection(registerDefinition: CollectibleRegisterDefinition, id: CollectionId): Set[Key] = {
    val db = getReadableDatabase
    try {
      val whereClause = s"${SQLiteStorageManager.collKey}=$id"
      val cursor = db.query(tableName(registerDefinition), Array(SQLiteStorageManager.idKey), whereClause, null, null, null, null, null)

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
    } finally {
      db.close()
    }
  }
}

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

class SQLiteStorageManager(context :Context, dbName: String, override val registerDefinitions :Seq[RegisterDefinition])
    extends SQLiteOpenHelper(context, dbName, null, SQLiteStorageManager.currentDbVersion)
    with StorageManager {

  // The following code is copied from AbstractStorageManager, and it should be centralised
  // TODO: Centralise this code
  if (registerDefinitions.toSet.size < registerDefinitions.size) {
    throw new IllegalArgumentException("Duplicated register definitions are not allowed")
  }

  val singleReferences :Seq[(RegisterDefinition, RegisterDefinition)] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[ForeignKeyFieldDefinition]
  } yield {
      (regDef, fieldDef.asInstanceOf[ForeignKeyFieldDefinition].target)
    }

  if (singleReferences.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a foreign key" +
      " field must have as target one of the definitions given")
  }

  val groupReferences :Seq[(RegisterDefinition, CollectibleRegisterDefinition)] = for {
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

  private def tableName(regDef :RegisterDefinition) :String = s"R${registerDefinitions.indexOf(regDef)}"
  private def tableName(reg :Register) :String = tableName(reg.definition)

  private def fieldName(regDef :RegisterDefinition, fieldDef :FieldDefinition) :String = s"C${regDef.fields.indexOf(fieldDef)}"
  private def fieldName(regDef :RegisterDefinition, field :Field) :String = fieldName(regDef, field.definition)

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
        case _:CollectibleRegisterDefinition => Seq(s"${SQLiteStorageManager.collKey} INTEGER") ++ fields
        case _ => fields
      }).mkString(", ")

      val query = s"CREATE TABLE IF NOT EXISTS ${tableName(regDef)} ($idKey INTEGER PRIMARY KEY AUTOINCREMENT, $columns)"
      logi(s"Executing SQLite query: $query")
      db.execSQL(query)
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

    val englishKey = insert(db, registers.Language(englishConceptKey, SQLiteStorageManager.englishCode, enAlphabetKey)).get
    val spanishKey = insert(db, registers.Language(spanishConceptKey, SQLiteStorageManager.spanishCode, spAlphabetKey)).get
    val japaneseKey = insert(db, registers.Language(japaneseConceptKey, SQLiteStorageManager.japaneseCode, kanjiAlphabetKey)).get

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
    }
  }

  private def getMapFor(db: SQLiteDatabase, regDef: RegisterDefinition, filter: ForeignKeyField): scala.collection.Map[Key, Register] = {
    val array = (regDef match {
      case _: CollectibleRegisterDefinition =>
        Array(SQLiteStorageManager.idKey, SQLiteStorageManager.collKey)
      case _ =>
        Array(SQLiteStorageManager.idKey)
    }) ++ regDef.fields.map(fieldName(regDef,_))

    val whereClause = s"${fieldName(regDef, filter)}=${filter.key.index}"
    val cursor = db.query(tableName(regDef), array, whereClause, null, null, null, null, null)

    if (cursor == null) Map()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Map()
      else {
        val buffer = new ListBuffer[(Key, Register)]()
        do {
          val group = regDef match {
            case _: CollectibleRegisterDefinition => cursor.getInt(1)
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

  override def getMapFor(registerDefinition: RegisterDefinition, filter: ForeignKeyField): scala.collection.Map[Key, Register] = {
    val db = getReadableDatabase
    try {
      getMapFor(db, registerDefinition, filter)
    } finally {
      db.close()
    }
  }

  private def getMapForCollection(db: SQLiteDatabase,
      registerDefinition: CollectibleRegisterDefinition, id: CollectionId): Map[Key, Register] = {
    val keys = Seq(SQLiteStorageManager.idKey) ++ registerDefinition.fields.map(fieldName(registerDefinition,_))
    val cursor = db.query(tableName(registerDefinition), keys.toArray,
      s"${SQLiteStorageManager.collKey}=${id.toString}", null, null, null, null, null)

    val result = scala.collection.mutable.Map[Key, Register]()
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

  override def getMapForCollection(registerDefinition: CollectibleRegisterDefinition, id: CollectionId): Map[Key, Register] = {
    val db = getReadableDatabase
    try {
      getMapForCollection(db, registerDefinition, id)
    } finally {
      db.close()
    }
  }

  private def fromCursor(regDef :RegisterDefinition, cursor :Cursor) :Register = {
    val resultFields = for (fieldDef <- regDef.fields) yield {
      val value = cursor.getString(cursor.getColumnIndex(fieldName(regDef, fieldDef)))
      fieldDef match {
        case UnicodeFieldDefinition => UnicodeField(value.toInt)
        case LanguageCodeFieldDefinition => LanguageCodeField(value)
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
    if (key.storageManager != this) {
      throw new IllegalArgumentException("This key do not belong to this storage manager")
    }

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
      val query = s"INSERT INTO ${tableName(register)} ($keys) VALUES ($values)"
      logi(s"Executing query: $query")
      db.execSQL(query)
      find(db, register).lastOption
    }
    else None
  }

  private def insert(db :SQLiteDatabase, coll :Register.CollectionId, register: Register): Unit = {
    val regDef = register.definition
    val keys = regDef.fields.map(fieldName(regDef,_)).mkString(", ")
    val values = register.fields.map(sqlValue).mkString(", ")
    val query = s"INSERT INTO ${tableName(register)} (${SQLiteStorageManager.collKey}, $keys) VALUES ($coll, $values)"
    logi(s"Executing query: $query")
    db.execSQL(query)
  }

  override def insert(collectionId :Register.CollectionId, register :Register) = {
    val db = getWritableDatabase
    try {
      insert(db, collectionId, register)
      find(db, register).lastOption
    } finally {
      db.close()
    }
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
    val array = regDef match {
      case _: CollectibleRegisterDefinition =>
        Array(SQLiteStorageManager.idKey, SQLiteStorageManager.collKey)
      case _ =>
        Array(SQLiteStorageManager.idKey)
    }
    val cursor = db.query(tableName(regDef), array, null, null, null, null, null, null)

    if (cursor == null) Set()
    else try {
      if (cursor.getCount <= 0 || !cursor.moveToFirst()) Set()
      else {
        val buffer = new ListBuffer[Key]()
        do {
          val group = regDef match {
            case _: CollectibleRegisterDefinition => cursor.getInt(1)
            case _ => 0
          }
          buffer += obtainKey(regDef, group, cursor.getInt(0))
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

  private def replace(db: SQLiteDatabase, register: Register, key: Key): Boolean = {
    val currentOption = get(db, key)
    if (currentOption.isDefined) {
      val expr = register.fields.map(f => s"${fieldName(register.definition, f)}=${sqlValue(f)}")
        .mkString(", ")
      val query = s"UPDATE ${tableName(key.registerDefinition)} SET $expr WHERE ${
        SQLiteStorageManager.idKey
      }=${key.index}"
      logi(s"Executing query: $query")
      db.execSQL(query)

      true
    }
    else false
  }

  override def replace(register: Register, key: Key): Boolean = {
    val db = getWritableDatabase
    try {
      replace(db, register, key)
    } finally {
      db.close()
    }
  }

  private def existReference(db :SQLiteDatabase, key: Key, referencerRegDef: RegisterDefinition, referencerFieldDef: ForeignKeyFieldDefinition): Boolean = {
    val whereClause = s"${fieldName(referencerRegDef, referencerFieldDef)}=${key.index}"
    val cursor = db.query(tableName(referencerRegDef), Array(SQLiteStorageManager.idKey), whereClause, null, null, null, null, null)

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
      val query = s"DELETE FROM ${tableName(key.registerDefinition)} WHERE ${
        SQLiteStorageManager.idKey
      }=${key.index}"
      logi(s"Executing query: $query")
      db.execSQL(query)
      true
    }
    else false
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

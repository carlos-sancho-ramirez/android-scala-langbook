package sword.langbook.android.db

import android.content.Context
import android.database.Cursor
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import sword.db.Register.CollectionId
import sword.db._
import sword.langbook.db.registers.ConceptReferenceField

import scala.collection.mutable.ListBuffer

object SQLiteStorageManager {
  val dbName = "LanguageDb"
  val currentDbVersion = 4
  val idKey = "id"
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
      val columns = regDef.fields.map { fieldDef =>
        val sqlType = fieldDef match {
          case _: CharSequenceField => "TEXT"
          case _ => "INTEGER"
        }

        s"${fieldName(regDef, fieldDef)} $sqlType"
      }.mkString(", ")
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

    insert(db, registers.Language(insertConcept("English").get))
    insert(db, registers.Alphabet(insertConcept("English alphabet").get))
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
      case _ => throw new UnsupportedOperationException("Undefined field definition")
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

  override def insert(register: Register): Option[Key] = {
    val db = getReadableDatabase
    try {
      insert(db, register)
    } finally {
      db.close()
    }
  }

  override def insert(registers: Traversable[Register]): Option[CollectionId] = ???

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
  override def delete(key: Key): Boolean = ???
  override def getKeysForCollection(registerDefinition: CollectibleRegisterDefinition, id: CollectionId): Set[Key] = ???
}

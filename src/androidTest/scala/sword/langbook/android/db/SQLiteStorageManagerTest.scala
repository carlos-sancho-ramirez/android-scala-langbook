package sword.langbook.android.db

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.test.InstrumentationTestCase
import sword.db._

class NoInitializedSqliteStorageManager(context :Context, dbName: String,
    registerDefinitions :Seq[RegisterDefinition]) extends SQLiteStorageManager(context, dbName,
    registerDefinitions) {

  override def initializeDatabase(db: SQLiteDatabase): Unit = {}
}

class SQLiteStorageManagerTest extends InstrumentationTestCase {

  // StorageManager should not be limited to use the FieldDefinitions defined, but the current
  // implementation of SQLiteStorageManager maps each known definition to its proper SQL value type,
  // so unknown types cannot be processed.
  // TODO: Find a solution to allow any field definition

  val testDbName = "testDb"

  val numRegDef = new CollectibleRegisterDefinition {
    override val fields = List(UnicodeFieldDefinition)
  }

  val numReg = new Register {
    override val fields = List(UnicodeField(0x40))
    override val definition = numRegDef
  }

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]) = {
    val context = getInstrumentation.getTargetContext
    context.deleteDatabase(testDbName)
    new NoInitializedSqliteStorageManager(context, testDbName, registerDefinitions)
  }

  def testAlwaysClean(): Unit = {
    val manager1 = newStorageManager(List(numRegDef))
    val key = manager1.insert(numReg).get

    val manager2 = newStorageManager(List(numRegDef))
    if (manager2.get(key).isDefined) {
      throw new AssertionError("Permanent storage?")
    }
  }
}

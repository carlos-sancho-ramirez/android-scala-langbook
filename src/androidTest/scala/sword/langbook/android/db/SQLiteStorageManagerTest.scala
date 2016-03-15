package sword.langbook.android.db

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.test.InstrumentationTestCase
import junit.framework.Assert
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

  val numRegForeignKeyFieldDef = new ForeignKeyFieldDefinition {
    override val target = numRegDef
  }

  val numRegRefRegDef = new RegisterDefinition {
    override val fields = List(numRegForeignKeyFieldDef)
  }

  val numRegCollRefFieldDef = new CollectionReferenceFieldDefinition {
    override val target = numRegDef
  }

  val numRegCollRefRegDef = new RegisterDefinition {
    override val fields = List(numRegCollRefFieldDef)
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

  private def ensureIllegalArgumentExceptionThrown(expression: => Any) = {
    try {
      expression
      throw new AssertionError("IllegalArgumentException expected. But nothing thrown")
    }
    catch {
      case x: IllegalArgumentException => // This is OK
      case x: Exception => throw new AssertionError(s"IllegalArgumentException expected. But ${x.getClass.getSimpleName} thrown")
    }
  }

  def testThrowOnDuplicatedRegisterDefinition(): Unit = {
    ensureIllegalArgumentExceptionThrown {
      newStorageManager(List(numRegDef, numRegDef))
    }
  }

  def testThrowIfExistingRegisterDefinitionWithOuterForeignKey(): Unit = {
    ensureIllegalArgumentExceptionThrown {
      newStorageManager(List(numRegRefRegDef))
    }
  }

  def testThrowIfExistingRegisterDefinitionWithOuterCollectionReference(): Unit = {
    ensureIllegalArgumentExceptionThrown {
      newStorageManager(List(numRegCollRefRegDef))
    }
  }

  private def assertDefined[T](opt: Option[T]): Unit = {
    Assert.assertTrue(opt.isDefined)
  }

  private def assertEquals(expected: Any, given: Any): Unit = {
    Assert.assertEquals(expected, given)
  }

  def testInsertAndRetrieveRegisterWithGivenIdentifier(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    assertDefined(keyOption)

    val regOption = storageManager.get(keyOption.get)
    assertDefined(regOption)
    assertEquals(numReg, regOption.get)
  }

  def testReturnValueMoreThanOnceForTheSameKey(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    assertDefined(keyOption)

    val regOption1 = storageManager.get(keyOption.get)
    assertDefined(regOption1)
    assertEquals(numReg, regOption1.get)

    val regOption2 = storageManager.get(keyOption.get)
    assertDefined(regOption2)
    assertEquals(numReg, regOption2.get)
  }
}

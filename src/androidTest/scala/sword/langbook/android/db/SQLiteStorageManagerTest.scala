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

  class NumRegister(value :Int) extends Register {
    override val fields = List(UnicodeField(value))
    override val definition = numRegDef
  }

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]) = {
    val context = getInstrumentation.getTargetContext
    context.deleteDatabase(testDbName)
    new NoInitializedSqliteStorageManager(context, testDbName, registerDefinitions)
  }

  def testAlwaysClean(): Unit = {
    val manager1 = newStorageManager(List(numRegDef))
    val encodedKey = manager1.insert(numReg).get.encoded

    val manager2 = newStorageManager(List(numRegDef))
    val key = manager2.decode(encodedKey).get
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

  private def assertDefined[T](opt: Option[T]): T = {
    Assert.assertTrue(opt.isDefined)
    opt.get
  }

  private def assertEquals(expected: Any, given: Any): Unit = {
    Assert.assertEquals(expected, given)
  }

  private def assertTrue(value: Boolean): Unit = {
    Assert.assertTrue(value)
  }

  private def assertFalse(value: Boolean): Unit = {
    Assert.assertFalse(value)
  }

  def testInsertAndRetrieveRegisterWithGivenIdentifier(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numReg))
    val reg = assertDefined(storageManager.get(key))
    assertEquals(numReg, reg)
  }

  def testReturnValueMoreThanOnceForTheSameKey(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numReg))

    val reg1 = assertDefined(storageManager.get(key))
    assertEquals(numReg, reg1)

    val reg2 = assertDefined(storageManager.get(key))
    assertEquals(numReg, reg2)
  }

  def testInsertAndDeleteRegisterWithGivenIdentifier(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numReg))
    assertTrue(storageManager.delete(key))
  }

  def testNotDeleteMoreThanOnceForTheSameKey(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numReg))
    assertTrue(storageManager.delete(key))
    assertFalse(storageManager.delete(key))
  }

  def testNotAcceptKeysGeneratedByAnotherStorageManagerInstance(): Unit = {
    val storageManagerA = newStorageManager(List(numRegDef))
    val storageManagerB = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManagerA.insert(numReg))

    ensureIllegalArgumentExceptionThrown {
      storageManagerB.get(key)
    }
  }

  def testCannotInsertARegisterPointingToNothing(): Unit = {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val numRegKey = assertDefined(storageManager.insert(numReg))
    assertTrue(storageManager.delete(numRegKey))

    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = numRegKey
        override val definition = numRegForeignKeyFieldDef
      })
      override val definition = numRegRefRegDef
    }

    assertTrue(storageManager.insert(reg2).isEmpty)
  }

  def testCannotDeleteRegisterPointedByAnother(): Unit = {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val insertedKey = assertDefined(storageManager.insert(numReg))

    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = insertedKey
        override val definition = numRegForeignKeyFieldDef
      })
      override val definition = numRegRefRegDef
    }

    val key2 = assertDefined(storageManager.insert(reg2))

    assertFalse(storageManager.delete(insertedKey))
    assertTrue(storageManager.delete(key2))
    assertTrue(storageManager.delete(insertedKey))
  }

  def testInsertCollectionInSingleOperation(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val reg1 = new NumRegister(5)
    val reg2 = new NumRegister(7)
    val reg3 = new NumRegister(23)

    val list = List(reg1, reg2, reg3)
    val collId = assertDefined(manager.insert(list))

    val keys = manager.getKeysFor(reg1.definition)
    assertEquals(list.size, keys.size)

    for (key <- keys) {
      assertEquals(collId, key.group)
    }
  }
}

package sword.langbook.android.db

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.test.InstrumentationTestCase
import junit.framework.Assert
import sword.db.Register.CollectionId
import sword.db.StorageManager.Key
import sword.db._

class NoInitializedSqliteStorageManager(context :Context, dbName: String,
    registerDefinitions :Seq[RegisterDefinition[Register]]) extends SQLiteStorageManager(context, dbName,
    registerDefinitions) {

  override def initializeDatabase(db: SQLiteDatabase): Unit = {}
}

class SQLiteStorageManagerTest extends InstrumentationTestCase {

  // StorageManager should not be limited to use the FieldDefinitions defined, but the current
  // implementation of SQLiteStorageManager maps each known definition to its proper SQL value type,
  // so unknown types cannot be processed.
  // TODO: Find a solution to allow any field definition

  val testDbName = "testDb"

  val numRegDef = new CollectibleRegisterDefinition[numReg] {
    override val fields = List(UnicodeFieldDefinition)
    override def from(values: Seq[String],
        keyExtractor: FieldDefinition => String => Option[Key]): Option[numReg] = {
      if (values.size == 1) {
        try {
          Some(numReg(values.head.toInt))
        }
        catch {
          case _: NumberFormatException => None
        }
      }
      else None
    }
  }

  val numRegFieldValue = 0x40

  case class numReg(value: Int) extends Register {
    override val fields = List(UnicodeField(value))
    override val definition = numRegDef
  }

  val numRegInstance = numReg(numRegFieldValue)

  case class numRegForeignKeyField(key: StorageManager.Key) extends ForeignKeyField {
    override val definition = numRegForeignKeyFieldDef
  }

  val numRegForeignKeyFieldDef = new ForeignKeyFieldDefinition {
    override val target = numRegDef
    override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
      keyExtractor(value).map(numRegForeignKeyField)
    }
  }

  val numRegRefRegDef = new RegisterDefinition[numRegRefReg] {
    override val fields = List(numRegForeignKeyFieldDef)
    override def from(values: Seq[String],
        keyExtractor: FieldDefinition => String => Option[Key]): Option[numRegRefReg] = {
      if (values.size == 1) keyExtractor(numRegForeignKeyFieldDef)(values.head).map(numRegRefReg)
      else None
    }
  }

  case class numRegRefReg(key: StorageManager.Key) extends Register {
    override val fields = List(numRegForeignKeyField(key))
    override val definition = numRegRefRegDef
  }

  val numRegCollRefFieldDef = new CollectionReferenceFieldDefinition {
    override val target = numRegDef
    override def newField: CollectionId => CollectionReferenceField = ???
  }

  class NumRegister(value :Int) extends Register {
    override val fields = List(UnicodeField(value))
    override val definition = numRegDef
  }

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition[Register]]) = {
    val context = getInstrumentation.getTargetContext
    context.deleteDatabase(testDbName)
    new NoInitializedSqliteStorageManager(context, testDbName, registerDefinitions)
  }

  def testAlwaysClean(): Unit = {
    val manager1 = newStorageManager(List(numRegDef))
    val encodedKey = manager1.insert(numRegInstance).get.encoded

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

  private def ensureUnsupportedOperationExceptionThrown(expression: => Any) = {
    try {
      expression
      throw new AssertionError("UnsupportedOperationException expected. But nothing thrown")
    }
    catch {
      case x: UnsupportedOperationException => // This is OK
      case x: Exception => throw new AssertionError(s"UnsupportedOperationException expected. But ${x.getClass.getSimpleName} thrown")
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
    val numRegCollRefRegDef = new RegisterDefinition[Register] {
      override val fields = List(numRegCollRefFieldDef)
      override def from(values: Seq[String],
        keyExtractor: FieldDefinition => String => Option[Key]): Option[Register] = ???
    }

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

  private def assertEmpty[T](coll: Traversable[T]): Unit = {
    Assert.assertTrue(coll.isEmpty)
  }

  private def assertTrue(value: Boolean): Unit = {
    Assert.assertTrue(value)
  }

  private def assertFalse(value: Boolean): Unit = {
    Assert.assertFalse(value)
  }

  def testInsertAndRetrieveRegisterWithGivenIdentifier(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))
    val reg = assertDefined(storageManager.get(key))
    assertEquals(numRegInstance, reg)
  }

  def testReturnValueMoreThanOnceForTheSameKey(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))

    val reg1 = assertDefined(storageManager.get(key))
    assertEquals(numRegInstance, reg1)

    val reg2 = assertDefined(storageManager.get(key))
    assertEquals(numRegInstance, reg2)
  }

  def testInsertAndDeleteRegisterWithGivenIdentifier(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))
    assertTrue(storageManager.delete(key))
  }

  def testNotDeleteMoreThanOnceForTheSameKey(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))
    assertTrue(storageManager.delete(key))
    assertFalse(storageManager.delete(key))
  }

  def testNotAcceptKeysGeneratedByAnotherStorageManagerInstance(): Unit = {
    val storageManagerA = newStorageManager(List(numRegDef))
    val storageManagerB = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManagerA.insert(numRegInstance))

    ensureIllegalArgumentExceptionThrown {
      storageManagerB.get(key)
    }
  }

  def testCannotInsertARegisterPointingToNothing(): Unit = {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val numRegKey = assertDefined(storageManager.insert(numRegInstance))
    assertTrue(storageManager.delete(numRegKey))

    val reg2 = numRegRefReg(numRegKey)
    assertEmpty(storageManager.insert(reg2))
  }

  def testCannotDeleteRegisterPointedByAnother(): Unit = {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val insertedKey = assertDefined(storageManager.insert(numRegInstance))

    val reg2 = numRegRefReg(insertedKey)
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

  def testInsertRegisterIntoAnExistingCollection():Unit = {
    val manager = newStorageManager(List(numRegDef))
    val reg1 = new NumRegister(5)
    val reg2 = new NumRegister(7)
    val reg3 = new NumRegister(23)

    val list = List(reg1, reg2, reg3)
    val collId = manager.insert(list).get

    val reg4 = new NumRegister(14)
    assertDefined(manager.insert(collId, reg4))

    val keys = manager.getKeysFor(reg1.definition)
    assertEquals(4, keys.size)

    for (key <- keys) {
      assertEquals(collId, key.group)
    }
  }

  def testInsertMoreThanOneCollection(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val reg1 = new NumRegister(5)
    val reg2 = new NumRegister(7)
    val reg3 = new NumRegister(23)
    val reg4 = new NumRegister(45)
    val reg5 = new NumRegister(58)

    val list1 = List(reg1, reg2, reg4)
    val list2 = List(reg3, reg4, reg5)
    val coll1Id = assertDefined(manager.insert(list1))
    val coll2Id = assertDefined(manager.insert(list2))

    val keys = manager.getKeysFor(reg1.definition)
    assertEquals(list1.size + list2.size, keys.size)

    assertEquals(list1.toSet, keys.filter(_.group == coll1Id).flatMap(manager.get).toSet)
    assertEquals(list2.toSet, keys.filter(_.group == coll2Id).flatMap(manager.get).toSet)
  }

  def testThrowUnsupportedOperationExceptionOnInsertingCollectionForNonCollectibleRegisters(): Unit = {
    val myRegDef = new RegisterDefinition[Register] {
      override val fields = List(UnicodeFieldDefinition)
      override def from(values: Seq[String],
        keyExtractor: FieldDefinition => String => Option[Key]): Option[Register] = ???
    }

    val manager = newStorageManager(List(myRegDef))
    class MyNumReg(value :Int) extends Register {
      override val definition = myRegDef
      override val fields = List(UnicodeField(value))
    }

    val reg1 = new MyNumReg(5)
    val reg2 = new MyNumReg(7)
    val reg3 = new MyNumReg(23)

    val list = List(reg1, reg2, reg3)
    ensureUnsupportedOperationExceptionThrown {
      manager.insert(list)
    }

    assertEmpty(manager.getKeysFor(myRegDef))
  }

  def testThrowUnsupportedOperationExceptionOnInsertingCollectionWithDifferentCollectibleRegistersDefinitions(): Unit = {
    val myRegDef = new CollectibleRegisterDefinition[Register] {
      override val fields = List(UnicodeFieldDefinition)
      override def from(values: Seq[String],
        keyExtractor: FieldDefinition => String => Option[Key]): Option[Register] = ???
    }

    val manager = newStorageManager(List(numRegDef, myRegDef))
    class MyNumReg(value :Int) extends Register {
      override val definition = myRegDef
      override val fields = List(UnicodeField(value))
    }

    val reg1 = new NumRegister(5)
    val reg2 = new MyNumReg(7)

    val list = List(reg1, reg2)
    ensureUnsupportedOperationExceptionThrown {
      manager.insert(list)
    }

    assertEmpty(manager.getKeysFor(numRegDef))
    assertEmpty(manager.getKeysFor(myRegDef))
  }

  def testReturnNullSetBeforeInsertingAnything(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    assertEmpty(manager.getKeysFor(numRegDef))
  }

  def testReturnJustTheKeyForTheInsertedRegister(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val key = assertDefined(manager.insert(numRegInstance))

    assertEquals(1, manager.getKeysFor(numRegDef).size)
    assertTrue(manager.getKeysFor(numRegDef).contains(key))
  }

  def testReturnJustKeysForInsertedRegisters(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val key1 = assertDefined(manager.insert(numRegInstance))
    val key2 = assertDefined(manager.insert(numRegInstance))

    assertEquals(2, manager.getKeysFor(numRegDef).size)
    assertTrue(manager.getKeysFor(numRegDef) contains key1)
    assertTrue(manager.getKeysFor(numRegDef) contains key2)
  }

  def testReplaceOneRegisterByAnotherWithSameDefinition(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))

    val regB = new Register {
      override val fields = List(UnicodeField(numRegFieldValue + 1))
      override val definition = numRegDef
    }
    assertFalse(numRegInstance == regB)
    assertTrue(storageManager.replace(regB, key))

    val reg = assertDefined(storageManager.get(key))
    assertEquals(regB, reg)
  }

  def testNotReplaceRegisterWhenKeyNotDefinedPreviously(): Unit = {
    val storageManager = newStorageManager(List(numRegDef))
    val key = assertDefined(storageManager.insert(numRegInstance))

    val regB = new Register {
      override val fields = List(UnicodeField(numRegFieldValue + 1))
      override val definition = numRegDef
    }
    assertFalse(numRegInstance == regB)

    assertTrue(storageManager.delete(key))
    assertFalse(storageManager.replace(regB, key))
  }

  def testReturnAllRegisterKeysWithGivenCollectionId(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val groups = List[Int](1,2,3,1,2,1)
    val collections = groups.indices.map(_ + 1).zip(groups).groupBy { case (_,group) => group }
      .flatMap { case (group, items) =>
        val regs = items.map { case (index,_) => new Register {
          override val fields = List(UnicodeField(index))
          override val definition = numRegDef
        }}

        manager.insert(regs).map(id => (group, id))
      }

    for (group <- groups.toSet[Int]) {
      val expected = groups.indices.zip(groups).filter { case (_,g) => g == group } map { x => x._1 + 1 }
      val keys = manager.getKeysForCollection(numRegDef, collections.find(_._1 == group).head._2)
      assertEquals(expected.toSet, keys.flatMap(manager.get).map(_.fields.head.asInstanceOf[UnicodeField].value))
    }
  }

  def testReturnMapContainingAllInsertedRegistersGroupedByKey(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val inserted = for (i <- 0 until 10) yield {
      val reg = new Register {
        override val fields = List(UnicodeField(i))
        override val definition = numRegDef
      }
      assertDefined(manager.insert(reg).map(key => (key, reg)))
    }

    val map = manager.getMapFor(numRegDef)
    assertEquals(inserted.size, map.size)
    for (insertion <- inserted) {
      val opt = assertDefined(map.get(insertion._1))
      assertEquals(insertion._2, opt)
    }
  }

  def testReturnMapMatchingAllRegistersWithTheirKeysForGivenCollectionId() = {
    val manager = newStorageManager(List(numRegDef))
    val groups = List[Int](1,2,3,1,2,1,3)
    val collections = groups.indices.map(_ + 1).zip(groups).groupBy { case (_,group) => group }
      .flatMap { case (group, items) =>
        val regs = items.map { case (index,_) => new Register {
          override val fields = List(UnicodeField(index))
          override val definition = numRegDef
        }}

        manager.insert(regs).map(id => (group, id))
      }

    for (group <- groups.toSet[Int]) {
      val expected = groups.indices.zip(groups).filter { case (_,g) => g == group } map { x => x._1 + 1 }
      val regs = manager.getMapForCollection(numRegDef, collections.find(_._1 == group).head._2)
      assertEquals(expected.toSet, regs.values.map(_.fields.head.asInstanceOf[UnicodeField].value).toSet)
    }
  }

  def testReturnKeysContainingSameStorageManagerInstance(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val key = assertDefined(manager.insert(numRegInstance))
    assertEquals(manager, key.storageManager)
  }

  def testEncodeKeyAndDecodeItBack(): Unit = {
    val manager = newStorageManager(List(numRegDef))
    val key = assertDefined(manager.insert(numRegInstance))

    val str = manager.encode(key)
    val newKey = assertDefined(manager.decode(str))
    assertEquals(key, newKey)
  }

  def testEncodeKeyAndDecodeItFromAnotherInstanceWithSameTypeAndConfiguration() = {
    val managerA = newStorageManager(List(numRegDef))
    val keyA = assertDefined(managerA.insert(numRegInstance))

    val str = managerA.encode(keyA)

    val managerB = newStorageManager(List(numRegDef))
    val keyB = assertDefined(managerB.decode(str))
    assertEquals(managerB, keyB.storageManager)

    assertEquals(keyA.index, keyB.index)
    assertEquals(keyA.group, keyB.group)
  }
}

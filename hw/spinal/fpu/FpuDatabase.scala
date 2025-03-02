package fpu

import spinal.core._
import spinal.core.fiber._
import spinal.core.fiber.Retainer
import scala.collection.mutable

/**
 * Provide a API to access a HashMap which uses Element[_ <: Any] as keys
 * The Database object provide a SpinalHDL ScopeProperty[DataBase] allowing to have one globally accessible implicit database
 * That globally shared database can be used as a way to exchange "global" variables in a given context (e.g., VexiiRiscv core 3)
 */
class Database {
  // User API
  def update[T](key: Element[T], value: T) = key.set(this, value)
  def apply[T](key: Element[T]): T = key.getOn(this)
  def on[T](body: => T) = Database(this).on(body)

  // "private" API
  val storage = mutable.LinkedHashMap[Element[_ <: Any], Any]()
  def storageUpdate[T](key: Element[T], value: T) = storage.update(key, value)
  def storageGet[T](key: Element[T]): T = storage.apply(key).asInstanceOf[T]
  def storageGetElseUpdate[T](key: Element[T], create: => T): T = storage.getOrElseUpdate(key, create).asInstanceOf[T]
  def storageExists[T](key: Element[T]) = storage.contains(key)
}

/**
 * Provide a global implicit database
 */
object Database extends ScopeProperty[Database] {
  def on[T](body: => T) = this(new Database).on(body) // Set the implicit database to be used on the given body of code

  // Define new Element instances (keys for the database)
  def value[T]() = new ElementValue[T]()
  def blocking[T]() = new ElementBlocking[T]()
  def landa[T](body: => T) = new ElementLanda(body)
}

object Element {
  implicit def toValue[T](p: Element[T]): T = p.get

  class ThingIntPimper(p: Element[Int]) {
    def bits = BitCount(p.get)
    def bit = BitCount(p.get)
  }

  implicit def thingIntPimperFunc(p: Element[Int]): ThingIntPimper = new ThingIntPimper(p)
}

/**
 * Represent a thing which can be in a database (this is the key)
 */
abstract class Element[T](sp: ScopeProperty[Database] = Database) extends Nameable {
  // User API
  def get: T = getOn(sp.get)
  def apply: T = getOn(sp.get)
  def set(value: T): Unit = set(sp.get, value)
  def isEmpty: Boolean = isEmpty(sp.get)

  // Private API
  def isEmpty(db: Database): Boolean
  def getOn(db: Database): T
  def set(db: Database, value: T): Unit
}

/**
 * Simple implementation, which allows getting/setting a value
 * Will throw an exception if we try to get something which isn't set
 */
class ElementValue[T](sp: ScopeProperty[Database] = Database) extends Element[T](sp) {
  def getOn(db: Database): T = db.storageGet(this)
  def set(db: Database, value: T) = db.storageUpdate(this, value)
  override def isEmpty(db: Database): Boolean = ???
}

/**
 * Same as ElementValue, but based on the SpinalHDL Fiber API
 * Will suspend the fiber thread if getting an unset value until it’s set
 */
class ElementBlocking[T](sp: ScopeProperty[Database] = Database) extends Element[T](sp) with Area {
  val thing = new ElementValue[Handle[T]]()
  def getHandle(db: Database): Handle[T] = db.storageGetElseUpdate(thing, new Handle[T].setCompositeName(this))
  def getOn(db: Database): T = getHandle(db).get
  def set(db: Database, value: T) = {
    val handle = getHandle(db)
    if (handle.isLoaded) {
      assert(handle.get == value, s"DB was set to ${handle.get} before, can't overwrite to $value")
    }
    handle.load(value)
  }

  def soon(): Unit = {
    getHandle(sp.get).willBeLoadedBy = AsyncThread.current
    AsyncThread.current.willLoadHandles += getHandle(sp.get)
  }
  override def isEmpty(db: Database): Boolean = !getHandle(db).isLoaded
}

/**
 * The body provides the processing to generate the value
 */
class ElementLanda[T](body: => T, sp: ScopeProperty[Database] = Database) extends ElementValue[T](sp) {
  override def getOn(db: Database): T = {
    if (!db.storageExists(this)) {
      db.storageUpdate(this, body)
    }
    super.getOn(db)
  }

  override def set(db: Database, value: T) = ???
}

object FpuDatabase {
  val customOps = Database.blocking[Map[String, FpuOperation.E]]
  val customMicroOps = Database.blocking[Map[String, MicrocodeOp.E]]
  val microcodeSequences = Database.blocking[Map[FpuOperation.E, Seq[MicrocodeInstruction]]]
  val updatesComplete = Retainer()

  microcodeSequences.set(Map(
    FpuOperation.NONE -> Seq(instr(MicrocodeOp.FINALIZE, 0, 0, 0, 0))
  ))

  def instr(op: MicrocodeOp.E, srcA: Int, srcB: Int, dest: Int, next: Int): MicrocodeInstruction = {
    val i = MicrocodeInstruction()
    i.op := op
    i.srcA := srcA
    i.srcB := srcB
    i.dest := dest
    i.nextPc := next
    i
  }

  def updateCustomOps(key: String, newOps: Map[String, FpuOperation.E]): Unit = {
    val current = customOps.get
    val updated = current ++ newOps
    println(s"Updating customOps for $key: $updated")
    customOps.set(updated)
    // Example usage of updatesComplete() within the database
    // This signals that an update has occurred; plugins can wait on it
    updatesComplete() // Creates a RetainerHold, implicitly managed by await()
  }

  def updateMicrocodeSequences(key: FpuOperation.E, newSequences: Map[FpuOperation.E, Seq[MicrocodeInstruction]]): Unit = {
    if (key == null) {
      println("Warning: Skipping updateMicrocodeSequences with null key")
      return
    }
    val current = microcodeSequences.get
    val filtered = newSequences.filter { case (k, v) => !current.contains(k) || current(k) == v }
    val updated = current ++ filtered
    println(s"Updating microcodeSequences for $key: $updated")
    microcodeSequences.set(updated)
    // Example usage of updatesComplete() within the database
    updatesComplete() // Signals update completion
  }

  // Optional: Method to explicitly wait for updates to complete
  def waitForUpdates(): Unit = {
    updatesComplete.await() // Plugins or other code can call this to wait
  }
}
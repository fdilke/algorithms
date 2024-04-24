package com.fdilke.freefire

import cats.free.Free
import Free.liftF
import cats.arrow.FunctionK
import cats.{Id, ~>}
import scala.collection.mutable

// based on the example from the typelevel documentation
object FreeMonadKVStore:

    // Step 0. Create an ADT

    sealed trait KVStoreA[A]
    case class Put[T](key: String, value: T) extends KVStoreA[Unit]
    case class Get[T](key: String) extends KVStoreA[Option[T]]
    case class Delete(key: String) extends KVStoreA[Unit]  

    // Step 1. Create a type
    type KVStore[A] = Free[KVStoreA, A]

    // Step 2. Create smart constructors

    // Put returns nothing (i.e. Unit).
    def put[T](key: String, value: T): KVStore[Unit] =
        liftF[KVStoreA, Unit](Put[T](key, value))

    // Get returns a T value.
    def get[T](key: String): KVStore[Option[T]] =
        liftF[KVStoreA, Option[T]](Get[T](key))

    // Delete returns nothing (i.e. Unit).
    def delete(key: String): KVStore[Unit] =
        liftF[KVStoreA, Unit](Delete(key))

    // Update composes get and set, and returns nothing.
    def update[T](
        key: String
    )(
        f: T => T
    ): KVStore[Unit] =
        for
            vMaybe <- get[T](key)
            _ <- vMaybe.map {
                v => put[T](key, f(v))
            } getOrElse ( Free.pure(()) )
        yield ()

    // Step 4. Create a little program that works on the ADT
    def littleProgram(): KVStore[Option[Int]] =
        for
            _ <- put("wild-cats", 2)
            _ <- update[Int]("wild-cats") { _ + 12 }
            _ <- put("tame-cats", 5)
            n <- get[Int]("wild-cats")
            _ <- delete("tame-cats")
        yield n        

    // Step 5. Create an impure compiler (with side effects) that works on the ADT
    def impureCompiler: KVStoreA ~> Id  =
        new (KVStoreA ~> Id):
            // a very simple (and imprecise) key-value store
            val kvs: mutable.Map[String, Any] =
                mutable.Map.empty

            override def apply[A](
                fa: KVStoreA[A]
            ): Id[A] =
                fa match
                    case Put(key, value) =>
                        println(s"put($key, $value)")
                        kvs(key) = value
                        ()
                    case Get(key) =>
                        println(s"get($key)")
                        kvs.get(key).asInstanceOf[A]
                    case Delete(key) =>
                        println(s"delete($key)")
                        kvs.remove(key)
                        ()

    // Step 6. Run it with Free.foldMap

    def impureRun[R](prog: KVStore[R]): R = 
        prog.foldMap(impureCompiler)
        
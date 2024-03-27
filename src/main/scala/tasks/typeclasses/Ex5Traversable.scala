package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  trait Traversable[T[_]]:
    extension [A](t: T[A]) def consume(consumer: A => Unit): Unit

//  object Traversable:
//    extension [A, T[A]: Traversable](t: T[A]) def consume(consumer: A => Unit): Unit = t.consume(consumer)

object TraversableGivenInstances:
  import Ex5Traversable.*, Traversable.*

  given Traversable[Optional] with
    extension [A](t: Optional[A]) def consume(consumer: A => Unit): Unit = t match
        case Just(a) => consumer(a)
        case _ => ()

  given Traversable[Sequence] with
    extension [A](t: Sequence[A]) def consume(consumer: A => Unit): Unit = t match
        case Cons(head, tail) => consumer(head); tail.consume(consumer)
        case _ => ()

@main def tryTraversable =
  import Ex5Traversable.*
  import TraversableGivenInstances.given

  val opt: Optional[Int] = Just(10)
  opt.consume(a => log(a))

  val seq: Sequence[String] = Cons("Hello", Cons(" World!", Nil()))
  seq.consume(a => log(a))


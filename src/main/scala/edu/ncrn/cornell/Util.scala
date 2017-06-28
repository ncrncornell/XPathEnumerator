package edu.ncrn.cornell

object Util {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    @specialized def ===(other: A): Boolean = self == other
    @specialized def =!=(other: A): Boolean = self != other

  }
}
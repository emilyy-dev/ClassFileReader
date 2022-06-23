package io.github.emilyydev.classreader.accessflag

trait AccessFlag[A] {

  val FlagMap: Map[Int, A]
  val Names: Map[A, String]

  final def asSet(flagSet: Int): Set[A] =
    Set.from(
      FlagMap
        .filter { case (flagValue, _) => isFlagSet(flagSet, flagValue) }
        .values
    )

  final def isFlagSet(flagSet: Int, flag: Int): Boolean = (flagSet & flag) == flag

  final def setAsNumber(flagSet: Set[A]): Int =
    FlagMap
      .filter { case (_, flag) => flagSet.contains(flag) }
      .keys
      .reduce(_ | _)
}

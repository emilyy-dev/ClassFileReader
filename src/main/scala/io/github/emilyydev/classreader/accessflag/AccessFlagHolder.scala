package io.github.emilyydev.classreader.accessflag

trait AccessFlagHolder {
  val LegalFlags: Set[AccessFlag]
  final val ToAccessFlagSet: Int => Set[AccessFlag] = AccessFlag.asSet(LegalFlags)
}

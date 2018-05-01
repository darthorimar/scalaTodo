package object renderer {
  implicit class SeqEitherOps[A, B](seq: Seq[Either[A, B]]) {
    def sequence: Either[A, Seq[B]] =
      seq.foldRight(Right(Seq.empty): Either[A, Seq[B]]) { case (xs, x) =>
        x.flatMap((y: Seq[B]) => xs.map(b => b+:y))
      }
  }
}

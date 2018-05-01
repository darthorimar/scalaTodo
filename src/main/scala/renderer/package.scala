package object renderer {
  implicit class SeqEitherOps[A, B](seq: Seq[Either[A, B]]) {
    def sequence: Either[A, Seq[B]] =
      seq.foldLeft(Right(Seq.empty): Either[A, Seq[B]]) { case (x, xs) =>
        x.flatMap((y: Seq[B]) => xs.map(b => b+:y))
      }
  }
}

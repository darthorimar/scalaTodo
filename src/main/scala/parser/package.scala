import fastparse.WhitespaceApi

package object parser {
  val specialChars = Seq('%', '\n', '}', '{')

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }

}

import fastparse.WhitespaceApi

package object parser {
  val specialChars = Seq('%', '\n', '}', '{')
  val keywords = Seq("true", "false", "and", "or", "xor")
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
}

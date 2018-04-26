package darthorimar.parser

import java.time.{LocalDate, LocalDateTime, LocalTime}

import utest._

object ParserCommonTest extends TestSuite with ParserTestMixin {
  import ParserCommon._
  val tests = Tests {
    'numberTest - {
      testParser("123", number, Some(123))
      testParser("d1", number, None)
    }

    'boolConstTest - {
      testParser("true", boolConst, Some(true))
      testParser("false", boolConst, Some(false))
      testParser("oops", boolConst, None)
    }

    'stringTest - {
      testParser("\"\"", string, Some(""))
      testParser("\"nya\"", string, Some("nya"))
      testParser("nya\"", string, None)
    }

    'variableTest - {
      testParser("val", variable, Some("val"))
      testParser("Val", variable, None)
    }

    'dateTest - {
      testParser("Date(2018-10-11)", date,
        Some(LocalDate.of(2018, 10, 11).atStartOfDay()))
      testParser("Date(10:20:30)", date,
        Some(LocalTime.of(10,20,30).atDate(LocalDate.ofEpochDay(0))))
      testParser("Date(2011-12-03T10:15:30)", date,
        Some(LocalDateTime.of(2011, 12,3,10,15,30)))
    }
  }
}

package darthorimar.todolist

import java.time.LocalDateTime

import utest._

object FunctionsTest extends TestSuite{
  val date = DateType(LocalDateTime.parse("2011-12-03T10:15:30"))
  val tests = Tests{
    'dateFunctionsTest - {
      assert(Functions.callFunction("date", Seq(date)) == Right(StrType("2011-12-03")))
      assert(Functions.callFunction("time", Seq(date)) == Right(StrType("10:15:30")))
      assert(Functions.callFunction("monthName", Seq(date)) == Right(StrType("DECEMBER")))
    }

    'intRangeTest - {
      assert(Functions.callFunction("range",
        Seq(IntType(0), IntType(2))) == Right(SeqType(Seq(IntType(0), IntType(1), IntType(2)))))
      assert(Functions.callFunction("range",
        Seq(IntType(0), IntType(2))) == Right(SeqType(Seq(IntType(0), IntType(1), IntType(2)))))
    }

    'dateRangeTest - {
      assert(Functions.callFunction("dayRange",
        Seq(date, DateType(date.v.plusDays(2))))
        == Right(SeqType(Seq(date, DateType(date.v.plusDays(1)), DateType(date.v.plusDays(2))))))
    }
  }
}

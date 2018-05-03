# Todo list in Scala

Allows you to create todo lists in customizable templates and the use them in web browser or terminal

## Syntax:
* nested todo items:
```
#This is a header :)

this
 is
  a
   nested
 items
  in
  todo
list
```
* Expressions in any place
```
the answer to
 The Ultimate Question
 of %{"Life"},
 the %{"Uni"+"verse"},
 and Everything
 is %{21 * 2}
```
* Boolean, Integer, String, Date, and Sequence expressions
```
%{x > 4 and "ny" + "a" = "nya" or hours(Date(11:12:13)) = 12 xor range(2) = Seq(0,1,2)}
```
* If expressions
```
%if rand(0, 10) = 5
 you are lucky
%else
 oops
```

* Lor loops
```
%loop i <- range(10)
 %{i} is a good number
```

* Loops on time and date
```
%loop t <- dayRange(today(), Date(2018-10-10))
 %{day(t)} %{monthName(t)}
```


* Definitions
```
%%def renderTime m h s
 The time is %{m}:%{h}:%{s}
What's time now?
 %def renderTime %{7} %{40} %{0}
```

* And getting random bash.org quotes :)
```
%{bash()}
```


## Config files support
To customize a template just create a `.conf` file like the following
```
x=5
y="nya!"
cake="false"
primes=Seq(2,3,5,7,11)
```

## Two rendering mode
* Console mode: outputs todo lists to the console
* Web rendering mode: starts a web server with interactive todo list

* Rick's example
```
#Rick's todo list

%%def typicalMorning d
 Remember that today is %{format(d, "dd-MM-yy")}
 And then %{randChoice(ideas)}

%loop d <- dayRange(now(), addWeek(now()))
 %{dayOfWeek(d)}
  %if contains(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31), day(d))
   Today is a "prime" day!
  %else
   Today is a usual day
  Read a cool bash quote:
     %{bash()}
  %def typicalMorning %{d}
  Remember what does int mean %{rand(0,10)} %{rand(0,10)} %{rand(0,10)}
  Drink sth
  %loop phrase <- randSubseq(phrases, 3)
   %{phrase}
```
First two days of console output
```
Rick's todo list

Thursday
 Today is a "prime" day!
 Read a cool bash quote:
  Телефонный разговор.
  xxx: Здравствуйте, я такой-то такой-то, а вам сейчас удобно разговаривать?
  yyy: Здравствуйте, да.
  xxx: Мне тоже.
  yyy: О_о
 Remember that today is 03-05-18
 And then Go th the Anatomy Park
 Remember what does int mean 10 8 0
 Drink sth
 GRASSSSS... tastes bad!
 BURGERTIME!
 And that's why I always say, 'Shumshumschilpiddydah!'
Friday
 Today is a usual day
 Read a cool bash quote:
  xxx: читаю новости
  17 апреля: ученые случайно создали фермент, разлагающий пластик
  18 апреля: ученые случайно улучшили фермент, разлагающий пластик
  Мы на верном пути!
 Remember that today is 04-05-18
 And then Become a Pickle Rick
 Remember what does int mean 4 1 6
 Drink sth
 Rikitikitavi!
 And that's the wayyyyyy the news goes!
 Wubbalubbadubdub!
```

Example files are `./ricksWeek.templ` and `./ricksWeek.conf`

## Command line Usage
```
Usage: todo.jar [options]

  -t, --template <file>     Template File
  -m, --mode <web|console>  Application mode
  -c, --config <file>       Config file (optional)
```
## Technologies
* Scala as main programming language
* fastParse as parser-combinator library
* scopt for parsing cmd arguments
* utest for testing
* akka-http for web sever
* JS, bootstrap on frontend

import dataheretic.compiler.DHCompiler
import fastparse.all._


val h1 = "\t\tid\tname\tage"
val h2 =
  """   hi there   rasputin""".stripMargin

//Headers(2).parse(h1)
//DHCompiler.Rules.Headers(2).parse(h2)

P("abc").rep(min = 0, max = 0).parse("").get
import mill._, scalalib._

object eliotc extends RootModule with ScalaModule {
  def scalaVersion = "3.3.1"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-effect:3.5.2",
    ivy"org.typelevel::cats-free:2.10.0",
    ivy"io.github.timwspence::cats-stm:0.13.4",
    ivy"org.apache.logging.log4j:log4j-core:2.22.1",
    ivy"com.github.scopt::scopt:4.1.0"
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.17",
      ivy"org.typelevel::cats-effect-testing-scalatest:1.5.0"
    )

    def testFramework = "org.scalatest.tools.Framework"
  }
}

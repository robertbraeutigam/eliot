import mill._, scalalib._

object eliotc extends RootModule with ScalaModule {
  def scalaVersion = "3.3.1"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-effect:3.5.2",
    ivy"io.github.timwspence::cats-stm:0.13.4"
  )
}

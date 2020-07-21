name := "scala-books-exercises"
version := "1.0"

scalaVersion in ThisBuild := "2.12.8"
scalacOptions in ThisBuild ++= Seq(
	"-deprecation",
	"-feature",
	"-explaintypes",
	"-unchecked",
	"-language:higherKinds",
	"-language:implicitConversions",
	"-Ywarn-dead-code",
	"-Ywarn-numeric-widen",
	"-Ywarn-value-discard",
	"-Ywarn-unused:imports",
	"-Ywarn-unused:locals",
	"-Ywarn-unused:params",
	"-Ywarn-unused:privates",
	"-encoding",
	"utf8"
)


libraryDependencies ++=
  Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.typelevel" %% "cats-core" % "2.0.0-M1",
    "junit" % "junit" % "4.12" % "test"
  )

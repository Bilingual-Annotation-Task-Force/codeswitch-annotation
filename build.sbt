name := "bilingual-annotation"
 
version := "0.0.1"
 
organization := "something"
 
scalaVersion := "2.11.2"
 
libraryDependencies ++= Seq(
"junit" % "junit" % "4.10" % "test",
"com.novocode" % "junit-interface" % "0.8" % "test->default"
)
 
scalacOptions ++= Seq("-deprecation")

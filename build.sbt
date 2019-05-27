name := "mdd"

organization := "fr.uphf"

version := "2.0-SNAPSHOT"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.7" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
	)

scalacOptions ++= Seq(
   "-Xdisable-assertions"
  , "-deprecation" 
//	"-unchecked" 
    , "-Xlint" 
   , "-target:jvm-1.8"
//	
//	"-feature"
//	"-Yinline-warnings"
)

//wartremoverWarnings ++= Warts.all


publishTo :=  {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${(target in Test).value / "test-reports"}")


// EclipseKeys.withBundledScalaContainers := false
licenses := Seq("LGPL 3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt"))

homepage := Some(url("https://github.com/concrete-cp/mdd"))

publishMavenStyle := true

pomExtra in Global := {
  <scm>
    <connection>scm:git:github.com/concrete-cp/mdd.git</connection>
    <url>github.com/concrete-cp/mdd.git</url>
  </scm>

  <developers>
    <developer>
      <id>scand1sk</id>
      <name>Julien Vion</name>
      <url>http://vion.free.fr/perso</url>
    </developer>
  </developers>
}

testOptions in Test += Tests.Argument("-oDF")

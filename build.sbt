android.Plugin.androidBuild

lazy val root = (project in file("."))
    .dependsOn(generic)
    .settings(
      javacOptions ++= Seq("-target","1.7","-source","1.7"),
      scalaVersion := "2.11.7",
      libraryDependencies += "com.android.support" % "appcompat-v7" % "22.2.1"
)

lazy val generic = (project in file("generic"))
    .settings(
      exportJars := true
    )


android.Plugin.androidBuild

lazy val root = (project in file("."))
    .dependsOn(generic)
    .settings(
      javacOptions ++= Seq("-target","1.7","-source","1.7"),
      scalaVersion := "2.11.7",
      libraryDependencies ++= Seq(
        "com.android.support" % "appcompat-v7" % "23.1.1",
        "com.android.support" % "design" % "23.1.1"
      )
)

lazy val generic = (project in file("generic"))
    .settings(
      exportJars := true
    )

sourceDirectories in Android := Seq(file("src/main/scala"), file("src/androidTest/scala"))

proguardOptions in Android ++= Seq(
    "-keep public class sword.langbook.android.db.SQLiteStorageManagerTest { public void test*(); }",
    "-keep public class android.support.v7.widget.SearchView { public <init>(android.content.Context); public <init>(android.content.Context, android.util.AttributeSet); }"
)

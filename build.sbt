// Name of the project
name := "ScalaFX Demos"

// Project version
version := "8.0.20-R7-SNAPSHOT"

// Version of Scala used by the project
scalaVersion := "2.11.4"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "2.2.67-R10"

libraryDependencies += "org.scalafx" % "scalafxml-core_2.11" % "0.2.1"

// Java7用の設定：Java8を使っている環境ならばコメントアウトして下さい．
unmanagedJars in Compile += Attributed.blank(file(scala.util.Properties.javaHome) / "/lib/jfxrt.jar")

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

scalaSource in Compile := baseDirectory.value / "src"

// Java7では動かない一部のデモのコンパイルを禁止する設定です．
// Java8を使っているのであれば，以下をコメントアウトすると三次元グラフィックスのデモなどが見られます．
excludeFilter in (Compile, unmanagedSources) ~= { _ ||
  new FileFilter {
    def accept(f: File) = {
      val path = f.getPath
      path.containsSlice("/graphics3d/") || path.containsSlice("/scene/text/")
    }
  }
}

resourceDirectory in Compile := baseDirectory.value / "resources"

target := Path.userHome / ".tmp" / "scalafx" / "demos"

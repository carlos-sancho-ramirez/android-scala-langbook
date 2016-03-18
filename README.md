# android-scala-langbook

Langbook is intended to be a tool to help people in their task of learning new
languages and master the ones they may already know. A database is used to store
words for the different languages and link them with concepts in a sensible way
that may bring basic semantic.

This repository contains all Android platform dependent sources. See also
[generic-scala-langbook](https://github.com/carlos-sancho-ramirez/generic-scala-langbook)
for the Langbook platform independent code.

## How to build and run this project

This project uses the [Android SDK Plugin for SBT](https://github.com/pfn/android-sdk-plugin)
to simplify the build process. Please follow the previous link for further details on it.

In short, follow the following steps to start:
  * Download and install the [SBT (Simple Build Tool)](http://www.scala-sbt.org/).
    Version required is 0.13 or higher.
  * From command line, just execute 'sbt android:compile' to compile the
    application or 'sbt android:run' to run it directly in any connected
    device or emulator.

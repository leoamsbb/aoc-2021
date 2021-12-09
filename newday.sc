#!/usr/bin/env -S scala-cli

// using lib com.lihaoyi::os-lib:0.7.8

import java.lang.IllegalArgumentException
import java.lang.RuntimeException

// Parse and validate input
val validDayFormat = """(0[1-9]|1\d|2[0-5])""".r

val validDayErrorMessage =
  "Day must be between 01 and 25 inclusive with leading 0."

val nameOfDay: String = "day" + (args.toList match {
  case _args if _args.nonEmpty =>
    validDayFormat.findFirstMatchIn(_args.head.toString) match {
      case Some(value) => value.matched
      case None =>
        throw new IllegalArgumentException(
          "Invalid input for day. " + validDayErrorMessage
        )
    }
  case _ =>
    throw new IllegalArgumentException(
      "Program needs a day number. " + validDayErrorMessage
    )
})

// Check if there is a directory with name
val exisitingDirectories = os.list(os.pwd).toSeq.map(_.last)

if (exisitingDirectories.contains(nameOfDay)) {
  throw new IllegalArgumentException(
    s"Invalid name provided. There is already a folder/file with name `${nameOfDay}`"
  )
}

// validate that there is a template folder
val templateDirName = "dayXX_template"
val templateSourceDir = os.pwd / templateDirName
if (!os.list(os.pwd).contains(templateSourceDir)) {
  throw new RuntimeException(
    s"Could not find the template directory `${templateDirName}`. The working directory must be the same as script and template"
  )
}

// Make directory
val destinationDir = os.pwd / nameOfDay
os.makeDir(destinationDir)

// Copy over files
for (sourceFilePath <- os.list.stream(templateSourceDir)) {
  os.copy.into(sourceFilePath, destinationDir)
}

println(s"New folder for ${nameOfDay} created from template! Happy coding :)")

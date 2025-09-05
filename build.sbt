lazy val root = Project(id = "capabilities", base = file("."))
  .settings(moduleName := "root")
  .aggregate(numbers, capture)

lazy val numbers = project
  .settings(moduleName := "number-guessing")

lazy val capture = project
  .settings(moduleName := "capture-checking")


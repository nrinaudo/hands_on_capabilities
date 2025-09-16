lazy val root = Project(id = "capabilities", base = file("."))
  .settings(moduleName := "root")
  .aggregate(numbers, capture, errors)

lazy val numbers = project
  .settings(moduleName := "number-guessing")

lazy val capture = project
  .settings(moduleName := "capture-checking")

lazy val errors = project
  .settings(moduleName := "errors")



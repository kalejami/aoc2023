ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / libraryDependencies ++= Seq("org.typelevel" %% "cats-effect" % "3.5.2")

lazy val root = (project in file(".")).settings(name := "aoc2023")

lazy val day1 = (project in file("modules/day1")).dependsOn(root)
lazy val day2 = (project in file("modules/day2")).dependsOn(root)
lazy val day3 = (project in file("modules/day3")).dependsOn(root)
lazy val day4 = (project in file("modules/day4")).dependsOn(root)
lazy val day5 = (project in file("modules/day5")).dependsOn(root)
lazy val day6 = (project in file("modules/day6")).dependsOn(root)
lazy val day7 = (project in file("modules/day7")).dependsOn(root)
lazy val day8 = (project in file("modules/day8")).dependsOn(root)
lazy val day9 = (project in file("modules/day9")).dependsOn(root)
lazy val day10 = (project in file("modules/day10")).dependsOn(root)
lazy val day11 = (project in file("modules/day11")).dependsOn(root)
lazy val day12 = (project in file("modules/day12")).dependsOn(root)
lazy val day13 = (project in file("modules/day13")).dependsOn(root)
lazy val day14 = (project in file("modules/day14")).dependsOn(root)
lazy val day15 = (project in file("modules/day15")).dependsOn(root)
lazy val day16 = (project in file("modules/day16")).dependsOn(root)
lazy val day17 = (project in file("modules/day17")).dependsOn(root)
lazy val day18 = (project in file("modules/day18")).dependsOn(root)
lazy val day19 = (project in file("modules/day19")).dependsOn(root)
lazy val day20 = (project in file("modules/day20")).dependsOn(root)
lazy val day21 = (project in file("modules/day21")).dependsOn(root)
lazy val day22 = (project in file("modules/day22")).dependsOn(root)
lazy val day23 = (project in file("modules/day23")).dependsOn(root)
lazy val day24 = (project in file("modules/day24")).dependsOn(root)
lazy val day25 = (project in file("modules/day25")).dependsOn(root)

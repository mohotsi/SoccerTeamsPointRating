

object GameRating extends App {
  val data = new MatchResultsMarshall("./src/main/scala/matchesResults.txt")


  val pointsEarnedPerTeam =data.getPointsEarnedPerMatch().sortBy(_.teamName).reverse.sortBy(_.points).reverse



  (0 to pointsEarnedPerTeam.length - 1).foreach { index =>
    val team = pointsEarnedPerTeam(index)
    println(s"""${index + 1}. ${team.teamName.trim}, ${team.points} pts""")
  }



}

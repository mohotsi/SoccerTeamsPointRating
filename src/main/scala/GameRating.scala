import scala.io.Source

case class Score(teamName: String, goals: Int)

case class Game(team: List[Score])
case class Team(name: String)
case class TeamPoints(teamName: String, points: Int)
object GameRating extends App {
  val data = Source.fromFile("./src/main/scala/matchesResults.txt").getLines().toList

  // get all the team name list
  val teamNames = data.flatMap(game => game.split(",")).map(_.replaceAll("([a-zA-Z\\s]+)\\s\\d+", "$1"))
    .distinct

  val games = data.map(game => game.split(",").toList.map { side =>
    def take(variable: String) = side.replaceAll("([a-zA-Z\\s]+)\\s(\\d)+", variable)

    Score(take("$1"), take("$2").toInt)
  }).map(Game)

  val pointsEarnedPerTeam = teamNames.flatMap { team =>
    games.map(game => TeamPoints(team, getGameEarnedPoints(team, game)))
  }.groupBy(_.teamName).map(teamScore => TeamPoints(teamScore._1, teamScore._2.map(_.points).sum)).toList.sortBy(_.points).reverse



  (0 to pointsEarnedPerTeam.length - 1).foreach { index =>
    val team = pointsEarnedPerTeam(index)
    println(s"""${index + 1}. ${team.teamName.trim}, ${team.points} pts""")
  }



  def getGameEarnedPoints(teamName: String, game: Game): Int = {
    if (game.team.indexWhere(_.teamName == teamName) == -1) {
      0
    }
    else {
      val teamsScores = game.team.partition(_.teamName == teamName)
      val teamScore = teamsScores._1.head.goals
      val opponentScore = teamsScores._2.head.goals
      if (teamScore == opponentScore) {
        1
      }
      else if (teamScore > opponentScore) {
        3
      }
      else { //teamScore < opponentScore
        0
      }

    }
  }
}

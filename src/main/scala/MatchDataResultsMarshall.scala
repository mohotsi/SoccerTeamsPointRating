

import scala.io.Source

case class Score(teamName: String, goals: Int)

case class Game(teams: List[Score])
case class Team(name: String)
case class TeamPoints(teamName: String, points: Int)
class MatchResultsMarshall(fileUrl:String) {
  val data = Source.fromFile("./src/main/scala/matchesResults.txt").getLines().toList
def getTeamNames()={
  data.flatMap(game => game.split(",")).map(_.replaceAll("([a-zA-Z\\s]+)\\s\\d+", "$1"))
    .distinct
}
def getGameMatches()={
  data.map(game => game.split(",").toList.map { side =>
    def take(variable: String) = side.replaceAll("([a-zA-Z\\s]+)\\s(\\d)+", variable)

    Score(take("$1"), take("$2").toInt)
  }).map(Game)
}
  def getPointsEarnedPerMatch()={
    getTeamNames().flatMap { team =>
      getGameMatches().map(game => TeamPoints(team, getPointsEarnedInGameMatch(team, game)))
    }.groupBy(_.teamName).map(teamScore => TeamPoints(teamScore._1, teamScore._2.map(_.points).sum)).toList.sortBy(_.points).reverse
  }

  def PointsEarnedPerTeam():List[TeamPoints]={
    getTeamNames().flatMap { team =>
      getGameMatches().map(game => TeamPoints(team, getPointsEarnedInGameMatch(team, game)))
    }.groupBy(_.teamName).map(teamScore => TeamPoints(teamScore._1, teamScore._2.map(_.points).sum)).toList
  }

  def getPointsEarnedInGameMatch(teamName: String, game: Game): Int = {
    if (game.teams.indexWhere(_.teamName == teamName) == -1) {
      0
    }
    else {
      val teamsScores = game.teams.partition(_.teamName == teamName)
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

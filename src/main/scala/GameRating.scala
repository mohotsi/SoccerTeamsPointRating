import scala.io.Source
case class Score(teamName:String, goals:Int)
case class Match(team: List[Score])
case class TeamPoints(teamName:String, points:Int)
object GameRating extends App {
  case class Team(name:String)

val data=Source.fromFile("./src/main/scala/matchesResults.txt").getLines().toList

val teams=data.flatMap(game=>game.split(",")).map(_.replaceAll("([a-zA-Z\\s]+)\\s\\d+","$1"))
  .distinct

val matches=data.map(game=>game.split(",").toList.map{side=>
  def take(variable:String)=side.replaceAll("([a-zA-Z\\s]+)\\s(\\d)+",variable)
  Score(take("$1"),take("$2").toInt)
}).map(Match)
val teamsPoints=teams.flatMap { team =>
  matches.map(game => TeamPoints(team,getGamePoint(team,game)))
}.groupBy(_.teamName).map(teamScore=>TeamPoints(teamScore._1,teamScore._2.map(_.points).sum)).toList.sortBy(_.points).reverse

  (0 to teamsPoints.length-1).foreach { index =>
    val team =teamsPoints(index)
      println(s"""${index + 1}. ${team.teamName.trim}, ${team.points} pts""")
  }


def  getGamePoint(teamName:String, game:Match): Int = {
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

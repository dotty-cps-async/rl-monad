package cps.rl.tournament

/**
 * Result of a single match between two agents.
 */
case class MatchResult(
  agent1Name: String,
  agent2Name: String,
  agent1Wins: Int,
  agent2Wins: Int,
  draws: Int
) {
  def totalGames: Int = agent1Wins + agent2Wins + draws
}

/**
 * Aggregated statistics for one agent across all matches.
 */
case class AgentStats(
  wins: Int = 0,
  losses: Int = 0,
  draws: Int = 0
) {
  def games: Int = wins + losses + draws
  def winRate: Float = if (games > 0) wins.toFloat / games else 0f
  def points: Float = wins + draws * 0.5f
}

/**
 * Match execution mode.
 */
enum MatchMode {
  case Evaluate  // Inference only, frozen models
  case Train     // Models can learn during match
}

/**
 * Tournament configuration.
 *
 * @tparam A Agent descriptor type (game-specific)
 * @param agents List of agent descriptors to compete
 * @param gamesPerMatch Number of games per matchup
 * @param matchMode Whether to evaluate or train during matches
 * @param alternateFirstPlayer Whether to alternate who goes first
 */
case class TournamentConfig[A](
  agents: List[A],
  gamesPerMatch: Int = 10,
  matchMode: MatchMode = MatchMode.Evaluate,
  alternateFirstPlayer: Boolean = true
)

/**
 * Tournament result with standings and match details.
 */
case class TournamentResult(
  standings: Map[String, AgentStats],
  matchResults: List[MatchResult],
  totalGames: Int
) {
  /**
   * Print standings sorted by points (wins + 0.5*draws).
   */
  def printStandings(): Unit = {
    val sorted = standings.toList.sortBy(-_._2.points)
    println("=== Tournament Standings ===")
    sorted.zipWithIndex.foreach { case ((name, stats), rank) =>
      println(f"${rank + 1}. $name: W=${stats.wins} L=${stats.losses} D=${stats.draws} " +
        f"(${stats.winRate * 100}%.1f%% win rate, ${stats.points}%.1f pts)")
    }
    println(s"Total games played: $totalGames")
  }

  /**
   * Print detailed match results.
   */
  def printMatchResults(): Unit = {
    println("=== Match Results ===")
    matchResults.foreach { m =>
      println(s"${m.agent1Name} vs ${m.agent2Name}: " +
        s"${m.agent1Wins}-${m.agent2Wins}-${m.draws} (W-L-D)")
    }
  }
}

/**
 * Type class for running matches between agents.
 * Implement this for each game type.
 *
 * @tparam A Agent descriptor type
 */
trait TournamentMatchRunner[A] {
  /**
   * Get the display name for an agent.
   */
  def agentName(agent: A): String

  /**
   * Play a match between two agents.
   *
   * @param agent1 First agent descriptor
   * @param agent2 Second agent descriptor
   * @param games Number of games to play
   * @param mode Whether to evaluate or train
   * @param alternateFirst Whether to alternate who goes first each game
   * @return Match result with win/loss/draw counts
   */
  def playMatch(agent1: A, agent2: A, games: Int, mode: MatchMode, alternateFirst: Boolean): MatchResult
}

/**
 * Generic round-robin tournament runner.
 * Uses type class pattern to remain game-agnostic.
 *
 * @tparam A Agent descriptor type (game-specific)
 * @param config Tournament configuration
 * @param runner Game-specific match runner (provided as given/implicit)
 */
class Tournament[A](config: TournamentConfig[A])(using runner: TournamentMatchRunner[A]) {

  /**
   * Run the tournament and return results.
   */
  def run(): TournamentResult = {
    val agentNames = config.agents.map(runner.agentName)
    println(s"Starting tournament with ${config.agents.length} agents: ${agentNames.mkString(", ")}")
    println(s"Games per match: ${config.gamesPerMatch}, Mode: ${config.matchMode}")
    println()

    // Generate all pairs for round-robin
    val pairs = for {
      i <- config.agents.indices
      j <- (i + 1) until config.agents.length
    } yield (config.agents(i), config.agents(j))

    // Play all matches
    val matchResults = pairs.zipWithIndex.map { case ((a1, a2), idx) =>
      val name1 = runner.agentName(a1)
      val name2 = runner.agentName(a2)
      println(s"Match ${idx + 1}/${pairs.length}: $name1 vs $name2")

      val result = runner.playMatch(a1, a2, config.gamesPerMatch, config.matchMode, config.alternateFirstPlayer)

      println(s"  Result: ${result.agent1Wins}-${result.agent2Wins}-${result.draws} (W-L-D)")
      result
    }.toList

    val standings = aggregateStats(agentNames.toList, matchResults)
    val totalGames = matchResults.map(_.totalGames).sum

    println()
    TournamentResult(standings, matchResults, totalGames)
  }

  /**
   * Aggregate individual match results into per-agent statistics.
   */
  private def aggregateStats(names: List[String], results: List[MatchResult]): Map[String, AgentStats] = {
    names.map { name =>
      val stats = results.foldLeft(AgentStats()) { (acc, m) =>
        if (m.agent1Name == name) {
          acc.copy(
            wins = acc.wins + m.agent1Wins,
            losses = acc.losses + m.agent2Wins,
            draws = acc.draws + m.draws
          )
        } else if (m.agent2Name == name) {
          acc.copy(
            wins = acc.wins + m.agent2Wins,
            losses = acc.losses + m.agent1Wins,
            draws = acc.draws + m.draws
          )
        } else {
          acc
        }
      }
      name -> stats
    }.toMap
  }
}

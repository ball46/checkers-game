import cats.effect.testing.scalatest.AsyncIOSpec
import checkers.models.*
import checkers.services.{GameService, GameServiceImpl}
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

class GameServiceSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  def createGameService(): GameService = new GameServiceImpl()

  test("createGame should create a new game") {
    val gameService = createGameService()
    val result = gameService.createGame("TestGame", singlePlayer = false)
    result.map {
      case Right(gameResponse) =>
        gameResponse.board.pieces.flatten should not be empty
      case Left(_) => fail("Game creation failed")
    }
  }

  test("getListGame should return a list of games") {
    val gameService = createGameService()
    val createResult = gameService.createGame("TestGame", singlePlayer = false)
    createResult.flatMap {
      case Right(_) =>
        val result = gameService.getListGame
        result.map { games =>
          games should not be empty
        }
      case Left(_) => fail("Game creation failed")
    }
  }

  test("findGameById should return the correct game") {
    val gameService = createGameService()
    val createResult = gameService.createGame("TestGame", singlePlayer = false)
    createResult.flatMap {
      case Right(gameResponse) =>
        val findResult = gameService.findGameById(gameResponse.id)
        findResult.map {
          case Some(foundGame) => foundGame.id shouldBe gameResponse.id
          case None => fail("Game not found")
        }
      case Left(_) => fail("Game creation failed")
    }
  }

  test("deleteGame should delete the game") {
    val gameService = createGameService()
    val createResult = gameService.createGame("TestGame", singlePlayer = false)
    createResult.flatMap {
      case Right(gameResponse) =>
        val deleteResult = gameService.deleteGame(gameResponse.id)
        deleteResult.flatMap {
          case Right(_) =>
            val findResult = gameService.findGameById(gameResponse.id)
            findResult.map {
              case Some(_) => fail("Game was not deleted")
              case None => succeed
            }
          case Left(_) => fail("Game deletion failed")
        }
      case Left(_) => fail("Game creation failed")
    }
  }
}
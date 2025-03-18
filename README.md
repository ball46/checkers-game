# Checkers Game Backend

A functional programming implementation of the Checkers game logic built with Scala.

## Overview

This project provides a complete backend implementation of the classic Checkers board game. It handles all game rules including piece movement, captures, king promotion, and win conditions using a functional, immutable approach.

## Features

* **Pure Functional Design**: Immutable data structures and pure functions for predictable behavior
* **Complete Game Rules**: Standard checkers rules including mandatory captures and multi-jumps
* **Game State Management**: Track game status, player turns, and win conditions
* **Direction-based Movement Logic**: Clean separation of movement rules by direction and piece type
* **API Ready**: Core logic abstracted for easy integration with any frontend

## Tech Stack

* **Scala**: Functional programming language on the JVM
* **SBT**: Scala build tool for dependency management and compilation
* **ScalaTest**: Testing framework for comprehensive test coverage

## Architecture

The application follows functional programming principles with clear separation of concerns:

### Core Components

* **Game**: Main class representing a game state with players and board
* **Board**: Immutable representation of the game board and piece positions
* **DirectionLogic**: Handles movement rules in different directions
* **MoveLogic**: Validates move legality and performs piece movements
* **GameStateLogic**: Manages game state transitions and victory conditions

### Key Design Principles

* **Immutability**: All state changes produce new instances rather than modifying existing ones
* **Pure Functions**: Functions depend only on their inputs, not external state
* **Tail Recursion**: Efficient recursive algorithms for searching possible moves
* **Option/Either Types**: Robust error handling without exceptions

## Project Structure

```
src/
├── main/
│   └── scala/
│       └── checkers/
│           ├── models/         # Core domain models
│           │   ├── Board.scala       # Board representation and movement
│           │   ├── Game.scala        # Game state and rules
│           │   └── Position.scala    # Board position coordinates
│           └── utils/          # Utility functions
└── test/
    └── scala/
        └── checkers/
            └── LogicGameSpec.scala   # Game logic tests
```

## Game Mechanics

1. **Board Representation**: 8x8 grid with pieces on dark squares
2. **Piece Movement**: Diagonal movement with forward-only for regular pieces, any diagonal for kings
3. **Captures**: Jump over opponent pieces to capture them
4. **Multi-jumps**: Continue capturing with the same piece when possible
5. **King Promotion**: Pieces reaching the opposite end become kings
6. **Game End**: Game ends when a player has no valid moves or pieces

## Getting Started

1. Clone the repository
2. Install SBT if not already installed:
   ```
   brew install sbt    # macOS
   apt install sbt     # Ubuntu/Debian
   ```
3. Compile the project:
   ```
   sbt compile
   ```
4. Run tests:
   ```
   sbt test
   ```

## Usage

```scala
// Create a new game
val game = Game.initialTuneBasePlayer("MyGame")

// Make a move
val moveResult = game.makeMove(Move(Position(0, 5), Position(1, 4)))

// Get valid moves for current player
val validMoves = game.getValidMovesForPlayer(White)
```

## Related Repositories

* **Frontend**: A web frontend that consumes this backend API is available for a complete Checkers gaming experience
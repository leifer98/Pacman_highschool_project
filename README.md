# Pacman High School Project

please note I did this project when i was 17 years old, apologies in advance.

## Introduction
Pacman is originally an arcade game first released in Japan on May 22, 1980. The game is designed for a single player who competes against the computer at the chosen difficulty level. The game requires real-time reaction and forward-thinking strategy.

![image](https://github.com/leifer98/Pacman_highschool_project/assets/24610228/46db73e4-6f91-4d8a-b0aa-f213488d1d39)

*Description: The initial positioning of Pacman and ghosts.*

In this project, Pacman is controlled by the player and appears as a yellow circle with a mouth. The ghosts are controlled by the computer and appear as green ghosts. The objective of the game is to navigate Pacman through a maze while eating all the pills or ghosts using the power pill.

## Maze Structure
The maze has areas marked in black where characters cannot pass. There are holes in the middle of the map on the left and right sides allowing passage to the opposite side of the map.

## Game Mechanics
- **Initial State**: Pacman starts below the central box and ghosts above it.
- **Controls**: 
  - W / Up Arrow - Move Up
  - D / Right Arrow - Move Right
  - S / Left Arrow - Move Left
  - X / Down Arrow - Move Down
- **Objective**: Eat all regular pills or all ghosts using the power pill.

## End of Game
- **Victory for Computer**: Occurs when ghosts eat Pacman, displaying a "Game Over" screen.
- **Victory for Pacman**: Occurs when Pacman eats all the ghosts or all the regular pills, displaying a "Good Job" screen.

![image](https://github.com/leifer98/Pacman_highschool_project/assets/24610228/06ff0885-1d1b-4b54-90a4-22a82a0aa8ce)

*Description: Screenshot of the game over screen.*

![image](https://github.com/leifer98/Pacman_highschool_project/assets/24610228/329e24e4-fdbe-46d9-b18e-42e1ac65d318)


*Description: Screenshot of the victory screen.*

## Target Audience
The game is intended for all users. While the rules are simple, once understood, the game is engaging and interesting for everyone.

## Usage and Running the System
1. **Load the File**: Load the file `pacman_v8.pl`.
2. **Start the Game**: Type `start` and press ENTER.
3. **Difficulty Selection**: Choose the desired difficulty level or exit the game.
4. **Gameplay**: Ensure the mouse is over the window to control Pacman.

## Suggestions for Improvement
- **Graphics**: Create graphics that adapt to the screen size.
- **Enhancements**:
  - Different maps/mazes
  - More difficulty levels
  - More types of points (e.g., cherries from the original game)
  - Add Ms. Pacman
  - Add background animation to the start screen
  - Improve the victory and defeat screens

## Running the Code
To run the project, you need to set up a Prolog environment. Follow the steps below to install and run the game:

1. **Install SWI-Prolog**:
   - Download and install SWI-Prolog from [SWI-Prolog website](https://www.swi-prolog.org/Download.html).

2. **Load the Project**:
   - Open SWI-Prolog.
   - Load the `pacman_v8.pl` file by using the command:
     ```prolog
     ?- [pacman_v8].
     ```

3. **Start the Game**:
   - Run the start command:
     ```prolog
     ?- start.
     ```
   - Choose the difficulty level and start playing.

*



For detailed explanation and further instructions, refer to the project documentation.

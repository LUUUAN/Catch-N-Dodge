# Running Instruction
``
stack install CnD --local-bin-path ""
``


``
./CnD
``

##### Project Description

In this project, our group intends to use **Haskell** and **brick** library to achieve a terminal game. The goal of this game is to allow the character to collect as much bricks as possible. There will be bricks falling from the top of the terminal with two colors, the blue one and the red one. If the character manipulated by users moving at the bottom of the terminal screen catch the blue one, the user will get a point. However, if the player catch a red one. The game will finish.

##### Rule of the game

There are two types of bricks falling from the top of the terminal screen, a red one and a blue one. User can press <- or -> to move the character at the bottom of the terminal screen to move left or right. When user catch a blue brick, he will receive one point and when he catch a red one, the game will finish and the final score will be count. To increase the difficulty of the game, bricks are falling with different speed.

##### Game Control

The player can only use the "<-" and "->" to manipulate the character to move left or right.

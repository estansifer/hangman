# hangman

Timed hangman against a computer opponent that "cheats".

Your score for each game is 8 - (number of wrong guesses),
with your total score being added over as many hangman
games you play in 3 minutes. The computer will always use
words of length 8 or longer, and gives you a correctly
guessed letter at the beginning. The computer is free to
change the word it is thinking of in response to your
guesses.

There is no penalty for repeating letters you have already
guessed.

To compile (requires Haskell, specifically GHC):

    bash make.sh

To run:

    ./bin/Hangman [path-to-dictionary]

If no command line argument is given, it uses the provided
dictionary.

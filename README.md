# Markov-Chat
While working as a teaching assistant for Functional Languages & AI (CSCI-2322), I prototyped a Markov chain text generator for a new project in the next semester. This is that model, along with some additional scripts to change its corpus.

The models folder contains all four Markov models:
**MarkovL1:** A one-dimensional Markov chain, meaning that its key consists of a single word<br>
**MarkovL2:** A two-dimensional Markov chain, meaning that its key consists of two words<br>
**MarkovLN:** An N-dimensional Markov chain, meaning that its key consists of N words<br>
**MarkovLNChar:** An N-dimensional Markov chain, whose key consists of N characters

For logistical purposes, only LN and LNChar are used in the actual program.

input.txt contains the corpus, which by default consists of generic fortunes.

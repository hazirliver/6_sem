# INTERVAL [0;T] == [0;10]
# STEP == 0.05
# NUMBER n = 160
# EXPECTED VALUE m(t) = 1 + exp(-t)
# covar func = 2*exp(-abs(tau)/2)


###1###
Tt <- 0.1
Hh <- 0.05
n <- 160

Mm <- function(t) {1 + exp(-t)}
covar <- function(t) {2*exp(-abs(t)/2)}


_dim <- 
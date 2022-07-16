## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5, 
  fig.height = 3,
  fig.align='center',
  global.par = TRUE
)

## -----------------------------------------------------------------------------
# Method to print part of uniroot output
print.list <- function(x, digits = max(3L, getOption("digits") - 3L)) {
  names(x)[1:3] <- c("root", "f(root)", "iterations")
  print.default(format(x[1:3], digits = digits), print.gap = 2L, quote = FALSE)
}

## ----setup--------------------------------------------------------------------
library(itp)

## -----------------------------------------------------------------------------
# Lambert
lambert <- function(x) x * exp(x) - 1
itp(lambert, c(-1, 1))
uniroot(lambert, c(-1, 1), tol = 1e-10)

## ---- echo = FALSE------------------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
curve(lambert, -1, 1, main = "Lambert")
abline(h = 0, lty = 2)
abline(v = itp(lambert, c(-1, 1))$root, lty = 2)
par(oldpar)

## -----------------------------------------------------------------------------
# Trigonometric 1
trig1 <- function(x) tan(x - 1 /10)
itp(trig1, c(-1, 1))
uniroot(trig1, c(-1, 1), tol = 1e-10)

## ---- echo = FALSE------------------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
curve(trig1, -1, 1, main = "Trigonometric 1")
abline(h = 0, lty = 2)
abline(v = itp(trig1, c(-1, 1))$root, lty = 2)
par(oldpar)

## -----------------------------------------------------------------------------
# Polynomial 3
poly3 <- function(x) (x * 1e6 - 1) ^ 3
itp(poly3, c(-1, 1))
# Using n0 = 0 leads to (slightly) fewer iterations, in this example
poly3 <- function(x) (x * 1e6 - 1) ^ 3
itp(poly3, c(-1, 1), n0 = 0)
uniroot(poly3, c(-1, 1), tol = 1e-10)

## ---- echo = FALSE------------------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
curve(poly3, -1, 1, main = "Polynomial 3")
abline(h = 0, lty = 2)
abline(v = itp(poly3, c(-1, 1))$root, lty = 2)
par(oldpar)

## -----------------------------------------------------------------------------
# Staircase
staircase <- function(x) ceiling(10 * x - 1) + 1 / 2
itp(staircase, c(-1, 1))
uniroot(staircase, c(-1, 1), tol = 1e-10)

## ---- echo = FALSE------------------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
curve(staircase, -1, 1, main = "Staircase", n = 10000)
abline(h = 0, lty = 2)
abline(v = itp(staircase, c(-1, 1))$root, lty = 2)
par(oldpar)

## -----------------------------------------------------------------------------
# Warsaw
warsaw <- function(x) ifelse(x > -1, sin(1 / (x + 1)), -1)
# Function increasing over the interval
itp(warsaw, c(-1, 1))
uniroot(warsaw, c(-1, 1), tol = 1e-10)
# Function decreasing over the interval
itp(warsaw, c(-0.85, -0.8))
uniroot(warsaw, c(-0.85, -0.8), tol = 1e-10)

## ---- echo = FALSE------------------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
curve(warsaw, -1, 1, main = "Warsaw", n = 1000)
abline(h = 0, lty = 2)
abline(v = itp(warsaw, c(-1, 1))$root, lty = 2)
abline(v = itp(warsaw, c(-0.85, -0.8))$root, lty = 2)
par(oldpar)

## ----lambert_root_Cpp---------------------------------------------------------
# Lambert, using an external pointer to a C++ function
lambert_ptr <- xptr_create("lambert")
res <- itp(lambert_ptr, c(-1, 1))
res

## ----lambert_root_itp_c-------------------------------------------------------
# Calling itp_c()
res <- itp_c(lambert_ptr, pars = list(), a = -1, b = 1)
res

## ----plot_itp, fig.show='hold'------------------------------------------------
oldpar <- par(mar = c(4, 4, 1, 1))
plot(res, main = "Lambert")
par(oldpar)


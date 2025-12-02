#' Binomial log-likelihood and graphical MLE
#'
#' These functions provide a simple example of maximum likelihood estimation
#' for binomial data. `logbin2()` computes the log-likelihood for two binomial
#' experiments with the same probability of success. `mymaxlikg()` evaluates
#' a log-likelihood function over a grid of parameter values, plots the
#' likelihood curve, and returns the MLE.
#'
#' @param theta Numeric probability of success (between 0 and 1).
#' @param lfun A function that computes the log-likelihood at a given parameter value.
#' @return For `logbin2()`, a numeric log-likelihood value.
#' For `mymaxlikg()`, the numeric value of the parameter that maximizes the likelihood.
#' @export
#' @examples
#' logbin2(0.5)
#' mymaxlikg(lfun = logbin2, theta = seq(0,1,length=1000))
#' @rdname binomialMLE
logbin2 <- function(theta) {
  log(dbinom(1, prob = theta, size = 2)) +
    log(dbinom(7, prob = theta, size = 8))
}

#' @export
#' @rdname binomialMLE
mymaxlikg <- function(lfun = "logbin2", theta) {
  nth <- length(theta)  # nu. of valuse used in theta
  thmat <- matrix(theta, nr = nth, nc = 1, byrow = TRUE) # Matrix of theta
  z <- apply(thmat, 1, lfun) # z holds the log lik values
  zmax <- max(which(z == max(z)))  # finding the INDEX of the max lik
  plot(theta, exp(z), type = "l") # plot of lik
  abline(v = theta[zmax], col = "Blue")   #  verical line through max
  axis(3, theta[zmax], round(theta[zmax], 4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}

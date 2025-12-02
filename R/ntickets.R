#' ntickets
#'
#' @param N integer - number of available seats on flight
#' @param gamma numeric - probability of overbooking
#' @param p numeric - probability that passenger shows up
#'
#' @returns a named list containing:
#' \describe{
#'   \item{nd}{maximum ticket number using discrete binomial distribution}
#'   \item{nc}{maximum ticket number using normal approximation}
#'   \item{N}{number of available seats on flight}
#'   \item{p}{probability that passenger shows up}
#'   \item{gamma}{overbooking probability}
#' }
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)

ntickets <- function(N, gamma, p) {
  #function calculates the numbers of tickets to sell

  #define objective function using discrete (binomial) distribution
  objective_discrete <- function(n) {
    1 - gamma - pbinom(N, size = n, prob = p)
  }

  #define objective function using normal approximation
  objective_continuous <- function(n) {
    mu <- n * p  #mean number of passengers who show up
    sigma <- sqrt(n * p * (1 - p))  #standard deviation
    1 - gamma - pnorm(N, mean = mu, sd = sigma)
  }

  #find maximum number of tickets (nd) using discrete method
  nd <- NA  # initialize result
  for (n in N:(N + 300)) {
    #probability of overbooking exceeds gamma
    if (objective_discrete(n) > 0) {
      nd <- n - 1
      break
    }
  }

  #find maximum number of tickets (nc) using normal approximation
  nc <- NA  #initialize result
  for (n in N:(N + 300)) {
    if (objective_continuous(n) > 0) {
      nc <- n - 1
      break
    }
  }

  #plotting
  plot_enabled <- getOption("ntickets.plot", TRUE)
  if (plot_enabled) {
    #plot objective functions for both methods
    n_vals <- seq(N, N + 50)  #range of tickets
    obj_discrete_vals <- sapply(n_vals, objective_discrete)  #discrete values
    obj_continuous_vals <- sapply(n_vals, objective_continuous)  #normal approx values

    #create plots
    par(mfrow = c(2, 1), mar = c(5, 5, 4, 2))

    #discrete plot
    plot(n_vals, obj_discrete_vals, type = "n",
         ylab = "Objective", xlab = "Number of Tickets Sold",
         main = paste("Objective Vs n to find optimal tickets sold (", nd, ") gamma=", gamma, " N=", N, " discrete", sep = ""))

    #draw short lines between each dot
    for (i in 1:(length(n_vals) - 1)) {
      segments(n_vals[i], obj_discrete_vals[i], n_vals[i + 1], obj_discrete_vals[i + 1], col = "black", lwd = 2)
    }

    #draw dots
    points(n_vals, obj_discrete_vals, col = "blue", pch = 16)

    #solid horizontal line at y = 0
    abline(h = 0, col = "red", lty = 1, lwd = 1)

    #solid vertical line at optimal ticket number
    abline(v = nd, col = "red", lwd = 1)

    #continuous plot
    plot(n_vals, obj_continuous_vals, type = "l", col = "black", lwd = 2,
         ylab = "Objective", xlab = "Number of Tickets Sold",
         main = paste("Objective Vs n to find optimal tickets sold (", round(nc, 9), ") gamma=", gamma, " N=", N, " continuous", sep = ""))

    #solid horizontal line at y = 0
    abline(h = 0, col = "blue", lty = 1, lwd = 1)

    #solid vertical line at optimal ticket number
    abline(v = nc, col = "blue", lwd = 1)
  }

  # return results
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

ntickets(N = 400, gamma = 0.02, p = 0.95)



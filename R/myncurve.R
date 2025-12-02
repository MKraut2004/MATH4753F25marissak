if(getRversion() >= "2.15.1")  utils::globalVariables(c("x"))

#' Plot and shade normal curve with area
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a value up to which area is calculated
#'
#' @returns a list with mu, sigma, a, and P(X ≤ a)
#' @export
#'
#' @examples
#' myncurve(mu = 100, sigma = 15, a = 120)
#'
#' @importFrom grDevices rainbow rgb
#' @importFrom graphics abline curve points polygon segments
#' @importFrom stats dnorm pbinom pnorm
#' @importFrom utils tail

myncurve <- function(mu, sigma, a) {
  #plot normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        col = "blue",
        ylab = "Density",
        main = paste("Normal Curve with mu =", mu, "sigma =", sigma))

  #shade area from -∞ to a
  x_fill <- seq(mu - 3 * sigma, a, length.out = 500)
  y_fill <- dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill[1], x_fill, tail(x_fill, 1)),
          c(0, y_fill, 0),
          col = rgb(0.2, 0.6, 0.8, 0.4),
          border = NA)

  #calculate probability
  prob <- pnorm(a, mean = mu, sd = sigma)

  #return results
  return(list(mu = mu, sigma = sigma, a = a, P_X_le_a = prob))
}


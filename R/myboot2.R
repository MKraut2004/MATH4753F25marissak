#' Bootstrap confidence intervals
#'
#' @param iter Number of bootstrap iterations (default 10000).
#' @param x Numeric vector of data.
#' @param fun Function to apply (e.g., mean, median, var).
#' @param alpha Significance level (default 0.05 for 95% CI).
#' @param ... Additional plotting arguments passed to hist()
#'
#' @returns A list with:
#'   \item{fun}{The function used}
#'   \item{t0}{Point estimate from the original sample}
#'   \item{ci}{Bootstrap confidence interval}
#'   \item{xstat}{Vector of bootstrap statistics}
#' @export
#'
#' @examples
#' set.seed(68)
#' sam <- rnorm(20, mean = 10, sd = 4)
#' myboot2(sam, fun = mean, iter = 10000, alpha = 0.05)

myboot2 <- function(x, fun = "mean", iter = 10000, alpha = 0.05, ...){
  fun <- match.fun(fun)
  n = length(x) #sample size
  y = sample(x, n*iter, replace = TRUE)
  rs.mat = matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun) #vector that has iter vals
  ci = quantile(xstat, c(alpha/2, 1 - alpha/2))

  #histogram
  para = hist(xstat, freq = FALSE, las = 1, main = "Histogram of Bootstrap sample statistics",...)

  mat = matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  #mat will be a matrix that contains the data, this is done so   that I can use apply()

  pte = apply(mat, 2, fun) #pte - point estimate

  abline(v = pte, lwd = 3, col = "Black") #vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4) #create segment for ci
  text(ci[1], 0, paste("(",round(ci[1], 2), sep = ""), col = "Red",
       cex = 3)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""),
       col = "Red", cex = 3)
  text(pte, max(para$density)/2, round(pte, 2), cex = 3)

  return(list(fun = fun, pte = pte, ci = ci, xstat = xstat))
}

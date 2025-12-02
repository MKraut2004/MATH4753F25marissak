#' Birthday function
#'
#' @param x numeric vector of the number of people in a group
#'
#' @return numeric probability of at least two people sharing a birthday
#' @export
#'
#' @examples
#' Birthday(20:24)
Birthday <- function(x){
  1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
}

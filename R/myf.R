#' Visualize repeated sampling from a uniform distribution
#'
#' @param n integer - number of samples per iteration
#' @param iter integer - number of iterations to run
#' @param time numeric - pause time between plots in seconds
#'
#' @returns no return value - function is called for its side effect: generating barplots
#' @export
#'
#' @examples
#' myf(n = 1000, iter = 10, time = 0.5)
myf=function(n, iter = 10, time = 0.5){
  for(i in 1:iter){
    #make a sample
    s=sample(1:10, n, replace = TRUE)

    #turn the sample into a factor
    sf=factor(s,levels = 1:10)

    #make a barplot
    barplot(table(sf) / n, beside = TRUE, col = rainbow(10),
            main = paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim = c(0, 0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}

#' One Way Table Analysis
#'
#' @param data Vector of counts of interest from the data table
#' @param catagories Vector of the names of the categories of the counts
#' @param alpha The alpha value to make the desired \eqn{100(1-alpha)\%} CI
#'
#' @importFrom graphics abline arrows barplot par
#' @importFrom stats chisq.test fisher.test qnorm xtabs
#'
#' @description This function takes the data from a one way table, makes a plot of the proportions with CIs,
#'    and runs a chi squared test of uniformity.
#'
#' @return The named list of the results of the chi squared test of uniformity.
#'
#' @export
#'
oneway <- function(data, catagories,alpha=.05){
  n <- sum(data)
  p <- data/n #Turns counts into proportions
  q <- 1-p
  var <-(p*q)/n
  z <- qnorm(1 - alpha/2)
  lowerbound <- p-z*sqrt(var)#Calculating the lower bound
  upperbound <- p+z*sqrt(var)#Calculating the upper bound
  par(mfrow=c(1,1))
  plot<-barplot(p, names.arg=catagories,density=p*n,ylim=c(0,max(upperbound)),main="Plot of Proportions")
  mid <-(plot)
  arrows(mid,p,mid,upperbound, length = .25, angle=90)
  arrows(mid,p,mid,lowerbound, length = .25, angle=90)
  abline(h=mean(p))
  chi<-chisq.test(data)
  list(chisq=c(chi))
}


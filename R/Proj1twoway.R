#' Two Way Table Analysis
#'
#' @param data List of complete 2-way data set with each value being associated with its specific factors.
#' @param nums Two way table of counts by both factors
#' @param cat1 One way table of counts by first factor
#' @param cat2 One way table of counts by second factor
#' @param alpha The alpha value to make the desired \eqn{100(1-alpha)\%} CI
#' @importFrom graphics abline arrows barplot par
#' @importFrom stats chisq.test fisher.test qnorm xtabs
#'
#' @description This function takes the data from a two way table, makes plots of the proportions of themarginal
#'    distributions of each factor with CIs, makes a plot of the overall distribution with CIs,
#'    and runs a chi squared test of independence of factors.
#'
#' @return The named list of the results of the chi squared test of independence.
#'
#' @export
#'
twoway <- function(data,nums,cat1,cat2,alpha=.1){
  n <-sum(nums)
  c1<-xtabs(nums~cat1)
  c2<-xtabs(nums~cat2)
  pc1 <-c1/n
  pc2 <-c2/n
  p <-nums/n
  z <- qnorm(1 - alpha/2)

  par(mfrow=c(1,3))
#Make plot of one way analysis of the first factor
  l1 <- pc1-z*sqrt((pc1*(1-pc1))/n)#Calculating the lower bound
  u1 <- pc1+z*sqrt((pc1*(1-pc1))/n)#Calculating the upper bound
  plot1<-barplot(pc1,ylim=c(0,max(u1)), main="One Way Plots of Proportions for Factor 1")
  mid1 <-(plot1)
  arrows(mid1,pc1,mid1,u1, length = .25, angle=90)
  arrows(mid1,pc1,mid1,l1, length = .25, angle=90)
  abline(h=mean(pc1))


  #Make plot of one way analysis of the second factor
  l2 <- pc2-z*sqrt((pc2*(1-pc2))/n)#Calculating the lower bound
  u2 <- pc2+z*sqrt((pc2*(1-pc2))/n)#Calculating the upper bound
  plot2<-barplot(pc2,ylim=c(0,max(u2)),main="One Way Plot of Proportions for Factor 2")
  mid2 <-(plot2)
  arrows(mid2,pc2,mid2,u2, length = .25, angle=90)
  arrows(mid2,pc2,mid2,l2, length = .25, angle=90)
  abline(h=mean(pc2))

  l <- p-z*sqrt((p*(1-p))/n)#Calculating the lower bound
  u <- p+z*sqrt((p*(1-p))/n)#Calculating the upper bound
  plot<-barplot(p,names.arg=c("1,1","1,2","2,1","2,2"),ylim=c(0,max(u)),beside=TRUE,main="Two Way Plot of Proportions")
  mid <-(plot)
  arrows(mid,p,mid,u, length = .25, angle=90)
  arrows(mid,p,mid,l, length = .25, angle=90)
  abline(h=mean(p))

  chi<-chisq.test(nums)
  list(chisq=c(chi))
}


#' Fishers Exact Test
#'
#' @param data Two way table of counts of interest
#' @importFrom graphics abline arrows barplot par
#' @importFrom stats chisq.test fisher.test qnorm xtabs
#'
#' @description The null hypothesis of independence leads to the cells
#'    of the table being distributed according to the hypergeometric distribution.
#'
#' @return The named list of the results of the Fisher's Exact Test.
#'
#' @export
#'
fishies <- function(data){
  par(mfrow=c(1,2))
  n <- sum(data)
  p <- data/n #Turns counts into proportions
  plot1<-barplot(p,beside=FALSE,main="Two Way Plot of Proportions", legend.text=TRUE)
  mid1 <-(plot1)
  abline(h=mean(p))
  plot2<-barplot(p,beside=TRUE,main="Two Way Plot of Proportions", legend.text=TRUE)
  mid2 <-(plot2)
  abline(h=mean(p))
  fish<-fisher.test(data)
  list(fishers<-c(fish))

}

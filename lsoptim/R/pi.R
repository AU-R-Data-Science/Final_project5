#' Pi function
#' @description Define the probability of success or failure
#' @param m numerical data set to be fitted to a logistic regression curve
#'
#' @return logistic regression curve
#' @export roxygen2::roxygenize
#'
#' @examples Pi(data)
Pi <- function(m){
  Pi_result<-1/(1+exp(-m))
  return(Pi_result)
}


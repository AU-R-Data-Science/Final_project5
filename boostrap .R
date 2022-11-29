
#boostrap confidence interval 
Boostrap <- function(x, alpha =0.05,R =1000, seed = NULL, ...) {
  #Calculate the mean
  x <- x[!is.na(x)]
  estimate <- mean(x)
  n <- length(x)
  boot_mean <- rep(NA, R)
  for (i in 1:R){
    x_sampling <-x[sample(1:n, replace = TRUE)]
    boot_mean[i] <- mean(x_sampling)
  }
  confint<-quantile(boot_mean, c(alpha/2, 1 - alpha/2))
  boostrap<-list(alpha=alpha, mean=estimate, n_sample=R, boostrap_confidence=confint)
  return(boostrap)
}

 

  


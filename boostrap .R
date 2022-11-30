
Boostrap <- function(X=matrix(rnorm(20,2,4),10,4),y=matrix(rnorm(10,5,3),10,1),alpha =0.05,R =20, seed = NULL, ...){
    #Calculate the mean
    X<-as.matrix(X)
    y<-as.matrix(y)
    n <- nrow(X)
    for (k in 1:R){
      for(i in 1:nrow(X)) {
        sample_X = X[sample(1:n, nrow(X), replace = TRUE), ]
        sample_y=y[sample(1:n, nrow(y), replace = TRUE), ]
      }
      coeff[k]<-quantile(beta_hat(sample_X, SampleY), c(alpha/2, 1-alpha))
    }
    LOWERCONFIDENCEINTERVAL = mean(coeff[,1])
    UPPERCONFIDENCEINTERVAL = mean(coeff[,2])
    confidence<-cbind(LOWERCONFIDENCEINTERVAL,UPPERCONFIDENCEINTERVAL)
    return(confidence)
}


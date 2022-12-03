library(ggplot2)
library(caret)
library(dplyr)
library(lattice)

#' Pi function
#' Define the probability of success or failure
#' @param m numerical data set to be fitted to a logistic regression curve
#'
#' @return logistic regression curve
#' @export
#'
#' @examples Pi(data)
Pi <- function(m){
  Pi_result<-1/(1+exp(-m))
  return(Pi_result)
}

#' Objective function
#' Finds the beta hat that will minimize the objective function
#' @param beta the coefficient vector
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
obj_fn <- function(beta, x, y){
  n <- length(y) # number of training examples
  p <- Pi(x%*%beta)
  cost.fn <- (t(-y)%*%log(p)-t(1-y)%*%log(1-p))/n
  # cost.fn
}

#' Gradient descent function
#' Constraint function
#' @param beta the coefficient vector
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
grad <- function(beta, x, y){
  n <- length(y)
  p <- Pi(x%*%beta)
  grad <- (t(x)%*%(p - y))/n
  # grad
}

#' Optimize function
#'
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
beta_hat <- function(x, y){
  x <- as.matrix(x)
  n <- nrow(x)
  y<- as.matrix(y)
  intercept <- rep(1,n)
  x <- cbind(intercept,x)
  #initialize beta
  beta <- solve(t(x)%*%x)%*%(t(x)%*%y)
  #use the optim function to perform gradient descent
  obj_fnOpti <- optim(beta, obj_fn, grad, x = x, y = y)
  #return coefficients
  coeff<- obj_fnOpti$par
  return(coeff)
}


##### Training our model with our generated dataset
# writing code for the prediction on new data set Z
#This function will take as input newdata, the origninal x and y
#' Title
#'
#' @param Z
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
model_predict <- function(Z, x, y){
  x <- as.matrix(x)
  n <- nrow(x)
  y<- as.matrix(y)
  intercept <- rep(1,n)
  x <- cbind(intercept,x)
  #initialize beta
  beta <- (solve(t(x) %*% x))%*%(t(x) %*% y)
  #use the optim function to perform gradient descent
  obj_fnOpti <- optim(beta, obj_fn, grad, x=x, y=y)
  #return coefficients
  model<- obj_fnOpti$par
  #writing the prediction function
  Z <- as.matrix(Z)
  n <- nrow(Z)
  intercept <- rep(1,n)
  Z <-  cbind(intercept,Z)
  pred<- exp(Z%*%model)/(1+exp(Z%*%model))
  return(pred)
}



#function that gives response 0,1 for y
#' Title
#'
#' @param Z
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
response<- function(Z,x,y){
  y.prediction <- model_predict(Z,x,y)
  glm.pred = rep ("0",nrow(Z))
  glm.pred[y.prediction > 0.5]="1"
  gpp <- factor(glm.pred)
  return(gpp)
}


##### Function for the confusion matrix

#' Confusion matrix
#'
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#'
#' @return
#' @export
#'
#' @examples
confM <- function(x, y){
  library(caret)
  y.prediction <- model_predict(x,x,y)
  glm.pred <- rep ("0",length(y))
  glm.pred[y.prediction > 0.5]="1"
  gpp <- factor(glm.pred)
  newy <- factor(y)
  return(confusionMatrix(data = gpp, reference = newy))
}

set.seed(8)
y <- sample(c(0,1), size = 20, replace = TRUE)
n <- length(y)
x1 <- rpois(n, lambda = 3)
x2 <- rnorm(n, -3, 5)
x3 <- rexp(n, rate = 2)
# int <- rep(1, n)
x <- cbind(x1, x2, x3)
data <- data.frame(y, x)
data.x <- data[, -1]
data.y <- data[, 1]
beta_hat(data.x, data.y)

true.model <- glm(y ~ x1 + x2 + x3, family=binomial(link ="logit"), data = data)
true.model$coefficients

#####  Test data.
set.seed(51)
n = length(y)
newdata <- data.frame(x1 =rpois(15,lambda = 3), x2 =runif(15,-1, 1), x3 = rexp(15, rate= 5) )
newy <- sample(c(0,1), size = 15, replace = TRUE)

##### Outputs from the confusion matrix

conf.mat <- confM(newdata,newy)
paste("False Discovery Rate is", conf.mat$table[1,2]/(conf.mat$table[1,1] + conf.mat$table[1,2]))
paste("Diagnositic odds Ratio is", (conf.mat$table[1,1]*conf.mat$table[2,2])/(conf.mat$table[1,2]*conf.mat$table[2,1]))
conf.mat

#' bootstrap function
#' boostrap confidence interval
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param alpha
#' @param R
#' @param seed accounts for randomness and will allow for the same generated numbers
#' @param ...
#'
#' @return
#' @export
#' @examples
Boostrap <- function(x, y, alpha = 0.05, R = 20, seed = NULL, ...){
  #Calculate the mean
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x) + 1
  coeff.boot <- matrix(NA, nrow = R, ncol = p)
  for (k in 1:R){
    row <- sample(1:n, replace = TRUE)
    x.sample <- x[row, ]
    y.sample <- y[row]
    coeff.boot[k,] <- beta_hat(x.sample, y.sample)}
  beta.mean <- apply(coeff.boot, 2, mean)
  beta.sd <- apply(coeff.boot, 2, sd)
  Lower.conf <- beta.mean - qnorm(1-alpha/2)*beta.sd
  Upper.conf <- beta.mean + qnorm(1-alpha/2)*beta.sd
  conf.interval <- cbind(beta.mean, beta.sd, Lower.conf, Upper.conf)
  return(conf.interval)
}
Boostrap(x,y)


#checking prediction
(predict(true.model,newdata, type="response"))
model_predict(newdata, data.x, data.y)


m <- seq(-3, 3, by = 0.01)
n <- exp(2-2*m)/(1+exp(2-2*m))
beta_hat(m,n)
yp<- model_predict(m,m,n)
plot(m,yp, "l")

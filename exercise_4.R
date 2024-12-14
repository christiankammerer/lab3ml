mean_squared_error <- function(true, predicted){
  error <- mean((true-predicted)^2)
  return(error)
}

library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
train <- mydata[1:25,] # Training
test <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
winit <- runif(10,-1,1)


# 1 #
nn <- neuralnet(Sin~Var, train, hidden=10, startweights = winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn,test), col="red", cex=1)
mean_squared_error(predict(nn,test),test$Sin) 
# overall good prediction quality. Between Var = 5 and Var = 6 it is a little bit of though.
# It might be explained by the missing training data in this interval.

# 2 # 

h1 <- function(x) x

# use this instead of max(0,x), as max() is not differentiable and throws error when training nn
h2 <- function(x) ifelse(x >= 0, x, 0) 

h3 <- function(x) log(1+exp(x))

nn_h1 <- neuralnet(Sin~Var, train, hidden=10, act.fct=h1,startweights=winit)
nn_h2 <- neuralnet(Sin~Var, train, hidden=10, act.fct=h2,startweights=winit)
nn_h3 <- neuralnet(Sin~Var, train, hidden=10, act.fct=h3,startweights=winit)

nn_h1$act.fct

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h1,test), col="red", cex=1)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h2,test), col="red", cex=1)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h3,test), col="red", cex=1)


mean_squared_error(predict(nn_h1,test),test$Sin) 

mean_squared_error(predict(nn_h2,test),test$Sin) 

mean_squared_error(predict(nn_h3,test),test$Sin) 

# 3 #
#set.seed(1234567890)
Var <- runif(500, 0, 50)
mydata_3 <- data.frame(Var, Sin=sin(Var))

# Plot of the training data (black), test data (blue), and predictions (red)
plot(mydata_3, cex=2)
#points(mydata_3, col = "blue", cex=1)
plot(mydata_3[,1],predict(nn,newdata=mydata_3),col="red",cex=0.5, xlab="Var", ylab="Sin")
points(mydata_3,cex=1)

# until Var=10 the prediction is very good, but then converges towards:
min(predict(nn,mydata_3))

mean_squared_error(mydata_3[,1],predict(nn,newdata=mydata_3))

# 4 #
nn$weights
nn$weights[[1]][[1]][2,]
sum(nn$weights[[1]][[2]])
plot(nn)
# The convergence probably happens because of the lack of training data for Var > 10
# TODO: Why towards -9.5?
sum()
# 5 #
set.seed(1234567890)
Var <- runif(500,0,10)
mydata_5 <- data.frame(Var, Sin=sin(Var))


nn5 <- neuralnet(Var~Sin, mydata_5, hidden=10, threshold=0.1, startweights = )
plot(mydata_5[,2], mydata_5[,1], xlim=c(0,20))
points(predict(nn5,newdata=mydata_5))
plot(mydata_5)
plot(mydata_5[,2],predict(nn5,newdata=mydata_5),col="red",cex=0.5, xlim=c(0,10), ylim=c(-1,8), xlab="Var", ylab="Sin")
points(mydata_5, cex=1)
predict(nn5,newdata=mydata_5)

plot(mydata_5$Sin,predict(nn5,newdata=mydata_5),col="red",ylab="Var", xlab="Sin")
points(mydata_5$Sin,mydata_5$Var)
title("Predict x from sin(x)")
legend(x="bottomleft", legend=c("train","prediction"), fill=c("blue", "red"))
#TODO: plot and comment the results
mean_squared_error(predict(nn5,mydata_5), mydata_5$Var)

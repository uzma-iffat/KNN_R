############# Part 1 #######################
pop.size <- 10000
pop.mean <- 100
pop.sd <- 15

num.of.samples <- 200
sample.size <- 100

set.seed(1234)
norm.pop <- rnorm(pop.size,pop.mean,pop.sd) 
plot(norm.pop)

k <- 1:200
sample.mean <- numeric(length=length(num.of.samples))
sample.sd <- numeric(length=length(num.of.samples))
for (i in seq(along=k)) {
   pop.sample=sample(norm.pop,sample.size,replace=FALSE)
   sample.mean[i] <- mean(pop.sample)
   sample.sd[i] <- sd(pop.sample)
 }
 mean_mean <- mean(sample.mean) ## Mean of mean = 100.14348
 mean_sd <- mean(sample.sd) ## Mean of SD = 14.8114
 sd_mean <- sd(sample.mean) ## SD of mean = 1.480945
 sd_sd <- sd(sample.sd) ## SD of SD = 1.049044
 hist(sample.mean)
 hist(sample.sd)
 
 ##################### Part 2 ###################
 
 ########## Code given to generate the dataset ###########
 
 library("ggplot2")
 
 beta0 <- 1
 beta1 <- -2
 beta2 <- 6
 beta3 <- -1
 sigma <- 2
 
 set.seed(7890)
 
 x <- seq(0,4.95,0.05)
 f_x <- beta0 + beta1*x + beta2*x^2 + beta3*x^3
 epsilon <- rnorm(n=100,mean=0,sd=sigma)
 y <- f_x + epsilon
 
 x.test <- seq(0,5,0.1)
 f_x.test <- beta0 + beta1*x.test + beta2*x.test^2 + beta3*x.test^3
 epsilon.test <- rnorm(n=length(x.test),mean=0,sd=sigma)
 y.test <- f_x.test + epsilon.test
 
 library("mvtnorm")
 z <- rmvnorm(n=100,mean=rep(0,20))
 z.test <- rmvnorm(n=51,mean=rep(0,20))
 
 train.x <- cbind(x,z)
 test.x <- cbind(x.test,z.test)
 
 ############ Part 2 : Question 1 #################
 library("FNN")
 
 knn15.train <- knn.reg(train=train.x,test=train.x,y=y,k=15)
 mse.knn15.train <- mean((y-knn15.train$pred)^2) ## Train MSE = 25.2799
 
 knn15.test <- knn.reg(train=train.x,test=test.x,y=y,k=15)
 mse.knn15.test <- mean((y.test - knn15.test$pred)^2) ## Test MSE = 30.90956
 
 ############# Part 2 : Question 2 ###############
 
 ks <- 1:30
 mse.knn.train <- numeric(length(ks))
 mse.knn.test <- numeric(length(ks))
 for (i in seq(along=ks)) {
   knn.model.train <- knn.reg(train=train.x,test=train.x,y=y,k=i)
   knn.model.test <- knn.reg(train=train.x, test=test.x, y=y, k=i)
   mse.knn.train[i] <- mean((y-knn.model.train$pred)^2)
   mse.knn.test[i] <- mean((y.test-knn.model.test$pred)^2)
 }
 
 plot_mse = ggplot() + geom_point(aes(x=ks,y=mse.knn.train),size=3) + geom_line(aes(x=ks,y=mse.knn.train),col="red") +geom_point(aes(x=ks,y=mse.knn.test),size=3) + geom_line(aes(x=ks,y=mse.knn.test),col="blue")
 plot_mse
 
 optimal.k <- which.min(mse.knn.test) ## Optimal k = 13
 optimal.mse <- min(mse.knn.test) ## MSE at optimal k = 28.26
 
  
 ############ Part 2 : Question 3 #####################33
  #knn model with x only 
 
 train.x.x <- matrix(x,ncol=1)
 test.x.x <- matrix(x.test,ncol=1)
 j <- 1:30
 mse.knn.x.train <- numeric(length(j))
 mse.knn.x.test <- numeric(length(j))
 for (i in seq(along=j)) {
   knn.xmodel.train <- knn.reg(train=train.x.x,test=train.x.x,y=y,k=i)
   knn.xmodel.test <- knn.reg(train=train.x.x,test=test.x.x,y=y,k=i)
   mse.knn.x.train[i] <- mean((y-knn.xmodel.train$pred)^2)
   mse.knn.x.test[i] <- mean((y.test-knn.xmodel.test$pred)^2)
 }
 optimal.k.x <- which.min(mse.knn.x.test) ## Optimal k =8
 optimal.mse.x <- min(mse.knn.x.test) ## Optimal MSE = 3.8300
 
 plot_mse_compare <- plot_mse + geom_point(aes(x=j,y=mse.knn.x.train),size=2) + geom_line(aes(x=j,y=mse.knn.x.train),col="green") + geom_point(aes(x=j,y=mse.knn.x.test),size=2) + geom_line(aes(x=j,y=mse.knn.x.test),col="black")
 plot_mse_compare
 
 ## < include analysis for MSE and K with 
 ##   respect to using x alone as compared to x,z> ##
 
 ############ Part 2 : Question 4 ###############
 
 js <- 1:20
 ks <- 1:30
 
 mse.knn.xz.train <- numeric(length=length(ks))
 mse.knn.xz.test <- numeric(length=length(ks))
 xz.optimal.k <- numeric(length=length(js))
 xz.optimal.mse <- numeric(length=length(js))
 
 for (j in seq(along=js))
 {
 
 train.xz <- cbind(x,z[,1:j])
 test.xz <- cbind(x.test,z.test[,1:j])
 
 
 for (i in seq(along=ks)) {
   knn.xz.model.train <- knn.reg(train=train.xz,test=train.xz,y=y,k=i)
   knn.xz.model.test <- knn.reg(train=train.xz, test=test.xz, y=y, k=i)
 
   mse.knn.xz.train[i] <- mean((y-knn.xz.model.train$pred)^2)
   mse.knn.xz.test[i] <- mean((y.test-knn.xz.model.test$pred)^2)
 }
 xz.optimal.k[j] <- which.min(mse.knn.xz.test)
 xz.optimal.mse[j] <- min(mse.knn.xz.test)
 }
 mse.xz.optimal.plot <- ggplot() + geom_point(aes(x=js,y=xz.optimal.mse),size=3)+geom_line(aes(x=js,y=xz.optimal.mse),col="blue")
 mse.xz.optimal.plot
 
 ## < interpret result > ## 
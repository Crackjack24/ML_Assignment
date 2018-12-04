#===========================================================================================================
# Checking How the Root Mean Square Error will vary with respect to different orders of regression line
#==========================================================================================================

#Loading the House Prices Data from Local
lead = read.csv('D:/R/Codes/train.csv')
#Setting the Seed value to 0
set.seed(0)


#Splitting the code into train and test data as 850 and 100 
train = lead[1:850, ]
test = lead[851:950, ]

#Creating the Vector
m <- c(50,100,150,200,250,300,350,400,450,500,550,600,650)
vector = c()

#Looping over the vector 
for(i in  m){
  x = sample(1:nrow(train),i)
  print(nrow(train[x,]))
  
 # fitting regression model over each value
  m1 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2) + I(GrLivArea^3) + I(GrLivArea^4) +
             I(GrLivArea^5) + I(GrLivArea^6) + I(GrLivArea^7), train[x,])
  pred = predict(m1, newdata=test)
  
  #Calculating the Root Meansquare value
  error <- sqrt(sum((pred-test$SalePrice)^2))
  #print(pred)
  vector <- c(vector,error)
}

#Plotting the sample vs RMSE error
jpeg("Complexity_Vs_TestErrors_order7.jpg")
plot(m,vector,xlab = "Number of Samples",ylab = "Root MeanSquare Error(Test_Error)",main ="Regression graph for Sample_Size VS RMSE of order 7)", pch=19, cex=0.5)
lines(m, vector, col='darkgreen', type='l', pch=19)
dev.off() 

####Complexity Vs Test Error for 11th order polynomial regression line
m <- c(50,100,150,200,250,300,350,400,450,500,550,600,650)
vector = c()

#Looping over the vector 
for(i in  m){
  x = sample(1:nrow(train),i)
  print(nrow(train[x,]))
  
  # fitting regression model over each value
  m1 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2) + I(GrLivArea^3) + I(GrLivArea^4) +
             I(GrLivArea^5) + I(GrLivArea^6) + I(GrLivArea^7)+I(GrLivArea^8) + I(GrLivArea^9) + I(GrLivArea^10)++ I(GrLivArea^10), train[x,])
  pred = predict(m1, newdata=test)
  
  #Calculating the Root Meansquare value
  error <- sqrt(sum((pred-test$SalePrice)^2))
  #print(pred)
  vector <- c(vector,error)
}

#Plotting the sample vs RMSE error
#jpeg("Complexity_Vs_TestErrors_order11.jpg")
plot(m,vector,xlab = "Number of Samples",ylab = "Root MeanSquare Error(Test_Error)",main ="Regression graph for Sample_Size VS RMSE of order 11)", pch=19, cex=0.5)
lines(m, vector, col='darkgreen', type='l', pch=19)
#dev.off() 


lead = read.csv('D:/R/Codes/train.csv')
library("ggplot2")

train = lead[1:300, ]
test = lead[301:350, ]

set.seed(1)
rand1 = sample(1:nrow(train),20)
rand2 = sample(1:nrow(train),20)
rand3 = sample(1:nrow(train),20)
rand4 = sample(1:nrow(train),20)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand1,]
m1 <- lm(SalePrice ~ GrLivArea,train1 )
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree1_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 1", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m1)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[rand1,]
m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree2_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 2", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m2)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[rand1,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree7_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice order 7", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m7)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[rand1,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree8_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 8", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m8)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[rand1,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree9_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order9", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m9)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[rand1,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA

jpeg("Linear_Regression_Degree10_sample20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 10", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m10)[order(train1$GrLivArea)], col='red', type='l') 
dev.off() 


jpeg("Combined_Regression_Graph_samplesize20.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Living Area vs SalePrice for different complexities", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m1)[order(train1$GrLivArea)], col='red', type='l') 
lines(sort(train1$GrLivArea), fitted(m2)[order(train1$GrLivArea)], col='blue', type='l') 
lines(sort(train1$GrLivArea), fitted(m7)[order(train1$GrLivArea)], col='green', type='l') 
lines(sort(train1$GrLivArea), fitted(m8)[order(train1$GrLivArea)], col='orange', type='l') 
lines(sort(train1$GrLivArea), fitted(m9)[order(train1$GrLivArea)], col='chartreuse', type='l') 
lines(sort(train1$GrLivArea), fitted(m10)[order(train1$GrLivArea)], col='#009999', type='l') 
legend("topleft", legend=c("Order 1", "Order 2","Order 7","Order 8","Order 9","Order 10"),
       col=c("red", "blue","green","orange","chartreuse","#009999"), lty=1:2, cex=0.8)

dev.off() 

#vec <- c(1,2,7,8,9,10)
#train_error <- c(train_error1,train_error2,train_error7,train_error8,train_error9,train_error10)
#test_error <- c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10)


jpeg("Complexity_Vs_Errors_samplesize20.jpg")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(train_error1,train_error2,train_error7,train_error8,train_error9,train_error10), z =  c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))

ggplot(df1, aes(x)) + 
  geom_line(aes(y = y, colour = "Train_Error")) + 
  geom_line(aes(y = z, colour = "Test_Error"))+labs(title = "Model_Complexity Vs Mean_Square(Test_Error)\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") 
dev.off() 
####################################################################################################################################################

train = lead[1:300, ]
test = lead[301:350, ]

set.seed(1)
rand1 = sample(1:nrow(train),100)
rand2 = sample(1:nrow(train),100)
rand3 = sample(1:nrow(train),100)
rand4 = sample(1:nrow(train),100)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand1,]
m1 <- lm(SalePrice ~ GrLivArea,train1 )
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree1_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 1", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m1)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[rand1,]
m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree2_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 2", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m2)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[rand1,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree7_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice order 7", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m7)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[rand1,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree8_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 8", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m8)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[rand1,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
jpeg("Linear_Regression_Degree9_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order9", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m9)[order(train1$GrLivArea)], col='red', type='l') 
dev.off()


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[rand1,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA

jpeg("Linear_Regression_Degree10_sample100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Regression graph for LivingArea vs SalePrice of order 10", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m10)[order(train1$GrLivArea)], col='red', type='l') 
dev.off() 


jpeg("Combined_Regression_Graph_samplesize100.jpg")
plot(train1$GrLivArea,train1$SalePrice,xlab = "LivingArea",ylab = "Saleprice",main ="Living Area vs SalePrice for different complexities", pch=19, cex=0.5)
lines(sort(train1$GrLivArea), fitted(m1)[order(train1$GrLivArea)], col='red', type='l') 
lines(sort(train1$GrLivArea), fitted(m2)[order(train1$GrLivArea)], col='blue', type='l') 
lines(sort(train1$GrLivArea), fitted(m7)[order(train1$GrLivArea)], col='green', type='l') 
lines(sort(train1$GrLivArea), fitted(m8)[order(train1$GrLivArea)], col='orange', type='l') 
lines(sort(train1$GrLivArea), fitted(m9)[order(train1$GrLivArea)], col='chartreuse', type='l') 
lines(sort(train1$GrLivArea), fitted(m10)[order(train1$GrLivArea)], col='#009999', type='l') 
legend("topleft", legend=c("Order 1", "Order 2","Order 7","Order 8","Order 9","Order 10"),
       col=c("red", "blue","green","orange","chartreuse","#009999"), lty=1:2, cex=0.8)

dev.off() 

#vec <- c(1,2,7,8,9,10)
#train_error <- c(train_error1,train_error2,train_error7,train_error8,train_error9,train_error10)
#test_error <- c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10)


jpeg("Complexity_Vs_Errors_samplesize100.jpg")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(train_error1,train_error2,train_error7,train_error8,train_error9,train_error10), z =  c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))

ggplot(df1, aes(x)) + 
  geom_line(aes(y = y, colour = "Train_Error")) + 
  geom_line(aes(y = z, colour = "Test_Error"))+labs(title = "Model_Complexity Vs Mean_Square(Test_Error)\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") 
dev.off() 


###########################################################################################################################################


train = lead[1:300, ]
test = lead[301:350, ]


set.seed(0)



#1,2,7,8,9,10

rand1 = sample(1:nrow(train),20)
rand2 = sample(1:nrow(train),20)
rand3 = sample(1:nrow(train),20)
rand4 = sample(1:nrow(train),20)
#print(i)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand1,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")

library("ggplot2")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample20.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Mean_SQuare_Error(Test_Error)\n", x = "Model_Complexity(1,2,7,8,9,10)", y = "Mean_SQuare_Error(Test_Error)", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off() 
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand2,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample20.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off()
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand3,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")



df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample20.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off()
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand4,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")



df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample20.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))

dev.off()

###########################################################################################################################################################################


###########################################################################################################################################


train = lead[1:300, ]
test = lead[301:350, ]


set.seed(0)



#1,2,7,8,9,10

rand1 = sample(1:nrow(train),100)
rand2 = sample(1:nrow(train),100)
rand3 = sample(1:nrow(train),100)
rand4 = sample(1:nrow(train),100)
#print(i)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand1,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")

library("ggplot2")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample1.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Mean_SQuare_Error(Test_Error)\n", x = "Model_Complexity(1,2,7,8,9,10)", y = "Mean_SQuare_Error(Test_Error)", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off() 
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand2,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")

df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample2.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off()
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand3,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")



df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample3.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))
dev.off()
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
train1 = train[rand4,]

#paste(j)
m1 <- lm(SalePrice ~ GrLivArea,train1 )
#m1 = paste(m1,i)
train_error1 = sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
#train1 = train[i,]


m2 <- lm(SalePrice ~ GrLivArea+ I(GrLivArea^2),train1 )
m2
train_error2 = sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2 = sum((pred-test$SalePrice)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#train1 = train[i,]
m7 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7),train1 )
m7
train_error7 = sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================
#train1 = train[i,]
m8 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8),train1 )
m8
train_error8 = sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================
#train1 = train[i,]
m9 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9),train1 )
m9
train_error9 = sum(m9$residuals^2)
pred = predict(m9, newdata=test)
test_error9 = sum((pred-test$SalePrice)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================
#train1 = train[randi,]
m10 <- lm(SalePrice ~ GrLivArea+I(GrLivArea^2)+I(GrLivArea^3)+I(GrLivArea^4)+I(GrLivArea^5)+I(GrLivArea^6)+I(GrLivArea^7)+I(GrLivArea^8)+I(GrLivArea^9)+I(GrLivArea^10),train1 )
m10
train_error10 = sum(m10$residuals^2)
pred = predict(m10, newdata=test)
test_error10 = sum((pred-test$SalePrice)^2)
#PLOTTING THE MODEL OVER THE DATA
#jpeg("Complexity_Vs_Errors.jpg")



df1 <- data.frame(x = c(1,2,7,8,9,10), y = c(test_error1,test_error2,test_error7,test_error8,test_error9,test_error10))
jpeg("Complexity_Vs_Errors_Sample4.jpg")
ggplot(df1, aes(x,y=y)) + geom_point()+
  geom_line(aes( colour = "y"))+labs(title = "Model Complexity Vs Test Error\n", x = "Model Complexity", y = "Test_Error", color = "Legend Title\n") +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red"))

dev.off()

library(ISLR)
install.packages("e1071")
library(e1071)
attach(Auto)
library(class)
mpg01 <- ifelse( mpg > median(mpg), yes = 1, no = 0)
Auto <- data.frame(mpg01,Auto)
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")
boxplot(origin ~ mpg01, data = Auto, main = "Origin vs mpg01")

##“mpg01” and “cylinders”, “weight”, “displacement” and “horsepower

index <- sort(sample(nrow(Auto), nrow(Auto)*.75))
train <- Auto[index,]
test <- Auto[-index,]

library(MASS)
lda.model <- lda(mpg01 ~ cylinders+displacement+horsepower+weight, data = train)
lda.predict <-  predict(lda.model, test)
table(lda.predict$class , test$mpg01)
mean(lda.predict$class !=test$mpg01 )

qda.model <- qda(mpg01 ~ cylinders+displacement+horsepower+weight, data = train)
qda.predict <-  predict(qda.model, test)
table(qda.predict$class , test$mpg01)
mean(qda.predict$class !=test$mpg01 )

glm.model <- glm(mpg01 ~ cylinders+displacement+horsepower+weight, data = train)
glm.prob <-  predict(glm.model, test)
glm.predict <- rep(0, length(glm.prob))
glm.predict[glm.prob > .5] <- 1
glm.predict
table(glm.predict,test$mpg01)
mean(glm.predict !=test$mpg01 )


nb.fit <- naiveBayes(mpg01 ~ cylinders+displacement+horsepower+weight, data = train)
nb.predict <- predict(nb.fit, test)
table(nb.predict,test$mpg01)
mean(nb.predict!=test$mpg01 )


train.X <-train[,c(3,4,5,6)]
test.X <- test[,c(3,4,5,6)]
train.mpg01 <-train$mpg01
knn.predict <-  knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.predict!=test$mpg01 )

testError <- rep(NA, 200)
for (k in 1:200){
  knn.pred <- knn(train.X, test.X, train.mpg01, k = k)
  testError[k] <- mean(knn.pred!=test$mpg01)
}
testError
which.min(testError)


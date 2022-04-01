library(ISLR)
library(dplyr)
library(ggplot2)
data <- na.omit(Hitters)

happy <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2)) 
train <- data.frame(data[happy==1, ])
test <- data.frame(data[happy==2, ])

model1 <- glm(League ~ HmRun,  family = "binomial", data = train)
model2 <- glm(League ~ RBI,  family = "binomial", data = train)

p1 <- as.data.frame(predict(model1, train, type = "response"))
p2 <- as.data.frame(predict(model2, train, type = "response"))

new.data1 <- cbind(train, p1)
new.data2 <- cbind(train, p2)

ggplot(new.data1, aes(x=predict(model1, train, type = "response"), y=League)) + stat_sum(color ="red") +stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)
ggplot(new.data2, aes(x=predict(model2, train, type = "response"), y=League)) + stat_sum(color ="red") +stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)

table(p < 0.5, train$League)
table(p != 0.5, train$League)
table(p > 0.5, train$League)


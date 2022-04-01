library(ISLR)
library(dplyr)
library(ggplot2)
data <- na.omit(Hitters)

happy <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2)) 
train <- data.frame(data[happy==1, ])
test <- data.frame(data[happy==2, ])

model1 <- glm(League ~ HmRun,  family = "binomial", data = train)
model2 <- glm(League ~ RBI,  family = "binomial", data = train)

p <- as.data.frame(predict(model, train, type = "response"))

new.data <- cbind(train, p)
ggplot(new.data, aes(x=predict(model, train, type = "response"), y=League)) + stat_sum(color ="red") +stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)


table(p < 0.5, train$League)
table(p != 0.5, train$League)
table(p > 0.5, train$League)


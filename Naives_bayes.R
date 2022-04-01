library(dplyr)
library(ggplot2)
library(naivebayes)
data2 <- na.omit(Hitters) 

sample2 <- sample(2, nrow(data2), replace = T, prob = c(0.8, 0.2)) 
train_2 <- data2[sample2 == 1, ]
test_2 <-  data2[sample2 == 2, ]


model3 <- naive_bayes(League ~ HmRun, data = train_2, usekernel = F)
model4 <- naive_bayes(League ~ RBI, data = train_2, usekernel = F)

p3 <-  as.data.frame(predict(model3, train_2, type = "response"))
p4 <-  as.data.frame(predict(model4, train_2, type = "response"))

new.data3 <- cbind(train_2, p3)
new.data4 <- cbind(train_2, p4)

ggplot(new.data3, aes(x = predict(model3, train_2, type = "response") , y = League)) + stat_sum(color = "blue") + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)
ggplot(new.data4, aes(x = predict(model4, train_2, type = "response") , y = League)) + stat_sum(color = "blue") + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)

table(p2 < 0.5, train_2$League)
table(p2 != 0.5, train_2$League)
table(p2 > 0.5, train_2$League)

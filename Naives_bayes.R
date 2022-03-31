library(dplyr)
library(ggplot2)
library(naivebayes)
data2 <- na.omit(Hitters) 

sample2 <- sample(2, nrow(data2), replace = T, prob = c(0.8, 0.2)) 
train_2 <- data2[sample2 == 1, ]
test_2 <-  data2[sample2 == 2, ]


model2 <- naive_bayes(League ~ Hits + HmRun + RBI, data = train_2, usekernel = F)

p2 <-  as.data.frame(predict(model, train_2, type = "response"))

new.data2 <- cbind(train_2, p2)

ggplot(new.data2, aes(x = predict(model, train_2, type = "response") , y = League)) + stat_sum(color = "blue") + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE)

table(p2 < 0.5, train_2$League)
table(p2 == 0.5, train_2$League)
table(p2 > 0.5, train_2$League)

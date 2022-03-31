library(dplyr)
library(ggplot2)
library(naivebayes)
data2 <- Hitters
sample2 <- sample(2, nrow(data2), replace = T, prob = c(0.8, 0.2)) 
train_2 <- data2[sample2 == 1, ]
test_2 <-  data2[sample2 == 2, ]


model2 <- naive_bayes(League ~ Hits, data = data, usekernel = F)

Predicted_data2 <- data.frame(Hits=seq(min(data$Hits), max(data$Hits), len=500))

Predicted_data2$League <- predict(model, Predicted_data, type = "response")

ggplot(train, aes(x=Hits, y=League)) + geom_point() + geom_smooth()


library(dplyr)
library(ggplot2)
library(naivebayes)
data4 <- Hitters
sample3 <- sample(2, nrow(data2), replace = T, prob = c(0.8, 0.2)) 
train_3 <- data2[sample2 == 1, ]
test_3 <-  data2[sample2 == 2, ]


model3 <- naive_bayes(AtBat ~ Hits, data = data, usekernel = F)

Predicted_data4 <- data.frame(Hits=seq(min(data$Hits), max(data$Hits), len=500))

Predicted_data4$AtBAt <- predict(model, Predicted_data, type = "response")

ggplot(train, aes(x=Hits, y=AtBat)) + geom_point() + geom_smooth()


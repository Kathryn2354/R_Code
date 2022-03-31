library(ISLR)
library(dplyr)
library(ggplot2)
data3 <- Hitters 
happy3 <- sample(2, nrow(data3), replace = T, prob = c(0.8, 0.2)) 
train3 <- data.frame(data3[happy3==1, ])
test3 <- data.frame(data3[happy3==2, ])

model3 <- glm(AtBat ~ Hits,  family = "binomial", data = train)

Predicted_data3 <- data.frame(Hits=seq(min(data$Hits), max(data$Hits), len=500))

Predicted_data3$AtBat <- predict(model, Predicted_data, type = "response")

ggplot(train3, aes(x=Hits, y=AtBat)) + geom_point() + stat_smooth(method ="glm", color = "green", se=FALSE, method.args = list(family = binomial)) +geom_smooth()

                                                                 
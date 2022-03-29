library(ISLR)
library(dplyr)
library(ggplot2)
data <- Hitters
happy <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2)) 
train <- data.frame(data[happy==1, ])
test <- data.frame(data[happy==2, ])

model <- glm(AtBat ~ Hits,  family = "binomial", data = train)

Predicted_data <- data.frame(Hits=seq(min(data$Hits), max(data$Hits), len=500))

Predicted_data$AtBat <- predict(model, Predicted_data, type = "response")

ggplot(train, aes(x=Hits, y=AtBat)) + geom_point() + stat_smooth(method="glm", color="green", se=FALSE, 
                                                               method.args = list(family=binomial)) + geom_smooth() 









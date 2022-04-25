#this initialized the library need to run this code 
require(randomForest)
require(MASS)
library(ggplot2)

#Boston is data set built in from the MASS library 
attach(Boston)

#this get the dimension of the data set of Boston
dim(Boston)

#This is used to generate a simulation to ensure all results, figures, etc are reproducible.
set.seed(101)

#creating a train set for random forest
train=sample(1:nrow(Boston),300)

# (1) Why was the training set created differently than how we created training and testing data sets when writing code for Bayesian Classification and Logistic Regression?
#   
#   Answer: The training set was created differently because out of the 506 row we are only taking a sample size of 300. For Naïve Bayes and Logistic Regression, you have to create the model first; while here we create the training set first and before creating the model.  In addition, in Naïve Bayes and Logistic Regression, both training and validation sets are in a table; In Random Forest, training and validation sets are just in the form of a list, instead of in a table.  

#We are creating an initial modal by combining the Boston dataset and the training set.
Boston.rf=randomForest::randomForest(medv ~ . , data = Boston , subset = train)

#summary of the initial modal
Boston.rf

#plot the summary of the initial model
plot(Boston.rf)

#fisrt initialize variable oob.err to a size of 13 and make sure the number are a double   
oob.err=double(13)

#first initialize variable test.err to a size of 13 and make sure the number are a double 
test.err=double(13)

#for loop use check all branch of the tree and make a prediction
for(mtry in 1:13){
  
  #created another model 
  rf=randomForest::randomForest(medv ~ . , data = Boston , subset = train, mtry=mtry, ntree=400)
  
  #add data to oob.err array from the modal
  oob.err[mtry] = rf$mse[400]
  
  #created a prediction
  pred<-predict(rf,Boston[-train,])
  
  #add data to test.err array with the created prediction
  test.err[mtry]=with(Boston[-train,],mean((medv - pred)^2)) 
  
  #print out the number from 1 to 13 in this loop
  cat(mtry," ")
}

# (2) Why is there a for loop?
#   
#   Answer: Loops are used in programming to repeat a specific block of code.  We can only use a for loop to populate both arrays with numbers, one array without prediction and the other with your created prediction. Thus, for loop is necessary to loop all 13 variables through the model inside the for loop. 


#get simulation data in test.err
test.err

#get simulation data in oob.err
oob.err 

# (3) What does OOB stand for and what does it have to do with Random Forests? How does Bootstrap come into this?
#   
#   Answer: OOB stands for Out Of Bag. Out of bag score is used to validate the Random Forest model. More specifically, OOB is a method of measuring the prediction error of Random Forests, among others. Random Forest is a classification algorithm consisting of many decision trees. Bootstrapping, together with OOB, is used to create an uncorrelated Forest, which has prediction that is more accurate than a single decision tree.  

#Creating a new plot
matplot(1:mtry, cbind(oob.err,test.err), pch=19 , col=c("red","blue"), type="b", ylab="Mean Squared Error", xlab="Number of Predictors Considered at each Split")

# (4.) What is a split?
#   
#   Answer: Node splitting in a Random Forest model is based on a random subset of features for each tree. Each tree in a random forest can pick only from a random subset of features. This will create more variations in the model, ultimately leading to lower correlation across trees and more diversification. 

#add a legend to the plot
legend("topright", legend=c("Out of Bag (Validation Set) Error", "Test (Training Set) Error"), pch=19, col=c("red","blue"))

# (5.) Is the above code doing a classification or using regression to predict a value? What is the difference between those two things? How is each calculated? (use words to explain, not code)
# 
# Answer: 
#   1) Yes, the above code is using regression to predict a value. 
# 
# 2) Random forest operates by constructing a multitude of decision trees.  The outputting can be either classification, which is the mode of the classes selected by most trees, or regression, which is mean or average prediction, of the individual trees.
# 
# 3) How is each calculated:
#   Regression: Using data on predictor variables (inputs, X) and a continuous response variable (output, Y) to build a model for a) predicting the value of the response from the predictors, and b) understanding the relationship between the predictors and the response. 

# For example: Predicting a person's income (Y) based on their age, education, sex, ethnicity, and occupation (X)
# 
# Classification: using data on predictor variables (inputs, X) and a categorical response variable (output, Y) to build a model for a) predicting the value of the response from the predictors and b) understanding the relationship between the predictors and the response.
# 
# For example: Predicting a person's cancer score (status) (Y) based on the parameters of cancer biomarkers (X).


# Test data results:
#   21.12475 14.10628 11.88104 10.93083 11.04072 10.78652 10.45534 10.87542 11.18493 11.30766 11.18873 11.33677  11.08301
# OOB data results:
#   22.60531 15.43563 12.75751 12.90572 12.74848 12.69902 12.30101 12.85144 12.87710 13.04877 12.88060 13.52747 13.67520

#create a table 
ImpData <- as.data.frame(importance(Boston.rf))

#change column name
ImpData$Var.Names <- row.names(ImpData)

#creating a ggplot of the data
ggplot(ImpData, aes(x=Var.Names, y=`IncNodePurity`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`IncNodePurity`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



# (6.) Explain how to read and understand each visualization the code produces.
# 
# Answer: See answers under each visualization
# 
# Figure 1. This plot shows Random Forest regression in R.  On the It is the visualization of variable importance. On the x-axis are the IncNodePurity value. On the y-axis are the 13 variables related to the owner-occupied homes in Boston suburb. The higher the IncNodePurity value is, the more important the feature will be. The smallest IncNodePurity is the valriable labeled as 'chas', which stands for Charles River dummy variable. Here it means 'chas' is not an important feature.  On the other hand, the largest IncNodePurity on the x-axis is 'lstat', which stands for lower status of the population (%). The 'lstat' here is the most important feature among all 13 variables. 

# Figure 2. This plot shows the error rate (y-axis) and the number of trees (between 0 - 500; x-axis). Judging from trend the curve is going, it is obvious to me that the error shown in Y-axis is drastically decreased when the number of trees is increased to approximately 50.  From the point where the number of trees is around 50 - 60, the curve stays pretty much flat, suggesting that 1) there should be at least 50-60 trees to achieve the desired error, 2) adding more trees (>50 or 60 trees) may not further reduce the error significantly. 

# Figure 3. This plot shows that the mean square error (y-axis) and the number of predictors (between 1 - 13; x-axis). While the Red line is the Out of Bag (variation set) Error Estimates and the Blue Line is the Test (Training set) Error. The Red line and the Blue Line are almost going both in parallel and smoothly, indicating that 1) they are closely correlated, and 2) the test set and the validation have quite similar quite smooth mean square error. In addition, Both the Red line and the Blue Line are and the error estimates are somewhat correlated too. 


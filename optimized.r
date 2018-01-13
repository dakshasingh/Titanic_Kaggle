#setting working directory "laggle is a seperate folder created for kaggle projects"
setwd("C:/Users/sai/Desktop/kaggle")

#given train and test data were downloaded in the kaggle folder, assigning train and test data to titanic.train and titanic.test repectively
titanic.train <- read.csv(file = "TitanicTrain.csv" , stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "TitanicTest.csv" , stringsAsFactors = FALSE, header = TRUE)

#checking the headers of train and test data //variables (or) column names
names(titanic.train)
names(titanic.test)

#checking if the data is properly loaded into the objects in R studio
head(titanic.train)
tail(titanic.train)
head(titanic.test)
tail(titanic.test)

#checking the structure of train data
str(titanic.train)

#combining both train and test data to carry on data cleaning // missing values etc
titanic.all <- rbind(titanic.train , titanic.test)

#to differentiate the train and test data to split all into train and test after data cleaning // assigning train to be true for IsTrainSet and False for IsTrainSet 
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#checking the new column i.e IsTrainSet
tail(titanic.test)
tail(titanic.train)


#In test data "Survived" column is missing so we add it with NA values 
titanic.test$Survived <- NA

#checking if Survived column has the NA values
head(titanic.train)
head(titanic.test)

#after the number of variables are made equal in both the datasets combining train and test to perform overall data cleaning by rbind
titanic.all <- rbind(titanic.train , titanic.test)

#checking if all the rows have been combined properly
dim(titanic.all)

#checking for NULL values in Embarked variable
titanic.all[titanic.all$Embarked == "", "Embarked"]

#checking the number of factors and NULL values
table(titanic.all$Embarked)

#inorder to treat the null values taking the mean of Embarked variable to replace null values with it
median(titanic.all$Embarked)

#as the median is "S" assigning the null values to it
titanic.all[titanic.all$Embarked == "", "Embarked"] <- "S"

#checking if the null values have been treated
table(titanic.all$Embarked)

#checking the Age variable for null values
table(is.na(titanic.all$Age))

#Categorical casting
#converting the required variables as factors
titanic.all$Pclass <- as.factor(titanic.all$Pclass)
titanic.all$Sex <- as.factor(titanic.all$Sex)
titanic.all$Embarked <- as.factor(titanic.all$Embarked)

#treating the missing values of Fare column using regression model tp predict values
is.na(titanic.all$Fare)
table(is.na(titanic.all$Fare))
titanic.all[is.na(titanic.all$Fare), "Fare"]

#checking for outliers
boxplot(titanic.all$Fare)

#treating the outliers
boxplot.stats(titanic.all$Fare)
boxplot.stats(titanic.all$Fare)$stats
boxplot.stats(titanic.all$Fare)$stats[5]
upper.whisker <- boxplot.stats(titanic.all$Fare)$stats[5]
outlier.whisker <- titanic.all$Fare < upper.whisker
outlier.whisker
#considering the data without outliers
titanic.all[outlier.whisker,]
str(titanic.all)

#building regression equation for data treatment
fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
#linear regression model
fare.model <- lm(
  formula = fare.equation, 
  data = titanic.all[outlier.whisker,]
)
summary(fare.model)

#getting the row of the missing values
fare.row <- titanic.all
   [
     is.na(titanic.all$Fare),
     c("Sex", "Age", "SibSp", "Embarked", "Parch", "Pclass")
     ]

#predicting the value from regression
predict(fare.model,titanic.all[is.na(titanic.all$Fare),c("Sex", "Age", "SibSp", "Embarked", "Parch", "Pclass")]  )
#the above statement can also be written as
predict(fare.model, fare.row)
fare.predictions <- predict(fare.model, fare.row)

#treating the missing value with the predicted value
titanic.all[is.na(titanic.all$Fare), "Fare"] <- fare.predictions

#checking if the value is treated
table(is.na(titanic.all$Fare))

#checking for missing values in Pclass // no missing values
str(titanic.all)
table(is.na(titanic.all$Pclass))
#checking the structures of all variables
str(titanic.all) 

table(is.na(titanic.all$Sex))
table(is.na(titanic.all$Age))
#in age variable we have missing values 
#treating age missing values
titanic.all[is.na(titanic.all$Age), "Age"]
#checking for outliers of age 
boxplot(titanic.all$Age)
boxplot.stats(titanic.all$Age)
upper.whisker.age <- boxplot.stats(titanic.all$Age)$stats[5]
outlier.whisker.age <- titanic.all$Fare < upper.whisker.age
outlier.whisker.age
titanic.all[outlier.whisker.age,]
str(titanic.all)
names(titanic.all)

#building regression equation for data treatment
age.equation <- "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
#linear regression model
age.model <- lm(
  formula = age.equation, 
  data = titanic.all[outlier.whisker.age,]
)
summary(age.model)

#getting the row of the missing values
age.row <- titanic.all[is.na(titanic.all$Age),
  c("Sex", "Fare", "SibSp", "Embarked", "Parch", "Pclass")]
age.row

#predicting the value from regression
predict(age.model,titanic.all[is.na(titanic.all$Age),c("Sex", "Fare", "SibSp", "Embarked", "Parch", "Pclass")]  )
#the above statement can also be written as
predict(age.model, age.row)
install.packages("base")
age.predictions <- round(predict(age.model, age.row))
age.predictions
table(is.na(age.predictions))
titanic.all[is.na(titanic.all$Age), "Age"] <- age.predictions
titanic.all$Age
table(is.na(titanic.all$Age))
titanic.all[is.na(titanic.all$Age), "Age"] <- median(titanic.all$Age, na.rm = TRUE)
table(is.na(titanic.all$Age))

names(titanic.all)
table(is.na(titanic.all$Pclass))

table(is.na(titanic.all$Sex))

table(is.na(titanic.all$SibSp))

table(is.na(titanic.all$Parch))

str(titanic.all)

table(is.na(titanic.all$Embarked))

#splitting train and test data
titanic.train <- titanic.all[titanic.all$IsTrainSet == TRUE, ]
titanic.test <- titanic.all[titanic.all$IsTrainSet == FALSE, ]

#checking if the split is correct
dim(titanic.test)
dim(titanic.train)

#checking if the data is properly loaded
tail(titanic.train)
tail(titanic.test)


#rechecking the variables
str(titanic.all)

#splitting the data into train and test after factorizing for proper accuracy
titanic.train <- titanic.all[titanic.all$IsTrainSet == TRUE, ]
titanic.test <- titanic.all[titanic.all$IsTrainSet == FALSE, ]
str(titanic.train)
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

#forming the equation for the model
names(titanic.train)
paste(names(titanic.train), sep = " ", collapse = "+")
Model.Equation <-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(Model.Equation)
survived.formula

#installing the randomForest package
install.packages("randomForest")
library(randomForest)

#applying the algorithm to the data
Titanic.Model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))
factors.eq <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

#predicting the survivors from the model using test data
Survived <- predict(Titanic.Model, newdata = titanic.test)
Survived
passengerId <- titanic.test$PassengerId
as.data.frame(passengerId)
outputDF <- as.data.frame(passengerId)
PassengerId <- titanic.test$passengerId
outputDF$Survived <- Survived
write.csv(outputDF, file = "Optimized.TitanicKaggle_daksha.csv", row.names = FALSE)

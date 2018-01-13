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

# taking the median of age removing the null values to treat null values
Age_Median <- median(titanic.all$Age, na.rm = TRUE)

#replacing null values with median
titanic.all[is.na(titanic.all$Age), "Age"] <- Age_Median

#checking fare variable for missing values
is.na(titanic.all$Fare)

#checking the number of missing value
table(is.na(titanic.all$Fare))
median(titanic.all$Fare)

#treating fare missing values with median of fare
median(titanic.all$Fare, na.rm = TRUE)
fare_median <- median(titanic.all$Fare, na.rm = TRUE)
titanic.all[(is.na(titanic.all$Fare)), "Fare"] <- fare_median
is.na(titanic.all$Fare)
table(is.na(titanic.all$Fare))

#splitting train and test data
titanic.train <- titanic.all[titanic.all$IsTrainSet == TRUE, ]
titanic.test <- titanic.all[titanic.all$IsTrainSet == FALSE, ]

#checking if the split is correct
dim(titanic.test)
dim(titanic.train)

#checking if the data is properly loaded
tail(titanic.train)
tail(titanic.test)

#checking the structures of all variables
str(titanic.all) 

#converting the required variables as factors
titanic.all$Pclass <- as.factor(titanic.all$Pclass)
titanic.all$Sex <- as.factor(titanic.all$Sex)
titanic.all$Embarked <- as.factor(titanic.all$Embarked)

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
write.csv(outputDF, file = "TitanicKaggle_daksha.csv", row.names = FALSE)

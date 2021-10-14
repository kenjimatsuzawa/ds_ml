training <- read.csv("./pml-training.csv")
testing <- read.csv("./pml-testing.csv")

training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

dim(training)
dim(testing)

head(testing)
head(training)


# based on the analysis on testing data set, anyway these fields are missed, then remove columns from training and testing
colSums(is.na(testing))
nacolmns <- names(testing[,colSums(is.na(testing)) == 20])

training <- training[, !(colnames(training) %in% nacolmns)]
testing <- testing[, !(colnames(testing) %in% nacolmns)]
dim(testing)
dim(training)

#confirmed no nas in the columns left
colSums(is.na(training))
colSums(is.na(testing))

# remove useless information
table(testing$new_window)
training <- training[, !(colnames(training) %in% c("X", "new_window", "problem_id"))]
testing <- testing[, !(colnames(testing) %in% c("X", "new_window", "problem_id"))]
dim(testing)
dim(training)
str(training)
str(testing)
training$cvtd_timestamp <- as.POSIXct(training$cvtd_timestamp)
testing$cvtd_timestamp <- as.POSIXct(testing$cvtd_timestamp)
str(training)
str(testing)
dim(testing)
dim(training)

head(training)
cor(training)
# explatory data analysis
table(training$classe, training$user_name)

library(caret)
set.seed(999)
# sprint training data to training and validation
inTrain <- createDataPartition(y = training$classe, p=0.7, list=F)
df_train <- training[inTrain, ]
df_val <- training[-inTrain, ]

# initial model with random forest
modOrg <- train(classe ~ ., method="rf", data=df_train)
predrf <- predict(modOrg, df_val)
confusionMatrix(as.factor(predrf), as.factor(df_val$classe))
varImp(modOrg$finalModel)
print(modOrg)
print(modOrg$finalModel)

# prediction for testing data set
predrftest <- predict(modOrg, testing)
predrftest

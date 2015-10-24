

# You should create a report describing how you built your model, how you used cross validation, 
# what you think the expected out of sample error is, 
# and why you made the choices you did.


#Loading the packages needed
library(caret)
library(randomForest)
library(doParallel)
library(foreach)
library(e1071)

#Read the data and transform #DIV/0! in NA cases.
pml.training <- read.csv("pml-training.csv", na.strings=c("#DIV/0!") )
pml.testing <- read.csv("pml-testing.csv", na.strings=c("#DIV/0!") )

#Cleaning data
#Transforming some columns in numeric and excluding features without values.
for(i in c(8:ncol(pml.training)-1)) {pml.training[,i] = as.numeric(as.character(pml.training[,i]))}
for(i in c(8:ncol(pml.testing)-1)) {pml.testing[,i] = as.numeric(as.character(pml.testing[,i]))}

feature_set <- colnames(pml.training[colSums(is.na(pml.training)) == 0])[-(1:7)]
model_data <- pml.training[feature_set]
feature_set <- colnames(pml.testing[colSums(is.na(pml.testing)) == 0])[-(1:7)]
model_data_TEST <- pml.testing[feature_set]

#Data Validation
#Split the training dataset into train and test datasets.The data was partioned by the "classe" variable to ensure
# the training set and test set contain examples of each class. 60% of the training data was allocated to the 
# training set and the remainder for the validation set.
rows <- createDataPartition(y=model_data$classe, p=0.6, list=FALSE )
train<- model_data[rows,]
test <- model_data[-rows,]


# Seven random forests with 150 trees each were built. The parallel processing were used to make it faster.

registerDoParallel()
x <- train[-ncol(train)]
y <- train$classe

rf <- foreach(ntree=rep(150, 6), .combine=randomForest::combine, .packages='randomForest') %dopar% {
        randomForest(x, y, ntree=ntree) 
}

# Provide error reports for both training and test data

predictions1 <- predict(rf, newdata=train)
confusionMatrix(predictions1,train$classe)

predictions2 <- predict(rf, newdata=test)
confusionMatrix(predictions2,test$classe)


#Conclusions

# So, as can be seen from the confusion matrix, this model is very accurate.


# Coursera provided code:

answers <- predict(rf, newdata=model_data_TEST)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers)




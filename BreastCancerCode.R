# Assign path to filepath variable
filepath <- "C:/Users/DELL/Desktop/Spring 2021/T and W analytics/breast-cancer-wisconsin.csv"

# Load the CSV file from the local path
dataset <- read.csv(filepath, header=TRUE,fileEncoding="UTF-8-BOM")

# Change attribute Group from character to factor
dataset[, 'Group'] <- as.factor(dataset[, 'Group'])

# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# summarize attribute distributions
summary(dataset)

# take a peek at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(dataset$Group)

# percentage distribution of classes
percentage <- prop.table(table(dataset$Group)) * 100
cbind(freq=table(dataset$Group), percentage=percentage)

# summarize attribute distributions
summary(dataset)

# move all all 9 inputs into a dataframe
df<−data.frame(dataset$Thickness,dataset$Cell.size,dataset$Cell.shape,dataset$Adhesion,dataset$Single.cell.size,dataset$Nuclei,dataset$Chromatin,dataset$Nucleoli,dataset$Mitoses)

# change the datatype to numeric for all inputs
df<−lapply(df,as.numeric)

# list types for each attribute
sapply(df, class)

# split input and output
x <- df[1:9]
y <- dataset[,10]

# box plot for classes
par(mfrow=c(1,9))
for(i in 1:9) {
  boxplot(x, main=names(df)[i])
}

plot(y)

library(ellipse)
library(caret)
library(e1071)
library(rattle)

# ellipse for input & output
featurePlot(x=x, y=y, plot="ellipse")

# boxplot for input & output
featurePlot(x=x, y=y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Group, p=0.80, list=FALSE)

# select 20% of the data for validation
test <- dataset[-validation_index,]

# use the remaining 80% of data to training and testing the models
train <- dataset[validation_index,]

# Test the Harness- Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Build   Model
set.seed(7)
fit.cart <- train(Group~., data=train, method="rpart", metric=metric, trControl=control)

# summarize   Model
print(fit.cart)

# estimate skill of CART on the test dataset
predictions <- predict(fit.cart, test)
confusionMatrix(predictions, as.factor(test$Group))

#plot graph for CART
fancyRpartPlot(fit.cart$finalModel)

# Build   Model
set.seed(7)
fit.lda <- train(Group~., data=train, method="lda", metric=metric, trControl=control)

# summarize   Model
print(fit.lda)

# estimate skill of CART on the test dataset
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, as.factor(test$Group))


# Build   Model
set.seed(7)
fit.knn <- train(Group~., data=train, method="knn", metric=metric, trControl=control)

# summarize   Model
print(fit.knn)

# estimate skill of CART on the test dataset
predictions <- predict(fit.knn, test)
confusionMatrix(predictions, as.factor(test$Group))

#plot graph for KNN
plot(fit.knn)

# Build   Model
set.seed(7)
fit.svm <- train(Group~., data=train, method="svmRadial", metric=metric, trControl=control)

# summarize   Model
print(fit.svm)

# estimate skill of CART on the test dataset
predictions <- predict(fit.svm, test)
confusionMatrix(predictions, as.factor(test$Group))

#plot graph for SVM
plot(fit.svm)

# Build   Model
set.seed(7)
fit.rf <- train(Group~., data=train, method="rf", metric=metric, trControl=control)

# summarize   Model
print(fit.rf)

#plot graph for RF
plot(fit.rf)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.rf)

# estimate skill of RF on the test dataset
predictions <- predict(fit.rf, test)
confusionMatrix(predictions, as.factor(test$Group))
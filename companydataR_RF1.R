


install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
install.packages("rpart")
library(rpart.plot) # use prp() to make cleaner plot with caret
install.packages("ISLR")
library(ISLR)
library(randomForest)
company_data <- read.csv(file.choose())
View(company_data)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
colnames(company_data)

# Change Sales to a qualitative variable by splitting it on the median.
company_data$Sales <- ifelse(company_data$Sales <= median(company_data$Sales), 'Low', 'High')
company_data$Sales <- factor(company_data$Sales)
View(company_data)

company_data<-na.omit(company_data)

library(caret)
#Split data into train / validation
set.seed(111)
split <- createDataPartition(y=company_data$Sales, p=0.6, list=FALSE)
train <- company_data[split,]
test <- company_data[-split,]

# Building a random forest model on training data 
fit.forest <- randomForest(Sales~.,data= train, na.action=na.roughfix,importance=TRUE)

# Training accuracy 
mean(train$Sales==predict(fit.forest,train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,train)
library(caret)


# Confusion Matrix
confusionMatrix(train$Sales, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=test)
mean(pred_test==test$Sales) # Accuracy = 78.6 % 


# Confusion Matrix 

confusionMatrix(test$Sales, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=2:11,cex=0.8,fill=2:11)


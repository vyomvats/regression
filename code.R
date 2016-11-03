library(kernlab)
library(caTools)

#setting randome seed so that results are reproducible on other systems
set.seed(123)

#reading the data
data = read.csv("C:\\Users\\Vyom\\Desktop\\credit_card_data.csv", header=F)

#splitting the data into test and train subsets with 80% of the data in training set
split = sample.split(data$V11, SplitRatio = 0.8)
train = subset(data, split==TRUE)
test = subset(data, split==FALSE)

# creating a function called check_accuracy that can calculate accuracy of
# the svm model for different values of C
check_accuracy = function(X){
  model_obj = ksvm(V11~., data = train, type = "C-svc", scaled=T, C=X)
  predicted <- predict(model_obj, test[,1:10])
  cf = table(test[,11], predicted)
  accuracy = (cf[1,1]+cf[2,2])/nrow(test)
  return(accuracy)
}

# initializing a vector called accuracy_values to store accuracy values from
# check_accuracy function
accuracy_values = vector(mode="numeric", length=0)

# finding value of C for which classification accuracy is maximum
for (X in 1:100){
  accuracy_values[X] = check_accuracy(X)
}
C_max = which.max(accuracy_values)

# preparing SVM model using this value
model_obj = ksvm(V11~., data = train, type = "C-svc", scaled=T, C=C_max)
print (model_obj)
predicted <- predict(model_obj,test[,1:10])

# preparing a confusion matrix to quantify accuracy
table(test[,11], predicted)
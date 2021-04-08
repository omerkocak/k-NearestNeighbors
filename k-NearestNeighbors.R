#KNN

library(ISLR)
library(class)
set.seed(4985912) #Set the seed for reproducibility
dataset_sample <- sample(1:nrow(Smarket), size=nrow(Smarket)*0.6) 
dataset_train <- Smarket[dataset_sample, ] #Select the 60% of rows
dataset_test <- Smarket[-dataset_sample, ] #Select the 40% of rows

comb1_acc <- numeric()
comb2_acc <- numeric()
comb3_acc <- numeric()


for(k in 1:30){
  #comb1
  comb1_predict <- knn(train=dataset_train[,c(2,3,4)], test=dataset_test[,c(2,3,4)], cl=dataset_train$Direction, k=k)
  comb1_acc <- c(comb1_acc, mean(comb1_predict==dataset_test$Direction))
  
  #comb2
  comb2_predict <- knn(train=dataset_train[,c(4,5,7)], test=dataset_test[,c(4,5,7)], cl=dataset_train$Direction, k=k)
  comb2_acc <- c(comb2_acc, mean(comb2_predict==dataset_test$Direction))
  
  #comb3
  comb3_predict <- knn(train=dataset_train[,c(3,6,7)], test=dataset_test[,c(3,6,7)], cl=dataset_train$Direction, k=k)
  comb3_acc <- c(comb3_acc, mean(comb3_predict==dataset_test$Direction))

}

my_acc <- numeric()

for(k in 1:30){
  #my combination
  my_predict <- knn(train=dataset_train[,c(1,7,8)], test=dataset_test[,c(1,7,8)], cl=dataset_train$Direction, k=k)
  my_acc <- c(my_acc, mean(my_predict==dataset_test$Direction))
}

# k value and highest accuary for my combination
which(my_acc==max(my_acc))
max(my_acc)



# k value and highest accuary for combination 1
which(comb1_acc==max(comb1_acc))
max(comb1_acc)

# k value and highest accuary for combination 2
which(comb2_acc==max(comb2_acc))
max(comb2_acc)

# k value and highest accuary for combination 3
which(comb3_acc==max(comb3_acc))
max(comb3_acc)


plot(comb1_acc, type="l", ylab="Trurth Rate",  xlab="K value", main="Accuracy changes with respect to k values for Combination 1")
plot(comb2_acc, type="l", ylab="Trurth Rate",  xlab="K value", main="Accuracy changes with respect to k values for Combination 2")
plot(comb3_acc, type="l", ylab="Trurth Rate",  xlab="K value", main="Accuracy changes with respect to k values for Combination 3")
plot(my_acc, type="l", ylab="Trurth Rate",  xlab="K value", main="Accuracy changes with respect to k values for my combination (Year,Volume,Today)")
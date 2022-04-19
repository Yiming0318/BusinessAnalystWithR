rm(list=ls())
getwd()
setwd("C:/Users/Gazi/Desktop/NYU_Classes/Fall_2020/Teaching/Complex_Systems_Engineering/software")
getwd()

#inout dataset

dataset <- read.csv('social.csv')

#for text file; use dataset <- read.table('social.txt', header=TRUE)
dataset <- dataset[3:5]
#dataset$Purchased <- as.factor(dataset$Purchased, levels = c(0,1))

#get training and testing data
set.seed(567)
n = nrow(dataset)
index_train_data <- sample(1:n,0.8*n)
training_set <- dataset[index_train_data,]
testing_set <- dataset[-index_train_data,]

#scale data
training_set[-3] <- scale(training_set[-3])

testing_set[-3] <- scale(testing_set[-3])

#develop SVM model

install.packages('e1071')
library(e1071)

classifier = svm(Purchased ~., data=training_set, type = 'C-classification', kernel='linear')

y_pred <- predict(classifier, newdata = testing_set[-3])
confusion_matrix <- table(testing_set[,3],y_pred)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#plot

set <- training_set

x1= seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2= seq(min(set[,2])-1, max(set[,2])+1, by=0.01)

grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[,-3], main='SVM(Training Set)', xlab = 'Age', ylab= 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)),add=TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'coral1','aquamarine'))
points(set,pch=21, bg= ifelse(set[,3]==1, 'green4','red3'))











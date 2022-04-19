rm(list=ls())
getwd()
setwd("C:/Users/Gazi/Desktop/NYU_Classes/Fall_2020/Teaching/Complex_Systems_Engineering/software")
getwd()

#inout dataset

dataset <- read.csv('cereals.csv')

#for text file; use dataset <- read.table('social.txt', header=TRUE)


#get training and testing data
set.seed(567)
n = nrow(dataset)
index_train_data <- sample(1:n,0.8*n)
training_set <- dataset[index_train_data,]
testing_set <- dataset[-index_train_data,]

#scaling
training_set <- scale(training_set)
testing_set <- scale(testing_set)

install.packages("neuralnet")
library(neuralnet)

#neural network model

NN <- neuralnet(rating~., data=training_set, hidden= 3, linear.output = T)

#hidden = 3 represents single layer with 3 neurons

plot(NN)


#testing the model
Predict = compute(NN, testing_set)
Predict$net.result




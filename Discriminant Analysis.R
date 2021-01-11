library(tidyverse)
library(MASS)
library(klaR)
library(heplots)

# Quadratic Discriminant Analysis (QDA)

str(biopsy)
head(biopsy)
dim(biopsy)
data = biopsy[,2:11]
dim(data)
head(data)

hom_test = boxM(Y = data[,1:9], group = data[,10], data = data)
ifelse(hom_test$p.value < 0.05, "The dataset contain heteroscedasticity", "The dataset contain homoscedasticity")

set.seed(5151)
dataset_size = nrow(data)
sample_size = dataset_size * 0.80 
training_sample = sample(1:dataset_size, size = sample_size)
testing_sample = data[-training_sample,]
testing_size = nrow(testing_sample)

train = data[training_sample,]
test = testing_sample[1:70,]
test2 =  testing_sample[71:140,]

fit_1 = qda(formula = class ~ ., data = train)
fit_pred_1 = predict(fit_1, newdata = test, interval = "confidence")
conf_mat_1 = table(fit_pred_1$class, test$class)
accuracy_1 = sum(diag(conf_mat_1)) / sum(conf_mat_1) * 100
accuracy_1

fit_pred_2 = predict(fit_1, newdata = test2, interval = "confidence")
conf_mat_2 = table(fit_pred_2$class, test2$class)
accuracy_2 = sum(diag(conf_mat_2)) / sum(conf_mat_2) * 100
accuracy_2

fit_pred_2$class

fit_pred_2$posterior

partimat(x = train[,1:9], grouping = train[,10],data = train, method = "qda")
partimat(x = test[,1:9], grouping = test[,10],data = test, method = "qda")
partimat(x = test2[,1:9], grouping = test2[,10],data = test2, method = "qda")

# Linear Discriminant Analysis (LDA)

library(biotools)

str(Puromycin)
data = Puromycin

hom_test = boxM(data = data[,1:2], grouping = data[,3])
ifelse(hom_test$p.value < 0.05, "The dataset contain heteroscedasticity", "The dataset contain homoscedasticity")

fit_lda = lda(formula = state ~ ., data = data)
fit_pred_lda = predict(fit_lda)
conf_mat = table(fit_pred_lda$class, data$state)
accuracy = sum(diag(conf_mat)) / sum(conf_mat) * 100
accuracy
var(fit_pred_lda$posterior)
var(fit_pred_2$posterior)

plot(fit_lda)
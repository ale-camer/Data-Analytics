library(Ecdat)
library(dplyr)

data = na.omit(Mroz)
data = data[,-17]
data = as.data.frame(data)
# data = data %>% mutate(work = ifelse(work == "yes", 1L, 0L))  

set.seed(12121)
dataset_size = nrow(data)
sample_size = dataset_size * 0.80 
training_sample = sample(x = 1:dataset_size, size = sample_size)
testing_sample = data[-training_sample,]

train = data[training_sample,]
test = testing_sample[1:76,]
test2 =  testing_sample[77:151,]

model = glm(formula = work ~ ., data = train, family = "gaussian")
summary(model)
prob = model %>% predict(test, type = "response")
pred = ifelse(prob >= 0.50, "yes", "no")
acc_1 = mean(pred == test$data.work)
acc_1

prob_2 = model %>% predict(test2, type = "response")
pred_2 = ifelse(prob_2 >= 0.50, "yes", "no")
acc_2 = mean(pred_2 == test2$data.work)
acc_2

par(mfrow = c(2,2))
plot(model)

par(mfrow = c(,2))
Epi::ROC(data = data1, form = data.work ~ data.educw + data.experience + data.wageh)

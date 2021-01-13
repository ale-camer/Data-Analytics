library(Ecdat)
library(dplyr)

data = na.omit(Fishing)
head(data)
str(data)
dim(data)

data_pca = princomp(data[,2:11], scores = T, cor = T)
pca_sum = summary(data_pca)
var_prop = data_pca$sdev^2 / sum(data_pca$sdev^2)
var_df = data.frame(Dimensions = 1:length(var_prop), varExp = var_prop)
cum_var_prop = cumsum(var_prop)
cum_var_df = data.frame(Dimensions = 1:length(var_prop), varExp = cum_var_prop)
cum_var_df

draft = cum_var_df %>% subset(varExp <= 0.80 & max(Dimensions)) %>% tail(n = 1) # dimensiones to retain
ndim = as.numeric(draft[1,1])

library(factoextra)
data_pca$loadings[,1:3]
fviz_pca_biplot(data_pca, geom.ind = "point", col.ind = ndim, palette = c("#00AFBB", "#E7B800", "#FC4E07"))

data1 = data.frame(data$mode, data$catch, data$cbeach, data$ppier, data$income)

dataset_size = nrow(data)
sample_size = dataset_size * 0.8
training_sample = data1[1:sample_size,]
head(training_sample)

fit = lm(formula = data.income ~ ., data = training_sample)
fit_sum = summary(fit)
p_values = fit_sum$coefficients[,4] <= 0.05

fit_2 = lm(formula = data.income ~ data.mode + data.cbeach + data.ppier, data = training_sample)
fit_sum2 = summary(fit_2)
p_values2 = fit_sum2$coefficients[,4] <= 0.05

dim(training_sample)
testing_data = data1[946:1182,]
dim(testing_data)
head(testing_sample1) = testing_data[1:119,]
testing_sample2 = testing_data[120:237,]

preds_1 = predict(fit_2, newdata = testing_sample1)
RSME_1 = sqrt(mean((testing_sample1$data.income - preds_1)^2))
RMSRE_1 = sqrt(mean(((testing_sample1$data.income - preds_1) / testing_sample1$data.income)^2))

preds_2 = predict(fit_2, newdata = testing_sample2)
RSME_2 = sqrt(mean((testing_sample2$data.income - preds_2)^2))
RMSRE_2 = sqrt(mean(((testing_sample2$data.income - preds_2) / testing_sample2$data.income)^2))

RSME_1
RMSRE_1
RSME_2
RMSRE_2

# Assumptions

library(car)
library(lmtest)

vif(fit_2) # multicolineality

normtest::ajb.norm.test(fit_2$residuals, nrepl = 1000) # normality
fBasics::jarqueberaTest(fit_2$residuals)
stats::shapiro.test(fit_2$residuals)
nortest::lillie.test(fit_2$residuals)
hist(fit_2$residuals)
abline(v = mean(fit_2$residuals), col = "red")

lmtest::bptest(fit_2) #heteroscedasticity
lmtest::gqtest(fit_2)
par(mfrow = c(2,2))
plot(fit_2)

crPlots(fit_2) # lineality

durbinWatsonTest(fit_2) # independence

outlierTest(fit_2) # outliers

avPlots(fit_2, ask=F, id.method="identify") # influential observations
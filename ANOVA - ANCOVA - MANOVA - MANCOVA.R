data("iris")
head(iris)

# one-way ANOVA

model1 = lm(Sepal.Length ~ Species, data = iris)
model2 = lm(Sepal.Width ~ Species, data = iris)
model3 = lm(Petal.Length ~ Species, data = iris)
model4 = lm(Petal.Width ~ Species, data = iris)
anova1 = anova(model1)
anova2 = anova(model2)
anova3 = anova(model3)
anova4 = anova(model4)
ifelse(anova1$`Pr(>F)`[1] <= 0.05, paste(model1$terms[[2]], "  is significant"), paste(model1$terms[[2]], "  is not significant"))
ifelse(anova2$`Pr(>F)`[1] <= 0.05, paste(model2$terms[[2]], "  is significant"), paste(model1$terms[[2]], "  is not significant"))
ifelse(anova3$`Pr(>F)`[1] <= 0.05, paste(model3$terms[[2]], "  is significant"), paste(model1$terms[[2]], "  is not significant"))
ifelse(anova4$`Pr(>F)`[1] <= 0.05, paste(model4$terms[[2]], "  is significant"), paste(model1$terms[[2]], "  is not significant"))

mean_comp1 = TukeyHSD(aov(model1))
mean_comp2 = TukeyHSD(aov(model2))
mean_comp3 = TukeyHSD(aov(model3))
mean_comp4 = TukeyHSD(aov(model4))
ifelse(mean_comp1$Species[,4] <= 0.05, "the difference between these groups is significant", "the difference between these groups is not significant")
ifelse(mean_comp2$Species[,4] <= 0.05, "the difference between these groups is significant", "the difference between these groups is not significant")
ifelse(mean_comp3$Species[,4] <= 0.05, "the difference between these groups is significant", "the difference between these groups is not significant")
ifelse(mean_comp4$Species[,4] <= 0.05, "the difference between these groups is significant", "the difference between these groups is not significant")
plot(mean_comp1)
plot(mean_comp2)
plot(mean_comp3)
plot(mean_comp4)

# two-way ANOVA

model11 = aov(Sepal.Width ~ Petal.Length * Species, data = iris)
model12 = aov(Sepal.Width ~ Petal.Length * Sepal.Length, data = iris)
model13 = aov(Sepal.Width ~ Petal.Length * Petal.Width, data = iris)
model14 = aov(Sepal.Width ~ Petal.Width * Species, data = iris)
model15 = aov(Sepal.Width ~ Petal.Width * Sepal.Length, data = iris)
model16 = aov(Sepal.Width ~ Sepal.Length * Species, data = iris)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)

library(AICcmodavg)

cand.set = list(model11, model12, model13, model14, model15, model16)
modnames = c("model11", "model12", "model13", "model14", "model15", "model16")

aictab(cand.set = cand.set, modnames = modnames) # best-fit model

# assumptions

test1 = shapiro.test(model1$residuals) # normality
test2 = shapiro.test(model2$residuals)
test3 = shapiro.test(model3$residuals)
test4 = shapiro.test(model4$residuals)

library(car)

test_var1 = leveneTest(y = iris$Sepal.Length, group = iris$Species) # homoscedasticity
test_var2 = leveneTest(y = iris$Sepal.Width, group = iris$Species)
test_var3 = leveneTest(y = iris$Petal.Length, group = iris$Species)
test_var4 = leveneTest(y = iris$Petal.Width, group = iris$Species)
ifelse(test_var1$`Pr(>F)`[1] >= 0.05, paste(model1$terms[[2]], "  there is homoscedasticity"), paste(model1$terms[[2]], "  there is heteroscedasticity"))
ifelse(test_var2$`Pr(>F)`[1] >= 0.05, paste(model1$terms[[2]], "  there is homoscedasticity"), paste(model1$terms[[2]], "  there is heteroscedasticity"))
ifelse(test_var3$`Pr(>F)`[1] >= 0.05, paste(model1$terms[[2]], "  there is homoscedasticity"), paste(model1$terms[[2]], "  there is heteroscedasticity"))
ifelse(test_var4$`Pr(>F)`[1] >= 0.05, paste(model1$terms[[2]], "  there is homoscedasticity"), paste(model1$terms[[2]], "  there is heteroscedasticity"))

test_var11 = bf.test(Sepal.Length ~ Species, data = iris)
test_var21 = bf.test(Sepal.Width ~ Species, data = iris)
test_var31 = bf.test(Petal.Length ~ Species, data = iris)
test_var41 = bf.test(Petal.Width ~ Species, data = iris)
ifelse(test_var11$p.value <= 0.05, paste(model1$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var21$p.value <= 0.05, paste(model2$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var31$p.value <= 0.05, paste(model3$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var41$p.value <= 0.05, paste(model4$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))

test_var111 = welch.test(Sepal.Length ~ Species, data = iris)
test_var211 = welch.test(Sepal.Width ~ Species, data = iris)
test_var311 = welch.test(Petal.Length ~ Species, data = iris)
test_var411 = welch.test(Petal.Width ~ Species, data = iris)
ifelse(test_var111$p.value <= 0.05, paste(model1$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var211$p.value <= 0.05, paste(model2$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var311$p.value <= 0.05, paste(model3$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))
ifelse(test_var411$p.value <= 0.05, paste(model4$terms[[2]], "  there is heteroscedasticity"), paste(model1$terms[[2]], "  there is homoscedasticity"))

# if assumptions are not met

library(onewaytests)

test11 = kw.test(Sepal.Length ~ Species, data = iris) 
test21 = kw.test(Sepal.Width ~ Species, data = iris)
test31 = kw.test(Petal.Length ~ Species, data = iris)
test41 = kw.test(Petal.Width ~ Species, data = iris)
ifelse(test1$p.value <= 0.05, paste(model1$terms[[2]], " has a normal distribution"), ifelse(test11$p.value <= 0.05, paste(model1$terms[[2]], " has a normal distribution"), paste(model1$terms[[2]], " doesn't have a normal distribution")))
ifelse(test1$p.value <= 0.05, paste(model1$terms[[2]], " has a normal distribution"), ifelse(test21$p.value <= 0.05, paste(model2$terms[[2]], " has a normal distribution"), paste(model1$terms[[2]], " doesn't have a normal distribution")))
ifelse(test1$p.value <= 0.05, paste(model1$terms[[2]], " has a normal distribution"), ifelse(test31$p.value <= 0.05, paste(model3$terms[[2]], " has a normal distribution"), paste(model1$terms[[2]], " doesn't have a normal distribution")))
ifelse(test1$p.value <= 0.05, paste(model1$terms[[2]], " has a normal distribution"), ifelse(test41$p.value <= 0.05, paste(model4$terms[[2]], " has a normal distribution"), paste(model1$terms[[2]], " doesn't have a normal distribution")))

# ANCOVA

data(mtcars)
str(mtcars)

model1 = aov(mpg ~ cyl + disp, data = mtcars)
model2 = aov(mpg ~ cyl * disp, data = mtcars)
summary(model1)
summary(model2)

result = anova(model1,model2)
ifelse(result$`Pr(>F)`[2] <= 0.05, paste("the interaction between the variables is significant"), paste("the interaction between the variables is not significant"))

# one-way MANOVA

data(iris3)
head(iris3)

# assumptions 

library(mvnormtest)

group1 = iris3[, , 1] # multivariate normality
group2 = iris3[, , 2]
group3 = iris3[, , 3]
group1_t = t(group1)
group2_t = t(group2)
group3_t = t(group3)
group1_test = mshapiro.test(group1_t) # if p.value < 0.05 then log(data) and finally mshapiro.test(data) 
group2_test = mshapiro.test(group2_t)
group3_test = mshapiro.test(group3_t)
ifelse(group1_test$p.value <= 0.05, "it doesn't have multivariate normality", "it has multivariate normality")
ifelse(group2_test$p.value <= 0.05, "it doesn't have multivariate normality", "it has multivariate normality")
ifelse(group3_test$p.value <= 0.05, "it doesn't have multivariate normality", "it has multivariate normality")

library(biotools)

setosa = data.frame(iris3[, , 1], as.factor(rep(x = "setosa", times = nrow(iris3[, , 1]))))
versicolor = data.frame(iris3[, , 2], as.factor(rep(x = "versicolor", times = nrow(iris3[, , 2]))))
virginica = data.frame(iris3[, , 3], as.factor(rep(x = "virginica", times = nrow(iris3[, , 3]))))
colnames(setosa) = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
colnames(versicolor) = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
colnames(virginica) = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris4 = rbind(setosa, versicolor, virginica)

hom_test = boxM(data = iris4[1:4], iris4[,5]) # homoscedasticity 
ifelse(hom_test$p.value <= 0.05, "there is heteroscedasticity", "there is homoscedasticity")

library(psych)

r = cor(iris4[,1:4]) # Bartlett's test of sphericity (cor_matrix != identity_matrix)
bar_test = cortest.bartlett(r, n = ncol(iris4))
ifelse(bar_test$p.value <= 0.05, "there is correlation among variables", "there is no correlation among variables")

iris_manova = manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris4)

library(heplots)

wilks_test = summary(iris_manova, test = "Wilks", type = "II")
pillai_test = summary(iris_manova, test = "Pillai", type = "II")
HLT_test = summary(iris_manova, test = "Hotelling-Lawley", type = "II")
roy_test = summary(iris_manova, test = "Roy", type = "II")

etasq(iris_manova, test = "Wilks", partial = T)
etasq(iris_manova, test = "Pillai", partial = T)
etasq(iris_manova, test = "Hotelling-Lawley", partial = T)

etasq(iris_manova, test = "Wilks", partial = F)
etasq(iris_manova, test = "Pillai", partial = F)
etasq(iris_manova, test = "Hotelling-Lawley", partial = F)

# two-way MANOVA

library(MASS)

iris_manova2 = lm(cbind(Sepal.Length, Sepal.Width, Petal.Length) ~ Petal.Width * Species, data = iris4)

man1_type2 = Manova(iris_manova2, multivariate = T, type = c("II"), test = ("Wilks")) # if p.value < 0.05 is statistically significant
man2_type2 = Manova(iris_manova2, multivariate = T, type = c("II"), test = ("Pillai"))
man3_type2 = Manova(iris_manova2, multivariate = T, type = c("II"), test = ("Hotelling-Lawley"))
man4_type2 = Manova(iris_manova2, multivariate = T, type = c("II"), test = ("Roy"))

# man1_type3 = Manova(iris_manova2, multivariate = T, type = c("III"), test = ("Wilks"))
# man2_type3 = Manova(iris_manova2, multivariate = T, type = c("III"), test = ("Pillai")) 
# man3_type3 = Manova(iris_manova2, multivariate = T, type = c("III"), test = ("Hotelling-Lawley"))
# man4_type3 = Manova(iris_manova2, multivariate = T, type = c("III"), test = ("Roy"))

etasq(iris_manova2, test = "Wilks", partial = T) # effect size
etasq(iris_manova2, test = "Pillai", partial = T)
etasq(iris_manova2, test = "Hotelling-Lawley", partial = T)

etasq(iris_manova2, test = "Wilks", partial = F)
etasq(iris_manova2, test = "Pillai", partial = F)
etasq(iris_manova2, test = "Hotelling-Lawley", partial = F)

# MANCOVA

library(effects)

iris_manova3 = manova(Sepal.Length ~ Petal.Width + Species + Petal.Width * Species, data = iris4)

wilks_test3 = summary(iris_manova3, test = "Wilks", type = "III") # if p.value < 0.05 is statistically significant
pillai_test3 = summary(iris_manova3, test = "Pillai", type = "III")
HLT_test3 = summary(iris_manova3, test = "Hotelling-Lawley", type = "III")
roy_test3 = summary(iris_manova3, test = "Roy", type = "III")

describeBy(iris4, iris4$Species)

factor(iris4$Species)

modelA = aov(Sepal.Length ~ Petal.Width + Species, data = iris4) # dependent variable
summary(modelA, type = "III") # if p.value < 0.05 is statistically significant influencing the outcome variable

adj_meansA = effect("Species", modelA, se = T, xlevels = 2) # compare mean vectors for each group
summary(adj_meansA)
adj_meansA$se

modelB = aov(Sepal.Width ~ Petal.Width + Species, data = iris4)
summary(modelB, type = "III")

adj_meansB = effect("Species", modelB, se = T, xlevels = 2)
summary(adj_meansB)
adj_meansB$se

modelC = aov(Petal.Length ~ Petal.Width + Species, data = iris4)
summary(modelC, type = "III")

adj_meansC = effect("Species", modelC, se = T, xlevels = 2)
summary(adj_meansC)
adj_meansC$se

# shorter path for MANCOVA

library(jmv)

mancova(data = iris4,
        deps = vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
        factors = Species)


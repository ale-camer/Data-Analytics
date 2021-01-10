library(psych)
data = bfi[,1:25]
data = na.omit(data)

# Exploratory Factorial Analysis (EFA)

library(paran)
library(dplyr)

R = cor(data)
cor_test = cortest.bartlett(R, n = nrow(data))
ifelse(cor_test$p.value <= 0.05, "There are underlying factors in this matrix", "There aren't underlying factors in this matrix")

kmo_test = KMO(R) 
scores = data.frame(kmo_test$MSAi)
colnames(scores) = "KMO"
cols = rownames(scores %>% filter(KMO > 0.7))
data1 = data %>% select(cols) 

r = fa.parallel(data1, fm = "ml", n.obs = nrow(data1))
nfact = r$nfact

s = paran(data1, n = nrow(data1))
nfact2 = s$Retained

fit = fa(data1, nfactors = nfact, fm = "pa", rotate = "none", n.obs = nrow(data1), scores="regression")
fit2 = fa(data1, nfactors = nfact, fm = "pa", rotate = "equimax", n.obs = nrow(data1), scores="regression") 
fit3 = fa(data1, nfactors = nfact, fm = "pa", rotate = "equimin", n.obs = nrow(data1), scores="regression") 
fit4 = fa(data1, nfactors = nfact, fm = "pa", rotate = "verimax", n.obs = nrow(data1), scores="regression") 
fit5 = fa(data1, nfactors = nfact, fm = "pa", rotate = "varimin", n.obs = nrow(data1), scores="regression") 
fit6 = fa(data1, nfactors = nfact, fm = "pa", rotate = "quartimax", n.obs = nrow(data1), scores="regression") 
fit7 = fa(data1, nfactors = nfact, fm = "pa", rotate = "quartimin", n.obs = nrow(data1), scores="regression") 
fit8 = fa(data1, nfactors = nfact, fm = "pa", rotate = "biquartimax", n.obs = nrow(data1), scores="regression") 
fit9 = fa(data1, nfactors = nfact, fm = "pa", rotate = "biquartimin", n.obs = nrow(data1), scores="regression") 
fit10 = fa(data1, nfactors = nfact, fm = "ml", rotate = "none", n.obs = nrow(data1), scores="regression")
fit11 = fa(data1, nfactors = nfact, fm = "ml", rotate = "equimax", n.obs = nrow(data1), scores="regression") 
fit12 = fa(data1, nfactors = nfact, fm = "ml", rotate = "equimin", n.obs = nrow(data1), scores="regression") 
fit13 = fa(data1, nfactors = nfact, fm = "ml", rotate = "verimax", n.obs = nrow(data1), scores="regression") 
fit14 = fa(data1, nfactors = nfact, fm = "ml", rotate = "varimin", n.obs = nrow(data1), scores="regression") 
fit15 = fa(data1, nfactors = nfact, fm = "ml", rotate = "quartimax", n.obs = nrow(data1), scores="regression") 
fit16 = fa(data1, nfactors = nfact, fm = "ml", rotate = "quartimin", n.obs = nrow(data1), scores="regression") 
fit17 = fa(data1, nfactors = nfact, fm = "ml", rotate = "biquartimax", n.obs = nrow(data1), scores="regression") 

library(lavaan)
library(parameters)
library(performance)

structure1 = fit %>% efa_to_cfa()
structure2 = fit2 %>% efa_to_cfa()
structure3 = fit3 %>% efa_to_cfa()
structure4 = fit4 %>% efa_to_cfa()
structure5 = fit5 %>% efa_to_cfa()
structure6 = fit6 %>% efa_to_cfa()
structure7 = fit7 %>% efa_to_cfa()
structure8 = fit8 %>% efa_to_cfa()
structure9 = fit9 %>% efa_to_cfa()
structure10 = fit10 %>% efa_to_cfa()
structure11 = fit11 %>% efa_to_cfa()
structure12 = fit12 %>% efa_to_cfa()
structure13 = fit13 %>% efa_to_cfa()
structure14 = fit14 %>% efa_to_cfa()
structure15 = fit15 %>% efa_to_cfa()
structure16 = fit16 %>% efa_to_cfa()
structure17 = fit17 %>% efa_to_cfa()

fit_cfa1 = cfa(structure1, data = data1)
fit_cfa2 = cfa(structure2, data = data1)
fit_cfa3 = cfa(structure3, data = data1)
fit_cfa4 = cfa(structure4, data = data1)
fit_cfa5 = cfa(structure5, data = data1)
fit_cfa6 = cfa(structure6, data = data1)
fit_cfa7 = cfa(structure7, data = data1)
fit_cfa8 = cfa(structure8, data = data1)
fit_cfa9 = cfa(structure9, data = data1)
fit_cfa10 = cfa(structure10, data = data1)
fit_cfa11 = cfa(structure11, data = data1)
fit_cfa12 = cfa(structure12, data = data1)
fit_cfa13 = cfa(structure13, data = data1)
fit_cfa14 = cfa(structure14, data = data1)
fit_cfa15 = cfa(structure15, data = data1)
fit_cfa16 = cfa(structure16, data = data1)
fit_cfa17 = cfa(structure17, data = data1)

perf = compare_performance(fit_cfa5, fit_cfa6, fit_cfa7, fit_cfa9, fit_cfa14, fit_cfa15, fit_cfa16)
perf_AIC = data.frame(perf$Model, round(sqrt(perf$AIC),2))
colnames(perf_AIC) = c("Model", "AIC Score")
perf_BIC = data.frame(perf$Model, round(sqrt(perf$BIC),2))
colnames(perf_BIC) = c("Model", "BIC Score")

perf_AIC[which.min(perf_AIC$`AIC Score`),][1]
perf_BIC[which.min(perf_BIC$`BIC Score`),][1]

fa_scores = data.frame(fit6$scores)
head(fa_scores)

library(NbClust)
library(ggplot2)

fa_scores_k = kmeans(fa_scores, nfact)

pairs(fa_scores,col=fa_scores_k$cluster)

library(semPlot)

semPaths(fit_cfa6, what="est", layout="circle", title=T, style="lisrel") 

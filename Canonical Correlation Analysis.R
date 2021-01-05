library(CCA)

data(USJudgeRatings)

first_table = USJudgeRatings[,1:4]
second_table = USJudgeRatings[,5:12]

correl = matcor(first_table, second_table) # correlations
correl$Xcor
correl$Ycor
correl$XYcor
cormat = img.matcor(correl, type = 2)
cormat
img.matcor(correl, type = 2)

can_cor = cc(first_table, second_table)
can_cor[1] # canonical correlations (correlation between tables)
can_cor[3:4] # raw canonical correlations (read the same way as the slope in a regression analysis)
can_cor$scores[3:6] # canonical loadings (correlations between observed variables and canonical variates, read the same way as in factor analysis)
plt.cc(can_cor, var.label = T)

library(CCP)

rho = can_cor$cor
n = nrow(first_table)
p = ncol(first_table)
q = ncol(second_table)

p.asym(rho, n, p, q, tstat = "Wilks") # if p.value < 0.05 the dimension is statistically significant
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")

# standardized canonical coefficients (read the same way as standardized regression coefficients)
stand_can_coef1 <- diag(sqrt(diag(cov(first_table)))) # standardized psych canonical coefficients diagonal matrix of psych sd's
stand_can_coef1 %*% can_cor$xcoef
stand_can_coef2 <- diag(sqrt(diag(cov(second_table)))) # standardized acad canonical coefficients diagonal matrix of acad sd's
stand_can_coef2 %*% can_cor$ycoef

# when standard deviations among variables are very different, it's best practice to analyse standardized canonical coefficients
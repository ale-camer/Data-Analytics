library(lpSolveAPI)

# Investment (miximization case)

portfolio = make.lp(nrow = 0, ncol = 3) 
name.lp(portfolio, "portfolio")
colnames(portfolio) = c("Stock A", "Stock B", "Stock C")

opt = lp.control(portfolio, simplextype = "primal", sense = "max", pivoting = "steepestedge")
rm(opt)

set.objfn(portfolio, obj = c(.085, .13, .0105))
set.type(portfolio, columns = 1:3, type = "real")

add.constraint(portfolio, xt = c(1,1,1), type = "=", rhs = 10000000)
add.constraint(portfolio, xt = c(1,0,0), type = "<=", rhs = 10000000/4)
add.constraint(portfolio, xt = c(-.5,-.5,1), type = ">=", rhs = 0)
add.constraint(portfolio, xt = c(-.8,.2,0), type = "<=", rhs = 0)
add.constraint(portfolio, xt = c(1,1,1), type = ">=", rhs = 0)

set.bounds(portfolio, lower = rep(0,3), upper = rep(Inf,3), columns = 1:3)
row.names(portfolio) = c("Total Investment", "Stock A", "Stock C", "Stock B", "Non negativity")
portfolio
solve(portfolio)

dec_var = matrix(get.variables(portfolio), 1, 3)
colnames(dec_var) = colnames(portfolio)
row.names(dec_var) = "Optimum Portfolio"
obj_func = get.objective(portfolio)
results = list(problem = portfolio, optimum = obj_func, dec_var = dec_var)
rm(obj_func, dec_var)
results

SensRHS = matrix(NA, nrow(portfolio), 5)
rownames(SensRHS) = rownames(portfolio)
colnames(SensRHS) = c("Clearance", "Shadow Price", "LI rhs", "rhs actual", "LS rhs")
SensRHS[,"Clearance"] = get.rhs(portfolio) - get.constraints(portfolio)
SensRHS[,"Shadow Price"] = get.sensitivity.rhs(portfolio)$duals[1:nrow(portfolio)]
SensRHS[,"rhs actual"] = get.rhs(portfolio)
SensRHS[SensRHS[,"Clearance"]==0,"LI rhs"] = get.sensitivity.rhs(portfolio)$dualsfrom[1:nrow(portfolio)][SensRHS[,"Clearance"]==0]
SensRHS[SensRHS[,"Clearance"]==0,"LS rhs"] = get.sensitivity.rhs(portfolio)$dualstill[1:nrow(portfolio)][SensRHS[,"Clearance"]==0]
SensRHS[SensRHS[,"Clearance"]>0,"LI rhs"] = get.constraints(portfolio)[SensRHS[,"Clearance"]>0]
SensRHS[SensRHS[,"Clearance"]>0,"LS rhs"] = Inf
round(SensRHS,4)

SensObj = matrix(NA, ncol(portfolio), 4)
rownames(SensObj) = colnames(portfolio)
colnames(SensObj) = c("Min.Coef.Obj.", "Coef.Obj.", "Max.Coef.Obj.", "Reduced Price")
for (i in 1:ncol(portfolio)){
  SensObj[i,"Coef.Obj."]=get.column(portfolio,i)$column[1]
}
rm(i)
SensObj[,"Min.Coef.Obj."] = get.sensitivity.obj(portfolio)$objfrom
SensObj[,"Max.Coef.Obj."] = get.sensitivity.obj(portfolio)$objtill
SensObj[,"Reduced Price"] = -get.sensitivity.rhs(portfolio)$duals[(nrow(portfolio)+1):(nrow(portfolio)+ncol(portfolio))]
round(SensObj,4)

# Costs (minimization case)

prod_plan = make.lp(nrow = 0, ncol = 12)
name.lp(prod_plan, "prod_plan")
colnames(prod_plan) = c("x1","x2","x3","x4","x5","x6","l1","l2","l3","l4","l5","l6")

opt = lp.control(prod_plan, simpelxtype = "primal", sense = "min", pivoting = "steepestedge")
rm(opt)

set.objfn(prod_plan, obj = c(50, 45, 55, 48, 52, 50, 8, 8, 8, 8, 8, 8))
set.type(prod_plan, columns = 1:12, type = "real")

add.constraint(prod_plan, xt = c(1,0,0,0,0,0,-1,0,0,0,0,0), type = "=", rhs = 100)
add.constraint(prod_plan, xt = c(0,1,0,0,0,0,1,-1,0,0,0,0), type = "=", rhs = 250)
add.constraint(prod_plan, xt = c(0,0,1,0,0,0,0,1,-1,0,0,0), type = "=", rhs = 190)
add.constraint(prod_plan, xt = c(0,0,0,1,0,0,0,0,1,-1,0,0), type = "=", rhs = 140)
add.constraint(prod_plan, xt = c(0,0,0,0,1,0,0,0,0,1,-1,0), type = "=", rhs = 220)
add.constraint(prod_plan, xt = c(0,0,0,0,0,1,0,0,0,0,1,0), type = "=", rhs = 110)

set.bounds(prod_plan, lower = rep(0,12), upper = rep(Inf,12), columns = 1:12)
rownames(prod_plan) =  c("Window 1","Window 2","Window 3","Window 4","Window 5","Window 6")
prod_plan
solve(prod_plan)

dec_var = matrix(get.variables(prod_plan), 1, 12)
colnames(dec_var) = colnames(prod_plan)
rownames(dec_var) = "Optimum Mix"
obj_func = get.objective(prod_plan)
results = list(problem = prod_plan, optimum = obj_func, dec_var = dec_var)
rm(obj_func, dec_var)  
results

SensRHS = matrix(NA, nrow(prod_plan), 5)
rownames(SensRHS) = rownames(prod_plan)
colnames(SensRHS) = c("Clearance", "Shadow Price", "LI rhs", "rhs actual", "LS rhs")
SensRHS[,"Clearance"] = get.rhs(prod_plan) - get.constraints(prod_plan)
SensRHS[,"Shadow Price"] = get.sensitivity.rhs(prod_plan)$duals[1:nrow(prod_plan)]
SensRHS[,"rhs actual"] = get.rhs(prod_plan)
SensRHS[SensRHS[,"Clearance"]==0,"LI rhs"] = get.sensitivity.rhs(prod_plan)$dualsfrom[1:nrow(prod_plan)][SensRHS[,"Clearance"]==0]
SensRHS[SensRHS[,"Clearance"]==0,"LS rhs"] = get.sensitivity.rhs(prod_plan)$dualstill[1:nrow(prod_plan)][SensRHS[,"Clearance"]==0]
SensRHS[SensRHS[,"Clearance"]>0,"LI rhs"] = get.constraints(prod_plan)[SensRHS[,"Clearance"]>0]
SensRHS[SensRHS[,"Clearance"]>0,"LS rhs"] = Inf
round(SensRHS,4)

SensObj = matrix(NA, ncol(prod_plan), 4)
rownames(SensObj) = colnames(prod_plan)
colnames(SensObj) = c("Min.Coef.Obj.", "Coef.Obj.", "Max.Coef.Obj.", "Reduced Price")
for (i in 1:ncol(prod_plan)){
  SensObj[i,"Coef.Obj."]=get.column(prod_plan,i)$column[1]
}
rm(i)
SensObj[,"Min.Coef.Obj."] = get.sensitivity.obj(prod_plan)$objfrom
SensObj[,"Max.Coef.Obj."] = get.sensitivity.obj(prod_plan)$objtill
SensObj[,"Reduced Price"] = -get.sensitivity.rhs(prod_plan)$duals[(nrow(prod_plan)+1):(nrow(prod_plan)+ncol(prod_plan))]
round(SensObj,4)
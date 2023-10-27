f <- function(x) 1/4*x[1]^4 + 1/2*x[2]^2 - x[1]*x[2] + x[1] - x[2]
f(c(0,1))

optim(c(0, 1), f)

f<-function(x) {
  x^3-2*x^2-3*x+5
}
f(2)
optimize(f,lower=1,upper=5)
curve(f, 1, 5)

f<-function(x) {
  y<-100*(x[2]-x[1]^2)^2+(1-x[1])^2
  y
}
nlm(f,c(-1.2,1))

####################################
fn1=function(x)
{
  exp(x[1]*x[2]*x[3]*x[4]*x[5])
}

eqn1=function(x){
  z1=x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
  z2=x[2]*x[3]-5*x[4]*x[5]
  z3=x[1]*x[1]*x[1]+x[2]*x[2]*x[2]
  return(c(z1,z2,z3))
}

library(Rsolnp)
x0 = c(-2, 2, 2, -1, -1)
powell=solnp(x0, fun = fn1, eqfun = eqn1, eqB = c(10, 0, -1))  # 求解最小值
powell
fn1(powell$pars)
fn1(x0)
fn1(c(1,1,1,1,1))



##############################################
# install.packages("lpSolve")
library(lpSolve)
# 定义线性规划问题
lp.obj <- c(3, 4) # 目标函数系数
lp.con <- matrix(c(2, 1, 1, 3), nrow = 2, byrow = TRUE) # 约束条件系数矩阵
lp.con
lp.dir <- c("<=", "<=") # 约束条件方向
lp.rhs <- c(10, 15) # 约束条件右侧常数
lp.sol <- lp("max", lp.obj, lp.con, lp.dir, lp.rhs)
# 提取最优解和目标函数值
lp.sol$solution # 最优解
lp.sol$objval # 目标函数值

library(lattice)
library(rsm)
x.grid <- expand.grid(x1= seq(0,1,length=10),
                      x2= seq(0,1,length=10))
x.grid$y <- 90  - 8*x.grid$x1 - 40*x.grid$x1^2 - 8*x.grid$x2 - 80*x.grid$x2^2 + 
  80*x.grid$x1*x.grid$x2 + rnorm(nrow(x.grid),0,2)
str(x.grid)

res.lm <- lm(y ~ x1+x2+x1:x2+I(x1^2)+I(x2^2), data=x.grid)
summary(res.lm)
persp(res.lm, ~x1+x2, contours = "colors", col = topo.colors(100))

x.coded  <- coded.data(x.grid, x1.c~(x1-0.5)/0.25,
                               x2.c ~ (x2-0.5)/0.25)
x.coded
res.rsm  <- rsm(y ~ SO(x1.c, x2.c),   data=x.coded)
summary(res.rsm)
ridge <- steepest(res.rsm, dist = seq(2,8,0.5), descent=FALSE) #descent=F: max
ridge

library(Rsolnp)
x.var    <- c("x1","x2")
y.var    <-  "y"
x.mean   <- sapply(x.grid[,x.var],mean,na.rm=T)
x.lb     <-  sapply(x.grid[,x.var],min,na.rm=T) # LB,UB
x.ub     <-  sapply(x.grid[,x.var],max,na.rm=T)
delta    <- (x.ub-x.lb)*0.025 # stepsize 2.5% of X

df.collect  <- NULL 

objective   =function(x) {      
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  return( -predict(res.lm,xx)  ) # max(x) = min(-x)   
}

for (i in (1:20))   {
  
  xx.lb <- c(0.5,0.5) - i*delta
  xx.ub <- c(0.5,0.5) + i*delta
  
  res.nlp  <- solnp(fun=objective , LB=xx.lb,UB=xx.ub,
                    pars=x.mean, control=list(trace=0))
  
  solution           <- rbind(res.nlp$pars)
  colnames(solution) <- x.var
  df.collect         <- rbind(df.collect, data.frame(relax.step=i,
                                                     y.at.solution = -objective(solution) , 
                                                     rbind(solution) )  )
}
rownames(df.collect)  <- NULL
df.collect


###################################################################
doe1 <- import("datas/all.data.xlsx", sheet="DoE1")
doe1
df=doe1 %>% pivot_longer(9:10, names_to = "Respones", values_to = "y") 
df
x.var = colnames(doe1)[2:4]

form <-formula("y ~ (m.kat + v.ml + m.add)^2 + I(m.kat^2) + I(v.ml^2) + I(m.add^2)")
df.nest=df %>% select(-obsnr) %>% 
  group_nest(Respones) %>% 
  mutate(
    model=map(data, ~MASS::stepAIC(lm(form, data = .x)),trace=0,k=log(nrow(.x))),
    pred=map(model, ~predict(.x)),
    tidy=map(model, ~broom::glance(.x)),
    R2=map2(data, pred, ~round(cor(.y, .x$y)^2,3))
  ) %>% unnest(R2)
df.nest

source("script/relax_opt.R")
opt_val=df.nest %>% mutate(
                    resut=map2(data, model, ~relax_opt(.x, x.var, .y, 0.1, 2, type="max"))
                    )
opt_val$resut



res.lm=MASS::stepAIC(lm(ton.dmm ~ (m.kat+v.ml+m.add)^2 + I(m.kat^2) + I(v.ml^2) + I(m.add^2), 
                        data = doe1))
summary(res.lm)
relax_opt(doe1, x.var, res.lm, 0.1, 3, type="max")  ##data x1  x2  y


############################################
library(Rsolnp)
fn1=function(x){
  exp(x[1]*x[2]*x[3]*x[4]*x[5])
}

eqn1=function(x){
  z1=x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
  z2=x[2]*x[3]-5*x[4]*x[5]
  z3=x[1]*x[1]*x[1]+x[2]*x[2]*x[2]
  return(c(z1,z2,z3))
}

x0 = c(-2, 2, 2, -1, -1)
powell=solnp(pars=x0, fun = fn1, eqfun = eqn1, eqB = c(10, 0, -1))
powell$pars
solution=rbind(powell$pars)
colnames(solution) = cc("x1,x2,x3,x4,x5")
solution
fn1(powell$pars)

##################################################
df <- import("datas/yield_data2.csv")
str(df)
x.var=colnames(df %>% select(A:D))
fit=lm(Yield ~ A+B+C+D+A:D+I(B^2)+I(C^2)+I(D^2), data=df)
summary(fit)
source("script/relax_opt.R")
relax_opt(df, x.var, fit, 0.1, 2, type="max") 

par(mfrow = c(2,3)) 
persp(fit, ~ A+B+C+D, contours = "colors", col = topo.colors(100) ) 

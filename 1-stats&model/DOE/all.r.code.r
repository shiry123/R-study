rm(list=ls())
getwd()
##### file location 

input         <- file.path("D:","Mywd","datas","all.data.xlsx") 

######################################################################################################
#################### Start 1                           ###############################################
#################### Historical data analysis using RF ###############################################
######################################################################################################

library(lattice)
library(randomForest)
library(pals)
library(viridis)
library(readxl)

set.seed(12345)

##Read historical data 144 x 10 into data frame x

x.historical <- as.data.frame(read_excel(input,
              sheet="historical.data"))
str(x.historical)
colnames(x.historical)
x.var <- colnames(x.historical)[2:8]
y.var <- colnames(x.historical)[9:10]


rf.models <- list()
R2        <- NULL

for (i in (1:length(y.var)) )   {    # rf modeling; add models to list rf.models
  
  rf.models[[i]] <- randomForest(formula(paste(y.var[i],"~",paste(x.var,collapse="+"), sep="")),
                                 data=x.historical) 
  R2             <- c(R2, round(cor(predict(rf.models[[i]]),x.historical[,y.var[i]])^2,2))
  
}
names(R2) <- y.var

R2 # show fit in terms of R2

# show fit as plot observed versus predicted (only ton.dmm shown)


plot(predict(rf.models[[1]]), x.historical$ton.dmm,type="n",
     xlab="predicted ton.dmm", ylab="observed ton.dmm",
     main=paste("R2=",R2[1]))
text(predict(rf.models[[1]]), x.historical$ton.dmm,
     labels=x.historical$obsnr)
abline(0,1,col="red")                                       # diagonal in red
abline(lm(x.historical$ton.dmm~predict(rf.models[[1]]))$coef)          # linear fit in black
grid()
     

# plot RF model ton.dmm~ as conditional trellisplot

# create m.kat x m.add x p.co2 x T given t.h, p.h2, v.ml at median 

x.pred <- expand.grid(m.kat = seq(min(x.historical$m.kat,na.rm=T), 
                                  max(x.historical$m.kat,na.rm=T), length=15),
                      m.add = seq(min(x.historical$m.add), 
                                  max(x.historical$m.add), length=15),
                      p.co2  = seq(min(x.historical$p.co2), 
                                   max(x.historical$p.co2), length=3),
                      T     = seq(min(x.historical$T), 
                                  max(x.historical$T), length=3),
                      t.h=median(x.historical$t.h),
                      p.h2=median(x.historical$p.h2),
                      v.ml=median(x.historical$v.ml))  


x.pred$y1 <- predict(rf.models[[1]],x.pred)

print(wireframe(y1 ~ m.kat*m.add|p.co2*T, data=x.pred,
                zlab=list(label="dmm",cex=0.5,rot=90),
                xlab=list(label="m.kat",cex=0.5,rot=0),
                ylab=list(label="m.add",cex=0.5,rot=0),
                drape=TRUE,      
                at=do.breaks(c(50,600),100),
                col.regions = parula(100),
                strip=TRUE, pretty=TRUE, 
                scales = list(arrows = FALSE,
                              x=c(cex=0.5),
                              y=c(cex=0.5),
                              z=c(cex=0.5)) ,
                screen = list(z = 300, x = -55, y=0)  ,
                
                cuts=1000,
                par.settings = list(superpose.line = list(lwd=3))
)  )

######################################################################################################
#################### Historical data analysis using RF ###############################################
#################### End 1                             ###############################################
######################################################################################################



######################################################################################################
#################### Start 2a                  #######################################################
#################### First DoE by augmentation #######################################################
######################################################################################################

library(AlgDesign)

# select promising subspace (9 unique runs) based on RF analysis and augment D-optimally
x.promising  <- unique(x.historical[x.historical$ton.dmm>400,x.var])

dim(x.promising) # 9 x 7
sapply(x.promising,range) # show ranges; note T, p.h2, p.co2, t.h being constant


# create full factorial candidate set 3 x 3 x 3

candidate <- expand.grid(m.kat=seq(min(x.promising$m.kat),  
                                   max(x.promising$m.kat),length=3),
                         v.ml=seq(min(x.promising$v.ml),  
                                  max(x.promising$v.ml),length=3),
                         m.add=seq(min(x.promising$m.add),  
                                   max(x.promising$m.add),length=3) )

candidate  <- rbind(x.promising[,1:3],candidate) # rowbind x.promising and candidate set


set.seed(12345)
doe.cat <- optFederov(~ (m.kat + v.ml + m.add)^2+ I(m.kat^2) + I(v.ml^2) + I(m.add^2),
                      data=candidate,center=T,augment=T,rows=(1:9),
                      criterion="D", nTrials=15)$design  # d-optimal cat design
                                                          # comprising 9 historical 
                                                          # and 6 additional candidates from candidate set
dim(doe.cat)

rownames(doe.cat )   <- NULL

## check condition number of augmented design, 1 indicates a perfectly hyperspherical design, 
#  large positive number >>1 indicates poor designs
kappa(model.matrix(~ (m.kat + v.ml + m.add)^2 + I(m.kat^2) + I(v.ml^2) + I(m.add^2) , 
                   data=data.frame(scale(doe.cat)   )   ) )
### 8.519979

doe.cat # run #10-#15 in the lab 

######################################################################################################
#################### First DoE by augmentation #######################################################
#################### End 2a                    #######################################################
######################################################################################################



######################################################################################################
#################### Start 2b                      ###################################################
#################### Analysis of first design DoE1 ###################################################
######################################################################################################

library(MASS)


doe1 <- as.data.frame(read_excel(input,
               sheet="DoE1"))
dim(doe1)  # 44 x 10


x.var <- colnames(doe1)[2:4]
y.var <- colnames(doe1)[9:10]


ols.models <- list()
R2         <- NULL

form <- "~ (m.kat + v.ml + m.add)^2 + I(m.kat^2) + I(v.ml^2) + I(m.add^2) " # parametric formula for linear 
                                                                            # models 

for (i in (1:length(y.var)) )   {    # rf modeling; add models to list rf.models
 
  ols.models[[i]] <-  stepAIC(lm(formula(paste(y.var[i],form, sep="")),
                                 data=doe1, x=T, y=T),k = log(nrow(doe1))) # BIC criterion
  R2              <- c(R2, round(cor(predict(ols.models[[i]]),doe1[,y.var[i]])^2,2))
  
}
names(R2) <- y.var
R2 # show fit in terms of R2

# show fit as plot observed versus predicted (only ton.dmm shown)


plot(predict(ols.models[[1]]), doe1$ton.dmm,type="n",xlab="predicted ton.dmm", ylab="observed ton.dmm",
     main=paste("R2=",R2[1]))
text(predict(ols.models[[1]]), doe1$ton.dmm,labels=doe1$obsnr)
abline(0,1,col="red")                                       # diagonal in red
abline(lm(doe1$ton.dmm~predict(ols.models[[1]]))$coef,lty=2)          # linear fit in black
grid()


## show ton.dmm = g3(m.kat,m.add,v.ml) as trellis plot

x.pred <- expand.grid(m.kat = seq(min(doe1$m.kat), max(doe1$m.kat), length=15    ),
                      m.add = seq(min(doe1$m.add), max(doe1$m.add), length=15    ),
                      v.ml  = seq(min(doe1$v.ml), max(doe1$v.ml), length=3    )  )


x.pred$y1 <- predict(ols.models[[1]],x.pred)



print(wireframe(y1 ~ m.kat*m.add|v.ml, data=x.pred,
                zlab=list(label="dmm",cex=0.5,rot=90),
                xlab=list(label="m.kat",cex=0.5,rot=0),
                ylab=list(label="m.add",cex=0.5,rot=0),
                drape=TRUE,      
                at=do.breaks(c(100,1200),100),
                col.regions = parula(100),
                strip=T, 
                scales = list(arrows = FALSE,
                              x=c(cex=0.5),
                              y=c(cex=0.5),
                              z=c(cex=0.5)) ,
                screen = list(z = 300, x = -60, y=0) , layout=c(3,1)
                
)  )

######################################################################################################
#################### Analysis of first design DoE1 ###################################################
#################### End 2b                        ###################################################
######################################################################################################



######################################################################################################
#################### Start 2c                                                                    #####
#################### Relaxation of DoE1: Optimization OLS1 subject to relaxed border constraints #####
######################################################################################################

library(Rsolnp)


x.var          <- colnames(doe1)[2:4]
x0             <- sapply(doe1[,x.var],mean,na.rm=T)
m              <- x0
s              <- sapply(doe1[,x.var],sd,na.rm=T)
names(x0)      <- x.var
x.lb           <-  sapply(doe1[,x.var],min,na.rm=T)
x.ub           <-  sapply(doe1[,x.var],max,na.rm=T)

delta          <- (x.ub-x.lb)/10 # stepsize 10% of initial design


df.collect  <- NULL 



######################### 
# given predictors x the function returns OLS predictions y=g(x) for each  
  y.val   = function(x) {
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  collect          <- NULL
  for (iii in (1:length(y.var)) )   {
    collect <- c(collect,predict(ols.models[[iii]],xx))
  }
  return( collect  )
}
######################### 
# given x expected ton.dmm is returned
obj   =function(x) {  	
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  return( predict(ols.models[[1]],xx)  )          
}

minus.obj   =function(x) {  	
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  return( -predict(ols.models[[1]],xx)  )          
}
#########################  

for (i in (1:3))   {
  
  
  xx.lb <- x.lb - i*delta
  xx.ub <- x.ub + i*delta
  
  res  <- gosolnp(fun=minus.obj, LB=xx.lb,UB=xx.ub) # note: -obj <=> max(obj)
  tt <- system.time(res)
  sol         <- data.frame(optimal.x=round(res$pars,4))
  a           <- y.val(res$pars)
  names(a)    <- y.var
  solution    <-  rbind(round(res$pars,4))
  colnames(solution)  <- x.var
  
  df.collect  <- rbind(df.collect, 
                       data.frame(relax.step=i*10,
                                  objective=obj(res$pars), 
                                  target="max(ton.dmm)",
                                  rbind(solution),rbind(a) ,  
                                  return.code=res$convergence,
                                  stringsAsFactors =FALSE,
                                  time.min=round(tt[3]/60,2) )  )
  
}
rownames(df.collect)  <- NULL

df.collect  # show results of relaxation 10, 20, 30%

######################################################################################################
#################### Relaxation of DoE1: Optimization OLS1 subject to relaxed border constraints #####
#################### End 2c                                                                      #####
######################################################################################################



######################################################################################################
#################### Start 3a                                               ##########################
#################### Create linear design DoE2 around relaxation1 trial 20% ##########################
######################################################################################################

library(AlgDesign)

## full factorial candidate set
candidate <- expand.grid(T=seq(80,  100 ,length=2),
                         p.H2=seq(80,  100 ,length=2),
                         p.CO2=seq(15,  25 ,length=2),
                         t.H=seq(16,  20 ,length=2) )

set.seed(12345)
doe2 <- optFederov(~ T + p.H2 + p.CO2 + t.H ,
                      data=candidate,center=T,
                      criterion="D", nTrials=5)$design   # d-optimal cat design

kappa(model.matrix(~ T + p.H2 + p.CO2 + t.H ,
                   data=data.frame(scale(doe2)   )   ) )
# 1.499816

rownames(doe2)  <- NULL

doe2                   # 5 trials to estimate the linear effects of T, p.h2, p.co2, t.H in the range
sapply(doe2,range)

######################################################################################################
#################### Create linear design DoE2 around relaxation1 trial 20% ##########################
#################### End 3a                                                 ##########################
######################################################################################################



######################################################################################################
#################### Start 3b              ###########################################################
#################### OLS2 modeling of DoE2 ###########################################################
######################################################################################################

doe2 <- as.data.frame(read_excel(input,
                  sheet="DoE2"))
dim(doe2)  # 21 x 10


x.var <- colnames(doe2)[5:8]
y.var <- colnames(doe2)[9:10]


ols2.models <- list()
R2         <- NULL

form <- "~ T +	p.h2 +	p.co2 +	t.h " # parametric formula for linear 
# models in R notation

for (i in (1:length(y.var)) )   {    # rf modeling; add models to list rf.models
  
  ols2.models[[i]] <-  stepAIC(lm(formula(paste(y.var[i],form, sep="")),data=doe2,x=T,y=T),k = log(nrow(doe2)))
  R2              <- c(R2, round(cor(predict(ols2.models[[i]]),doe2[,y.var[i]])^2,2))
  
}
names(R2) <- y.var
R2 # show fit in terms of R2

# show fit as plot observed versus predicted (only ton.dmm shown)


plot(predict(ols2.models[[1]]), doe2$ton.dmm,type="n",xlab="predicted ton.dmm", ylab="observed ton.dmm",
     main=paste("R2=",R2[1]))
text(predict(ols2.models[[1]]), doe2$ton.dmm,labels=doe2$obsnr)
abline(0,1,col="red")                                                  # diagonal in red
abline(lm(doe2$ton.dmm~predict(ols2.models[[1]]))$coef,lty=2)          # linear fit in black
grid()


###### trellis plot of OLS2 effects

x.pred <- expand.grid(     T = seq(min(doe2$T), max(doe2$T), length=15    ),
                           p.h2 = seq(min(doe2$p.h2), max(doe2$p.h2), length=15    ),
                           p.co2  = seq(min(doe2$p.co2), max(doe2$p.co2), length=3    ),
                           t.h    = seq(min(doe2$t.h), max(doe2$t.h), length=3  )  )

x.pred$y1 <- predict(ols2.models[[1]],x.pred)




print(wireframe(y1 ~ T*p.h2|p.co2*t.h, data=x.pred,
                zlab=list(label="dmm",cex=0.5,rot=90),
                xlab=list(label="T",cex=0.5,rot=0),
                ylab=list(label="p.h2",cex=0.5,rot=0),
                drape=TRUE,      
                at=do.breaks(c(-50,2600),100),
                col.regions = parula(100),
                strip=T, 
                scales = list(arrows = FALSE,
                              x=c(cex=0.5),
                              y=c(cex=0.5),
                              z=c(cex=0.5)) ,
                screen = list(z = 60, x = -55, y=0)  
                
)  )

######################################################################################################
#################### OLS2 modeling of DoE2 ###########################################################
#################### End 3b                ###########################################################
######################################################################################################



######################################################################################################
#################### Start 3c                                                                    #####
#################### Relaxation of DoE2: Optimization OLS2 subject to relaxed border constraints #####
######################################################################################################

library(Rsolnp)


x.var          <- colnames(doe2)[5:8]
x0             <- sapply(doe2[,x.var],mean,na.rm=T)
m              <- x0
s              <- sapply(doe2[,x.var],sd,na.rm=T)
names(x0)      <- x.var
x.lb           <-  sapply(doe2[,x.var],min,na.rm=T)
x.ub           <-  sapply(doe2[,x.var],max,na.rm=T)

delta          <- (x.ub-x.lb)/4 # stepsize 25% of initial design


df.collect  <- NULL 



#########################
# given predictors x the function returns OLS predictions y=g(x) for each  
y.val   = function(x) {
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  collect          <- NULL
  for (iii in (1:length(y.var)) )   {
    collect <- c(collect,predict(ols2.models[[iii]],xx))
  }
  return( collect  )
}
#########################
# given x expected ton.dmm is returned
obj   =function(x) {  	
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  return( predict(ols2.models[[1]],xx)  )          
}

obj.minus   =function(x) {  	
  xx               <- data.frame(rbind(x))
  colnames(xx)     <- x.var 
  return( -predict(ols2.models[[1]],xx)  )          
}


######################### 

for (i in (0:2))   {
  
  
  xx.lb <- x.lb - i*delta
  xx.ub <- x.ub + i*delta
  
  tt <- system.time(res  <- gosolnp(fun=obj.minus , LB=xx.lb,UB=xx.ub)) # note: -obj
  
  sol         <- data.frame(optimal.x=round(res$pars,4))
  a           <- y.val(res$pars)
  names(a)    <- y.var
  solution    <-  rbind(round(res$pars,4))
  colnames(solution)  <- x.var
  
  df.collect  <- rbind(df.collect, data.frame(relax.step=i*25,objective=obj(res$pars), target="max(ton.dmm)",
                                              rbind(solution),rbind(a) ,  return.code=res$convergence,
                                              stringsAsFactors =FALSE,time.min=round(tt[3]/60,2) )  )
  
}
rownames(df.collect)  <- NULL

df.collect  # show results of relaxation 0, 25, 50%

######################################################################################################
#################### Relaxation of DoE2: Optimization OLS2 subject to relaxed border constraints #####
#################### End 3c                                                                      #####
######################################################################################################



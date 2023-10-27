# dt[i,j,by]
library(data.table)
dt=data.table(x=rep(1:2, each=9), 
              y=1:18, 
              z=18:1, 
              grp=rep(c("A","B"), each=1),
              val=rep(c("x1","x2","x3"), each=6))
dt
dt[, x.f:=as.factor(x)]
dt[x==1 & y>1]  #filter
dt[grp=="A"]
dt[, .(y, z)]  #select
dt[y %between% c(2, 4),.(y,z)]
dt[, sum:=x+y+z]
dt[,':='(sum_xy=x+y,
         yz=y*z)]
dt[order(-x), .(TotalN=.N, Zmean=mean(z), Zsd=sd(z), Zmax=max(z)), 
   by=grp]
dt[,.(TotalN=.N, Zmean=mean(z), Zsd=sd(z), Zmax=max(z)), 
   by=.(val,grp)]

dt[, ":="(TotalN=.N, Zmean=mean(z), Zsd=sd(z), Zmax=max(z)),
   by=x]

dt[, lapply(.SD, mean), .SDcols = 1:3]
cols=colnames(dt[,1:3])
dt[, paste0(cols, "_m") := lapply(.SD, mean), .SDcols = cols]

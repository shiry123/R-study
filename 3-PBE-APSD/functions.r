#正态性检测
SWTest.p <- function(mat) {
  shapiro.test(mat)$p.value
}
# sample size > 50
KSTest.p <- function(mat) {
  ks.test(mat, pnorm)$p.value
}
###### APSD 计算##########################################################################################
# mass 输入stage0~filter 的沉积量，FR对于流速28.3L/min， FPF=FPD/sum(throat~filter)*100%

ACI.APSD=function(mass, FR, type=c("MMAD","GSD","FPD")){
    type=match.arg(type)
    mass=as.numeric(mass)
    mass[is.na(mass)]=0
    cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
    cm=rev(cm.temp[-length(cm.temp)])
    z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
  
    location.s=c("stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7","filter")
    ECD28.3=c(9.0,5.8,4.7,3.3,2.1,1.1,0.7,0.43,0)
    ECD60=c(8.6,6.5,4.4,3.2,1.9,1.2,0.55,0.26,0)
    ECD90=c(8,6.5,5.2,3.5,2.6,1.7,1,0.22,0)
    dat=data.frame(mass=mass, z=z, location.s=location.s, ECD28.3=ECD28.3,ECD60=ECD60,ECD90=ECD90)

    #ECD
    if(FR < 60){
      dat$ECD=dat$ECD28.3 * (28.3/FR)^0.5
    }else if(FR >=60 & FR<90){
      dat$ECD=dat$ECD60 * (60/FR)^0.5
    }else{
      dat$ECD=dat$ECD90 * (90/FR)^0.5
    }
    
    lm.data=subset(dat, location.s %in% c("stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7"))
    mod=lm(z~log(ECD), data=lm.data)
    MMAD=exp((0-coef(mod)[1])/coef(mod)[2])   #z=0
    sizeX=exp((1-coef(mod)[1])/coef(mod)[2])   #z=1
    GSD=sizeX/MMAD
    FPD=sum(subset(dat,ECD<5,select=mass), na.rm = TRUE)
    
    switch (type,
      "MMAD" = return(MMAD),
      "GSD" = return(GSD),
      "FPD" = return(FPD)
    )
    
}

######################################################################
C.ACI.APSD=function(mass, FR, type=c("MMAD","GSD","FPD")){
  type=match.arg(type)
  mass=as.numeric(mass)
  mass[is.na(mass)]=0
  cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
  cm=rev(cm.temp[-length(cm.temp)])
  z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
  
  location.s=c("stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7","filter")
  ECD28.3=c(9.0,5.8,4.7,3.3,2.1,1.1,0.7,0.43,0)
  ECD60=c(8.6,6.5,4.4,3.2,1.9,1.2,0.55,0.26,0)
  ECD90=c(8,6.5,5.2,3.5,2.6,1.7,1,0.22,0)
  dat=data.frame(mass=mass, z=z, location.s=location.s, ECD28.3=ECD28.3,ECD60=ECD60,ECD90=ECD90)

  
  #ECD
  if(FR < 60){
    dat$ECD=dat$ECD28.3 * (28.3/FR)^0.5
  }else if(FR >=60 & FR<90){
    dat$ECD=dat$ECD60 * (60/FR)^0.5
  }else{
    dat$ECD=dat$ECD90 * (90/FR)^0.5
  }
  
  lm.data=subset(dat, location.s %in% c("stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7"))
  fit=lm(z~log(ECD), data=subset(lm.data, z>=-1 & z<=1))
  R2=summary(fit)[["r.squared"]]
    
  find_z =function(x, n){
      out=NULL
      for(i in 1:(length(x)-1)){
        out[i]=ifelse(x[i]>n & x[i+1]<n, 1, 0) 
      }
      return(out)
  }
  
  nr.z0=which(find_z(lm.data$z,n=0) %in% 1)
  data.z0=lm.data[c(nr.z0, (nr.z0 + 1)),]
  fit0=lm(z~log(ECD), data=data.z0)
  MMAD=exp((0-coef(fit0)[1])/coef(fit0)[2])   #z=0
  
  nr.z1 = which(find_z(lm.data$z,n=1) %in% 1)
  nr.zminus1 = which(find_z(lm.data$z,n=-1) %in% 1)
  
  if(!identical(nr.z1, integer(0))){
    data.z1=lm.data[c(nr.z1, (nr.z1 + 1)),]
    fit1=lm(z~log(ECD), data=data.z1)
    sizeX=exp((1-coef(fit1)[1])/coef(fit1)[2])
    GSD=sizeX/MMAD
  }else if(!identical(nr.zminus1, integer(0))){
    data.zminus1=lm.data[c(nr.zminus1, (nr.zminus1 + 1)),]
    fit2=lm(z~log(ECD), data=data.zminus1)
    sizeY=exp((1-coef(fit2)[1])/coef(fit2)[2])
    GSD=MMAD/sizeY
  }else{
    fit3=lm(z~log(ECD), data=lm.data[c(1,2),])
    sizeX=exp((1-coef(fit3)[1])/coef(fit3)[2])
    GSD=sizeX/MMAD
  }
  
  nr.ECD5=which(find_z(lm.data$ECD,n=5) %in% 1)
  fit.ECD5=lm(z~log(ECD), data=lm.data[c(nr.ECD5,nr.ECD5+1),])
  z5=coef(fit.ECD5)[2]*log(5)+coef(fit.ECD5)[1]
  FPD=pnorm(z5)*sum(dat$mass, na.rm=TRUE)
  
  switch (type,
          "MMAD" = return(MMAD),
          "GSD" = return(GSD),
          "FPD" = return(FPD)
  )
  
}

##########################################################
NGI.APSD=function(mass, FR, type=c("MMAD","GSD","FPD")){
  type=match.arg(type)
  mass=as.numeric(mass)
  mass[is.na(mass)]=0
  cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
  cm=rev(cm.temp[-length(cm.temp)])
  z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
  
  location.s=c("stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc")
  ECD15=c(14.1,8.61,5.39,3.3,2.08,1.36,0.98,0)
  ECD60=c(8.06,4.46,2.82,1.66,0.94,0.55,0.34,0)
  factor1=c(0.54,0.52,0.50,0.47,0.52,0.60,0.67,0)
  factor2=c(0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
  dat=data.frame(mass=mass, z=z, location.s=location.s, ECD15=ECD15,ECD60=ECD60,factor1=factor1,factor2=factor2)
  
  pow=function(x,n){return(x^n)}
  #ECD
  if(FR >= 30){
    dat$ECD=dat$ECD60 * pow(60/FR, dat$factor1)
  }else{
    dat$ECD=dat$ECD15 * pow(15/FR, dat$factor2)
  }
  
  lm.data=subset(dat, location.s %in% c("stage1","stage2","stage3","stage4","stage5","stage6","stage7"))
  mod=lm(z~log(ECD), data=lm.data)
  MMAD=exp((0-coef(mod)[1])/coef(mod)[2])   #z=0
  sizeX=exp((1-coef(mod)[1])/coef(mod)[2])   #z=1
  GSD=sizeX/MMAD
  FPD=sum(subset(dat,ECD<5,select=mass), na.rm = TRUE)
  
  switch (type,
          "MMAD" = return(MMAD),
          "GSD" = return(GSD),
          "FPD" = return(FPD)
  )
  
}
############################################################################################

C.NGI.APSD=function(mass, FR, type=c("MMAD","GSD","FPD")){
  type=match.arg(type)
  mass=as.numeric(mass)
  mass[is.na(mass)]=0
  cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
  cm=rev(cm.temp[-length(cm.temp)])
  z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
  
  location.s=c("stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc")
  ECD15=c(14.1,8.61,5.39,3.3,2.08,1.36,0.98,0)
  ECD60=c(8.06,4.46,2.82,1.66,0.94,0.55,0.34,0)
  factor1=c(0.54,0.52,0.50,0.47,0.52,0.60,0.67,0)
  factor2=c(0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
  dat=data.frame(mass=mass, z=z, location.s=location.s, ECD15=ECD15,ECD60=ECD60,factor1=factor1,factor2=factor2)
  
  pow=function(x,n){return(x^n)}
  #ECD
  if(FR >= 30){
    dat$ECD=dat$ECD60 * pow(60/FR, dat$factor1)
  }else{
    dat$ECD=dat$ECD15 * pow(15/FR, dat$factor2)
  }
  
  lm.data=subset(dat, location.s %in% c("stage1","stage2","stage3","stage4","stage5","stage6","stage7"))
  fit=lm(z~log(ECD), data=subset(lm.data, z>=-1 & z<=1))
  R2=summary(fit)[["r.squared"]]
  
  find_z =function(x, n){
    out=NULL
    for(i in 1:(length(x)-1)){
      out[i]=ifelse(x[i]>n & x[i+1]<n, 1, 0) 
    }
    return(out)
  }
  
  nr.z0=which(find_z(lm.data$z,n=0) %in% 1)
  data.z0=lm.data[c(nr.z0, (nr.z0 + 1)),]
  fit0=lm(z~log(ECD), data=data.z0)
  MMAD=exp((0-coef(fit0)[1])/coef(fit0)[2])   #z=0
  
  nr.z1 = which(find_z(lm.data$z,n=1) %in% 1)
  nr.zminus1 = which(find_z(lm.data$z,n=-1) %in% 1)
  
  if(!identical(nr.z1, integer(0))){
    data.z1=lm.data[c(nr.z1, (nr.z1 + 1)),]
    fit1=lm(z~log(ECD), data=data.z1)
    sizeX=exp((1-coef(fit1)[1])/coef(fit1)[2])
    GSD=sizeX/MMAD
  }else if(!identical(nr.zminus1, integer(0))){
    data.zminus1=lm.data[c(nr.zminus1, (nr.zminus1 + 1)),]
    fit2=lm(z~log(ECD), data=data.zminus1)
    sizeY=exp((1-coef(fit2)[1])/coef(fit2)[2])
    GSD=MMAD/sizeY
  }else{
    fit3=lm(z~log(ECD), data=lm.data[c(1,2),])
    sizeX=exp((1-coef(fit3)[1])/coef(fit3)[2])
    GSD=sizeX/MMAD
  }
  
  nr.ECD5=which(find_z(lm.data$ECD,n=5) %in% 1)
  fit.ECD5=lm(z~log(ECD), data=lm.data[c(nr.ECD5,nr.ECD5+1),])
  z5=coef(fit.ECD5)[2]*log(5)+coef(fit.ECD5)[1]
  FPD=pnorm(z5)*sum(dat$mass, na.rm=TRUE)
  
  switch (type,
          "MMAD" = return(MMAD),
          "GSD" = return(GSD),
          "FPD" = return(FPD)
  )
  
}

df=read_clip_tbl()
df
mass=df$m
mass=as.numeric(mass)
mass[is.na(mass)]=0
mass
cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
cm=rev(cm.temp[-length(cm.temp)])
cm
z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
z
lm(z~cm, data=dat)

dat=df %>% filter(str_detect(stages, "^stage")|stages=="Filter")
C.ACI.APSD(dat$amount,60,"MMAD")

source("script/apsd_calc.R")
apsd_calc(df,"stages","amount",FR=15,presep = FALSE, type="citdas", impactor = "ngi")
apsd_calc(df,"stages","mass",FR=28.3,presep = FALSE, type="citdas", impactor = "aci")
apsd_calc(df,"stages","amount",FR=60,presep = TRUE, type="citdas", impactor = "aci")

apsd_calc(df,"stages","amount",FR=15,presep = FALSE, type="citdas", impactor = "ngi")
apsd_calc(df,"stages","amount",FR=60,presep = TRUE, type="citdas", impactor = "ngi")

library(ggtrendline)
dat=apsd_calc(df,"stages","amount",FR=15,presep = FALSE, type="citdas", impactor = "ngi")[[2]] %>% 
    filter(str_detect(stages, "^stage"))
dat
source("script/trendline_plot.R")
trendline_plot(dat, ECD, z, "log2P")

########################################批量处理
df=read_clip_tbl()
df
colnames(df)
df.nest=df %>% select(id,Flow_rate, 9:19) %>% 
  pivot_longer(-c(id,Flow_rate)) %>% 
  df_nest_by(vars = c("id", "Flow_rate"))
df.nest
df.nest$data[[1]]

apsd_calc = function(data, location, mass, FR=28.3, presep=FALSE, type="citdas", impactor="aci"){
  library(slider)
  data=data %>% tidyfst::replace_na_dt(to = 0)
  
  # z 值计算
  if(isFALSE(presep)){
    if(impactor=="ACI" | impactor=="aci" ){
      ecd.dat=data %>% mutate(
        location.s=c("throat","stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7","filter"),
        ECD28.3=c(NA,9.0,5.8,4.7,3.3,2.1,1.1,0.7,0.43,0),
        ECD60=c(NA,8.6,6.5,4.4,3.2,1.9,1.2,0.55,0.26,0),
        ECD90=c(NA,8,6.5,5.2,3.5,2.6,1.7,1,0.22,0)
      ) %>% 
        mutate(ECD=case_when(FR <60          ~ECD28.3*(28.3/FR)^0.5,
                             FR >=60 & FR<90 ~ECD60*(60/FR)^0.5,
                             FR >=90         ~ECD90*(90/FR)^0.5))
      
      x.levels=c("filter","stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0","throat")
      data.z=ecd.dat %>% 
        mutate(location.s = fct_relevel(location.s, x.levels)) %>% 
        filter(location.s %in% 
                 c("filter","stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0")) %>% 
        arrange(location.s) %>% 
        mutate(temp=cumsum(pull(., mass))/sum(pull(., mass)),
               cm=lag(temp),
               z=qnorm(cm)) %>% select(-temp)
      lm.data=data.z %>% 
        filter(location.s %in% c("stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0")) %>% 
        arrange(desc(location.s))
    }else{
      ecd.dat.temp=data %>% 
        mutate(
          location.s=c("throat","stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc"),
          ECD15=c(NA,14.1,8.61,5.39,3.3,2.08,1.36,0.98,0),
          ECD60=c(NA,8.06,4.46,2.82,1.66,0.94,0.55,0.34,0),
          factor1=c(NA,0.54,0.52,0.50,0.47,0.52,0.60,0.67,0),
          factor2=c(NA,0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
        )
      
      get_ecd=function(.data, flow){
        pow=function(x,n){return(x^n)}
        
        if(flow >= 30){
          dat=.data  %>% 
            mutate(ECD=map2_dbl(ECD60,factor1, ~ .x*pow(60/flow, .y)))
        }else{
          dat=.data  %>% 
            mutate(ECD=map2_dbl(ECD15,factor2, ~ .x*pow(15/flow, .y)))
        }
        return(dat)
      }
      
      ecd.dat= get_ecd(ecd.dat.temp, FR)
      x.levels=c("moc","stage7","stage6","stage5","stage4","stage3","stage2","stage1","throat")
      data.z=ecd.dat %>% 
        mutate(location.s = fct_relevel(location.s, x.levels)) %>% 
        filter(location.s %in% 
                 c("moc","stage7","stage6","stage5","stage4","stage3","stage2","stage1")) %>% 
        arrange(location.s) %>% 
        mutate(temp=cumsum(pull(., mass))/sum(pull(., mass)),
               cm=lag(temp),
               z=qnorm(cm)) %>% select(-temp)
      lm.data=data.z %>% 
        filter(location.s %in% c("stage7","stage6","stage5","stage4","stage3","stage2","stage1")) %>% 
        arrange(desc(location.s))
    }
    
    #有预分离器
  } else {
    if(impactor=="ACI" | impactor=="aci" ){
      ecd.dat=data %>% mutate(
        location.s=c("throat","preseperator","stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7","filter"),
        ECD28.3=c(NA,NA,9.0,5.8,4.7,3.3,2.1,1.1,0.7,0.43,0),
        ECD60=c(NA,NA,8.6,6.5,4.4,3.2,1.9,1.2,0.55,0.26,0),
        ECD90=c(NA,NA,8,6.5,5.2,3.5,2.6,1.7,1,0.22,0)
      ) %>% 
        mutate(ECD=case_when(FR <60                 ~ECD28.3*(28.3/FR)^0.5,
                             FR >=60 & FR<90 ~ECD60*(60/FR)^0.5,
                             FR >=90                ~ECD90*(90/FR)^0.5))
      
      x.levels=c("filter","stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0","preseperator","throat")
      data.z=ecd.dat %>% 
        mutate(location.s = fct_relevel(location.s, x.levels)) %>% 
        filter(location.s %in% 
                 c("filter","stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0")) %>% 
        arrange(location.s) %>% 
        mutate(temp=cumsum(pull(., mass))/sum(pull(., mass)),
               cm=lag(temp),
               z=qnorm(cm)) %>% select(-temp)
      lm.data=data.z %>% 
        filter(location.s %in% c("stage7","stage6","stage5","stage4","stage3","stage2","stage1","stage0")) %>% 
        arrange(desc(location.s))
    }else{
      ecd.dat.temp=data %>% 
        mutate(
          location.s=c("throat","preseperator","stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc"),
          ECD15=c(NA,NA,14.1,8.61,5.39,3.3,2.08,1.36,0.98,0),
          ECD60=c(NA,NA,8.06,4.46,2.82,1.66,0.94,0.55,0.34,0),
          factor1=c(NA,NA,0.54,0.52,0.50,0.47,0.52,0.60,0.67,0),
          factor2=c(NA,NA,0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
        )
      
      get_ecd=function(.data, flow){
        pow=function(x,n){return(x^n)}
        
        if(flow >= 30){
          dat=.data  %>% 
            mutate(ECD=map2_dbl(ECD60,factor1, ~ .x*pow(60/flow, .y)))
        }else{
          dat=.data  %>% 
            mutate(ECD=map2_dbl(ECD15,factor2, ~ .x*pow(15/flow, .y)))
        }
        return(dat)
      }
      ecd.dat= get_ecd(ecd.dat.temp, FR)
      
      x.levels=c("moc","stage7","stage6","stage5","stage4","stage3","stage2","stage1","preseperator","throat")
      data.z=ecd.dat %>% 
        mutate(location.s = fct_relevel(location.s, x.levels)) %>% 
        filter(location.s %in% 
                 c("moc","stage7","stage6","stage5","stage4","stage3","stage2","stage1")) %>% 
        arrange(location.s) %>% 
        mutate(temp=cumsum(pull(., mass))/sum(pull(., mass)),
               cm=lag(temp),
               z=qnorm(cm)) %>% select(-temp)
      lm.data=data.z %>% 
        filter(location.s %in% c("stage7","stage6","stage5","stage4","stage3","stage2","stage1")) %>% 
        arrange(desc(location.s))
    }
  }
  
  ###计算MMAD
  switch(type,
         "usp" = {
           fit0=lm(z~log(ECD), data=lm.data %>% filter(!is.infinite(lm.data$z)))
           MMAD=exp((0-coef(fit0)[1])/coef(fit0)[2])
           sizeX=exp((1-coef(fit0)[1])/coef(fit0)[2])
           GSD=sizeX/MMAD
           R2=round(summary(fit0)[["r.squared"]],4)
           FPD=sum(data.z %>% filter(ECD <5) %>% pull(., mass))
           FPF=FPD/sum(pull(ecd.dat, mass))
           results=tibble(MMAD,GSD,FPD,FPF,R2,sizeX)
         },
         "citdas"={
           fit=lm(z~log(ECD), data=filter(lm.data, z>=-1, z<=1))
           r2=round(summary(fit)[["r.squared"]],4)
           #find z       
           find_z =function(x, n=0){
             x=unlist(x)
             if_else(x[1]>n & x[2]<n, 1, 0) 
           }
           #find data
           data_sub=function(data, var){
             x=which(pull(data, var) %in% 1)
             slice(data, x:(x+1)) 
           }
           
           data.find=lm.data %>%
             mutate(z0 = slide(z, ~find_z(.x, 0), .before =0, .after = 1),
                    z1 = slide(z, ~find_z(.x, 1), .before =0, .after = 1),
                    z.minus1 = slide(z, ~find_z(.x, -1), .before =0, .after = 1))
           
           fit1=lm(z~log(ECD), data=data_sub(data.find, "z0"))
           fit4=lm(z~log(ECD), data=data.find %>% mutate(new=abs(ECD-5)) %>%
                     slice_min(new,n=2) %>% select(-new))
           z5=coef(fit4)[2]*log(5)+coef(fit4)[1]
           
           ## 若无sizeX ，采用sizeY     
           if(!is_empty(which(pull(data.find, "z1") %in% 1))){
             fit2=lm(z~log(ECD), data=data_sub(data.find, "z1"))
             MMAD=exp((0-coef(fit1)[1])/coef(fit1)[2])
             sizeX=exp((1-coef(fit2)[1])/coef(fit2)[2])
             GSD=sizeX/MMAD
             R2=round(summary(fit)[["r.squared"]],4)
             FPD=pnorm(z5)*sum(pull(data.z, mass))
             FPF=FPD/sum(pull(ecd.dat, mass))
             results=tibble(MMAD,GSD,FPD,FPF,R2,sizeX)
             
           } else if (!is_empty(which(pull(data.find, "z.minus1") %in% 1))){
             fit3=lm(z~log(ECD), data=data_sub(data.find, "z.minus1"))
             MMAD=exp((0-coef(fit1)[1])/coef(fit1)[2])
             sizeY=exp((1-coef(fit3)[1])/coef(fit3)[2])
             GSD=MMAD/sizeY
             R2=round(summary(fit)[["r.squared"]],4)
             FPD=pnorm(z5)*sum(pull(data.z, mass))
             FPF=FPD/sum(pull(ecd.dat, mass))
             results=tibble(MMAD,GSD,FPD,FPF,R2,sizeY)
             
           } else {
             fit2=lm(z~log(ECD), data=data.find %>% mutate(new=abs(z-1)) %>%
                       slice_min(new,n=2) %>% select(-new))
             MMAD=exp((0-coef(fit1)[1])/coef(fit1)[2])
             sizeX=exp((1-coef(fit2)[1])/coef(fit2)[2])
             GSD=sizeX/MMAD
             R2=round(summary(fit)[["r.squared"]],4)
             FPD=pnorm(z5)*sum(pull(data.z, mass))
             FPF=FPD/sum(pull(ecd.dat, mass))
             results=tibble(MMAD,GSD,FPD,FPF,R2,sizeX)
           }
           
         } # citdas 结果
  )
  return(list(results,lm.data))
}

r=df.nest %>% mutate(
  .results=map2(data,Flow_rate,
                ~apsd_calc(.x,"name","value",FR=.y, presep=TRUE, type="usp", impactor="aci")[[1]]),
  data.z=map2(data,Flow_rate,
               ~apsd_calc(.x,"name","value",FR=.y, presep=TRUE, type="usp", impactor="aci")[[2]])
) %>% 
  select(-Flow_rate,-data) %>% 
  left_join(df, by="id") %>% 
  unnest(.results)
r
r %>% select(id, data.z, Type_of_throat) %>% 
  unnest(data.z) %>% 
  filter(!is.infinite(z)) %>% 
  ggplot(aes(log(ECD),z, color=Type_of_throat))+
  geom_point()+
  geom_smooth(method ="lm",formula ="y ~ x",se=FALSE)

###############################################################################
source("script/multi_calc_apsd.R")
multi_calc_apsd(df,"flow_rate", 2, presep=FALSE, type="usp", impactor="aci")

multi_calc_apsd(df,"Flow_rate", 10, presep=FALSE, type="citdas", impactor="aci") %>% 
  select(name, MMAD, GSD, FPD, FPF, R2)
multi_calc_apsd(df,"flow.rate", 5, presep=TRUE, type="citdas", impactor="aci") %>% 
  select(name, Capsule_batch,MMAD, GSD, FPD, FPF, R2)
multi_calc_apsd(df,"flow.rate", 2, presep=FALSE, type="citdas", impactor="ngi") %>% 
  select(name, MMAD, GSD, FPD, FPF, R2)

apsd=multi_calc_apsd(df,"flow.rate", 7, presep=TRUE, type="usp", impactor="aci") 
apsd


###############################################################
x=tibble(
  location.s=c("throat","stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc"),
  ECD15=c(NA,14.1,8.61,5.39,3.3,2.08,1.36,0.98,0),
  ECD60=c(NA,8.06,4.46,2.82,1.66,0.94,0.55,0.34,0),
  factor1=c(NA,0.54,0.52,0.50,0.47,0.52,0.60,0.67,0),
  factor2=c(NA,0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
) 
x
f=function(data, FR){
  pow=function(x,n) {x^n}
  
  if(FR >= 30){
    ecd.dat=data  %>% 
      mutate(ECD=map2_dbl(ECD60,factor1, ~ .x*pow(60/FR,.y)))
  }else{
    ecd.dat=data  %>% 
      mutate(ECD=map2_dbl(ECD15,factor2, ~ .x*pow(15/FR,.y)))
  }
  return(ecd.dat)
}
f(x, 20)

library(slider)
slide(lm.data$z, ~ .x, .before = 0, .after = 1, .complete=TRUE)
x=list(c("1.23","0.95"))
x=unlist(x)
if_else(x[1]>1 & x[2]<1, 1, 0)

find_z =function(x, n=0){
  x=unlist(x)
  if_else(x[1]>n & x[2]<n, 1, 0)
}

find_z(x,0)

data.find=lm.data %>%
  mutate(z0 = slide(z, ~find_z(.x, 0), .before =0, .after = 1),
         z1 = slide(z, ~find_z(.x, 1), .before =0, .after = 1),
         z.minus1 = slide(z, ~find_z(.x, -1), .before =0, .after = 1)
         )
data.find
is_empty(which(pull(data.find, "z1") %in% 1))

data_sub=function(data, var){
  x=which(pull(data, var) %in% 1)
  slice(data, x:(x+1))
}
data_sub(data.find, "z1")

################################################################
z=c(1.4,1.1,0.9,0.5,0.4,-0.4,-0.9,-1.1)
stages=c("stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7")
dat=data.frame(stages=stages, z=z)
dat
subset(dat, z>=-1 & z<=1)

find_z =function(x, n){
  out=NULL
  for(i in 1:(length(x)-1)){
    out[i]=ifelse(x[i]>n & x[i+1]<n, 1, 0) 
  }
  return(out)
}

nr.z1=which(find_z(dat$z,n=1) %in% 1)
nr.z1
identical(nr.z1, integer(0))

data.z1=dat[c(nr.z1, (nr.z1 + 1)),]

nr.z0=which(find_z(dat$z,n=0) %in% 1)
data.z0=dat[c(nr.z0, (nr.z0 + 1)),]

nr.zminus1=which(find_z(dat$z,n=-1) %in% 1)
data.zminus1=dat[c(nr.zminus1, (nr.zminus1 + 1)),]

location.s=c("stage1","stage2","stage3","stage4","stage5","stage6","stage7","moc")
ECD15=c(14.1,8.61,5.39,3.3,2.08,1.36,0.98,0)
ECD60=c(8.06,4.46,2.82,1.66,0.94,0.55,0.34,0)
factor1=c(0.54,0.52,0.50,0.47,0.52,0.60,0.67,0)
factor2=c(0.2692,0.4279,0.4339,0.5208,0.6130,0.7124,0.8598,0)
dat=data.frame(location.s=location.s, ECD15=ECD15,ECD60=ECD60,factor1=factor1,factor2=factor2)
dat
dat$ECD=dat$ECD60 * pow(60/45, dat$factor1)

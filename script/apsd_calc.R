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
  dat1=data.z %>% select(name,value,ECD,cm,z)
  return(list(dat1,results))
}
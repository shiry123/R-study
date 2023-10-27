PBE_calc = function(data, P, B, C, dv, P.level, m=3, logData=FALSE,
                    sigmaT0=0.1,sithap=2.0891, alpha=0.05){
  
  P=enquo(P)       #Product
  B=enquo(B)       #Batches
  C=enquo(C)       #Canister or
  dv=enquo(dv)

if(isFALSE(logData)){
  dat = data %>% mutate(LnValue=log(!!dv))
} else {
  dat = data %>% mutate(LnValue=!!dv)
}
  df.sum = dat %>% 
           group_by(!!P,!!B,!!C) %>%
           summarise( n=dplyr::n(),
                      across(LnValue, list(means = mean, sds = sd, vars=var), 
                             .names = "{.fn}" ), .groups="drop")
  
  data_R=df.sum %>% dplyr::filter(!!P==P.level[1]) #RLD 为第一个
  data_T=df.sum %>% dplyr::filter(!!P==P.level[2]) 
  
  # l=batches（批次）  n=Container(瓶数)  m 瓶内测试个数
  l.R=length(unique(pull(data_R, !!B)))
  l.T=length(unique(pull(data_T, !!B)))
  
  n.R=data_R %>% group_by(!!B) %>% summarise(n=dplyr::n()) %>% pluck(2,1)
  n.T=data_T %>% group_by(!!B) %>% summarise(n=dplyr::n()) %>% pluck(2,1)
  
  ####  MSB MSW
  MSB_R = m*var(data_R$means)
  MSW_R = sum(data_R$vars)/(n.R*l.R) 
  sigmaR = sqrt(MSB_R/m+(m-1)*MSW_R/m)
  
  MSB_T = m*var(data_T$means)
  MSW_T = sum(data_T$vars)/(n.T*l.T) 
  sigmaT = sqrt(MSB_T/m+(m-1)*MSW_T/m)
  
   #process
  Ed=(mean(data_R$means)-mean(data_T$means))^2
  E1= MSB_T/m
  E2= (m-1)*MSW_T/m
  E3s= -(1+sithap)*MSB_R/m
  E4s= -(1+sithap)*(m-1)*MSW_R/m
  Eq1=sum(c(Ed,E1,E2,E3s,E4s))
  
  # Constant-scaled
  E3c= -MSB_R/m
  E4c= -(m-1)*MSW_R/m
  Eq2= sum(c(Ed,E1,E2,E3c,E4c,-sithap*sigmaT0^2))
  # Confidence Bound
  Hd=(abs(mean(data_R$means)-mean(data_T$means))+
         qt(1-alpha, l.T*n.T + l.R*n.R - 2)*
         sqrt(MSB_T/(l.T*n.T*m)+MSB_R/(l.R*n.R*m)))^2
  H1=  (l.T*n.T-1)*E1/qchisq(alpha, l.T*n.T-1)
  H2=  l.T*n.T*(m-1)*E2/qchisq(alpha, l.T*n.T*(m-1))
  H3s= (l.R*n.R-1)*E3s/qchisq(1-alpha, l.R*n.R-1)
  H4s= l.R*n.R*(m-1)*E4s/qchisq(1-alpha, l.R*n.R*(m-1))
  H3c= (l.R*n.R-1)*E3c/qchisq(1-alpha, l.R*n.R-1)
  H4c= l.R*n.R*(m-1)*E4c/qchisq(1-alpha, l.R*n.R*(m-1))
  Ud=  (Hd-Ed)^2
  U1=  (H1-E1)^2
  U2=  (H2-E2)^2
  U3s= (H3s-E3s)^2
  U4s= (H4s-E4s)^2
  U3c= (H3c-E3c)^2
  U4c= (H4c-E4c)^2
  # Reference-scaled
  Uq1= sum(c(Ud,U1,U2,U3s,U4s))
  # Constant-scaled
  Uq2= sum(c(Ud,U1,U2,U3c,U4c))
  #Hn
  Hn1= Eq1+sqrt(Uq1)
  Hn2= Eq2+sqrt(Uq2)
  #sum
RSC=tibble(
    Parameters_By_RSC = c("Ed","E1","E2","E3s","E4s","Eq",
                         "Hd","H1","H2","H3s","H4s",
                         "Ud","U1","U2","U3s","U4s","Uq","Hn1"),
    Results_By_RSC = c(Ed,E1,E2,E3s,E4s,Eq1,
                      Hd,H1,H2,H3s,H4s,
                      Ud,U1,U2,U3s,U4s,Uq1,Hn1))

CSC=tibble(Parameters_By_CSC = c("Ed","E1","E2","E3c","E4c","Eq",
                                  "Hd","H1","H2","H3c","H4c",
                                  "Ud","U1","U2","U3c","U4c","Uq","Hn2"),
                Results_By_CSC = c(Ed,E1,E2,E3c,E4c,Eq2,
                                  Hd,H1,H2,H3c,H4c,
                                  Ud,U1,U2,U3c,U4c,Uq2,Hn2))

sum_df=cbind(RSC, CSC)  # bind_cols(RSC, CSC) tidy格式

lnm=tibble(Product=P.level, 
           l=c(l.R, l.T), 
           n=c(n.R, n.T), 
           m=m,
           Sigma=c(sigmaR, sigmaT)
)


#结论
  conclution_df=if(sigmaR>sigmaT0){
    if(Hn1<0){
      tibble(Name=c("sigmaR","Hn1","Conclusion"),
             Results=c(sigmaR,Hn1,"Pass PBE"))
    }else{
      tibble(Name=c("sigmaR","Hn1","Conclusion"),
             Results=c(sigmaR,Hn1,"Fail PBE"))
    }
  } else {
    if(Hn2<0){
      tibble(Name=c("sigmaR","Hn2","Conclusion"),
             Results=c(sigmaR,Hn2,"Pass PBE"))
    } else {
      tibble(Name=c("sigmaR","Hn2","Conclusion"),
             Results=c(sigmaR,Hn2,"Fail PBE"))
    }
  }
  return(list(sum_df, conclution_df, lnm))
}
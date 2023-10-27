df<-import("rawdata/Budesonide_pbe_rawdata.xlsx",sheet=1,as="dt")
names(df)[4] <- "LnValue"
df=df %>% mutate_vars(1:2, .func=as.character)
str(df)


lnm=df %>% 
  group_by(Product) %>% 
   summarise(l=n_distinct(Batches),
             n=n_distinct(Container)/l,
             m=n_distinct(Stage),
            .groups="drop")


##########################################################
library(tidyfst)
df.sum=df %>%
  group_dt(by =.(Product,Batches,Container),  #.() = list()
           summarise_vars("LnValue", .func = list(mean,sd,var))
  ) %>% 
wider_dt(name = "fun_name", value = "LnValue")
df.sum
############################################################

mnl<-data.frame(m=3,n=10,l=3)
# REF
sumbottle_R <- df %>%
  filter(Product=="R") %>% 
  group_by(Container) %>% 
  summarise(means=mean(LnValue),
            sds=sd(LnValue),
            vars=var(LnValue))
sumbottle_R


dataselect_R<-df %>%
  filter(Product=="R",Stage=="B")%>%
  select(Batches,Container,Product)
dataselect_R
data_R=merge(dataselect_R,sumbottle_R)
data_R
MSBR<-mnl$m*var(data_R$means)
MSWR<-sum(data_R$vars)/(mnl$n*mnl$l) 
sigmaR<-sqrt(MSBR/mnl$m+(mnl$m-1)*MSWR/mnl$m)
sigmaR
#TEST
sumbottle_T <- df %>%
  filter(Product=="T") %>% 
  group_by(Container) %>% 
  summarise(means=mean(LnValue),
            sds=sd(LnValue),
            vars=var(LnValue))
sumbottle_T
dataselect_T<-df %>%
  filter(Product=="T",Stage=="B")%>%
  select(Batches,Container,Product)
dataselect_T
data_T=merge(dataselect_T,sumbottle_T)
data_T
MSBT<-mnl$m*var(data_T$means)
MSWT<-sum(data_T$vars)/(mnl$n*mnl$l) 
sigmaT<-sqrt(MSBT/mnl$m+(mnl$m-1)*MSWT/mnl$m)
sigmaT
#sigma
sigmaT0=0.1
sithap=2.0891
alpha=0.05
# Process
Ed<-(mean(data_R$means)-mean(data_T$means))^2
E1<-MSBT/mnl$m
E2<-(mnl$m-1)*MSWT/mnl$m
E3s=-(1+sithap)*MSBR/mnl$m
E4s=-(1+sithap)*(mnl$m-1)*MSWR/mnl$m
Eq1<-sum(c(Ed,E1,E2,E3s,E4s))
# Constant-scaled
E3c=-MSBR/mnl$m
E4c=-(mnl$m-1)*MSWR/mnl$m
Eq2<-sum(c(Ed,E1,E2,E3c,E4c,-sithap*sigmaT0^2))
# Confidence Bound
Hd<-(abs(mean(data_R$means)-mean(data_T$means))+
       qt(1-alpha,mnl$l*mnl$n+mnl$l*mnl$n-2)*
       sqrt(MSBT/(mnl$l*mnl$n*mnl$m)+MSBR/(mnl$l*mnl$n*mnl$m)))^2
H1<-(mnl$l*mnl$n-1)*E1/qchisq(alpha,mnl$l*mnl$n-1)
H2<-mnl$l*mnl$n*(mnl$m-1)*E2/qchisq(alpha,mnl$l*mnl$n*(mnl$m-1))
H3s<-(mnl$l*mnl$n-1)*E3s/qchisq(1-alpha,mnl$l*mnl$n-1)
H4s<-mnl$l*mnl$n*(mnl$m-1)*E4s/qchisq(1-alpha,mnl$l*mnl$n*(mnl$m-1))
H3c<-(mnl$l*mnl$n-1)*E3c/qchisq(1-alpha,mnl$l*mnl$n-1)
H4c<-mnl$l*mnl$n*(mnl$m-1)*E4c/qchisq(1-alpha,mnl$l*mnl$n*(mnl$m-1))
Ud<-(Hd-Ed)^2
U1<-(H1-E1)^2
U2<-(H2-E2)^2
U3s<-(H3s-E3s)^2
U4s<-(H4s-E4s)^2
U3c<-(H3c-E3c)^2
U4c<-(H4c-E4c)^2
# Reference-scaled
Uq1<-sum(c(Ud,U1,U2,U3s,U4s))
# Constant-scaled
Uq2<-sum(c(Ud,U1,U2,U3c,U4c))
#Hn
Hn1<-Eq1+sqrt(Uq1)
Hn2<-Eq2+sqrt(Uq2)
#sum
mylist1<-list(id=1:18,Parameters_ByRSC=c("Ed","E1","E2","E3s","E4s","Eq",
                                         "Hd","H1","H2","H3s","H4s",
                                         "Ud","U1","U2","U3s","U4s","Uq","Hn1"),
              Results_ByRSC=c(Ed,E1,E2,E3s,E4s,Eq1,
                              Hd,H1,H2,H3s,H4s,
                              Ud,U1,U2,U3s,U4s,Uq1,Hn1))
RSC<-as.data.frame(mylist1)
RSC
mylist2<-list(Parameters_ByCSC=c("Ed","E1","E2","E3c","E4c","Eq",
                                 "Hd","H1","H2","H3c","H4c",
                                 "Ud","U1","U2","U3c","U4c","Uq","Hn2"),
              Results_ByCSC=c(Ed,E1,E2,E3c,E4c,Eq2,
                              Hd,H1,H2,H3c,H4c,
                              Ud,U1,U2,U3c,U4c,Uq2,Hn2))
CSC<-as.data.frame(mylist2)
CSC
sum_df<-cbind(RSC,CSC)
sum_df
# PBE
conclution_df<-if(sigmaR>sigmaT0){
  if(Hn1<0){
    data.frame(Name=c("sigmaR","Hn1","Conclusion"),
               Results=c(sigmaR,Hn1,"Pass PBE"))
  }else{
    data.frame(Name=c("sigmaR","Hn1","Conclusion"),
               Results=c(sigmaR,Hn1,"Fail PBE"))
  }
}else{
  if(Hn2<0){
    data.frame(Name=c("sigmaR","Hn2","Conclusion"),
               Results=c(sigmaR,Hn2,"Pass PBE"))
  }else{
    data.frame(Name=c("sigmaR","Hn2","Conclusion"),
               Results=c(sigmaR,Hn2,"Fail PBE"))
  }
}
conclution_df




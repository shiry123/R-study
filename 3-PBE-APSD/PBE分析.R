df=import("rawdata/other/PBE数据模板.xlsx", sheet=1)
sample_n(df,10)

df %>% 
  mutate(mu=pmap_dbl(pick(B1:E120), ~mean(c(...), na.rm=TRUE)))


df.long=df %>% 
  pivot_longer(B1:E120, names_to = c(".value","act"),  names_pattern ="(.)(.*)") %>% 
  pivot_longer(B:E, names_to = "lifestage",values_to = "amount") %>% 
  drop_na() %>% 
  mutate(lifestage=fct_relevel(lifestage, c("B","M","E"))) %>% 
  mutate(act=as.numeric(act))
df.long
source("script/data_plot.R")
data_plot(df.long, x=lifestage, y=amount, trace=lot,facets = product,
             type="point",add="mean")
  geom_hline(yintercept=80*0.80,color="black")+ #不能超过1个
  geom_hline(yintercept=80*1.2,color="black")+
  geom_hline(yintercept=80*0.75,color="red")+
  geom_hline(yintercept=80*1.25,color="red")

df.long %>% 
  group_by(product) %>%
  summarise(l=n_distinct(lot),
            n=n_distinct(bottle)/l,
            m=n_distinct(lifestage),
            .groups="drop")
########################################################################
getwd()
setwd("D:/Mywd")
df=import("rawdata/other/PBE-Analysis.xlsx", sheet ="data")
source("script/data_calc.R")
data_calc(df, cols=c(B,M,E),func=list(mean,sd,var), row.calc = TRUE)
df.long=df %>% mutate(across(c(Product,Batches,Canister), as.factor)) %>% 
        pivot_longer(c(B,M,E), names_to = "life.stage", values_to = "FPD")
df.long
data_calc(df.long, cols=FPD,func=list(mean,sd,var),
          by=c(Product,Batches,Canister)) %>% arrange(desc(Product))
df.long %>% 
  group_by(Product) %>% 
  summarise(l=n_distinct(Batches),
            n=n_distinct(Canister)/l,
            m=n_distinct(life.stage),
            .groups="drop")

source("script/data_plot.R")
data_plot(df.long, x=Product, y=FPD, trace=Batches, type="box")

source("script/PBE_calc.R")
PBE_calc(df.long, Product, Batches, Canister, dv=FPD,
        P.level=c("R","T"), logData=FALSE, m=3)


df=import("rawdata/PBE-Analysis.xlsx", sheet ="data2")
df=df %>% mutate(across(c(Product,Batches,Canister), as.factor))
str(df)
summary(df)
df %>% 
  group_by(Product) %>% 
  summarise(l=n_distinct(Batches),
            n=n_distinct(Canister)/l,
            m=n_distinct(Stage),
            .groups="drop")

source("script/PBE_calc.R")
PBE_calc(df, Product, Batches, Canister, dv=logFPD, 
        P.level=c("REF","TEST"), logData=TRUE, m=3)

df1=df %>% filter(Stage %in% cc("B,E"))
PBE_calc(df1, Product, Batches, Canister, dv=logFPD, 
        P.level=c("REF","TEST"), logData=TRUE, m=2)


summarise_vars()
############################################################################
f=function(data, P, B, C, dv, P.level, m=3){
  P=enquo(P)
  B=enquo(B)
  C=enquo(C)
  dv=enquo(dv)
  
  df.sum=data %>% 
      mutate(LnValue=log(!!dv)) %>% 
      group_by(!!P,!!B,!!C) %>%
      summarise(
      n=dplyr::n(),
      across(LnValue, list(means = mean, sds = sd, vars=var), 
             .names = "{.fn}" ), .groups="drop")
  data_R=df.sum %>% dplyr::filter(!!P==P.level[1]) 
  MSB_R<-m*var(data_R$means)
  
  l.R=length(unique(pull(data_R, !!B)))
  n.R=data_R %>% group_by(!!B) %>% summarise(n=dplyr::n()) %>% pluck(2,1)
  
  
  return(list(data_R,MSB_R,l.R,n.R))
}
f(df, Product, Batches, Canister, dv=logFPD, 
  P.level=c("REF","TEST"),  m=1)

getwd()
library(tidyfst)
ct<-import("rawdata/BE22m.xlsx",sheet=1, as="dt")
head(ct)
# ggplot
ct %>% 
  mutate(Subject=factor(ct$Subject)) %>% 
  group_by(Subject,Formulation) %>% 
  ggplot(aes(x=Time,y=Conc,color=Formulation))+
  facet_wrap(~Subject,ncol=6)+
  geom_point()+
  geom_line()+
  theme_bw()

ct %>% 
  mutate(Subject=factor(ct$Subject)) %>% 
  dplyr::filter(if_all(Time , ~.x >= 0 )) %>% 
  ggplot(aes(x=Time,y=Conc,color=Formulation))+
  facet_wrap(~Subject,ncol=6)+
  geom_point()+
  geom_line()+
  theme_bw()
  
# REF
dataref<-filter_dt(ct,Formulation=="R")
#NCA
library(NonCompart)
Unit(timeUnit="h",concUnit="ug/L",doseUnit="ug")
ncaref<-tblNCA(dataref, key="Subject", colTime="Time", colConc="Conc", 
               dose=375000,  adm="Infusion", dur=0.5, 
               doseUnit="ug", concUnit="ug/L", R2ADJ=0.9, MW=0)

datatest<-filter_dt(ct,Formulation=="T")
ncatest<-tblNCA(datatest, key="Subject", colTime="Time", colConc="Conc", 
                dose=25, adm="Infusion", dur=0.5,
                doseUnit="ug", concUnit="ug/L", R2ADJ=0.9, MW=0)

selectdatatest=datatest %>% filter_dt(Time=="0")  %>% 
                        select_dt(Subject,Sequence,Period,Formulation)

pktest = ncatest %>% select_dt(Subject,CMAX,TMAX,AUCLST,AUCIFO)

pkt=left_join_dt(selectdatatest, pktest, by="Subject") %>%
            mutate_dt(LNCMAX=log(CMAX),
                      LNAUCLST=log(AUCLST),
                      LNAUCIFO=log(AUCIFO)
                      )
pkt

####################################################################
# selectdataref<-subset(dataref, Time=="0",
#                       select=c(Subject,Sequence,Period,Formulation))
selectdataref = dataref %>% filter_dt(Time=="0")  %>% 
                 select_dt(Subject,Sequence,Period,Formulation)

pkref = ncaref %>% select_dt(Subject,CMAX,TMAX,AUCLST,AUCIFO)

pkr=left_join_dt(selectdataref, pkref, by="Subject") %>%
         mutate_dt(LNCMAX=log(CMAX),
                   LNAUCLST=log(AUCLST),
                   LNAUCIFO=log(AUCIFO)
                  )
pkr
pk<-rbind(pkt, pkr)
pk
with(pk, table(Sequence,Period,Formulation))
export(pk,"outfiles/BE22m-pk.xlsx")


pk = pk %>% mutate_vars("Subject|Sequence|Period|Formulation", as_factor)
pk

# lme包提供了非线性混合效应模型的功能，可以处理非线性关系和非正态分布的数据。
# lmer包只能处理线性混合效应模型，但是可以处理多层嵌套或交叉因素的模型。
library(nlme)
modeAUCLST1<-lme(LNAUCLST~ 1+Period+Formulation+Sequence, data=pk,
                 random= ~Sequence|Subject)  #随机因素 Subject
summary(modeAUCLST1)
anova(modeAUCLST1) # 生成一个拟合模型的方差分析表

#lm做方差分析
modAUCLST2<-lm(LNAUCLST~Subject+Period+Formulation, data=pk) 
summary(modAUCLST2)
anova(modAUCLST2)

#CI
library(PowerTOST)
mse_AUCLST<-sum(residuals(modAUCLST2)^2)/modAUCLST2$df.residual
mse_AUCLST
mse <- anova(modAUCLST2)[[3]][[4]]
mse
cv_AUCLST<-mse2CV(mse_AUCLST) 
cv_AUCLST

mean(pkr$LNAUCLST)
pk.sum=pk %>% summarise_dt(avg=mean(LNAUCLST),
                    std=mean(LNAUCLST),
                    by=Formulation) 

pe_AUCLST = exp(mean(pkr$LNAUCLST) -mean(pkt$LNAUCLST) )  #GMR
pe_AUCLST

CI.BE(alpha=0.05, pe=pe_AUCLST, CV=cv_AUCLST, n=33, 
                 design = "2x2", robust = FALSE) 
power.TOST(theta1=0.8,theta0=pe_AUCLST,
                              CV=cv_AUCLST, n=33, design = "2x2")
sampleN.TOST(CV=cv_AUCLST, theta0=pe_AUCLST)
# 通过个体内变异或者几何均值比的90%置信区间可以计算出最低理论样本量


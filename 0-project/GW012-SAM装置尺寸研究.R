#####################################################################
library(openxlsx)
apsd<- openxlsx::read.xlsx("rawdata/GW012/GW012-SAM装置尺寸研究.xlsx",sheet="apsd",
                 colNames = FALSE,rowNames = TRUE)
apsd=data.frame(t(apsd))
apsd= apsd %>% mutate(across(9:28,~as.numeric(.x)))
glimpse(apsd)
apsd=apsd %>% 
  unite(col = "Canister", Canister_Batch, Canister_Id) %>% 
  unite(col = "Actuator",Device_Batch_MBA, Device_Id_MBA, remove = F)

plot_data=apsd %>% select(1:18) %>%  
         pivot_longer(cols =9:18, names_to ="location", values_to = "amount") %>%
         mutate(location=fct_relevel(location,
                              c("Throat","Stage0","Stage1","Stage2","Stage3",
                                "Stage4","Stage5","Stage6","Stage7","Filter"))) 
ggplot(plot_data, aes(x=location, y=amount, color=Device_Batch_MBA))+
  geom_line(aes(group=Actuator))+
  geom_point(size=2)+
  theme_pubr()+
  theme(panel.grid.major = element_line(),
        legend.position = "right")

### 平均
plot_data %>% 
  ggplot(aes(x=location, y=amount, group=Device_Batch_MBA))+
  stat_summary(data=plot_data %>% filter(Device_Batch_MBA == "AFS62A"),
               fun.data = "mean_sd", geom="pointrange")+
  stat_summary(aes(color=Device_Batch_MBA), 
               fun="mean",geom="line",linewidth=1)+
  stat_summary(fun="mean",geom="point",size=1.5, color="black")+
  theme_classic()+
  theme(panel.grid.major = element_line(),
        legend.position = "right")

# DDU
ddu<- import("rawdata/GW012/GW012-SAM装置尺寸研究.xlsx",sheet="ddu")

ddu %>% 
  unite(col = "Canister", Canister_Batch, Canister_Id) %>% 
  unite(col = "Actuator",Device_Batch_MBA, Device_Id_MBA, remove = F) %>% 
  ggplot(aes(x=Device_Batch_MBA, y=Delivered_Dose_μg, 
             fill=Device_Batch_MBA))+
  geom_boxplot()+
  geom_jitter(width=0.1)+
  geom_hline(aes(yintercept=80*0.75), color="Red") +  #75~125%
  geom_hline(aes(yintercept=80*1.25), color="Red")+
  geom_hline(aes(yintercept=80*0.80))+
  geom_hline(aes(yintercept=80*1.2))+
  geom_hline(aes(yintercept=80), linetype="dashed")+
  theme_classic()+
  theme(panel.grid.major = element_line(),
        legend.position = "right")


#################################################################
sp<- import("rawdata/GW012/GW012-SAM装置尺寸研究.xlsx",sheet="sp") %>%
  janitor::clean_names()
glimpse(sp)
sp=sp %>% 
  separate(lot_number, c("canister_batch","canister_id", "o.act")) %>% 
  unite("canister",canister_batch, canister_id, remove=FALSE) %>% 
  mutate(o.act=as.numeric(o.act),
         act=o.act+actuation_number) %>% 
  select(-o.act) %>% 
  rename(actuator=identification_number) %>% 
  separate(actuator, c("actuator_batch", "actuator_id"),  remove = FALSE) 
colnames(sp)
ggplot(sp, 
       aes(x=act, y=area_mm_2, color=actuator_batch))+
  geom_line(aes(group=actuator))+
  geom_point2()

### boxplot
ggplot(sp, 
       aes(x=actuator, y=area_mm_2, fill=actuator_batch))+   #  ovality 
  stat_boxplot(geom = "errorbar",width=0.2)+ 
  geom_boxplot(outlier.shape = 8)+
  theme_lucid(axis.text.angle = 90)


unique(sp$actuator_batch)
sp %>% get_comparisons("actuator_batch")

ggplot(sp,
       aes(x=actuator_batch, y=area_mm_2, fill=actuator_batch))+
  geom_boxplot()+
  geom_jitter(width = 0.2)+
  theme_lucid(axis.text.angle = 90)+
  stat_compare_means(method="anova", label.y = 1300)+
  stat_compare_means(comparisons=sp %>% get_comparisons("actuator_batch"), method="t.test")

#############################################
pg<- import("rawdata/GW012/GW012-SAM装置尺寸研究.xlsx",sheet="pg") %>% 
  janitor::clean_names()
colnames(pg)
pg=pg %>% 
  separate(lot_number, cc("canister_batch,canister_id, o.act")) %>% 
  unite("canister",canister_batch, canister_id, remove=FALSE) %>% 
  mutate(o.act=as.numeric(o.act),
         act=o.act+actuation_number) %>% 
  select(-o.act) %>% 
  rename(actuator=identification_number) %>% 
  separate(actuator, cc("actuator_batch, actuator_id"),  remove = FALSE) 
glimpse(pg)

ggplot(pg %>% filter(plume_angle_deg <= 50),   
       aes(x=actuator, y=plume_angle_deg, fill=actuator_batch))+
  stat_boxplot(geom = "errorbar",width=0.2)+ 
  geom_boxplot(outlier.shape = 8)+
  theme_lucid(axis.text.angle = 90)

ggplot(pg, aes(x=actuator, y=plume_width_mm, fill=actuator_batch))+
  stat_boxplot(geom = "errorbar",width=0.2)+ 
  geom_boxplot(outlier.shape = 8)+
  theme_lucid(axis.text.angle = 90)

#####
ggplot(pg %>% filter(plume_angle_deg <= 50), 
       aes(x=actuator_batch, y=plume_angle_deg, fill=actuator_batch))+
  geom_boxplot()+
  geom_jitter(width = 0.2)+
  theme_lucid(axis.text.angle = 0)+
  stat_compare_means(method="anova", label.y = 55)+
  stat_compare_means(comparisons=sp %>% get_comparisons("actuator_batch"), method="t.test")

############################################################################
df=import()
str(df)
colnames(df)
res.lm=lm(C_FPD_μg ~ OD_mm+JL_mm, data=df)
summary(res.lm)
anova(res.lm)
coef(res.lm)


summary(res.lm)$adj.r.squared

broom::tidy(res.lm)
broom::glance(res.lm)
broom::augment(res.lm)


dat=df %>% select(5:6,31)
dat
x.var=colnames(dat[,1:2])
x.var
sapply(df[,x.var], mean,na.rm=T)

source("script/relax_opt.R")
relax_opt(df, x.var, res.lm, stepsize=0.05, n=2, type="max")  ##data x1  x2  y

#######################################################
# x=c(OD, JL)
f=function(x){
  coef(res.lm)[[1]]+coef(res.lm)[[2]]*x[1]+coef(res.lm)[[3]]*x[2]
}
f(c(0.24,0.54))
##################################################################
x.pred <- expand_grid(
  OD_mm = seq(min(df$OD_mm,na.rm=T), 
              max(df$OD_mm,na.rm=T), length=15),
  JL_mm  = seq(min(df$JL_mm), 
              max(df$JL_mm), length=15))
str(x.pred)
x.pred=x.pred %>% modelr::add_predictions(res.lm)
x.pred
library(lattice)
wireframe(pred ~ OD_mm*JL_mm, data=x.pred,
          scales = list(arrows = FALSE),
          col.regions =pals::parula(100),  #pals::parula(100) 颜色
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 10, x = -80, y=5))


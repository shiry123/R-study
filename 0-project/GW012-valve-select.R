sp<- import("rawdata/GW012/GW012-valve-select.xlsx",sheet="sp") 
sp
sp %>% summarise(across(everything(),  ~ sum(is.na(.x))))
sp %>% map_df(~ sum(is.na(.)))
sp %>%
  filter(if_any(everything(), is.na))

sp=sp %>% drop_na() %>%
mutate(
  lifestage=sjmisc::rec(act, rec="lo:60=B; 61:100=M; else=NA"),
  test.date=lubridate::as_date(Actuation_Date),
  formulation=map_chr(Canister_Batch, ~if_else(str_sub(.x,1,1)=="A","RLD","CF")),
  valve=sjmisc::rec(Canister_Batch,
            rec="AFS62A,W21111203=Teva; 
                W21111204 =Bespak; 
                W21111205 = Aptar;
                W21111206 =Lindal;
                       else=NA")) %>% 
  unite(col = "canister",Canister_Batch, Canister_Id, remove = FALSE) %>% 
  unite(col = "actuator",Device_Batch_MBA, Device_Id_MBA)

bxp=ggplot(data=filter(sp,Area_mm2 <= 600),
           aes(x=actuator, y=Area_mm2, fill=valve))+
  ggh4x::facet_nested(~lifestage+formulation+valve)+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))
bxp

sp.filter=sp %>% filter(Area_mm2 <= 600) %>% 
  select(lifestage,Canister_Batch,formulation,valve,canister,actuator,act, Area_mm2) %>% 
  convert_as_factor(Canister_Batch,formulation,valve,canister,actuator) %>% 
  arrange(formulation,valve,canister)
sp.filter
summary(sp.filter)
sp.filter %>% filter(formulation=="CF") %>% 
  anova_test(Area_mm2 ~ lifestage+actuator*valve)
  print_table(file = "outfiles/out.doc")


############################### 随机化实验 M
sp.cf=sp.filter %>% filter(formulation=="CF", lifestage=="M") 
with(sp.cf,table(actuator,valve))

### ANOVA分析
df.aov=anova_test(data=sp.cf, Area_mm2 ~ actuator*valve)
df.aov
get_anova_table(df.aov)
pwc=sp.cf %>% 
  group_by(actuator) %>% 
  emmeans_test(Area_mm2~valve, p.adjust.method = "bonferroni") %>% 
  filter(p.adj.signif != "ns")
pwc
pwc=pwc %>% add_xy_position(x="valve")

df.mean=sp.cf %>%
  group_by(actuator,valve) %>%
  get_summary_stats(Area_mm2,type = "mean_sd")
df.mean

source("script/data_calc.R")
data_calc(sp.cf, cols=Area_mm2, by=c(actuator,valve)) 

p=qplot(data=sp.cf, x=valve, y=Area_mm2, geom="boxplot",color=valve)+
  facet_nested(~ actuator, nest_line = element_line(linetype = 1))+
  theme_modern(axis.text.angle = 90)+
  geom_text(data=df.mean, aes(label=paste("n=",{n}), y=-Inf), 
            size=3, hjust =0.5, vjust=-0.5, color="black")
p+stat_pvalue_manual(pwc)+
  labs(caption = get_pwc_label(pwc))
library(afex)
# sp2.cf$ID=1:nrow(sp2.cf)
sp.cf=sp.cf %>% rowid_to_column("id") %>% convert_as_factor(id)
str(sp.cf)
fit2=aov_car(data=sp.cf, Area_mm2 ~ actuator*valve+Error(id))
summary(fit2)
emmeans(fit2, pairwise ~ valve|actuator)
afex::afex_plot(fit2, ~actuator, ~valve,  mapping = "color")+
  theme_lucid(axis.text.angle = 0)

###############全部分析
sp.sum=sp %>% filter(Area_mm2 <= 600) %>% 
  group_by(canister, actuator, lifestage) %>% 
  get_summary_stats(Area_mm2,type = "mean_sd") 
sp.sum
rld.line=sp.sum %>% 
  mutate(m1=mean*0.9, m2=mean*1.1) %>%
  filter(canister %in% c("AFS62A_001002",
                         "AFS62A_038384",
                         "AFS62A_090138"))
rld.line
rld.ratio=sp.sum %>%
  group_by(actuator,lifestage) %>% 
  mutate(first.area=first(mean, order_by = canister),
         ratio=map2_dbl(mean, first.area, ~.x/.y))
rld.ratio
###ggplot2
ggplot(data=sp %>% filter(Area_mm2 <= 600), aes(x=canister,y=Area_mm2))+
  facet_nested(~lifestage+actuator)+  #, labeller = "label_both"
  stat_boxplot(geom = "errorbar",width=0.5)+ 
  geom_boxplot(aes(color=valve), outlier.shape = 8)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  stat_summary(fun = mean, geom = "point",color = "black")+
  geom_text(data=rld.ratio, 
            aes(label=scales::percent(round(ratio,2)),y=-Inf),
            hjust =0.6,vjust=-1,size=2,
            color=if_else(rld.ratio$ratio<0.9,"Red","Black"))+
  geom_hline(data=rld.line, aes(yintercept = mean), linetype="dashed")+
  geom_hline(data=rld.line, aes(yintercept = m1), colour="red")+
  geom_hline(data=rld.line, aes(yintercept = m2), colour="red")+
  theme(legend.position = "top")

##########################################################################
sp %>%
  group_by(canister, actuator, lifestage) %>%
  get_summary_stats(Area_mm2,type = "mean_sd")
## across
sp.mean=sp %>%
  group_by(Canister_Batch) %>%
  summarise(
    n=dplyr::n(),
    across(c(Area_mm2,Ovality), list(Mean = mean, SD = sd), na.rm = TRUE,
           .names = "{stringr::str_remove(.col, '_mm2')}_{.fn}" ),
    .groups="drop"  #  keep  rowwise
  )
sp.mean
############################################################
pg<- read_excel("GW012-valve-select.xlsx",sheet="pg")
pg
summary(pg)
#################
pg=pg %>% drop_na() %>%
  mutate(
    lifestage=RECODE(act,"lo:56='B'; 90:hi='M'; else=NA"),
    test.date=lubridate::as_date(Actuation_Date),
    formulation=map_chr(Canister_Batch, ~if_else(str_sub(.x,1,1)=="A","RLD","CF")),
    valve=RECODE(Canister_Batch,
                 "c('AFS62A','W21111203')='Teva'; 
             'W21111204'='Bespak'; 
             'W21111205'='Aptar';
             'W21111206'='Lindal';
              else=NA")) %>% 
  unite(col = "canister",Canister_Batch, Canister_Id, remove = FALSE) %>% 
  unite(col = "actuator",Device_Batch_MBA, Device_Id_MBA)
pg
bxp=qplot(data=pg,x=actuator, y=Plume_Width_mm, geom="boxplot", fill=valve)+
  facet_nested(~lifestage+formulation+valve)+
  theme_bw()+theme(axis.text.x = element_text(angle=90,vjust=0.5))
bxp

pg %>% filter(valve=="Teva") %>% 
  group_by(lifestage) %>% 
  anova_test(Plume_Angle_deg ~ formulation*actuator) 

pg %>% filter(formulation=="CF") %>% 
  group_by(lifestage) %>% 
  anova_test(Plume_Angle_deg ~ actuator*valve)

#########################################随机化
pg2=pg %>% filter(lifestage == "M") %>% 
  select(formulation,valve,canister,actuator,act,
         Plume_Angle_deg,Plume_Width_mm, test.date) %>% 
  convert_as_factor(formulation,valve,canister,actuator) %>% 
  arrange(formulation,valve,canister)

pg2 %>% mutate(across(formulation:test.date, ~as.numeric(.x))) %>% 
  cor_mat() %>%
  cor_reorder() %>% pull_lower_triangle() %>% cor_plot(label = TRUE)

#散点图
qplot(data=pg2,x=act,y=Plume_Angle_deg, color=factor(test.date))+
  # facet_nested(actuator~formulation+valve)+
  theme_bw()
df.mean=pg2 %>% group_by(formulation, actuator,valve) %>% 
  get_summary_stats(c(Plume_Angle_deg,Plume_Width_mm), type="mean_sd")
df.mean
#boxplot
p1=qplot(data=pg2, x=valve, y=Plume_Angle_deg, geom="boxplot",color=valve)+
  facet_nested(~ formulation+actuator, nest_line = element_line(linetype = 1))+
  theme_modern(axis.text.angle = 90)+
  geom_text(data=df.mean, aes(label=paste("n=",{n}), y=-Inf), 
            size=3, hjust =0.5, vjust=-0.5, color="black")
p1
p2=qplot(data=pg2, x=valve, y=Plume_Width_mm, geom="boxplot",color=valve)+
  facet_nested(~ formulation+actuator, nest_line = element_line(linetype = 1))+
  theme_modern(axis.text.angle = 90)+
  geom_text(data=df.mean, aes(label=paste("n=",{n}), y=-Inf), 
            size=3, hjust =0.5, vjust=-0.5, color="black")
plot_grid(p1,p2, ncol = 1)
#ANOVA
table(filter(pg2, valve=="Teva")$formulation,
      filter(pg2, valve=="Teva")$actuator)
pg2 %>% filter(valve=="Teva") %>% 
  anova_test(Plume_Width_mm ~ formulation*actuator)
pg2 %>% filter(valve=="Teva") %>% 
  anova_test(Plume_Angle_deg ~ formulation*actuator)
# HH::interaction2wt(data=pg2 %>% filter(valve=="Teva"),
#                    Plume_Width_mm ~ formulation*actuator,
#                    par.strip.text=list(cex=.55))
#CF
pg2.cf=pg2 %>% filter(formulation=="CF") 
table(pg2.cf$valve, pg2.cf$actuator)
anova_test(data=pg2.cf, Plume_Angle_deg ~ actuator*valve)
anova_test(data=pg2.cf, Plume_Width_mm ~ actuator*valve)
# HH::interaction2wt(data=pg2.cf, Plume_Width_mm ~ actuator*valve,
#                    par.strip.text=list(cex=.55))
# log
pgcf.log=pg2.cf %>% mutate(logAngle=log(Plume_Angle_deg),
                  logWidth=log(Plume_Width_mm)) 
# Mean plot
pgcf.log %>% group_by(actuator, valve) %>% 
  get_summary_stats(c(logAngle,logWidth), type="mean_sd") %>% 
  ggplot(aes(x=valve, y=mean, color=actuator))+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2)+
  facet_nested(~variable+actuator, nest_line = element_line(linetype = 1))+
  geom_point()+
  theme_modern(axis.text.angle = 90)
#ANOVA
anova_test(data=pgcf.log, logAngle ~ actuator*valve)
anova_test(data=pgcf.log, logWidth ~ actuator*valve)

fit.aov=pg2.cf %>% rowid_to_column("id") %>% convert_as_factor(id) %>% 
  aov_car(data=., Plume_Width_mm ~ actuator*valve+Error(id))
summary(fit.aov)
emmip(fit.aov, valve ~ actuator)
afex_plot(fit.aov, ~valve, ~actuator, mapping = "color")+theme_lucid()

################################################################
ddu<- read_excel("GW012-valve-select.xlsx",sheet="ddu")
ddu$act = as.numeric(ddu$act)
ddu
ddu.life=ddu %>% 
  mutate(
    lifestage=RECODE(act,"lo:59='B'; 60:115='M'; 116:hi='E'; else=NA"),
    formulation=map_chr(Canister_Batch, ~if_else(str_sub(.x,1,1)=="A","RLD","CF")),
    valve=RECODE(Canister_Batch,
                 "c('AFS62A','W21111203')='Teva'; 
                 'W21111204'='Bespak'; 
                 'W21111205'='Aptar';
                'W21111206'='Lindal'; else=NA")) %>% 
  unite(col = "canister",Canister_Batch, Canister_Id, remove = FALSE) %>% 
  unite(col = "actuator",Device_Batch_MBA, Device_Id_MBA)
str(ddu.life)
# library(ggh4x)
ggplot(ddu.life, aes(x=act,y=DD_μg, color=actuator))+
  facet_nested(~formulation+valve)+  #, labeller = "label_both"
  geom_point()+
  stat_smooth(method = lm, formula =y~x , se = FALSE,
              linetype="dashed",alpha = 0.5, size=0.5)+
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120,by=60))+
  geom_hline(yintercept=60, colour="red")+
  geom_hline(yintercept=100, colour="red")+
  geom_hline(aes(yintercept=64), colour="red", linetype="dashed") +  #80~120%
  geom_hline(aes(yintercept=96), colour="red", linetype="dashed")+
  theme_bw()
#############################
ddu.life %>%
  df_nest_by(formulation, valve, actuator) %>% 
  mutate(model=map(data,~lm(DD_μg~lifestage, data=.x)),
         rmse=map2_dbl(model, data, modelr::rmse),
         r2=map2_dbl(model, data, modelr::rsquare),
         slope=map_dbl(model, ~coef(.x)[[2]]),
         p.val=map_dbl(model, ~broom::glance(.x)$p.value) %>%
               p_format(digits = 2) %>% p_mark_significant()
         )  
  print_table(file="outfile.doc")
#均值偏差（B、E）
ddu.life %>% filter(lifestage %in% c("B","E")) %>% 
    group_by(canister,actuator,lifestage) %>% 
    get_summary_stats(DD_μg, type = "mean_sd") %>% 
    group_by(canister) %>% 
    mutate(lead_mean=lead(mean),    #lag 
    diff_mean=map2_dbl(mean,lead_mean, ~.y-.x)) %>% 
    drop_na() %>% select(canister,actuator,diff_mean)

############################################################################
library(openxlsx)
apsd<- read.xlsx("GW012-valve-select.xlsx",sheet="apsd",
                colNames = FALSE,rowNames = TRUE)
apsd= data.frame(t(apsd)) %>% as_tibble()
colnames(apsd)
apsd= apsd %>% mutate(across(8:27,~as.numeric(.x))) 
str(apsd)
df=apsd %>%
  mutate(
    # lifestage=RECODE(Last_act,"10='B'; 69='M'; 115='E'; else=NA"),
    formulation=map_chr(Canister_Batch, ~if_else(str_sub(.x,1,1)=="A","RLD","CF")),
    valve=RECODE(Canister_Batch,
                 "c('AFS62A','W21111203')='Teva'; 
                 'W21111204'='Bespak'; 
                 'W21111205'='Aptar';
                'W21111206'='Lindal'; else=NA")) %>% 
  unite(col = "canister",Canister_Batch, Canister_Id) %>% 
  unite(col = "actuator",Device_Batch_MBA, Device_Id_MBA) %>% 
  unite(col = "valve_formulation",valve, formulation) %>% 
  unite(col = "sample",canister, actuator, remove = F)
colnames(df)
x.levels=cc("Throat,Stage0,Stage1,Stage2,Stage3,Stage4,Stage5,Stage6,Stage7,Filter")
colnames(plotdata)
plotdata=df %>% select(27,1:4,6:17) %>% 
  pivot_longer(-c(1:6), names_to ="location", values_to = "amount") %>% 
  unite(col = "sample_life",sample, life_stage, remove = F) %>% 
  mutate(location=fct_relevel(location, x.levels),
         life_stage=fct_relevel(life_stage, cc("B,M,E"))) 
plotdata %>% 
  ggplot(aes(x=location, y=amount, color=life_stage))+
  facet_grid(~valve_formulation)+
  geom_line(aes(group=sample_life))+     
  geom_point()+
  theme_lucid(axis.text.angle = 90)

############################################################################
shotweight<- read_excel("GW012-valve-select.xlsx",sheet="weight") 
shotweight$act = as.numeric(shotweight$act)
str(shotweight)
sw.life=shotweight %>% 
  mutate(
    lifestage=RECODE(act,"lo:59='B'; 60:115='M'; 116:hi='E'; else=NA"),
    formulation=map_chr(Canister_Batch, ~if_else(str_sub(.x,1,1)=="A","RLD","CF")),
    valve=RECODE(Canister_Batch,
                 "c('AFS62A','W21111203')='Teva'; 
                 'W21111204'='Bespak'; 
                 'W21111205'='Aptar';
                'W21111206'='Lindal'; else=NA")) %>% 
  unite(col = "canister",Canister_Batch, Canister_Id, remove = FALSE) %>% 
  unite(col = "actuator",Device_Batch_MBA, Device_Id_MBA) %>% 
  unite(col = "valve.from",formulation,valve)



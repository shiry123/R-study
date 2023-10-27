getwd()
setwd("D:/Mywd")
theme_set(theme_minimal())

df=read_clip_tbl() %>% janitor::clean_names()
df


##################################################################################
df=df %>% mutate(
  canister_batch=sjmisc::rec(canister_batch,rec ="AGA01A=AGA01A(RLD);
                                                  AGA56A=AGA56A(RLD);
                                                  W22101904=W22101904(Rxpack);
                                                  W22102705=W22102705(Bespack)")
) 

#含量
source("script/data_calc.R")
df %>% 
  group_by(canister_batch,conditions,days) %>%
  data_calc(cols =assay, fun_names = c("min","max","mean")) %>%
  ggplot(aes(x=days, y=mean, color=conditions))+
  geom_pointrange(aes(ymin=min,ymax=max))+
  geom_line(aes(group=conditions))+
  labs(y="含量(mg/g)")+
  facet_grid(~canister_batch, scales = "free")+
  geom_hline(yintercept = 1.7*0.95)+
  geom_hline(yintercept = 1.7*1.1)+
  scale_color_see()


#乙醇含量
df %>% group_by(canister_batch,conditions,days) %>% 
       get_summary_stats(amount_alcohol, type="full")

source("script/data_calc.R")
df %>% 
  group_by(canister_batch,conditions,days) %>%
  data_calc(cols = amount_alcohol, fun_names = c("min","max","mean")) %>% 
  ggplot(aes(x=days, y=mean, color=conditions))+
  facet_grid(~canister_batch, scales = "free")+
  geom_pointrange(aes(ymin=min,ymax=max))+
  geom_line(aes(group=conditions))+
  labs(y="乙醇含量/%")+
  geom_hline(yintercept = 8*0.90)+
  geom_hline(yintercept = 8*1.1)+
  scale_color_see()

#水分
df %>% 
  group_by(canister_batch,conditions,days) %>%
  data_calc(cols = amount_ppm, fun_names = c("min","max","mean")) %>% 
  ggplot(aes(x=days, y=mean, color=conditions))+
  facet_grid(~canister_batch, scales = "free")+
  geom_pointrange(aes(ymin=min,ymax=max))+
  geom_line(aes(group=conditions))+
  labs(y="水分/ppm")+
  scale_color_see()

###有关

df %>% 
  ggplot(aes(x=days, y=sample_pbt2, group=conditions, color=conditions))+
  geom_point()+
  geom_line()+
  facet_grid(~canister_batch, scales = "free")+
  labs(y="PBT-2 (μg)")+
  scale_color_see()


#失重

head(df)
df.long=df %>% 
  pivot_longer(-c(1:2)) %>% 
  drop_na() %>% 
  mutate(name=  str_replace(name,"^x","")) %>% 
  unite("canister", canister_batch, canister_id, remove = FALSE) %>% 
  separate(name, c("conditions","days")) %>% 
  mutate(conditions=as.factor(conditions))
df.long
levels(df.long$conditions)=c("25C/RH60","40C/RH75","60C")
ggplot(df.long, aes(x=interaction(days, conditions), y=value, color=canister_batch,))+
  geom_point()+
  geom_line(aes(group=canister))+
  ylim(18,19)+
  labs(x="", y="Weight(g)")+
  scale_color_see()+
  guides(x="axis_nested")+
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=10, face = "bold"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.text=element_text(size=10),
        legend.position="right",
        axis.line=element_line(colour="black", linewidth=0.6),
        axis.ticks=element_line(colour="black", linewidth=0.6),
        axis.text.x = element_text(angle =0, vjust =0.2))

df.long %>% 
  # summarize_vars(.cols=value, .func = mean, 
  #                by=.(canister_batch,conditions,days)) %>% 
  group_by(canister_batch,conditions,days) %>% 
  summarise(avg.weight=mean(value)) %>% 
  pivot_wider(names_from = "days", values_from = "avg.weight") %>% 
  mutate(d.6m=(`6m`-`0d`)/`0d`*100,
         d.10d=(`10d`-`0d`)/`0d`*100)
  # write_clip()

########################################################################
getwd()
ddu = import("rawdata/GW012/GW012-制剂稳定性研究-80mcg.xlsx",sheet="DDU") %>% 
  janitor::clean_names()
str(ddu)
ddu %>% distinct(canister_batch)
df=ddu %>% filter(sample_type == "DUSA") %>% 
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  unite("actuator", device_batch_mba, device_id_mba, remove = FALSE) %>% 
  mutate(life_stage=fct_relevel(life_stage, c("B","M","E")))
df
df=df %>% mutate(
  canister_batch=sjmisc::rec(canister_batch,rec ="AGA01A=AGA01A(RLD);
                                                  AGA56A=AGA56A(RLD);
                                                  W22101904=W22101904(Rxpack);
                                                  W22102705=W22102705(Bespack)")
) 

df %>% distinct(storage,canister)
df$act=as.numeric(df$act)
pd=0.6
df %>% 
  # unite("sample",canister_batch,life_stage, remove = FALSE) %>% 
  ggplot(aes(x=life_stage, y=delivered_dose, color=storage, group=canister))+
  facet_grid(~canister_batch, scales = "free")+
  geom_point2(position =position_dodge(pd), size=2)+
  geom_smooth(method = 'lm',formula = 'y ~ x',se=FALSE, linetype=1, linewidth=0.6)+
  geom_hline(aes(yintercept=80*0.75), color="Red") +  #75~125%
  geom_hline(aes(yintercept=80*1.25), color="Red")+
  geom_hline(aes(yintercept=80*0.80))+
  geom_hline(aes(yintercept=80*1.2))+
  geom_hline(aes(yintercept=80), linetype="dashed")+
  labs(y="delivered_dose (μg)")+
  scale_color_see()+
  grafify::theme_grafify(base_size =10)

#喷重
df %>% filter(shot_weight <80) %>% 
ggplot(aes(x=life_stage, y=shot_weight, color=storage, group=canister))+
  facet_grid(~canister_batch)+
  geom_point(position =position_dodge(pd))+
  geom_hline(aes(yintercept=59*0.85), color="Red") +  #75~125%
  geom_hline(aes(yintercept=59*1.15), color="Red")+
  geom_hline(aes(yintercept=59), linetype="dashed")+
  scale_color_see()+
  geom_smooth(method = 'lm',formula = 'y ~ x',se=FALSE, linetype=2)+
  labs(y="shot weight (mg)")

###########################################################################
# library(openxlsx)
# apsd=read.xlsx("GW012-阀门选择2-80mcg.xlsx",sheet="APSD",colNames = FALSE,rowNames = TRUE) 
# apsd= data.frame(t(apsd))
# colnames(apsd)
# apsd=apsd %>% mutate(across(c(10:29), ~as.numeric(.x)))

apsd = import("rawdata/GW012/GW012-制剂稳定性研究-80mcg.xlsx",sheet="APSD") %>% 
      janitor::clean_names()
colnames(apsd)
x.levels=c("actuator","throat","stage0","stage1","stage2","stage3","stage4","stage5","stage6","stage7","filter")

df=apsd %>% select(1:20) %>% 
  pivot_longer(all_of(x.levels), names_to ="location", values_to = "amount") %>% 
  unite("canister", canister_batch, canister_id, remove=FALSE) %>%
  unite("actuator", device_batch_mba, device_id_mba) %>%
  mutate(location=fct_relevel(location, x.levels),
         life_stage=fct_relevel(life_stage, c("B","M","E")))
str(df)
cf.0d=df %>% filter(canister_batch %in% c("W22101904","W22102705"), storage=="0d") %>% 
  select(canister_batch,canister,storage, life_stage, location,amount)

rld.0d.mean=df %>% filter(canister_batch %in% c("AGA01A", "AGA56A"),storage=="0d") %>% 
  group_by(storage, canister_batch,  life_stage, location) %>% 
  get_summary_stats(amount, type = "mean_sd")

theme_set(theme_minimal())
rld.0d.mean %>% 
  ggplot(aes(x=location, y=mean, color=canister_batch))+
  facet_grid(~life_stage)+
  geom_line(aes(group=canister_batch),linewidth=1)+  #平均值
  geom_pointrange2(aes(ymin =mean-sd, ymax =mean+sd), size=0.5)+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =90, vjust =0.2))+
  labs(y="amount(μg)")+
  scale_color_see()

source("script/data_calc.R")
apsd %>% 
  group_by(canister_batch,storage,life_stage) %>%
  data_calc(cols =ism, fun_names = c("mean","sd")) %>% 
  mutate(life_stage=fct_relevel(life_stage, c("B","M","E"))) %>% 
  ggplot(aes(x=canister_batch, y=mean, color=storage))+
  facet_grid(~life_stage)+
  geom_pointrange(aes(ymin=mean-sd,ymax=mean+sd),
                  position=position_dodge(0.5))+
  labs(y="ISM(μg)")

plots(p1,p2, n_rows = 2)

####
rld.0d.mean %>% 
  ggplot(aes(x=location, y=mean, group=canister_batch))+
  facet_grid(life_stage~.)+
  geom_line(aes(color=canister_batch),linewidth=1)+  #平均值
  geom_ribbon(aes(ymin =mean-sd, ymax =mean+sd, fill =canister_batch),
              alpha=0.3,color = "black", linetype = "dashed")+
  geom_line(data=cf.0d, 
            aes(x=location, y=amount, color=canister_batch, group=canister),
            linewidth=1)+
  geom_point2(data=cf.0d, 
            aes(x=location, y=amount, color=canister_batch, group=canister),
            size=2)+
  geom_hline(aes(yintercept=80), linetype="dashed")+
  labs(y="amount(μg)")



# 稳定性
df %>% 
  filter(life_stage=="B") %>% 
  group_by(canister_batch,storage,life_stage,location) %>%
  data_calc(cols =amount, fun_names = c("mean","sd")) %>% 
  ggplot(aes(x=location, y=mean, color=storage))+
  geom_pointrange2(aes(ymin=mean-sd, ymax=mean+sd))+
  geom_line(aes(group=storage), linewidth=1)+
  facet_grid(life_stage ~ canister_batch)+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =90, vjust =0.2))+
  scale_color_see()


apsd %>% 
  mutate(life_stage=fct_relevel(life_stage, c("B","M","E"))) %>% 
  ggplot(aes(x=canister_batch, y=fpd,color=storage))+
  facet_nested(~life_stage)+
  stat_summary(fun.data = "mean_sd", geom = "pointrange",
               position = position_dodge(0.2))+
  labs(y="mean FPD(μg)")+   # ISM(μg)
  theme(legend.position ="right",
        axis.text.x = element_text(angle =0, vjust =0.2))

################################### ECD ~ z
df1=apsd %>% 
  select(1:6,life_stage,actuator,11:20) %>% 
  rowid_to_column("obs") %>% 
  mutate(flow_rate = 28.3) %>% 
  relocate(flow_rate, .after = obs)
colnames(df1)


df.nest=df1 %>% 
  dplyr::select(obs, flow_rate, 11:20) %>% 
  pivot_longer(-c(obs, flow_rate)) %>% 
  group_nest(obs, flow_rate)

source("script/apsd_calc.R")
apsd_calc(df.nest$data[[1]],
          location = "name","value",FR=28.3, presep=FALSE, type="citdas", impactor="aci")

df.apsd=df.nest %>% mutate(
  results=map2(data,flow_rate,
                ~apsd_calc(.x,"name","value",FR=.y, presep=FALSE, type="citdas", impactor="aci")[[2]]),
  data.z=map2(data,flow_rate,
              ~apsd_calc(.x,"name","value",FR=.y, presep=FALSE, type="citdas", impactor="aci")[[1]])
) %>% select(-data)

dat2=df1 %>% select(obs,3:9) %>% 
     left_join(df.apsd, by="obs") %>% 
     select(-c(results,flow_rate)) %>% 
     unnest(data.z)
dat2

dat2 %>% 
  filter(life_stage=="B") %>% 
  mutate(
    canister_batch=sjmisc::rec(canister_batch,rec ="AGA01A=AGA01A(RLD);
                                                  AGA56A=AGA56A(RLD);
                                                  W22101904=W22101904(Rxpack);
                                                  W22102705=W22102705(Bespack)")
  ) %>% 
  ggplot(aes(x=log(ECD),y=z, color=storage))+
  geom_point2(size=2)+
  facet_grid(~canister_batch)+
  # geom_line(aes(group=obs))+
  geom_hline(aes(yintercept=1), linetype="dashed")+
  geom_smooth(method ="lm",formula ="y ~ x", se=F)+
  scale_color_see()

source("script/data_plot.R")
dat2.B=dat2 %>% filter(life_stage=="B")
data_plot(dat2.B, x=ECD,y=z, trace = storage,
          facets = canister_batch,
          type="line",line.group =obs) 


################################################################
sp<- import("rawdata/GW012/GW012-制剂稳定性研究-80mcg.xlsx",sheet="SP30") %>% 
  janitor::clean_names()
str(sp)
sp1=sp %>% 
  unite("canister",canister_batch, canister_id, remove=FALSE) %>% 
  mutate(canister_batch=sjmisc::rec(canister_batch,rec ="AGA01A=RLD_AGA01A;
                                                         AGA56A=RLD_AGA56A;
                                                         W22101904=Rxpack_W22101904;
                                                         W22102705=Bespack_W22102705;
                                                         W22101902=Rxpack_Placebo;
                                                         W22101903=Bespack_Placebo")) 
head(sp1)
sp1 %>% 
  group_by(storage, canister) %>% 
  get_summary_stats(area_mm_2, type ="mean_sd" ) #%>% export()
ggplot(sp1, aes(x=canister_batch, y=area_mm_2))+
  geom_point2(aes(color=storage, group=canister),
             position = position_dodge(0.5), size=2)+
  scale_color_see()+
  theme_lucid()


pd=0.8
ggplot(sp1, aes(x=canister_batch, y=area_mm_2,
                color=storage, group=canister))+
  geom_violin(position = position_dodge(pd),linewidth=1)+
  # geom_point2(position = position_dodge(0.6))+
  stat_summary(fun.data = "mean_sd", geom = "pointrange", 
               position = position_dodge(pd), color="black")+
  scale_color_see()+
  theme_lucid()


sp %>% 
  filter(storage=="40C/RH75-6M") %>%
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  ggplot(aes(x=interaction(canister_id, canister_batch), y=area_mm_2))+
  geom_point()+
  scale_color_see()+
  grafify::theme_grafify(base_size =10)+
  guides(x="axis_nested")+
  labs(x=NULL)

#################################################################################
pg<- import("rawdata/GW012/GW012-制剂稳定性研究-80mcg.xlsx",sheet="PG") %>% 
    janitor::clean_names()
str(pg)
pg1=pg %>% 
  unite("canister",canister_batch, canister_id, remove=FALSE) %>% 
  mutate(canister_batch=sjmisc::rec(canister_batch,rec ="AGA01A=RLD_AGA01A;
                                                         AGA56A=RLD_AGA56A;
                                                         W22101904=Rxpack_W22101904;
                                                         W22102705=Bespack_W22102705;
                                                         W22101902=Rxpack_Placebo;
                                                         W22101903=Bespack_Placebo")) 
head(pg1)
pd=0.8
ggplot(pg1, aes(x=canister_batch, y=plume_angle_deg,
                color=storage, group=canister))+
  geom_violin(position = position_dodge(pd),  na.rm = TRUE, linewidth=1)+
  # geom_point2(position = position_dodge(0.6))+
  stat_summary(fun.data = "mean_sd", geom = "pointrange", 
               position = position_dodge(pd), color="black")+
  scale_color_see()+
  theme_lucid()


################################################################################
source("script/data_plot.R")
sp %>% distinct(storage)
sp %>% 
  filter(storage=="40C/RH75-6M") %>%
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  data_plot(x=canister, y=area_mm_2, trace =canister_batch,
            type = "beeswarm", pd=0.1, add="mean", 
            legend.position = "none", axis.text.angle = 90)+
sp %>% 
  filter(storage=="40C/RH75-6M") %>%
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  data_plot(x=canister, y=ovality, trace =canister_batch,
            type = "beeswarm", pd=0.1, add="mean", 
             axis.text.angle = 90)


pg %>% 
  filter(storage=="40C/RH75-6M") %>%
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  data_plot(x=canister, y=plume_angle_deg, trace =canister_batch,
            type = "beeswarm", pd=0.1, add="mean", 
            legend.position = "none", axis.text.angle = 90)+
  pg %>% 
  filter(storage=="40C/RH75-6M") %>%
  unite("canister",canister_batch, canister_id, remove = FALSE) %>% 
  data_plot(x=canister, y=plume_width_mm, trace =canister_batch,
            type = "beeswarm", pd=0.1, add="mean", 
            axis.text.angle = 90)

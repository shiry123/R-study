df=import("rawdata/IP156-sp.xlsx",sheet=1,setclass="data.table")
str(df)
colnames(df)
df.long=df %>% 
  pivot_longer(6:11, names_to = c(".value","bottle"),  names_sep ="_") %>% 
  mutate(condition=fct_relevel(condition, c("T0","JS1M","JS3M", "JS6M","CQ3M"))) %>% 
  unite("condition.b",condition,bottle, remove = FALSE) %>% 
  mutate(bottle=as.factor(bottle),
         condition.b=as.factor(condition.b),
         batch=as.factor(batch)) %>% 
  mutate(condition.b=fct_relevel(condition.b, c("T0_1","T0_2","T0_3",
                                                "JS1M_1", "JS1M_2", "JS1M_3",
                                                "JS3M_1", "JS3M_2", "JS3M_3",
                                                "JS6M_1", "JS6M_2", "JS6M_3",
                                                "CQ3M_1", "CQ3M_2", "CQ3M_3")))
df.long
levels(df.long$condition.b)
levels(df.long$condition)=c("T0","加速-1M","加速-3M", "加速-6M","长期-3M" )
##缺失值
library(naniar)
str(df)
df.na=df %>% unite("sample",batch,distance)
vis_miss(df.na)
n_miss(df.na)
n_complete(df.na)
gg_miss_var(df.na, facet = sample)

ggplot(df.long, aes(x=batch, y=Ovality))+
  geom_miss_point()+
  facet_grid(~distance)+
  theme_minimal()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =90, vjust =0.2))
##################################################################
pd=0.7
str(df.long)
df.long %>%
  filter(distance=="sp30") %>% 
  ggplot(aes(x=batch, y=Area, group=condition.b))+   # Ovality  Area
  geom_point(aes(shape=bottle, color=condition),
             position = position_dodge(pd))+
  # facet_grid(~bottle, scales = "free")+
  # stat_summary(fun.data = "mean_sd", geom="pointrange",
  #              position = position_dodge(pd))+
  # coord_flip()+
  scale_color_see()+
  theme_bw()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =90, vjust =0.2))+
  labs(title = "Area (30mm)")




p1=df.long %>%
  filter(distance=="sp30") %>% 
  ggplot(aes(x=batch, y=Area, group=batch))+
  geom_point(aes(shape=bottle, color=condition))+
  # coord_flip()+
  scale_color_see()+
  theme_bw()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =90, vjust =0.2))+
  labs(title = "SP=30mm")
p1
p.val=df.long %>% 
  filter(distance=="sp30") %>% 
  emmeans_test(Area~batch, ref.group = "IP156DPC20230102NS(placebo)") %>% 
  filter(p.adj.signif !="ns") %>%
  add_xy_position(x="batch")
p.val
p1+stat_pvalue_manual(p.val, label = "p={round(p.adj,4)}", tip.length = 0.02)


#################################################################
p=df.long %>%
  filter(distance=="sp30") %>% 
  pivot_longer(c(Ovality,Area), names_to = "item") %>% 
  ggplot(aes(x=group, y=value, color=group))+
  ggbeeswarm::geom_beeswarm()+
  stat_summary(fun = "mean", geom = "crossbar",
               colour = "blue", linewidth = 0.3, width =0.6)+
  scale_color_see()+
  theme_bw()+
  # coord_flip()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =0, vjust =0.2))+
  labs(title = "距离 (30mm)", 
       subtitle="安慰剂（Placebo，批号：IP156DPC20230102NS）",
       x="", color="类别")+
  facet_wrap(~item, scales = "free")
# stat_compare_means(method="t.test")
p
dat=df.long %>%
  filter(distance=="sp30") %>% 
  pivot_longer(c(Ovality,Area),names_to = "item") %>% 
  group_by(group,item) %>% 
  summarise(n=n(),
            mu=round(mean(value, na.rm = TRUE),2),
            .groups="drop")
dat
p+geom_text(data=dat, 
            aes(label=mu, x=group, y=mu),
            color="blue", vjust =-0.5, hjust =1.2)+
  geom_text(data=dat, 
            aes(label=paste0("n=",{n}), x=group, y=-Inf),
            vjust =-0.5,size=3)


##60mm
p=df.long %>%
  filter(distance=="sp60") %>% 
  pivot_longer(c(Ovality,Area), names_to = "item") %>% 
  ggplot(aes(x=group, y=value, color=group))+
  ggbeeswarm::geom_beeswarm()+
  stat_summary(fun = "mean", geom = "crossbar",
               colour = "blue", linewidth = 0.3, width =0.6)+
  scale_color_see()+
  theme_bw()+
  # coord_flip()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =0, vjust =0.2))+
  labs(title = "距离 (60mm)", 
       subtitle="安慰剂（Placebo，批号：IP156DPC20230102NS）",
       x="", color="类别")+
  facet_wrap(~item, scales = "free")
# stat_compare_means(method="t.test")
p
dat=df.long %>%
  filter(distance=="sp60") %>% 
  pivot_longer(c(Ovality,Area),names_to = "item") %>% 
  group_by(group,item) %>% 
  summarise(n=n(),
            mu=round(mean(value, na.rm = TRUE),2),
            .groups="drop")
dat
p+geom_text(data=dat, 
            aes(label=mu, x=group, y=mu),
            color="blue", vjust =-0.5, hjust =1.2)+
  geom_text(data=dat, 
            aes(label=paste0("n=",{n}), x=group, y=-Inf),
            vjust =-0.5,size=3)

df %>%
  select(-c(ff_rsd_agreement, bdp_rsd_agreement)) %>% 
  pivot_longer(5:8) %>% 
  group_by(lab, name)

#######################################################################
df=read_clip_tbl() %>% janitor::clean_names()
str(df)
colnames(df)
df=df %>% mutate(across(ff_rsd_6:bdp_rsd_agreement, \(x) x*100))


df %>% 
  pivot_longer(5:10) %>% 
  group_by(name) %>% 
  wilcox_test(value~lab)

df.sum=df %>% 
  pivot_longer(5:10) %>% 
  group_by(lab, name) %>% 
  get_summary_stats(value, type = "mean_sd")
df.sum

theme_set(theme_minimal())
pd=0.5
df %>%
  select(-c(ff_rsd_agreement, bdp_rsd_agreement)) %>% 
  pivot_longer(5:8) %>% 
  ggplot(aes(x=name, y=value, color=lab, group=lab))+
  geom_point(position = position_dodge(pd))+
  geom_pointrange(data=df.sum %>% filter(!name %in% c("ff_rsd_agreement", "bdp_rsd_agreement") ), 
                  aes(ymin=mean-sd, ymax=mean+sd, y=mean),
                  color="black",
                  position = position_dodge(pd))+
  labs(y="Percentage (%)")


df %>%
  select(c(lab, ff_rsd_agreement, bdp_rsd_agreement)) %>% 
  pivot_longer(2:3) %>% 
  ggplot(aes(x=name, y=value, color=lab, group=lab))+
  geom_point(position = position_dodge(pd))+
  geom_pointrange(data=df.sum %>% filter(name %in% c("ff_rsd_agreement", "bdp_rsd_agreement") ), 
                  aes(ymin=mean-sd, ymax=mean+sd, y=mean),
                  color="black",
                  position = position_dodge(pd))+
  labs(y="Percentage (%)")+
  stat_compare_means(method="wilcox.test")


ggarrange(p1, p2, common.legend = TRUE, legend="right")
df
df %>%
  ggplot(aes(x=ff_rsd_6, color=lab))+
  geom_density()

library(gghalves)
df %>% 
  mutate(logff.rsd=log(bdp_rsd_6)) %>% 
  group_by(lab) %>% 
  get_summary_stats(logff.rsd, type="mean_sd") %>% 
  mutate(y=exp(mean),
         y1=exp(mean-sd),
         y2=exp(mean+sd)) %>% 
  ggplot(aes(x=lab))+
  geom_half_violin(data=df, aes(y=bdp_rsd_6,fill=lab), 
                   side = "l",
                   position = position_nudge(x=-0.02))+
  geom_pointrange(aes(y=y, ymin=y1, ymax=y2), color="blue")

ggplot(df, aes(x=lab, y=bdp_rsd_6, fill =lab)) +
  geom_violindot()




df=read_clip_tbl()
df$grp=as.factor(df$grp)
str(df)
df %>% 
  group_by(grp) %>% 
  get_summary_stats(x, type = "mean_ci")

df %>% 
  ggplot(aes(x=grp, y=x))+
  geom_point()


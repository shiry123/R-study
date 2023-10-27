#################################### 不同胶囊数 #####################
apsd=read_clip_tbl() %>% janitor::clean_names()
colnames(apsd)
apsd=apsd %>% mutate(capsule_batch=as.factor(capsule_batch),
                   number_of_capsules=as.factor(number_of_capsules))

theme_set(theme_minimal())
ggplot(apsd, aes(x=number_of_capsules, y=mass_balance))+
  geom_point(aes(color=number_of_capsules))+
  geom_hline(aes(yintercept=0.85))+
  geom_hline(aes(yintercept=1.15))+
  geom_hline(aes(yintercept=0.75), color="Red")+
  geom_hline(aes(yintercept=1.25), color="Red")



#排序
x.levels=c("capsuleshell","device","throat","preseparator","stage_1","stage_0",
           "stage1","stage2","stage3","stage4","stage5","stage6","filter")

df=apsd %>%  
  select(1:17) %>% 
  pivot_longer(-c(1:4), names_to ="location", values_to = "amount") %>%  
  mutate(location=fct_relevel(location, x.levels)) %>% 
  mutate(capsule_batch=as.factor(capsule_batch),
         location=as.factor(location),
         number_of_capsules=as.factor(number_of_capsules))
df
str(df)
levels(df$location)=c("Capsule shell","Device","Throat","Pre-separator","Stage-1","Stage-0",
                      "Stage1","Stage2","Stage3","Stage4","Stage5","Stage6","Filter")

source("script/data_calc.R")
df %>% 
  group_by(number_of_capsules,location) %>%
  data_calc(cols =amount, fun_names =c("mean","sd")) %>% 
  ggplot(aes(x=location, y= mean, color=number_of_capsules))+
  geom_pointrange2(aes(ymin=mean-sd, ymax=mean+sd))+
  geom_line(aes(group=number_of_capsules))


## box
ggbarplot(df, x="location", y="amount", fill="number_of_capsules",size = 1,
          palette = "jco",
          add="mean_sd", position = position_dodge(0.9))+
  # guides(y = ggprism::guide_prism_minor())+
  # ggprism::theme_prism(axis_text_angle=45)+
  # theme(legend.position =c(0.9,0.7),
  #       legend.title=element_text())+
  labs(fill="胶囊粒数",x=NULL, y="沉积量(μg)")


p1=ggboxplot(apsd, x="number_of_capsules", y="fpf",fill="number_of_capsules",
          palette = "jco",outlier.shape =8)+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="top",
        legend.title=element_text())+
  labs(fill="胶囊粒数",x=NULL, y="FPF (%)")+
  stat_compare_means(method = "anova",label.x.npc = "middle")
p1
p2=ggboxplot(apsd, x="number_of_capsules", y="mmad",fill="number_of_capsules",
          palette = "jco",outlier.shape =8)+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="none",
        legend.title=element_text())+
  labs(x=NULL, y="MMAD (μm)")+
  stat_compare_means(method = "anova",label.x.npc = "middle")
ggarrange(p1,p2,ncol = 2, common.legend = T)

###########图中图
p1=data_plot(df,x=location,y=amount, trace = number_of_capsules,
          type="box", x.text.angle=45)+
  labs(fill="胶囊粒数",x=NULL, y="沉积量(μg)")
p1
p2=data_plot(df %>% filter(location=="stage1"),
             x=location,y=amount, trace = number_of_capsules,
             type="box", x.text.angle=0)+
  theme(legend.position = "none")+
  labs(fill="胶囊粒数",x=NULL, y="沉积量(μg)")
p2
library(patchwork)
p1+inset_element(p2,  
                 left = 0.5, 
                 bottom = 0.5, 
                 right = unit(1, 'npc') - unit(1, 'cm'), 
                 top = unit(1, 'npc') - unit(1, 'cm'))
# 批量建模
df_na=df %>% drop_na()
df_lm= df_na %>% 
  df_nest_by(location) %>% 
  mutate(model=map(data, ~lm(amount ~number_of_capsules, data=.x)),
         pred=map(model, ~predict(.x)),
         rmse=map2_dbl(model, data, modelr::rmse),
         r2  =map2_dbl(model, data, modelr::rsquare),
         slope=map_dbl(model, ~coef(.x)[[2]]),
         p.val=map_dbl(model, ~broom::glance(.x)$p.value) %>%
               p_format(digits = 2) %>% p_mark_significant()
  ) 
df_lm

######################################## AIT 柱状图
apsd=import() %>% janitor::clean_names()
colnames(apsd)
apsd$type_of_throat=as.factor(apsd$type_of_throat)
str(apsd)
df=apsd %>% 
  select(1:18) %>% 
  pivot_longer(6:18, names_to = "location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, x.levels))
df
levels(df$location)=c("Capsule shell","Device","Throat","Pre-separator","Stage-1","Stage-0",
                      "Stage1","Stage2","Stage3","Stage4","Stage5","Stage6","Filter")

str(df)
p=ggbarplot(df, x="location", y="amount", fill = "type_of_throat",
            add = "mean_sd", palette = "jco", 
            position = position_dodge(0.8))+
  guides(y = ggprism::guide_prism_minor())+
  theme_prism(axis_text_angle=45)+
  theme(legend.position = c(0.8,0.8),
        legend.title=element_text("sans"))+
  labs(fill="喉管类型",x=NULL, y="沉积量(μg)")
p
stat.test=df %>% drop_na() %>% 
  group_by(location) %>% 
  t_test(amount~type_of_throat) %>% 
  add_significance() %>% 
  add_xy_position(x="location")
stat.test
# add_pvalue()
p+stat_pvalue_manual(data=stat.test, label="p.signif", xmin="xmin", xmax="xmax",
                     tip.length = 0,fontface = "italic", lineend = "round", bracket.size = 0.5)

#boxplot
p=ggboxplot(df, x="location", y="amount", fill="type_of_throat",
          palette = "jco",outlier.shape =8)+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=45)+
  theme(legend.position =c(0.7,0.8),
        legend.title=element_text())+
  labs(fill="喉管类型",x=NULL, y="沉积量(μg)")
p

p1=ggboxplot(apsd, x="type_of_throat", y="fpf",fill="type_of_throat",
             palette = "jco",outlier.shape =8, add="jitter")+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="top",
        legend.title=element_text())+
  labs(fill="喉管类型",x=NULL, y="FPF (%)")+
  stat_compare_means(method = "t.test",label.y = 45)
p1
p2=ggboxplot(apsd, x="type_of_throat", y="mmad",fill="type_of_throat",
             palette = "jco",outlier.shape =8, add="jitter")+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="none",
        legend.title=element_text())+
  labs(x=NULL, y="MMAD (μm)")+
  stat_compare_means(method = "t.test",label.y =5)
plots(p1,p2)

###########################呼吸模式 IVIVC
apsd=import() %>% janitor::clean_names()
colnames(apsd)
x.levels=c("capsuleshell","device","ait","mixing_inlet","preseparator","stage_1","stage_0",
             "stage1","stage2","stage3","filter")
apsd$inhalation_profiles=as.factor(apsd$inhalation_profiles)  

df=apsd %>% 
  select(1:15) %>% 
  pivot_longer(5:15, names_to = "location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, x.levels))
levels(df$location)=c("Capsule shell","Device","AIT","Mixing Inlet","Pre-separator","Stage-1","Stage-0",
                      "Stage1","Stage2","Stage3","Filter")
df
# mean+sd形式
df %>% 
  group_by(inhalation_profiles, location) %>% 
  get_summary_stats(amount, type="mean_sd") %>%
  mutate(meansd=str_c(round(mean,2),"±",round(sd,2))) %>% 
  select(inhalation_profiles, location, meansd) %>% 
  pivot_wider(names_from = inhalation_profiles, values_from =meansd )
 # print_table(file="outfiles/outfiles.doc")

#box
p=ggbarplot(df, x="location", y="amount", fill="inhalation_profiles",
            palette = "jco",position = position_dodge(0.8), add="mean_sd")+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=45)+
  theme(legend.position =c(0.7,0.8),
        legend.title=element_text())+
  labs(fill="呼吸模式",x=NULL, y="沉积量(μg)")
p
p1=ggboxplot(apsd, x="inhalation_profiles", y="fpf",fill="inhalation_profiles",
             palette = "jco",outlier.shape =8, add="jitter")+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="top",
        legend.title=element_text())+
  labs(fill="呼吸模式",x=NULL, y="FPF (%)")+
  stat_compare_means(method = "anova", label.y = 35)+
  stat_compare_means(comparisons = get_comparisons(apsd,inhalation_profiles),
                  aes(label = after_stat(p.signif)),method = "t.test",tip.length=0)
p1
p2=ggboxplot(apsd, x="inhalation_profiles", y="mmad",fill="inhalation_profiles",
             palette = "jco",outlier.shape =8, add="jitter")+
  guides(y = ggprism::guide_prism_minor())+
  ggprism::theme_prism(axis_text_angle=0)+
  theme(legend.position ="none",
        legend.title=element_text())+
  labs(x=NULL, y="MMAD (μm)")+
  stat_compare_means(method = "anova", label.y = 5)+
  stat_compare_means(comparisons = get_comparisons(apsd,inhalation_profiles),
                     aes(label = after_stat(p.signif)),method = "t.test",tip.length=0)
p2
plots(p1,p2)


#################数据处理 150L/min  转置数据
# apsd=read_clip_tbl()
apsd=import()
colnames(apsd)
dat=apsd %>% select(id,Inhalation_profiles,7:15) %>% 
  pivot_longer(-c(id,Inhalation_profiles))
dat
dat.nest=dat %>% group_nest(id,Inhalation_profiles)
dat.nest
dat.nest$data[[1]]

z_calc=function(data, mass){
  mass=enquo(mass)
  data=data %>% tidyfst::replace_na_dt(to = 0)
  data.z=data %>% mutate(
    location.s=c("AIT","Mixing_Inlet","Preseparator","Stage_1","Stage_0","Stage1","Stage2","Stage3","Filter")
  ) %>% 
    filter(location.s %in% c("Stage_1","Stage_0","Stage1","Stage2","Stage3","Filter")) %>% 
    mutate(ECD=c(5.4,4.1,2.8,2.0,1.2,0),  #与流速有关  150L/min
           location.s=fct_relevel(location.s, c("Stage_1","Stage_0","Stage1","Stage2","Stage3","Filter"))
    ) %>% 
    arrange(desc(location.s)) %>% 
    mutate(temp=lag(!!mass,default = 0),
           cm=cumsum(temp)/sum(!!mass),
           z=qnorm(cm)) %>% 
    select(-location.s, -temp)
  return(data.z)
}

z_calc(dat %>% filter(id=="1"), value)

dat.z=dat.nest %>% mutate(.results=map(data, ~z_calc(.x, value))) %>% 
  unnest(.results) %>% select(-data) %>% 
  filter(!is.infinite(z)) 
dat.z
dat.z.r=dat.z %>% 
  select(id,Inhalation_profiles,ECD,z) %>% 
  group_nest(Inhalation_profiles) %>% 
  mutate(mod=map(data, ~lm(z~log(ECD), data=.x)),
         k=map_dbl(mod, ~coef(.x)[[2]]),
         b=map_dbl(mod, ~coef(.x)[[1]]),
         R2=map_dbl(mod, ~summary(.x)[["r.squared"]])
         ) %>% select(-data,-mod) %>% 
  mutate(x.sol=-b/k, MMAD=exp(x.sol))
dat.z.r 
# write_clip()

dat.z %>% 
  ggplot(aes(x=log(ECD), y=z, color=Inhalation_profiles))+
  geom_point()+
  geom_smooth(method ="lm",formula ="y ~ x",se=FALSE)+
  geom_hline(aes(yintercept=0))+
  annotate(geom = "segment",x=1.37,y=0, xend =1.37 ,yend = -Inf,
           lty="dashed")+
  annotate(geom = "segment",x=1.43,y=0, xend =1.43 ,yend = -Inf,
           lty="dashed")+
  annotate(geom = "segment",x=1.46,y=0, xend =1.46 ,yend = -Inf,
           lty="dashed")+
  # annotate(geom = "text",x=1.37,y=0,label=1.37,vjust=-0.5,color="red",size=3)+
  scale_x_continuous(limits = c(0.5, 2))+
  theme_pubr()


#150L/min  excel bert
shortACI.APSD=function(mass, type=c("MMAD","GSD","FPD")){
  type=match.arg(type)
  mass=as.numeric(mass)
  mass[is.na(mass)]=0
  cm.temp=append(cumsum(rev(mass))/sum(mass),0,0)
  cm=rev(cm.temp[-length(cm.temp)])
  z=ifelse(is.infinite(qnorm(cm)),NA,qnorm(cm))
  location.s=c("stage_1","stage_0","stage1","stage2","stage3","filter")
  
  ECD150=c(5.4,4.1,2.8,2.0,1.2,0)
  
  dat=data.frame(mass=mass, z=z, location.s=location.s, ECD=ECD150)
  lm.data=subset(dat, location.s %in% c("stage_1","stage_0","stage1","stage2","stage3"))
  mod=lm(z~log(ECD), data=lm.data)
  MMAD=exp((0-coef(mod)[1])/coef(mod)[2])   #z=0
  sizeX=exp((1-coef(mod)[1])/coef(mod)[2])   #z=1
  GSD=sizeX/MMAD
  FPD=sum(subset(dat,ECD<5,select=mass), na.rm = TRUE)
  
  switch (type,
          "MMAD" = return(MMAD),
          "GSD" = return(GSD),
          "FPD" = return(FPD)
  )
  
}


###################################################################
df=clipr::read_clip_tbl()  #%>% janitor::clean_names()
df
library(NonCompart)
df1=df %>% filter(Inhalation_profiles == "Moderate COPD")
AUC(df1$Time, df1$Flow_Rate)/60

#######################吸入曲线图
df=import()
df

ggplot(df, aes(x=Time,y=Flow_Rate, group=Inhalation_profiles))+
  geom_line(aes(color=Inhalation_profiles),
            linewidth=1)+
  guides(y = guide_prism_minor())+
  ggsci::scale_color_jco()+
  theme_prism(axis_text_angle=0)+
  theme(legend.position =c(0.8,0.8),
        legend.title=element_text())+
  labs(x="吸气时间(s)", y="流速(L/min)", color="呼吸模式")+
  scale_x_continuous(expand=c(0, 0),limits = c(0, 4.5), breaks = seq(0,4.5,0.5))+
  scale_y_continuous(expand=c(0, 0),limits = c(0, 150), breaks = seq(0,150,30))

Inhale_FR=function(t, tmax, t.total, PIFR){
  if(t <= tmax){
    FR=PIFR*sin(pi/2*t/tmax)
  } else {
    FR=PIFR*cos(pi/2*(t-(tmax+0.15))/(t.total-(tmax+0.15)))
  }
  return(FR)
}
## t(分钟)
Inhale_FR(t=0.2, tmax=0.45, t.total=2, PIFR=40)


t=seq(0, 2, by=0.1)
y=map_dbl(t, ~Inhale_FR(.x, tmax=1, t.total=2, PIFR=44))
plot(t,y, type = "o")

ggplot(tibble(time=t, FR=y), aes(x=time, y=FR))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))


# m-COPD
df1 %>% mutate(
  FP.calc=map_dbl(Time, ~Inhale_FR(.x, tmax=0.5, t.total=2.1, PIFR=70))
) %>% 
  pivot_longer(c(Flow_Rate,FP.calc), names_to = "type", values_to = "FR") %>% 
  data_plot(Time,FR, trace = type)+
  geom_line(aes(group=type))+
  scale_x_continuous(limits = c(0, 3))

df2 %>% mutate(
  FP.calc=map_dbl(Time, ~Inhale_FR(.x, tmax=0.25, t.total=0.6, PIFR=137))
) %>% 
  pivot_longer(c(Flow_Rate,FP.calc), names_to = "type", values_to = "FR") %>% 
  data_plot(Time,FR, trace = type)+
  geom_line(aes(group=type))+
  scale_x_continuous(limits = c(0, 3))


apsd %>%
  mutate(
    pmap_dfr(across(-all_of(1:4)), ~lst(sum = sum(c(...),na.rm = T),
                                        recovery = sum/18))
  )




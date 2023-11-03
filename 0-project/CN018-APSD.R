################### T5 装置
getwd()
apsd=read_clip_tbl() %>% janitor::clean_names() %>% 
     convert_as_factor(capsule_batch)

apsd=import("rawdata/CN018/CN018-CF-product-APSD.xlsx",sheet="T5") %>% 
      janitor::clean_names()

str(apsd)
colnames(apsd)
levels.20=c("capsuleshell","device","throat","preseparator","stage0","stage1",
            "stage2","stage3","stage4","stage5","stage6","stage7","filter")

# levels.60=c("capsuleshell","device","throat","preseparator","stage_1","stage_0",
#             "stage1","stage2","stage3","stage4","stage5","stage6","filter")
# plotdata=apsd %>% 
#   pivot_longer(7:19, names_to ="location", values_to = "amount") %>% 
#   mutate(location=fct_relevel(location, levels.60)) %>% 
#   mutate(capsule_batch=as.character(capsule_batch)) 
# plotdata
plotdata=apsd %>% 
  rowid_to_column("id") %>% 
  select(1:21) %>% 
  pivot_longer(9:21, names_to ="location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, levels.20))
colnames(plotdata)

plotdata %>% 
  ggplot(aes(x=location, y=amount, color=capsule_batch))+
  geom_point2(aes(shape=device_type))+
  geom_line(aes(group=id))+
  facet_wrap(~capsule_batch)+
  theme_lucid(axis.text.angle = 90)

plotdata %>% 
  ggplot(aes(x=location, y=amount, color=capsule_batch))+
  geom_point2(stat="summary",fun="mean")+
  geom_line(aes(group=capsule_batch),
            stat="summary", fun="mean")+
  theme_lucid(axis.text.angle = 90)
setwd("D:/Mywd")
source("script/data_plot.R")
plotdata %>% 
  data_plot(x=location, y=amount,
            trace = capsule_batch,
            type="line", add="mean_sd")
apsd %>% 
  data_plot(x=capsule_batch, y=ism,
            trace =device_type,
            type="point", 
            add="mean_sd",
            add.params = list(position=position_nudge(x=0.2))
            )
apsd %>% 
  ggstripchart(x="capsule_batch",y="ism", 
               shape="device_type", color = "device_no",
               add="mean_sd", position = position_dodge(0.5),
               # palette = "jco",
               )+
  theme(panel.grid.major = element_line(),
        legend.position = "right")
 

###################################### 仿制药制剂对比研究
df= import("rawdata/CN018/CN018-RLD-APSD.xlsx",sheet="formulation") %>% 
    janitor::clean_names()
str(df)
colnames(df)
aci.20=c("capsuleshell","device","throat","preseparator","stage0","stage1",
            "stage2","stage3","stage4","stage5","stage6","stage7","filter")
df1=df %>% 
  rowid_to_column("id") %>% 
  select(all_of(aci.20),id,capsule_type,capsule_batch,device_type,device_no) %>% 
  pivot_longer(all_of(aci.20), names_to ="location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, aci.20)) %>% 
  unite("product",device_type,capsule_batch, remove = FALSE)
df1
source("script/data_plot.R")
data_plot(df1,
          x=location, y=amount,
          trace =product,
          type="line", add="mean_sd")

ggline(df1, x="location", y="amount", color="product", 
       add="mean_sd",add.params=list(width=0),
       palette = "jco")+
  theme(panel.grid.major = element_line(),
        legend.position="right")


#########
data_plot(df, 
          x=device_type, y=ex_device, 
          trace =capsule_batch,
          type="point", pd=0.5,
          add="mean")+
  geom_hline(yintercept = 10*0.65)+
  geom_hline(yintercept = 10*1.35)

##### 喉管+预分离器
tp.20=c("capsuleshell","device","tp","stage0","stage1",
            "stage2","stage3","stage4","stage5","stage6","stage7","filter")
df2=df %>% 
  rowid_to_column("id") %>% 
  select(all_of(tp.20),id,capsule_type,capsule_batch,device_type,device_no) %>%
  pivot_longer(all_of(tp.20), names_to ="location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, tp.20)) %>% 
  unite("product",device_type,capsule_batch, remove = FALSE) 

ggplot(df2, aes(x=location, y=amount,
                color=product, group=product))+
  geom_line(stat="summary", fun="mean")+
  stat_summary(fun.data = "mean_sd", geom = "pointrange")+
  scale_color_see()

# ism
df %>%  
  unite("product",device_type,capsule_batch, remove = FALSE) %>% 
  data_plot(x=product, y=ism, trace =capsule_type, type="pointrange")

my_comparisons <- list( c("Handihaler-us_107240", "Glenmark_21P007"), 
                        c("Handihaler-us_107240", "Handihaler-us_2213006"),
                        c("Handihaler-us_107240", "Handihaler-us_W23041801"))

df %>%  
  unite("product",device_type,capsule_batch, remove = FALSE) %>% 
  data_plot(x=product, y=ism, trace =capsule_type, type="pointrange")+
  stat_compare_means(comparisons = my_comparisons, 
                     aes(label = after_stat(p.signif)),
                     method = "t.test",tip.length=0)

df %>%  
  unite("product",device_type,capsule_batch, remove = FALSE) %>% 
  group_by(product) %>% 
  get_summary_stats(ism, type="mean_sd")

############################################################################
apsd=import("rawdata/CN018/CN018-RLD-APSD.xlsx",sheet="datasets") %>% 
  janitor::clean_names()
colnames(apsd)
str(apsd)
df=apsd %>% 
  filter_dt(capsule_batch %in% c("105504","106051","107240")) %>% 
  select_dt(flow_rate,capsule_batch,ex_device,ism,device_no) %>%    # MASS 中select 干扰
  convert_as_factor(flow_rate,capsule_batch)
str(df)
df %>% 
  ggplot(aes(x=weave_factors(device_no,capsule_batch), y=ex_device, 
             color=capsule_batch))+  # weave_factors
  geom_point2(size=2)+
  facet_nested(~flow_rate)+
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
        axis.text.x = element_text(angle =90, vjust =0.2))

fit_aov=aov(ism~ex_device+capsule_batch, data=df %>% filter(flow_rate=="60"))
summary(fit_aov)
library(HH)
ancova(ism~ex_device+capsule_batch, data=df %>% filter(flow_rate=="60"))



x.levels=c("capsuleshell","device","throat","preseparator","s0","s1",
           "s2","s3","s4","s5","s6","s7","filter")

df=apsd %>% 
  rowid_to_column("id") %>% 
  convert_as_factor(flow_rate) %>% 
  dplyr::select(all_of(x.levels),id,capsule_type,capsule_batch,device_type,device_no,flow_rate) %>% 
  pivot_longer(all_of(x.levels), names_to ="location", values_to = "amount") %>% 
  mutate(location=fct_relevel(location, x.levels)) %>% 
  unite("product",device_type,capsule_batch, remove = FALSE)
df
source("script/data_plot.R")
data_plot(df, 
          x=location, y=amount, 
          trace =product,
          type="line", 
          add="mean_sd", 
          axis.text.angle = 90,
          legend.position="top")+
  facet_grid(flow_rate~., scales = "free")


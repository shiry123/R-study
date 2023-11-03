getwd()
ddu <- import("rawdata/GW012/GW012-样品数据汇总.xlsx",sheet="RLD-DDU") %>% 
       janitor::clean_names()



#############################################################  
apsd <- read_clip_tbl() %>% janitor::clean_names()

apsd <- import("rawdata/GW012/GW012-样品数据汇总.xlsx",sheet="RLD-APSD") %>% 
        janitor::clean_names()
str(apsd)
colnames(apsd)

apsd %>% 
  group_by(canister_batch, life_stage) %>% 
  get_summary_stats(ism, type = "mean_sd")

source("script/data_plot.R")
apsd %>% 
  mutate(life_stage=fct_relevel(life_stage, c("B","M","E"))) %>% 
 data_plot(x=life_stage, y=ism,
           trace = canister_batch,
           type="point", pd=0.2)+
  geom_smooth()

######################## US & China RLD
ddu=import("rawdata/CN018/CN018-RLD DDU.xlsx",sheet="RLD-DDU") %>% 
    janitor::clean_names()
str(ddu)
# export(ddu, "outfiles/1.csv")

source("script/data_plot.R")

ddu %>% 
  convert_as_factor(flow_rate, capsule_batch) %>% 
  filter(sample_type=="DUSA") %>% 
  data_plot(x=capsule_batch, y=amount, 
            trace =device_no,
            facet=flow_rate,
            type="point", pd=0.5)



ddu %>% 
  convert_as_factor(flow_rate, capsule_batch) %>% 
  drop_na() %>% 
  data_plot(x=flow_rate, y=amount, 
            trace =capsule_batch,
            facet=sample_type,
            type="bar", pd=0.7, add="mean_sd")

ddu %>% 
  convert_as_factor(flow_rate, capsule_batch) %>% 
  filter(sample_type=="DUSA") %>% 
  data_plot(x=flow_rate, y=amount, 
            trace =capsule_batch,
            type="beeswarm", pd=0.5, add="mean")+
  geom_hline(aes(yintercept=10.2*0.65), color="Red")+
  geom_hline(aes(yintercept=10.2*1.35), color="Red")



ddu %>% 
  convert_as_factor(flow_rate, capsule_batch) %>% 
  filter(sample_type=="DUSA") %>% 
  anova_test(amount~flow_rate+capsule_batch)

source("script/data_calc.R")
ddu %>% 
  convert_as_factor(flow_rate, capsule_batch) %>% 
  filter(sample_type=="DUSA") %>% 
  drop_na() %>% 
  data_calc(cols=amount, 
            func =list(mean,sd),
            by=c(flow_rate, capsule_batch, sample_type)) %>% 
  pivot_wider(names_from = sample_type, values_from =c(mean,sd))

#################################
apsd %>% 
  filter(capsule_batch %in% c("105504","106051","107240","106071")) %>% 
  select(flow_rate,capsule_batch,ex_device) %>% 
  convert_as_factor(flow_rate) %>% 
  data_calc(cols=ex_device, 
            func =list(mean,sd),
            by=c(flow_rate, capsule_batch))

##############################################################
ddu=read_clip_tbl() %>% janitor::clean_names()
str(ddu)
ddu %>% 
  filter(sample_type=="DUSA") %>% 
data_plot(x=act, y=amount, 
          trace=actuator_no,
          type="point")+
  geom_hline(aes(yintercept=80*0.75), color="Red")+
  geom_hline(aes(yintercept=80*1.25), color="Red")


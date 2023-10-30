####################################### POC1
getwd()
ddu=read_clip_tbl() %>% janitor::clean_names()
ddu=import("rawdata/GW012/GW012-POC1-device.xlsx",sheet = "ddu") %>% 
   janitor::clean_names()
str(ddu)
colnames(ddu)
ddu %>% distinct(method)
source("script/data_plot.R")
ddu %>% 
  data_plot(x=actuator_no, y=amount, 
            trace =device_type,
            facet =c(method,sample_type),
            type = "point",
            axis.text.angle = 90)
ddu %>% 
  filter(sample_type=="DUSA") %>% 
  data_plot(x=actuator_no, y=amount, 
             trace =device_type,
             facet =method,
             type = "point",
            axis.text.angle = 90)+
  geom_hline(yintercept = 80,linetype=2)+
  geom_hline(yintercept = 80*0.75, color="red")+
  geom_hline(yintercept = 80*1.25, color="red")+
  geom_hline(yintercept = 80*0.8)+
  geom_hline(yintercept = 80*1.2)

source("script/data_calc.R")
ddu %>% 
  filter(sample_type=="DUSA") %>% 
  unite("canister", canister_batch,canister_id, remove = F) %>% 
  group_by(method,device_type) %>%
  data_calc(cols =amount, func = mean)
################################################
ddu %>% 
  filter(sample_type=="DUSA") %>% 
  drop_na() %>% 
  data_plot(x=actuator_no, y=shot_weight, 
            trace =device_type,
            facet =method,
          type = "point",
          axis.text.angle = 90)+
  geom_hline(yintercept = 59,linetype=2)+
  geom_hline(yintercept = 59*0.85)+
  geom_hline(yintercept = 59*1.15)
source("script/data_plot.R")
ddu %>% 
  filter(sample_type=="DUSA", method=="60ethanol") %>% 
  drop_na() %>% 
  data_plot(x=device_type, y=shot_weight, 
            trace =device_type,
            type = "box",
            axis.text.angle = 90)

ddu %>% 
  filter(sample_type=="DUSA") %>% 
  group_by(method,device_type) %>%
  data_calc(cols =shot_weight, func = mean)

################################### APSD ###################################
apsd=read_clip_tbl() %>% janitor::clean_names()

apsd=import("rawdata/GW012/GW012-POC1-device.xlsx",sheet = "apsd") %>% 
     janitor::clean_names()
str(apsd)
colnames(apsd)

x.levels=c("actuator","throat","stage0","stage1","stage2","stage3",
           "stage4","stage5","stage6","stage7","filter")
df=apsd %>% select(1:19) %>% 
  pivot_longer(all_of(x.levels), names_to ="location", values_to = "amount") %>% 
  unite("canister", canister_batch, canister_id, remove=FALSE) %>%
  mutate(location=fct_relevel(location, x.levels))
str(df)
data_plot(df,x=location,y=amount,
          trace =device_type,
          facet =method, 
          type="line",add = "mean_sd"
          )
########################################
source("script/data_plot.R")
data_plot(apsd, x=method, y=ex_actuator,
          trace =device_type,
          type="box",
          pd=0.8,
          add="point",
          add.params = list(position=position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1)),
          axis.text.angle = 0
          )
apsd %>% 
  filter(method=="60ethanol") %>% 
  ggplot(aes(x=device_type, y=ism))+
  geom_boxplot()+
  geom_jitter(aes(color=actuator_no), width = 0.1)


apsd %>% 
  filter(method=="60ethanol") %>% 
  t_test(ism~device_type)

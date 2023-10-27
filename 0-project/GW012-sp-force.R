df=import() %>% janitor::clean_names()
df=df %>% convert_as_factor(actuator_batch, force)
str(df)
df %>% map_df(~ sum(is.na(.)))
df %>%
  filter(if_any(everything(), is.na))

with(df, table(actuator_batch, force))
df %>% anova_test(area_mm2 ~ actuator_batch+force)

df %>% 
  group_by(actuator_batch) %>% 
  emmeans_test(area_mm2~force)

source("script/data_plot.R")
data_plot(df,
          x=actuator_batch, y=area_mm2, trace = force,
          type="point", add="mean")


########################################################
getwd()
set.wd(ask=TRUE)
df<- read_excel("20220917-20220921.xlsx",sheet="Input")
sp=df %>% 
  separate(`Lot #`, cc("Lot,ID,o.act")) %>% 
  unite("Canister",Lot, ID, remove=FALSE) %>% 
  mutate(o.act=as.numeric(o.act),
         act=o.act+`Actuation Number`) %>% 
  select(-o.act) %>% rename(Actuator=`Identification #`)
sp




# options('contrasts')
# options(contrasts=c('contr.sum','contr.poly'))
# options(contrasts=c('contr.treatment','contr.poly'))
# install.packages('installr')
# library(installr)
# updateR()
###########################################################################
packages=c("clipr","rio","janitor","tidyverse","tidyfst","skimr","performance","compareGroups",
           "ggpubr","ggprism","ggsci","afex","rsm","lmerTest","emmeans","rstatix","pwr",
           "Rsolnp","slider","patchwork","ggpmisc","GGally","DescTools","grafify")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

install.packages("tidyverse")
library(tidyfst)
pkg_load(afex)
pkg_unload(afex)
detach("package:afex", unload = TRUE)

# R包的卸载
# remove.packages("rrtable")

library(conflicted)
conflicted::conflict_scout()

#####################################
install.packages("bruceR", dep=TRUE)
library(bruceR)
bruceR::import()

##############################################################################
library(clipr)
df=clipr::read_clip_tbl()  #复制剪贴板
str(df)
df %>% select(1:4) %>% write_clip()
library(rio)
# readr包 - text files（如csv, tsv, fwf文件）
# haven包 - SPSS, Stata, and SAS files
# readxl包 - excel files
# DBI包 - databases
# jsonlite包 - json
# xml2包 - XML
# httr包 - Web APIs
# rvest包 - HTML (Web Scraping)

df=rio::import("rawdata/GW012/GW012-制剂稳定性研究-80mcg.xlsx",sheet="DDU",setclass = "data.table")
df
str(df)
rio::export(mtcars, "outfiles/mtcars.xlsx")

library(readxl)  # tidyverse包含
ddu<- read_excel("rawdata/GW012-DDUAPSD-RLD.xlsx",sheet="ddu")
library(openxlsx)
apsd_RLD=read.xlsx("rawdata/GW012-DDUAPSD-RLD.xlsx",sheet="apsd",
                   colNames = FALSE,rowNames = TRUE)
# library(writexl)
# write_xlsx(df, "outfiles/df.xlsx")

library(data.table)
df=fread("datas/DemoData.csv")

library(janitor)
df %>% janitor::clean_names()

##########################################################
library(tidyverse)
tidyverse_update()
library(tidyfst)
summarise_vars(df, Delivered_Dose_μg, mean, by=Canister_Batch)
library(performance)
library(effectsize)

library(ggh4x)
library(ggpubr)
library(see)
library(patchwork)
p1+p2

library(afex)
aov_car()
# install.packages("ggbeeswarm")
afex_plot()
mixed(Reaction ~ Days+(1+Days|Subject), sleepstudy)

library(slider)
slide()  #滑动迭代

library(lmerTest)
mod = lmer(Reaction ~ Days+(1+Days|Subject), sleepstudy)
mod
summary(mod)
mixed(Reaction ~ Days+(1+Days|Subject), sleepstudy)

library(emmeans)
emmeans(mod, pairwise ~ Days)


##批量读取+合并Excel文件，每个带多个Sheet
library(readxl)
files = list.files("datas/read_datas", pattern = "xlsx", 
                   full.names = TRUE, recursive = TRUE)
files
readPath = function(path) {
  map_dfr(readxl::excel_sheets(path), ~ readxl::read_xlsx(path, sheet = .x))
}
df = map_dfr(files, readPath)
df
###########################

library(rstatix)
data %>% get_summary_stats(DD_μg, type = "mean_sd") 

library(sjmisc)
df=sp %>% 
  mutate(formulation=sjmisc::rec(Canister_Batch, 
                         rec = "W21111203=R; W21111204=T;else=NA",append = FALSE))

library(modelr)
modelr::add_predictions()
library(broom)
tidy()
library(ggtrendline)
ggtrendline(df$Conc,df$Area,"line2P", CI.fill = NA) + 
  geom_point(aes(df$Conc,df$Area,color=df$test))

theme_set(theme_bw()+
            theme(axis.text.x = element_text(angle=0,vjust=0.5)))
library(rsm)
library(Rsolnp)

library(corrplot)
corrplot(corr =cor(mtcars[1:7]),order="AOE",type="upper",tl.pos="tp")
corrplot(corr = cor(mtcars[1:7]),add=TRUE, type="lower",
         method="number",order="AOE", col="black",
         diag=FALSE,tl.pos="n", cl.pos="n")

cor_mat(mtcars[1:7]) %>% 
  cor_reorder() %>%
  pull_lower_triangle() %>% 
  cor_plot(method = "number")

library(ggpmisc)
formula <- y ~ poly(x, 3, raw = TRUE)
formula <- y ~ log(x)
formula=as.formula("y~log(x)")
ggplot(df.long, aes(x=Conc, y=Area)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = formula)+
  facet_wrap(~test)
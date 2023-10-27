library(skimr)
skimr::skim(npk)
names(mtcars)
dat=mtcars %>% janitor::clean_names()
dat


library(compareGroups) 
data(regicor)
descrTable(year ~ ., regicor, hide.no="no", show.p.mul=TRUE)
res <- createTable(compareGroups(year ~ . -id, regicor), 
                   hide = c(sex=1), hide.no = 'no')
summary(res)
export2word(res, file ="res.docx")

library(compareGroups)
library(survival)
data(colon)#加载数据集
df <- colon#定义数据集
df <- df[,-df$study]#删除study列
str(df)#展示数据集

t1 <- descrTable(status ~rx + age,#构建表格方程
                 show.all = T,#显示所有人群情况，如果表格大可以删除此项
                 show.p.ratio =T,#显示分组后每个分组的P值情况
                 show.ratio = T,#显示OR或者HR值，函数会根据方程自动调整
                 data = df)#确定数据集
t1#展示数据集

fit.rx<- glm(status ~ rx + age, #方程
             family = binomial(link ="logit"),#连接函数为logit
             data = df)
summary(fit.rx)#展示逻辑回归的主要参数
exp(coef(fit.rx))#计算OR值


########################################################################
library(stargazer)
stargazer(attitude, type = "text")
linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical, data = attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data = attitude)
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance,
                    data = attitude,
                    family = binomial(link = "probit"))
stargazer(linear.1, linear.2, probit.model,
          title = "结果",
          type = "text")
stargazer(linear.1, linear.2, probit.model,
          title = "回归结果",
          dep.var.labels = c("Overall Rating", "High Rating"),
          covariate.labels = c("Handling of Complaints", "No Special Privileges", "Opportunity to Learn", "Performance-Based Raises", "Too Critical", "Advancement"),
          omit.stat = c("LL","ser","f"),
          type = "text") 

stargazer(linear.1, linear.2,
          title = "Table 1. Regression Analysis",
          dep.var.labels = c("Overall Rating", "High Rating"),
          covariate.labels = c("Handling of Complaints", "No Special Privileges", "Opportunity to Learn", "Performance-Based Raises", "Too Critical", "Advancement"),
          omit.stat = c("LL","ser","f"),
          ci = TRUE, ci.level = 0.95,
          single.row = FALSE,
          type = "html",
          digits = 2,
          no.space = TRUE,
          out = "outfiles/regression.doc")

#############################################################################
library(gtsummary)
trial
trial %>% 
  select(trt, age, grade, death) %>% 
  tbl_summary(by = trt, # 分组
              statistic = list(all_continuous() ~ "{mean} ({sd})"),  
              missing = "no" # 
  ) %>%
  add_n() %>% # 添加非NA观测值个数
  add_p() %>% # 添加P值
  add_overall() %>%
  modify_header(label = "**Variable**") %>% # 标签列header
  bold_labels()  #label 粗体


######################################################################
iris
iris %>% 
  tbl_summary(
    by = Species, # 分组
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c( "{mean} ({sd})",
                                      "{median} ({p25}, {p75})",
                                      "{min}, {max}"),
    missing = "no"
  ) %>% add_n() %>% add_p()

###################################################################

trial
trial %>%
  select(trt, age, marker,stage, grade, ttdeath) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata(
    strata =grade,   #c(stage, grade)
    .tbl_fun = ~.x %>% 
      tbl_summary(
        by = trt, # 分组
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c( "{mean} ({sd})",
                                          "{median} ({p25}, {p75})","{min}, {max}"),
        missing = "no"
      ) %>% add_n(),
    .header = "**{strata}**, N = {n}"
  )

df=import()
str(df)

df %>% 
  select(Inhalation_profiles,location,amount) %>% 
  tbl_summary(
    by = Inhalation_profiles, # 分组
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c( "{mean} ({sd})",
                                      "{median} ({p25}, {p75})",
                                      "{min}, {max}"),
    missing = "no"
  ) %>% add_n() 
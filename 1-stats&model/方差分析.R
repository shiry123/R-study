library(afex)     # needed for ANOVA functions.
library(emmeans)  # emmeans must now be loaded explicitly for follow-up tests.
library(multcomp) # for advanced control for multiple testing/Type 1 errors.


data("ToothGrowth")
df = ToothGrowth %>% mutate(dose=as.factor(dose))
str(df)
df %>% t_test(len ~ 1, mu = 0)
df %>% t_test(len ~ supp)
df %>% t_test (len ~ supp, paired = TRUE)
df %>% t_test(len ~ dose, ref.group = "0.5")
df %>% t_test(len ~ dose, ref.group = "all")
df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

trt<-c(rep("group1",30),rep("group2",30),rep("group3",30),rep("group4",30))
weight<-c(3.53,4.59,4.34,2.66,3.59,3.13,3.30,4.04,3.53,3.56,3.85,4.07,1.37,
          3.93,2.33,2.98,4.00,3.55,2.64,2.56,3.50,3.25,2.96,4.30,3.52,3.93,
          4.19,2.96,4.16,2.59,2.42,3.36,4.32,2.34,2.68,2.95,2.36,2.56,2.52,
          2.27,2.98,3.72,2.65,2.22,2.90,1.98,2.63,2.86,2.93,2.17,2.72,1.56,
          3.11,1.81,1.77,2.80,3.57,2.97,4.02,2.31,2.86,2.28,2.39,2.28,2.48,
          2.28,3.48,2.42,2.41,2.66,3.29,2.70,2.66,3.68,2.65,2.66,2.32,2.61,
          3.64,2.58,3.65,3.21,2.23,2.32,2.68,3.04,2.81,3.02,1.97,1.68,0.89,
          1.06,1.08,1.27,1.63,1.89,1.31,2.51,1.88,1.41,3.19,1.92,0.94,2.11,
          2.81,1.98,1.74,2.16,3.37,2.97,1.69,1.19,2.17,2.28,1.72,2.47,1.02,
          2.52,2.10,3.71)
data1<-data.frame(trt,weight)
data1
data1$trt <- factor(data1$trt)
str(data1)
boxplot(weight ~ trt, data = data1)

fit <- aov(weight ~ trt, data = data1)
summary(fit)

library(gplots)
gplots::plotmeans(weight~trt,
          xlab = "treatment",
          ylab = "weight",
          main="mean plot\nwith95% CI")
library(PMCMRplus)
summary(lsdTest(fit))
TukeyHSD(fit)

bartlett.test(weight ~ trt, data = data1)
library(car)
leveneTest(weight ~ trt, data = data1)

df11_1 <- data.frame(
  x1 = rep(c(" 外膜缝合"," 束膜缝合"), each = 10),
  x2 = rep(c(" 缝合1 个月"," 缝合2 个月"), each = 5),
  y = c(10,10,40,50,10,30,30,70,60,30,10,20,30,50,30,50,50,70,60,30)
)
df11_1
f1 <- aov(y ~ x1 * x2, data = df11_1)
summary(f1)
HH::interaction2wt(y ~ x1 * x2, data = df11_1)

df11_3=import("rawdata/other/datasets/例11-03-5种军装热感觉5-2-2.sav")
df11_3
df11_3$a <- factor(df11_3$a)
f3 <- aov(x ~ b * c * a, data = df11_3)
summary(f3)
HH::interaction2wt(x ~ b * c * a, data = df11_3)

#重复测量数据两因素两水平的方差分析
df12_1 <- import("rawdata/other/datasets/12-1.sav")
sample_n(df12_1,6)
str(df12_1)

df12_11 <-
  df12_1 %>%
  select(1:4) %>% 
  pivot_longer(cols = 2:3,names_to = "time",values_to = "hp") %>% 
  mutate(across(c(n,group,time), ~as.factor(.x)))
str(df12_11)

f1 <- aov(hp ~ time * group + Error(n/time), data = df12_11)
summary(f1)
boxplot(hp ~ group*time, data = df12_11, 
        col = c("gold","green"),
        main = " 两因素两水平重复测量方差分析")


df11_4 <- data.frame(
  a = rep(c("5 度","25 度"),each = 4),
  b = rep(c(0.5, 5.0), each = 2),
  c = c(10, 30),
  d = c(6.0, 8.0,8.0,6.0,8.0,6.0,6.0,8.0),
  x = c(86,95,91,94,91,96,83,88)
)
df11_4

df11_4$a <- factor(df11_4$a)
df11_4$b <- factor(df11_4$b)
df11_4$c <- factor(df11_4$c)
df11_4$d <- factor(df11_4$d)
str(df11_4)
f4 <- aov(x ~ a + b + c + d + a*b, data = df11_4)
summary(f4)

#裂区(混合设计 组间和组内)
df <- data.frame(factorA = factor(rep(c("a1","a2"),each=10)),
                 factorB = factor(rep(c("b1","b2"),10)),
                 id = factor(rep(c(1:10),each=2)),
                 y = c(15.75,19.00,15.50,20.75,15.50,18.50,17.00,20.50,16.50,20.00,
                       18.25,22.25,18.50,21.50,19.75,23.50,21.50,24.75,20.75,23.75)
)
df
str(df)

df %>% pivot_wider(names_from= "factorB", values_from = "y")

f <- aov(y ~ factorA * factorB + Error(id/factorB), data = df)
summary(f)

anova_test(y ~ factorA * factorB + Error(id/factorB), data = df)

library(afex)
f2=aov_car(y ~ factorA * factorB + Error(id/factorB), data = df)
f2
afex::afex_plot(f2, ~factorA,~factorB, error = "within" )
pairs(emmeans(f2, ~factorB|factorA))


#嵌套
df <- data.frame(factor1 = factor(rep(c("A","B","C"),each=6)),
                 factor2 = factor(rep(c(70,80,90,55,65,75,90,95,100),each=2)),
                 y = c(82,84,91,88,85,83,65,61,62,59,56,60,71,67,75,78,85,89)
)
str(df)
df

f <- aov(y ~ factor1/factor2, data = df)
summary(f)


######################
df <- data.frame(factor1 = factor(rep(c("L1","L2"),each=12)),
                 factor2 = factor(rep(c("A","B","C","D"),each=6)),
                 factor3 = factor(rep(c(70,80,90,55,65,75,90,95,100,40,45,50),each=2)),
                 y = c(82,84,91,88,85,83,65,61,62,59,56,60,71,67,75,78,85,89,88,85,83,65,61,62)
)
str(df)
df
f <- aov(y ~ factor1/factor2/factor3, data = df)
summary(f)
ggplot(df, aes(x=factor3, y=y, group=factor2, color=factor2))+
  facet_grid(~factor1, scales = "free")+
  geom_point()+
  geom_smooth(method = "lm", formula = "y~x", se=FALSE)

###################
df13_1 <- data.frame(x1=c(10.8,11.6,10.6,9.0,11.2,9.9,10.6,10.4,9.6,10.5,
                          10.6,9.9,9.5,9.7,10.7,9.2,10.5,11.0,10.1,10.7,8.5,
                          10.0, 10.4,9.7,9.4,9.2,10.5,11.2,9.6,8.0),
                     y1=c(9.4,9.7,8.7,7.2,10.0,8.5,8.3,8.1,8.5,9.1,9.2,8.4,
                          7.6,7.9,8.8,7.4,8.6,9.2,8.0,8.5,7.3,8.3,
                          8.6,8.7,7.6,8.0,8.8,9.5,8.2,7.2),
                     x2=c(10.4,9.7,9.9,9.8,11.1,8.2,8.8,10.0,9.0,9.4,8.9,
                          10.3,9.3,9.2,10.9,9.2,9.2,10.4,11.2,11.1,11.0,
                          8.6,9.3,10.3,10.3,9.8,10.5,10.7,10.4,9.4),
                     y2=c(9.2,9.1,8.9,8.6,9.9,7.1,7.8,7.9,8.0,9.0,7.9,8.9,
                          8.9,8.1,10.2,8.5,9.0,8.9,9.8,10.1,8.5,8.1,8.6,
                          8.9,9.6,8.1,9.9,9.3,8.7,8.7),
                     x3=c(9.8,11.2,10.7,9.6,10.1,9.8,10.1,10.3,11.0,10.5,
                          9.2,10.1,10.4,10.0,8.4,10.1,9.3,10.5,11.1,10.5,
                          9.7,9.2,9.3,10.4,10.0,10.3,9.9,9.4,8.3,9.2),
                     y3=c(7.6,7.9,9.0,7.8,8.5,7.5,8.3,8.2,8.4,8.1,7.0,7.7,
                          8.0,6.6,6.1,8.1,7.8,8.4,8.2,8.0,7.6,6.9,6.7,
                          8.1,7.4,8.2,7.6,7.8,6.6,7.2)
)
str(df13_1)
df13_11 <- df13_1 %>% 
  pivot_longer(cols = everything(), # 变长
               names_to = c(".value","group"),
               names_pattern = "(.)(.)"
  ) %>% 
  mutate(group = as.factor(group)) 
df13_11
# 注意公式的写法，一定是把协变量放在主变量前面！
fit <- aov(y ~ x + group, data = df13_11) 
summary(fit)
HH::ancovaplot(y ~ x + group, data = df13_11)

anova_test(y ~ x + group, data = df13_11, type = 1) %>% 
  get_anova_table()

theme_set(theme_minimal())
p1 <- ggplot(df13_11, aes(x=x,y=y))+
  geom_point(aes(color=group,shape=group))+
  geom_smooth(method = "lm",se=F,aes(color=group))+
  labs(y=NULL)

p2 <- ggplot(df13_11, aes(x=x,y=y))+
  geom_point(aes(color=group,shape=group))+
  geom_smooth(method = "lm",se=F,aes(color=group))+
  facet_wrap(~group)

library(patchwork)
p2 + p1 + plot_layout(guides = 'collect',widths = c(3, 1))

df <- import("rawdata/other/datasets/例13-02.sav")
df$block <- factor(df$block)
str(df)
fit <- aov(y ~ x + block + group, data = df) # 注意顺序
summary(fit)
car::Anova(fit)

#####################################################################
crop.data=import("datas/crop.data.csv")
crop.data=crop.data %>% convert_as_factor(block,density,fertilizer)
str(crop.data)
with(crop.data, table(density,fertilizer))
# anova_test()
two.way = aov(yield ~ fertilizer + density, data = crop.data)
interaction = aov(yield ~ fertilizer * density, data = crop.data)
blocking = aov(yield ~ block+fertilizer * density, data = crop.data)

glance(two.way)
AIC(two.way, interaction, blocking)
summary(two.way)

TukeyHSD(two.way)  # tukey_hsd(two.way)
plot(TukeyHSD(two.way))
PostHocTest(two.way, method = "hsd")

library(DescTools)
lsd=DescTools::PostHocTest(two.way, method = "lsd")
lsd
plot(lsd)
#Dunnett-t检验
DunnettTest(Ozone ~ Month, data = airquality)
DunnettTest(Ozone ~ Month, data = airquality, control="8", conf.level=0.9)


#SNK-q检验
library(PMCMRplus)
# library(PMCMRplus)
lsd <- PMCMRplus::snkTest(two.way)
summary(lsd)

# library(report)
report::report(two.way)

library(afex)
df=warpbreaks %>% rowid_to_column("id")
df
aw=aov_ez("id", "breaks", between = c("wool", "tension"), data=df)
summary(aw)
aw$Anova
afex_plot(aw, x="wool", trace="tension")+
  theme_pubr()
### mixed
data("Machines", package = "MEMSS")
Machines
m1 <- afex::mixed(score ~ Machine + (1+Machine|Worker), data=Machines)
m1
summary(m1)
summary(m1)$varcor
anova(m1)
pairs(emmeans(m1, ~Machine))  #  pairs(emmeans::emmeans(m1, "Machine"))
afex_plot(m1, ~Machine)
afex_plot(m1, "Machine", error = "within") 

library(lmerTest)
m2 = lmer(score ~ Machine + (1+Machine|Worker), data=Machines)
summary(m2)
anova(m2)

pkg_unload("DescTools")
#############################################################
data("sleepstudy", package="lme4")
mod <- lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
summary(mod)
anova(mod)
ggplot(data=sleepstudy %>% modelr::add_predictions(mod))+
  facet_wrap(~Subject)+
  geom_point(aes(x=Days, y=Reaction))+
  geom_line(aes(x=Days, y=pred), color="Red")


library(afex)
aov_car(value ~ treatment * gender + Error(id), fun_aggregate = mean,
        data = obk.long)

a1=aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
           data = obk.long)

afex_plot(a1, "hour", "gender", c("treatment", "phase"))

aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
       within = c("phase", "hour"), covariate = "age", 
       observed = c("gender", "age"), factorize = FALSE)



data("Machines", package = "MEMSS") 
# simple model with random-slopes for repeated-measures factor
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines)
m1
# suppress correlations among random effect parameters with || and expand_re = TRUE
m2 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, expand_re = TRUE)
m2

## compare:
summary(m1)$varcor
summary(m2)$varcor

#############################################################################
df=CO2
with(df, table(Type,Treatment))
df %>% anova_test(uptake~Type*Treatment+Error(Plant/conc)) %>% 
  get_anova_table()

pkg_load(afex)
mod=aov_car(uptake~Type*Treatment+Error(Plant/conc), data=df)
mod$Anova

#################
df <- ToothGrowth
df$dose <- as.factor(df$dose)
stat.test <- df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test
ggboxplot(
  df, x = "supp", y = "len",
  fill = "supp", palette = "lancet", facet.by = "dose",
  ylim = c(0, 40)
) +
  stat_pvalue_manual(stat.test, label = "p.adj", y.position = 35)

pairwise.test <- df %>% t_test(len ~ dose)
pairwise.test
ggboxplot(df, x = "dose", y = "len", color = "dose", palette = "lancet")+
  stat_pvalue_manual(
    pairwise.test, label = "p.adj.signif",
    y.position = c(29, 35, 39)
  )

stat.test <- df %>% t_test(len ~ dose, ref.group = "all")
stat.test
ggboxplot(df, x = "dose", y = "len", fill = "dose", 
          palette = "lancet", alpha = 0.6) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position = 35) +
  geom_hline(yintercept = mean(df$len), linetype = 2)





########################################################################################
Within_Data=import("datas/Chapter_21_Within_Data.csv")
Within_Data %>% head()
str(Within_Data)
Within_Data$Within_Cond <- factor(Within_Data$Within_Cond,
                                  levels = c(1,2),
                                  labels = c("Test", "Restudy")) 

Within_Data$Within_Time <-factor(Within_Data$Within_Time,
                                 levels = c(1,2),
                                 labels = c("Immediate", "Delayed"))
Within_Data$Subject=as.factor(Within_Data$Subject)

Describe(Within_Data)
Within_Data %>% 
  group_by(Within_Cond, Within_Time) %>% 
  get_summary_stats(DV, type="common")

source("script/data_plot.R")
data_plot(Within_Data, 
          x=Within_Time, y=DV, 
          trace =Within_Cond, 
          type="point", add="mean")
library(afex)
aov.1=aov_car(DV ~ Within_Cond*Within_Time + Error(Subject/Within_Cond*Within_Time),
              data=Within_Data)
aov.1
anova_test(DV ~ Within_Cond*Within_Time + Error(Subject/Within_Cond*Within_Time), 
           data=Within_Data) %>% 
  get_anova_table()

emm1=emmeans(aov.1, ~Within_Time|Within_Cond)
emm1
emm2=emmeans(aov.1, ~Within_Cond|Within_Time)
emm2
pairs(emm2)
afex_plot(aov.1, x=~Within_Time, ~Within_Cond, error = "within")

aov_car(DV ~ Within_Cond*Within_Time + Error(Subject/Within_Cond*Within_Time), data=Within_Data,
        anova_table = list(es = "pes"))
#This turns off GG correction (sphericity corrections disabled)
aov_car(DV ~ Within_Cond*Within_Time + Error(Subject/Within_Cond*Within_Time), data=Within_Data,
        anova_table = list(correction = "none")) 
#This turns off GG correction (sphericity corrections disabled)
aov_car(DV ~ Within_Cond*Within_Time + Error(Subject/Within_Cond*Within_Time), data=Within_Data,
        return="univariate")
summary(aov.1, return="univariate")


Mixed_Data=import("datas/Chapter_21_Mixed_Data.csv")
Mixed_Data %>% head()
str(Mixed_Data)
Mixed_Data$Subject<-as.factor(Mixed_Data$Subject)
Mixed_Data$Btw_Cond <- factor(Mixed_Data$Btw_Cond,
                              levels = c(0,1),
                              labels = c("Younger_Adult", "Older_Adult"))

Mixed_Data$Within_Cond <- factor(Mixed_Data$Within_Cond,
                                 levels = c(1,2),
                                 labels = c("Test", "Restudy"))
#########################################################################
library(randomizr)
com <- randomizr::complete_ra(100, num_arms = 2, conditions = c(" 试验组"," 对照组"))
com

library(blockrand)
res <- blockrand(n=100, num.levels = 2, levels = c(" 试验组"," 对照组"))
head(res)
table(res$treatment)
res.M <- blockrand(n = 60,
                   num.levels = 4,
                   levels = c(" 试验组1"," 试验组2"," 阳性对照组"," 阴性对照组"),
                   stratum = "男性",
                   id.prefix = "男", # id 前缀
                   block.sizes = c(3),
                   block.prefix = "男" # 前缀
)
table(res.M$treatment)
res.F <- blockrand(n = 60,
                   num.levels = 4,
                   levels = c(" 试验组1"," 试验组2"," 阳性对照组"," 阴性对照组"),
                   stratum = "女性",
                   id.prefix = "女", # id 前缀
                   block.sizes = c(3),
                   block.prefix = "女" # 前缀
)
table(res.F$treatment)
res <- cbind(res.M,res.F)
dim(res)
head(res)

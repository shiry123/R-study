# install.packages("bruceR", dep=TRUE)
library(bruceR)
getwd()
set.wd(ask = T)  # 不需要加任何参数
set.wd("../")
library(afex)
detach("package:afex")  #卸除已加载的包
installed.packages()[,c('Package','Version','LibPath')]
rio::import("datas/all.data.xlsx", sheet = 1, setclass="data.table")
dt=import("datas/all.data.xlsx", sheet = 1, as="dt") #data.table
dt
export(dt, file="outfiles/NewData.xlsx")
print_table(npk)
print_table(npk, file="outfiles/NPK_data.doc")
model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
GLM_summary(model)
print_table(model)
#### 一般线性模型 ####
lm1=lm(Temp ~ Month + Day, data=airquality)
lm2=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
GLM_summary(lm1)
model_summary(list(lm1, lm2))
model_summary(list(lm1, lm2), std=TRUE, digits=2)
model_summary(list(lm1, lm2), file="outfiles/OLS Models.doc")

#### 混合线性模型 ####
library(lmerTest)
hlm1=lmer(Reaction ~ (1 | Subject), data=sleepstudy)
hlm2=lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
hlm3=lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
HLM_summary(hlm1)
HLM_summary(hlm3, test.rand=TRUE)
model_summary(list(hlm1, hlm2, hlm3))
model_summary(list(hlm1, hlm2, hlm3), std=TRUE)
model_summary(list(hlm1, hlm2, hlm3), file="outfiles/HLM Models.doc")

# ?data.table::`:=`
d=data.table(var=c(NA, 0, 1, 2, 3, 4, 5, 6))
d
d[, ':='(
  var.new=RECODE(var, "lo:1='B'; c(2,3)=1; 4=2; 5:hi=3; else=999")
)]
d %>% mutate_dt(var.new=RECODE(var, "lo:1='B'; c(2,3)=1; 4=2; 5:hi=3; else=999"))


dt=as.data.table(psych::bfi)
dt
dt[, ':='(
  E=MEAN(dt, "E", 1:5, rev=c("E1", "E2"), likert=1:6),
  A=MEAN(dt, "A", 1:5, rev="A1", likert=1:6),
  C=MEAN(dt, "C", 1:5, rev=c(4, 5), likert=1:6),
  N=MEAN(dt, "N", 1:5, likert=1:6),
  O=MEAN(dt, "O", 1:5, rev=c(2, 5), likert=1:6)
)]
library(tidyfst)
dt %>% mutate_dt(  E=MEAN(., "E", 1:5, rev=c("E1", "E2"), likert=1:6),
                   A=MEAN(., "A", 1:5, rev="A1", likert=1:6),
                   C=MEAN(., "C", 1:5, rev=c(4, 5), likert=1:6),
                   N=MEAN(., "N", 1:5, likert=1:6),
                   O=MEAN(., "O", 1:5, rev=c(2, 5), likert=1:6)) %>%
  select_dt(E,A,C,N,O)

# install.packages("GPArotation")
data=data.table(psych::bfi)  
data
EFA(data, "E", 1:5)              # var + items
EFA(data, "E", 1:5, nfactors=2)  # var + items
EFA(data, varrange="A1:O5", nfactors="parallel", hide.loadings=0.45)

data.bfi=na.omit(psych::bfi)
CFA(data.bfi, "E =~ E[1:5]; A =~ A[1:5]; C =~ C[1:5]; N =~ N[1:5]; O =~ O[1:5]")

Describe(airquality)
Describe(airquality, plot=T, upper.triangle=TRUE, upper.smooth="lm")

Freq(airquality$Ozone)
Corr(airquality)

d1=as.data.table(between.3)
d1[, ':='(Y1=SCORE,Y2=rnorm(32))]
d1
d1$B=factor(d1$B, levels=1:2, labels=c("Low", "High"))
d1$C=factor(d1$C, levels=1:2, labels=c("M", "F"))
d1

## 单样本t检验 ##
gghistogram(d1, x="SCORE", fill="lightgray", add = "mean", add_density=TRUE)
TTEST(d1, "SCORE", test.value=5)

## 独立样本t检验 ##
TTEST(d1, "SCORE", x="A")
TTEST(d1, "SCORE", x="A", var.equal=FALSE)
TTEST(d1, y="Y1", x=c("A", "B", "C"))
TTEST(d1, y=c("Y1", "Y2"), x=c("A", "B", "C"))
t.test(data=d1, Y1~A)


d2=within.1
d2
## 配对样本t检验 ##
TTEST(d2, y=c("A1", "A2"), paired=TRUE)
TTEST(d2, y=c("A1", "A2", "A3", "A4"), paired=TRUE)

#### (4) 方差分析 ###
## 被试间设计 ##
between.2
m = MANOVA(between.3, dv="SCORE", between=c("A", "B","C"))
m %>% EMMEANS(c("A", "B"), by="C")
emmip(m, A ~ B | C, CIs=TRUE, CIarg = list(alpha = 1))
## 被试内设计 ##
within.3
m=MANOVA(within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
         within=c("A", "B", "C"))
## 混合设计 ##
mixed.3_2b1w
m=MANOVA(mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
         between=c("A", "C"), within="B")
df=mixed.3_2b1w %>% 
  rowid_to_column("ID") %>% 
  pivot_longer(B1:B2, names_to = "B", values_to = "Y") %>% 
  convert_as_factor(ID,A,C,B)
df
#B 组内因素
res.aov=anova_test(data=df, Y~A*C*B+Error(ID/B), type = 3)
get_anova_table(res.aov)

library(afex)
detach("package:afex")
fit.aov=afex::aov_car(data=df, Y~A*C*B+Error(ID/B))
summary(fit.aov)

fit=aov(data=df, Y~A*C*B+Error(ID/B))
summary(fit)


## 换个变量名 ##
data.new=mixed.3_1b2w
names(data.new)=c("Group", "Cond_01", "Cond_02", "Cond_03", "Cond_04")
data.new
MANOVA(data.new,
       dvs="Cond_01:Cond_04",
       dvs.pattern="Cond_(..)",
       between="Group",
       within="Condition")  


#协变量 age
fit.aov=MANOVA(afex::obk.long, 
               subID="id", 
               dv="value",
               between=c("treatment", "gender"),
               within=c("phase", "hour"),
               cov="age",
               sph.correction="GG",
               # file="aov_report.doc"
)



########################################################
d = as.data.table(within.1)
d$XYZ = 1:8
d
d %>% add({
  ID = str_extract(ID, "\\d")  # modify a variable
  XYZ = NULL                   # delete a variable
  A.mean = .mean("A", 1:4)          # create a new variable
  B = A.mean * 4    # new variable is immediately available
  C = 1        # never need ,/; at the end of any line
})

cc("a,b,c,d,e")
cc("A, B, C"," D|E ",c("F", "G"))
d = data.table(x=1:9, g=rep(1:3, each=3))
d
group_mean_center(d, "x", by="g", add.suffix="_c")
c("stage" %^% 0:7)
x=formula_paste(y ~ x)
is.character(x)
formula_paste(y ~ x + (1 | g))
formula_expand(y ~ a*b*c)
formula_expand("y ~ a*b*c")
rep_char("a", 5)

#######################################################
ccf_plot(chicken ~ egg, data=lmtest::ChickEgg, alpha.ns=0.3,
         pos.color="blue",
         neg.color="red",
         ci.color="black")
granger_test(chicken ~ egg, data=lmtest::ChickEgg)

a = tibble(地区=c("天津","甘肃","新疆"))
a
b = tibble(city = c("a天津市","上海闵行区","广东省","c河南","甘肃","d新疆自治区"),
           food = c("麻花","包子","早茶","胡辣汤","拉面","哈密瓜"))
b
# library(fuzzyjoin)
a %>% regex_inner_join(b, by=c("地区"="city"))


#############################################################################
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("usplos/YawMMF", force = TRUE)
# library(YawMMF)
# YawMMF::contr.simple(3)
# dat=YawMMF::Simplecoding(df, Factor ="supp,dose")
df.simp = YawMMF::Simplecoding(data = CO2, Factor = 'Plant,conc,Type,Treatment')
contrasts(df.simp$Type)
df = YawMMF::MixedModelDummy(Data = df.simp, FixEffects = 'Type*Treatment')
df
################################## lmer
library(lmerTest)
p=ggscatter(sleepstudy, x="Days", y="Reaction",size=1, facet.by = "Subject")
p
mod1=lm(Reaction ~ Days, sleepstudy)

p1=p+geom_line(data= sleepstudy %>% modelr::add_predictions(mod1),
               aes(x=Days, y=pred), color="Red")
mod2 <- lmer(Reaction ~ Days+(1+Days|Subject), sleepstudy)

p2=p+
  geom_line(data= sleepstudy %>% modelr::add_predictions(mod2),
            aes(x=Days, y=pred), color="Red")
plots(p1, p2, tags = c("lm","lmer"))
########################################################################
## 固定效应：attitude 和 gender
## 随机效应：subject 和 scenario
##全模型和零模型
#建立全模型
politeness = import("datas/politeness_data.csv")
str(politeness)
politeness = politeness %>% drop_na() %>% mutate(across(1:4, ~as.factor(.x))) 
m.full = lmer(frequency ~ attitude * gender + 
                         (1 + attitude * gender|subject) +
                         (1 + attitude * gender|scenario),
              # control = lmerControl(optCtrl = list(maxfun = 20000)),
                            data=politeness, REML=FALSE)  #ML法
summary(m.full)
anova(m.full)
isSingular(m.full, tol = 1e-4) #奇异模型检验

#建立零模型
m.zero = lmer(frequency ~ attitude + gender + 
                              (1|subject) + (1|scenario), 
                            data=politeness, REML=FALSE)
summary(m.zero)
anova(m.zero)
#模型优化
m1 = lmer(frequency ~ attitude+gender+(1+attitude+gender|subject)+(1+attitude+gender|scenario),
              data=politeness, REML=FALSE)  #ML法
m2 = lmer(frequency ~ attitude+gender+attitude:gender+(1|subject)+(1|scenario),
          data=politeness, REML=FALSE)  #ML法
summary(m1)
AIC(m1, m2)
BIC(m1, m2)
anova(m.zero,m.full,m1, m2)

mod = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), 
              data=politeness, REML=TRUE)
summary(mod)
anova(mod)
rand(mod)
pred.data=politeness %>% modelr::add_predictions(mod) 
pred.data

#事后检验
library(emmeans)
emmeans(m.zero, pairwise~attitude, adjust="none")
emmeans(m.zero, pairwise~attitude|gender, adjust="none")
emmeans(m.zero, pairwise~attitude|gender)
joint_tests(m.zero, by="attitude")

#############################################################################
rt_data <- import("datas/rt_dummy_data.csv",as="dt")
sample_n(rt_data,6)

rt_data$modality <- ifelse(rt_data$modality == "Audio-only", 0, 1)
rt_full.mod <- lmer(RT ~ 1 + modality + 
                      (1 + modality|PID) + (1 + modality|stim), 
                    data = rt_data)
# allFit(rt_full.mod)
#消除R中两个随机效应之间的相关性，只需在随机效应规范中的1的地方放一个0
# rt_full.mod <- lmer(RT ~ 1 + modality + 
#                       (0 + modality|PID) + (1|PID)+ (1 + modality|stim), 
#                     data = rt_data)

rt_full.mod <- lmer(RT ~ 1 + modality + 
                      (1 + modality|PID) + (1 + modality|stim), 
                    data = rt_data, 
                    control = lmerControl(optimizer = "bobyqa"))
summary(rt_full.mod)
r2(rt_full.mod)
anova(rt_full.mod)
report(rt_full.mod)

#似然比检验(Likelihood-ratio tests)
mixed(RT ~ 1 + modality + 
        (1 + modality|PID) + (1 + modality|stim), 
      data = rt_data, 
      control = lmerControl(optimizer = "bobyqa"),
      method="LRT")
coef(rt_full.mod)
broom.mixed::tidy(rt_full.mod)
broom.mixed::glance(rt_full.mod)
broom.mixed::augment(rt_full.mod)

#背景噪音水平  简单（编码为0）和困难（编码为1）
#纯音频（编码为0）和视听（编码为1）条件下识别语音
rt_data_interaction <- import("datas/rt_dummy_data_interaction.csv",as="dt")
rt_data_interaction
rt_int.mod = lmer(RT ~ 1 + modality + SNR + modality:SNR +
                     (1 + modality + SNR|stim) + (1 + modality + SNR|PID), 
                   data = rt_data_interaction)
summary(rt_int.mod)


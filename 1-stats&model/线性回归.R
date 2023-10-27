data("ToothGrowth")
df <- ToothGrowth %>% transmute(supp,dose=as.factor(dose),len)
str(df)
sample_n(df,5)
library(GGally)
ggpairs(df)

df %>% 
  group_by(supp) %>% 
  get_summary_stats(len, type="mean_sd")

contrasts(df$dose) #对比方式
mod=lm(len ~ supp*dose, data=df)
summary(mod)  
anova(mod)
car::Anova(mod, type = 3)
coef(mod)
GLM_summary(mod)
r2(mod)
broom::tidy(mod)
autoReg::modelPlot(mod)
library(GGally)
reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
ggcoef(reg)+
  labs(title="plot")
d <- as.data.frame(Titanic)
log.reg <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
ggcoef(log.reg, exponentiate = TRUE)
ggcoef_model(log.reg)

performance::check_model(mod)
shapiro.test(mod$residuals) # 残差正态性检验

####
pairs(emmeans (mod,  ~ dose|supp))
df %>% 
  group_by(supp) %>% 
  emmeans_test(len~dose)
#预测
newdat = slice_sample(df, n = 5)
newdat
# 新数据预测
predict(mod, newdat, interval = "confidence")
emmip(mod, ~dose|supp, 
      CIs = TRUE, CIarg = list(alpha = 1))   # trace.factors ~ x.factors | by.factors
p1=emmip(mod, supp~dose, 
      CIs = TRUE, CIarg = list(alpha = 1))+
  ylim(0,35)
p2=afex::afex_plot(mod, ~dose, ~supp)+
  ylim(0,35)
ggarrange(p1,p2)


# 多元线性回归
library(MASS)
state<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
as.data.table(state)
mod.full=lm(Murder ~ ., data = state)
summary(mod.full)
mod=MASS::stepAIC(mod.full)
mod
# 全子集回归-regsubsets()
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost,
                  data = state, nbest=4)
plot(leaps,scale = "adjr2")#选择调整R平方值

# install.packages("ggtrendline")
library(ggtrendline)
x <- c(1, 3, 6, 9,  13,   17)
y <- c(5, 8, 11, 13, 13.2, 13.5)
ggtrendline(x, y, model = "line2P")  
ggtrendline(x, y, model = "line3P", CI.fill = NA) + 
  geom_point(aes(x, y)) + theme_bw()

library(interactions)
fiti <- lm(mpg ~ hp * wt, data = mtcars)
sim_slopes(fiti, pred = hp, modx = wt, jnplot = TRUE)
tidy(fiti)
interact_plot(fiti, pred = hp, modx = wt, interval = TRUE)
interact_plot(fiti, pred = hp, modx = wt, plot.points = TRUE)
fitiris <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
interact_plot(fitiris, pred = Petal.Width, modx = Species, plot.points = TRUE)

fit <- lm(price ~ cut * color, data = diamonds)
cat_plot(fit, pred = color, modx = cut, interval = TRUE)

mpg2 <- mpg
mpg2$auto <- "auto"
mpg2$auto[mpg2$trans %in% c("manual(m5)", "manual(m6)")] <- "manual"
mpg2$auto <- factor(mpg2$auto)
mpg2$fwd <- "2wd"
mpg2$fwd[mpg2$drv == "4"] <- "4wd"
mpg2$fwd <- factor(mpg2$fwd)
## Drop the two cars with 5 cylinders (rest are 4, 6, or 8)
mpg2 <- mpg2[mpg2$cyl != "5",]
mpg2$cyl <- factor(mpg2$cyl)
## Fit the model
fit3 <- lm(cty ~ cyl * fwd * auto, data = mpg2)

# The line geom looks good for an ordered factor predictor
cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
         interval = TRUE)

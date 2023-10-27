# install.packages("datarium")
data("stress", package = "datarium")
str(stress)
# stress %>% sample_n_by(treatment, exercise)
ggscatter(stress, x = "age", y = "score", facet.by = c("treatment","exercise"))+
  stat_smooth()

#########协方差分析
stress %>%
  unite(col = "group", treatment, exercise) %>%
  anova_test(score ~ group*age)

res.aov=stress %>%
  anova_test(dv=score, wid=id, between = c(treatment, exercise), covariate=age)
get_anova_table(res.aov, correction = "GG")

# Pairwise comparisons
pwc <- stress %>% 
  group_by(exercise) %>%
  emmeans_test(
    score ~ treatment, covariate = age,
    p.adjust.method = "bonferroni"
  )
pwc
# Line plot
lp <- ggline(
  get_emmeans(pwc), x = "exercise", y = "emmean", 
  color = "treatment", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = treatment), 
    width = 0.1
  )
lp

# Comparisons between treatment group at each exercise level
pwc <- pwc %>% add_xy_position(x = "exercise", fun = "mean_se", step.increase = 0.2)
lp + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0,
    bracket.size = 0
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Fit the model, the covariate goes first
model <- lm(score ~ age + treatment*exercise, data = stress)
anova_test(model)
summary(model)
anova(model)            #显著性

broom::tidy(model) 
broom::glance(model) 
broom::augment(model) %>% pluck(".resid") %>% shapiro_test()
broom::augment(model) %>% levene_test(.resid ~ treatment*exercise)
broom::augment(model) %>% filter(abs(.std.resid) > 3) 


##############混合设计
# install.packages("datarium")
data("performance", package = "datarium")
str(performance) # t 组内因素
df <- performance %>%
  pivot_longer(t1:t2, names_to = "time", values_to = "score") %>%
  convert_as_factor(id,time)
df
##########################################################
sys_time_print({
  df %>%
    group_by(gender, stress) %>%
    get_summary_stats(score, type = "mean_sd")
})
sys_time_print({
  df %>% summarise_dt(avg = mean(score),
                      SD = sd(score),
                      n=.N,
                      by=.(gender, stress))
})
##############################################################
df %>% summarise_dt(avg = mean(score),
                    n=.N,
                    by=.(gender, stress))
df %>% summarise_vars(score, .func=list(mean,sd),
                      by=.(gender, stress))

res.aov <- anova_test(data =df, score~gender*stress*time+Error(id/time))
#####球形假设校正 重复测量
get_anova_table(res.aov, correction = "GG")
# HH::interaction2wt(data=df, score~gender*stress*time,
#                    par.strip.text=list(cex=.7))
#########################################################################
df %>%
  group_by(gender, stress, time) %>%
  identify_outliers(score)
df %>%
  group_by(gender, stress, time ) %>%
  shapiro_test(score)
shapiro_test(data=df_group_by(df, gender, stress, time), score)
##########################################################################
ggqqplot(df, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ stress, labeller = "label_both")
###########################################################################
df %>%
  group_by(time) %>%
  levene_test(score ~ gender*stress)

###############################################################################
data("ToothGrowth")
df <- ToothGrowth %>% mutate(dose=factor(dose),
                                     id=1:n())
str(df)
df %>% anova_test(len ~ supp*dose)

#########组内设计
df2=df %>% mutate(id=rep(1:10,6)) 
df2 %>% 
  anova_test(len ~ supp*dose + Error(id/(supp*dose))) %>% 
  get_anova_table()

#########################################################333

df3=ToothGrowth %>% df_unite(col = "dose_supp", dose, supp) %>% 
    df_nest_by(dose_supp)
df3
df_select(df3, dose_supp)

ToothGrowth %>%
  df_get_var_names(dose, len)

####################
df <- tibble(
group = c("a", "a", "b", "b", "c", "c"),
time = c("t1", "t2", "t1", "t2", "t1", "t2"),
value = c(5, 6, 1, 3, 4, 5)
)
df
# Convert group and time into factor variable
result <- df %>% convert_as_factor(group, time)
result
# Show group levels
levels(result$group)
df <- head(ToothGrowth)

df %>%
  df_label_both(dose, supp)

head(ToothGrowth) %>%
  df_unite(col = "dose_supp", vars = c("dose", "supp"))

ToothGrowth %>%
  group_by(dose) %>%
  doo(~t.test(len ~ supp, data =.) %>% tidy())


comparisons <- ToothGrowth %>%
  group_by(dose) %>%
  doo(~t.test(len ~ supp, data =.))
comparisons
comparisons$.results.

ToothGrowth %>% dunn_test(len ~ dose)

# Set c as the reference level (the first one)
result <- result %>%
  set_ref_level("group", ref = "c")
levels(result$group)

# Set the order of levels
result <- result %>%
  reorder_levels("group", order = c("b", "c", "a"))
levels(result$group)



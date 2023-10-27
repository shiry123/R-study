
library(ggprism)
tg<-ToothGrowth
tg$dose <- as.factor(tg$dose)

mean_sd(tg$len)
mean_sdl(tg$len)
mean_ci(tg$len)
mean_cl_normal(tg$len)
mean_se(tg$len)

pd=0.7
ggplot(tg, aes(x =dose, y =len, group=supp)) +
  geom_bar(aes(fill =supp), 
           stat = "summary", fun = mean, color = "black", width=0.5,
           position = position_dodge(pd)) +
  stat_summary(fun.data = 'mean_sdl', 
               geom = "errorbar", colour = "black",width = 0.2,
               position = position_dodge(pd))+
  guides(y = guide_prism_minor())+
  scale_fill_jco()+
  theme_prism(axis_text_angle=0)+
  theme(legend.position = "top",
        legend.title=element_text())



base <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_violin(aes(colour = dose, fill = dose), trim = FALSE) + 
  geom_boxplot(aes(fill = dose), width = 0.2, colour = "black") + 
  scale_y_continuous(limits = c(-5, 40))
base
p_vals <- tibble::tribble(
  ~group1, ~group2, ~p.adj,   ~y.position,
  "0.5",   "1",     8.80e-14, 35,
  "0.5",   "2",     1.27e-7,  39
)
p_vals
base + 
  scale_color_prism("floral") +   #可以设置“candy_bright”
  scale_fill_prism("floral") + 
  guides(y = "prism_offset_minor") +
  theme_prism(base_size = 16) + 
  theme(legend.position = "none") +  #可以调整legend的位置
  add_pvalue(p_vals, label = "p = {p.adj}", tip.length = 0, label.size = 4)

base+ add_pvalue(p_vals, label = "p.adj", tip.length = 0)

stat.test=tg %>% t_test(len~dose) %>% add_xy_position()
stat.test
base+ add_pvalue(stat.test, label = "p = {p.adj}", tip.length = 0, label.size = 4)


base <- ggplot(mpg, aes(x = as.factor(cyl), y = hwy)) +
  geom_jitter(width = 0.2) +
  theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(guide = guide_prism_bracket(width = 0.12, outside = FALSE)) +
  theme(axis.line.x = element_line(colour = "red"),
        axis.ticks.x = element_line(colour = "blue"),
        axis.text.x = element_text(colour = "green"))
pp<-base +labs(x=" ",y="hwy")+
  theme(axis.text.y =element_text (color="black"),panel.background = element_blank())
pp

###################################################################################
library(ggplot2)
library(ggprism)
library(ggbeeswarm)
library(rstatix)
data("wings")
wings
wings_pvals <- wings %>%
  group_by(sex, measure) %>%
t_test(
    percent.change ~ genotype, 
    p.adjust.method = "BH", 
    var.equal = TRUE, 
    ref.group = "Tps1MIC/+"
  ) %>%
  add_x_position(x = "measure", dodge = 0.9) %>%   # dodge must match points
  mutate(label = c("***", "*", "P = 0.26", "***", "***", "P = 0.65"))
wings_pvals
p=ggplot(wings, aes(x = measure, y = percent.change)) + 
  ggbeeswarm::geom_beeswarm(aes(fill = genotype), dodge.width = 0.9, shape = 21)+
  facet_wrap(~ sex, scales = "free", 
             labeller = labeller(sex = c(male = "\u2642", female = "\u2640"))) + 
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) + 
  theme_prism(base_line_size = 0.7)+
  scale_x_discrete(guide = guide_prism_bracket(width = 0.15), 
                   labels = scales::wrap_format(5)) + 
  scale_y_continuous(limits = c(-20, 12), expand = c(0, 0),
                     breaks = seq(-20, 10, 5), guide = "prism_offset") + 
  labs(y = "% change") +
  scale_fill_manual(values = c("#026FEE", "#87FFFF"), 
                    labels = c(expression("Tps"*1^italic("MIC")~"/ +"), 
                               expression("Tps"*1^italic("MIC")))) + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        strip.text = element_text(size = 14),
        legend.spacing.x = unit(0, "pt"),
        legend.text = element_text(margin = margin(r = 20)))+
  guides(fill = guide_legend(override.aes = list(size = 3)))

p2=p+stat_summary(aes(fill = genotype),fun = mean,geom = "crossbar",
                 position = position_dodge(0.9), colour = "red", linewidth = 0.4, width = 0.7,
                 show.legend = FALSE)
p2
p2+add_pvalue(wings_pvals, y = 10, xmin = "xmin", xmax = "xmax", 
             tip.length = 0, fontface = "italic", 
             lineend = "round", bracket.size = 0.5)

# vars_nm=c("vs","am")
# vars_nm="vs"
# formula(paste("~",paste(vars_nm, collapse="+")))
# mtcars[[vars_nm]]

data("wings")
wings
wings_pvals <- wings %>%
  group_by(sex, measure) %>%
  t_test(percent.change ~ genotype, 
         p.adjust.method = "BH", var.equal = TRUE, ref.group = "Tps1MIC/+") %>%
  add_x_position(x = "measure", dodge = 0.9) %>%
  add_significance()
wings_pvals
source("script/prism_plot.R")
prism_plot(wings, x=measure, y=percent.change,
           trace =genotype, facets =sex)

  
source("script/data_plot.R")
data_plot(wings, x=measure, y=percent.change, trace =genotype, facets =sex,
            stats = TRUE, type="point", x.text.angle=0)+
  labs(x="Measure", y="% Change")+    # labs(fill="", x="Measure", y="% Change")
  scale_fill_manual(values = c("#026FEE", "#87FFFF"), 
                    labels = c(expression("Tps1"^italic("MIC")~"/+"), 
                               expression("Tps1"^italic("MIC"))))+
  add_pvalue(wings_pvals, y = 11, xmin = "xmin", xmax = "xmax", 
             tip.length = 0, fontface = "italic", lineend = "round", bracket.size = 0.5)

##############################################################################
  df=ToothGrowth %>% convert_as_factor(dose, supp)
  df_p_val <- df %>% 
    rstatix::group_by(dose) %>% 
    rstatix::t_test(len ~ supp) %>% 
    rstatix::add_xy_position() %>% 
    add_significance()
  df_p_val
  p <- ggplot(df, aes(x =supp, y = len)) + 
    geom_boxplot(width = 0.2) + 
    facet_wrap(
      ~ dose, scales = "free", 
      labeller = labeller(dose = function(x) paste("dose =", x))
    ) + 
    theme_prism()
  p
  p + add_pvalue(df_p_val,tip.length = 0)
  
  
  source("script/data_plot.R")
  data_plot(df,x=supp,y=len,type="box")+
    facet_wrap(
      ~ dose, scales = "free", 
      labeller = labeller(dose = function(x) paste("dose =", x))
    )+
  add_pvalue(df_p_val, tip.length = 0)

  ggboxplot(df,x="supp",y="len", fill="supp")+
    facet_wrap(
      ~ dose, scales = "free", 
      labeller = labeller(dose = function(x) paste("dose =", x))
    ) + 
    theme_prism()+
    add_pvalue(df_p_val,tip.length = 0)


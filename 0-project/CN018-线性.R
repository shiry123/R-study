############################################################
df=tribble(
  ~Conc,     ~Area1,     ~Area2,     ~Area3,
  0.101008759,    7608.61,    7664.26,    7340.79,
  0.202017518,   13984.88,   14005.85,   13944.12,
  0.505043796,   33413.32,   33539.22,   33135.28,
  1.010087591,   62542.67,   62905.93,    61977.5,
  5.050437957,   294269.6,  295592.55,  293920.91,
  10.10087591,  586634.54,  585836.37,  587433.65,
  25.25218979, 1414477.15,  1426242.2, 1428479.07,
  50.50437957, 2682857.63, 2672632.09, 2661864.25,
  101.0087591, 4790701.99, 4798147.52, 4785982.69
)
df
source("script/data_summarise.R")
df.mean=data_summarise(df,contains("Area"), fun_names="mean",row_calc = TRUE)
df.mean
df.long=df %>% pivot_longer(-Conc, names_to = "test", values_to = "Area")
df.long

fit=lm(data=df.long, Area~log(Conc))
summary(fit)


round(summary(fit)[["r.squared"]],4)
broom::glance(fit)$r.squared
coef(fit)
broom::tidy(fit)


ggplot(df.long, aes(x=Conc, y=Area))+
  geom_point()+
  geom_smooth(method="lm",formula = 'y ~x')+
  facet_wrap(~test)


library(ggpmisc)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
y <- y / max(y)
my.data <- data.frame(x = x, y = y,
                      group = c("A", "B"),
                      y2 = y * c(1, 2) + c(0, 0.1),
                      w = sqrt(x))
my.data
my.formula <- y ~ poly(x, 3, raw = TRUE)
my.formula <- y ~ log(x)
my.formula <- y ~ x + I(x^2)
my.formula=as.formula("y~log(x)")
my.formula=as.formula("y~poly(x,2)")
my.formula
terms(my.formula)
all.vars(my.formula)
update(y ~ x1 + x2, ~. + x3)
as.formula(paste("y ~ x1 + x2", "x3", sep = "+"))

ggplot(my.data, aes(x=x, y=y, color=group)) +
  geom_point() +
  stat_poly_line(formula = my.formula) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = my.formula)+
  facet_wrap(~group, scales = "free")+
  theme_modern()+
  scale_color_oi()
  

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                  after_stat(eq.label), sep = "*\", \"*")),
               formula = formula)



library(ggtrendline)
ggtrendline(df.long$Conc, df.long$Area, "line2P")+ 
  geom_point(aes(df.long$Conc,df.long$Area))

source("script/trendline_plot.R")
trendline_plot(df.long, x=Conc, y=Area, "log2P")+
  theme_pubr()

##################################################################
# library(ggh4x)
df <- data.frame(
  item = c("Coffee", "Tea", "Apple", "Pear", "Car"),
  type = c("Drink", "Drink", "Fruit", "Fruit", "NA"),
  amount = c(5, 1, 2, 3, 1),
  stringsAsFactors = FALSE
)
df
ggplot(df, aes(weave_factors(item, type), amount)) +   # interaction(item, type)
  geom_col() +
  guides(x = "axis_nested")

#	
# Effect size (Cohen's d)
anova_test(extra ~ group, data = sleep)

library(pwr)
pwr.t.test(power = 0.6, d = 0.2, sig.level = 0.05,
           alternative = "greater")


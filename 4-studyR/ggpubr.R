library(ggpubr)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
df
ggscatter(df, x = "x", y = "y", 
          add = "reg.line") +
  stat_cor(label.y = 300, aes(label = after_stat(rr.label))) +  #p.label
  stat_regline_equation(label.y = 260)


##################
ggboxplot(iris, x = "Species", y = c("Sepal.Length", "Petal.Length"), 
          merge = TRUE,
          palette = "jco")

iris %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, Petal.Length, type = "mean_sd")
iris %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)

iris %>%
  cor_test(Sepal.Length, Petal.Length)
library(GGally)
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
results <- iris %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  group_by(Species) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots
iris %>%
  pivot_longer(c(Sepal.Length, Petal.Length), names_to = "variable") %>%
  group_by(variable) %>%
  levene_test(value ~ Species)
cbind(Sepal.Length, Petal.Length)
model <- lm(cbind(Sepal.Length, Petal.Length) ~ Species, data=iris)
Manova(model, test.statistic = "Pillai")

##############################################################
data("ToothGrowth")
df=ToothGrowth %>% convert_as_factor(dose)

p=ggboxplot(df, x="dose", y="len",fill = "dose", 
          palette ="jco", bxp.errorbar = TRUE, add="jitter")  #增加了jitter点，点shape由dose映射p
p
p + stat_compare_means(aes(label = after_stat(p.signif)),
                       method = "t.test", ref.group = "0.5")
p + stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))))

my_comparisons <- list(c("0.5", "1"), c("1", "2"), c("0.5", "2"))
my_comparisons=df %>% get_comparisons("dose")
ggboxplot(df, x = "dose", y = "len", fill= "dose", palette = "npg")+
  stat_compare_means(comparisons = my_comparisons,
                     method="t.test")


##################################################################### 
data("mtcars")
df2 <- mtcars
df2$cyl <- factor(df2$cyl)
df2$name <- rownames(df2)#添加一行name
head(df2[, c("name", "wt", "mpg", "cyl")])
p1=ggbarplot(df2, x="name", y="mpg", fill = "cyl", color = "white", 
          palette = "jco",#杂志jco的配色 
          sort.val = "desc",#下降排序 
          sort.by.groups=FALSE,#不按组排序 
          x.text.angle=60)
p2=ggbarplot(df2, x="name", y="mpg", fill = "cyl", color = "white", 
          palette = "jco",#杂志jco的配色 
          sort.val = "asc",#上升排序,区别于desc，具体看图演示 
          sort.by.groups=TRUE,#按组排序 
          x.text.angle=90)
figure=ggarrange(p1,p2, labels = c("A","B"))
figure
annotate_figure(figure,
                top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source:  mtcars data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "Here )!",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)



###########################################################################
data(mtcars)
df <- mtcars[, c("mpg","cyl","wt")]
df$cyl <- as.factor(df$cyl)
head(df)
ggplot(mpg, aes(cty, colour = factor(cyl))) +
  geom_density(aes(fill = after_scale(alpha(colour, 0.3))))
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(fill = stage(class, after_scale = alpha(fill, 0.4))))
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))

ggplot(mpg, aes(cty, colour = factor(cyl))) +
  geom_ribbon(
    stat = "density", outline.type = "upper",
    aes(
      fill = after_scale(alpha(colour, 0.3)),
      ymin = after_stat(group),
      ymax = after_stat(group + ndensity)
    )
  )
ggplot(mpg, aes(displ, class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(
    aes(label = after_stat(xmax), x = stage(displ, after_stat = xmax)),
         stat = "boxplot", hjust = -0.5
  )

ggplot(economics, aes(unemploy, date)) + geom_line(orientation = "y")
recent <- economics[economics$date > as.Date("2013-01-01"), ]
ggplot(recent, aes(date, unemploy)) + geom_line()
ggplot(recent, aes(date, unemploy)) + geom_step()
ggplot(economics, aes(date, unemploy)) +
  geom_line(colour = "red")

df <- data.frame(x = 1:3, y = c(4, 1, 9))
base <- ggplot(df, aes(x, y))
base + geom_path(linewidth = 10)
base + geom_path(linewidth = 10, lineend = "round")
base + geom_path(linewidth = 10, linejoin = "mitre", lineend = "butt")

x <- seq(0.01, .99, length.out = 100)
df <- data.frame(
  x = rep(x, 2),
  y = c(qlogis(x), 2 * qlogis(x)),
  group = rep(c("a","b"),
              each = 100)
)
df
p <- ggplot(df, aes(x=x, y=y, group=group))
p + geom_line(linetype = 2)
p + geom_line(aes(linetype = group))
p + geom_line(aes(colour = x))


#################################################################################
mydata = data.frame(
  group = rep(c("A", "B","C","D"), each=200),
  value = c(rnorm(200, mean = 2), rnorm(200, 6),rnorm(200,2,4),rnorm(200,6,4))
)

p=ggdensity(mydata, x = "value", y="..density..", #或者"..count.."，默认为"..density.."
          fill = "lightgray",
          add = "mean", #或者"median",
          rug = TRUE #在图形下方添加密度线
)
p
rrtable::image2pptx(p)
# ggsave("density1.pdf",width = 10,height = 10,units = "cm")
mydata = data.frame(
  group = rep(c("A", "B"), each=100),
  group2 = rep(c("g1","g2","g1","g2"),each=50),
  value = c(rnorm(100, 2), rnorm(100, 6, 4))
)
ggboxplot(mydata, x = "group", y = "value", width = 0.6, color = "black",fill="group2",
          palette = c("#00AFBB", "#E7B800"),
          xlab = F, #不显示x轴的label
          bxp.errorbar=T,bxp.errorbar.width=0.4, #添加errorbar
          size=1, #箱型图边线的粗细
          outlier.shape=NA, #不显示outlier
          legend = "right") #图例放右边
ggsave("box3.pdf",width = 10,height = 10,units = "cm")
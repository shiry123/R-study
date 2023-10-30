###############################################################################
library(lattice)
g <- expand.grid(x = 1:10, y = 5:15, gr = 1:4)
g$z <- log((g$x^g$gr + g$y^2) * g$gr)
g$gr= as.factor(g$gr)
str(g)
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .75, y = .75, corner = c(0.5, 0.5)))
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))
bwplot(voice.part ~ height, data = singer, xlab = "Height (inches)")
dotplot(variety ~ yield | year * site, data=barley)
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")
contourplot(z ~ x * y,  data = g,
            col.regions = pals::parula(100),
            cuts = 20, region = TRUE)

wireframe(z ~ x * y|gr, data = g, 
          col.regions = pals::parula(100),
          scales = list(arrows = FALSE),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -80, y=10))


f1=function(x)60*sin(pi/2*x/0.4)
f1(0.4)
f2=function(x)60*cos(pi/2*(x-(0.4+0.15))/(2-(0.4+0.15)))
f2(0.4)
curve(f1,0,0.4, xlim = c(0,2))
curve(f2,0.4,2, add = TRUE)
ggplot(data=data.frame(x=c(0,0.4)), aes(x))+
  stat_function(fun=f1, geom = "line")

x<-1:10
y<-5*exp(2*x)+3
grp=rep(c("A","B"),each = 5)
df<-data.frame(x=x,y=y, grp=grp)
df

exp_fun<-function(x,a,b,d){
  a*exp(b*x)+d
}

ggplot(data=df,aes(x=x,y=y))+
  geom_point(aes(color=grp))+
  stat_function(fun=exp_fun,
                args=list(a=5,b=2,d=3))+
  theme_pubr(x.text.angle=45)

##############################################################################
# mtcars$am=factor(mtcars$am, levels = c("0","1"), labels = c("No","Yes"))
# mtcars$vs=factor(mtcars$vs, levels = c("0","1"), labels = c("No","Yes"))
mtcars$id=1:nrow(mtcars)
mtcars$cyl=as.factor(mtcars$cyl)
mtcars$am=as.factor(mtcars$am)
mtcars$vs=as.factor(mtcars$vs)
str(mtcars)
levels(mtcars$am)=c("No","Yes")
table(mtcars$am)
sgtheme0 <- 
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=10, face = "bold"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.text=element_text(size=10),
        legend.position="right",
        axis.line=element_line(colour="black", linewidth=0.6),
        axis.ticks=element_line(colour="black", linewidth=0.6),
        axis.text.x = element_text(angle =0, vjust =0.2))

mtcars %>% 
  ggplot(aes(x=interaction(cyl,am), y=disp, color=vs))+  # weave_factors
  geom_point(position = position_dodge(0.5))+
  scale_color_see()+
  guides(x="axis_nested")+
  sgtheme0+
  labs(x="")
mtcars %>% 
  ggplot(aes(x=interaction(cyl,am), y=disp, color=vs))+  # weave_factors
  geom_point(position = position_dodge(0.5))+
  scale_color_see()+
  guides(x="axis_nested")+
  theme_pubr()+                              # 简单方法
  theme(panel.grid.major = element_line())

<<<<<<< HEAD



=======
>>>>>>> d40ea70 (update)
####### 云雨图
library(gghalves)
iris_plot <- iris %>% 
  mutate(Species = factor(Species, levels = c('versicolor', 'setosa', 'virginica')))
head(iris_plot)
<<<<<<< HEAD
ggplot(iris_plot , 
       aes(x = Species, y = Sepal.Length, fill=Species))+
  geom_boxplot(outlier.shape =8, width =0.05, color = "black")+
  gghalves::geom_half_violin(position = position_nudge(x =0.05, y = 0),
                             adjust=1, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Species)-0.1, y = Sepal.Length, color = Species),
             position = position_jitter(width = .05), size =0.25, shape = 20) 

library(ggrain)
ggplot(iris_plot , 
       aes(x = Species, y = Sepal.Length, fill=Species))+
  geom_rain()

=======
# ggplot(iris_plot , 
#        aes(x = Species, y = Sepal.Length, fill=Species))+
#   geom_boxplot(width =0.05, color = "black")+
#   gghalves::geom_half_violin(position = position_nudge(x =0.05),
#                              adjust=1, trim=FALSE, colour=NA, side = 'r') +
#   geom_point(aes(x = as.numeric(Species)-0.1, color = Species),
#              position = position_jitter(width = .05), size =0.25, shape = 20) 
# 
# library(ggrain)
# ggplot(iris_plot , 
#        aes(x = Species, y = Sepal.Length, fill=Species))+
#   ggrain::geom_rain()
# ##
>>>>>>> d40ea70 (update)
library(see)
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violindot()+
  theme(legend.position ="right",
        axis.text.x = element_text(angle =0, vjust =0.2))

ggplot(iris_plot , aes(x = Species, y = Sepal.Length, group=Species))+
  geom_boxplot(aes(fill = Species),
               outlier.shape = NA, width =0.05,color = "black")+
  geom_violinhalf(aes(fill = Species), 
                  position = position_nudge(x =0.05))+
  geom_point2(aes(x = as.numeric(Species)-0.1, color = Species),
             position = position_jitter(width =0.05))+
  coord_flip()


#######################################################################
source("script/data_calc.R")
df.sum=mtcars %>% 
  group_by(cyl, am) %>%
  data_calc(cols =disp, func = list(mean,sd))
df.sum
pd=0.2
df.sum %>% 
  ggplot(aes(x=cyl, y=mean, color=am))+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=0,
                position = position_dodge(pd))+
  geom_line(aes(group=am),
            position = position_dodge(pd))+
  geom_point2(size=2,
              position = position_dodge(pd))

mtcars %>% 
  ggplot(aes(x=cyl,y=disp, color=am))+
  geom_point2(stat="summary",fun="mean", size=2,
              position = position_dodge(pd))+
  geom_line(aes(group=am),
            stat="summary",fun="mean",
            position = position_dodge(pd))+
  stat_summary(fun.data = "mean_sd", geom="errorbar", width=0,
               position = position_dodge(pd))+
  facet_wrap(~vs, scales = "free")

############################
library(ggprism)
# library(ggbeeswarm)
data("wings")
source("script/data_plot.R")
p=data_plot(wings, x=measure,y=percent.change,
            trace =genotype, facet= sex,
            add="mean",pd=0.3,
            type="point")+
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3)
p
p+stat_compare_means(aes(label = after_stat(p.signif)), method = "t.test",
                     label.y = 12)

wings_pvals <- wings %>%
  group_by(sex, measure) %>%
  t_test(
    percent.change ~ genotype, 
    p.adjust.method = "BH", 
    var.equal = TRUE, 
    ref.group = "Tps1MIC/+"
  ) %>%
  add_x_position(x = "measure") %>%   # dodge must match points
  mutate(label = c("***", "*", "P = 0.26", "***", "***", "P = 0.65"))
wings_pvals

p+stat_pvalue_manual(wings_pvals, y = 10, xmin = "xmin", xmax = "xmax", 
               tip.length = 0, fontface = "italic", 
               lineend = "round", bracket.size = 0.5) 

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
stat.test <- compare_means(
  len ~ dose, data = ToothGrowth, group.by = "supp",
  method = "t.test", ref.group = "0.5"
)
stat.test
# Plot
bp <- ggbarplot(ToothGrowth, x = "supp", y = "len",fill = "dose", 
                palette = "jco",
                add = "mean_sd", 
                add.params = list(color = "red"),
                position = position_dodge(0.8))
bp
bp + stat_pvalue_manual(
  stat.test, x = "supp", y.position = 33, label = "p.signif",
  position = position_dodge(0.8),tip.length=0
)

################################
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
my_comparisons=get_comparisons(ToothGrowth,"dose")
my_comparisons
ggboxplot(ToothGrowth, x = "dose", y = "len",
          fill = "dose", palette = "npg",  
          ggtheme=theme_modern(axis.text.angle=45))+
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons, 
                     label.y = c(29, 35, 40), method="t.test")+
  stat_compare_means(label.y = 45, method = "anova") 

######################################################  自定义函数
mtcars$id=1:nrow(mtcars)
mtcars$id=as.factor(mtcars$id)
mtcars$cyl=as.factor(mtcars$cyl)
mtcars$am=as.factor(mtcars$am)
mtcars$vs=as.factor(mtcars$vs)
str(mtcars)
source("script/data_plot.R")
data_plot(mtcars, x=mpg,y=disp,
          type="line")  
data_plot(mtcars, x=mpg,y=disp,
          trace = am,
          type="line") 

data_plot(mtcars, x=cyl,y=disp,
          add="mean_sd",
          type="line")

data_plot(mtcars, x=cyl,y=disp,
          trace=vs,
          add="mean_sd",
          type="line", pd=0.1)
##
source("script/data_plot.R")
data_plot(mtcars, x=cyl, y=disp,
          type="box", 
          legend.position="top",
          axis.text.angle=45)
data_plot(mtcars, x=cyl, y=disp,
           type="box", 
           add="point",
           add.params = list(position=position_jitter(0.1)),
           legend.position="top",
           axis.text.angle=45)
data_plot(mtcars,
           x=cyl, y=disp, 
           trace=am,pd=0.8,
           facet= vs,
           type="box")
data_plot(mtcars, x=cyl, y=disp, 
           trace=am, pd=0.8,
           add="point",
           add.params = list(position=position_jitterdodge(jitter.width = 0.1,
                                                          dodge.width = 0.8)),
           type="box")
data_plot(mtcars, x=cyl, y=disp, 
          trace=am,pd=0.9,
          facet= vs,
          type="box", 
          labeller= labeller(vs=c("0"="VS:NO","1"="VS:Yes")))
######################################################

source("script/data_plot.R")
data_plot(mtcars, x=mpg,y=disp,
          type="point")
data_plot(mtcars, x=mpg,y=disp,trace=cyl,
          type="point")
data_plot(mtcars, x=mpg,y=disp,trace=am,
          type="point",
          add="reg.line")
data_plot(mtcars, x=mpg,y=disp,trace=am,
           type="point",
           add="reg.line", 
           add.params = list(reg.equ=TRUE, se=TRUE,
                             label.x.npc="center",
                             label.y.npc="top"))
source("script/data_plot.R")
data_plot(mtcars, x=cyl,y=disp,
           add="mean_sd",
           add.params = list(position = position_nudge(0.1)),
           type="point")
data_plot(mtcars, 
          x=cyl,y=disp,
          trace=am, pd=0.6,
          add="mean_sd",
          add.params = list(position =position_dodge(0.8)),
          type="point")

ggstripchart(mtcars, 
             x="cyl",y="disp",
             color="am",
             add = "mean_sd", 
             position = position_dodge(0.6))+
  theme(panel.grid.major = element_line())



data_plot(mtcars, x=cyl,y=disp,trace=am, facet=vs, 
          add="mean",
          type="point",pd=0.5)
data_plot(mtcars, x=cyl,y=disp,
           add="mean",
           type="beeswarm")
data_plot(mtcars, x=cyl,y=disp,
          trace = vs,pd=0.5,
          add="mean",
          type="beeswarm")
####################################
source("script/data_plot.R")
data_plot(mtcars, x=cyl,y=disp, 

          trace=am, facet=vs,
          type="bar",add="mean_sd", pd=0.9)

data_plot(mtcars, x=cyl,y=disp, 
           type="bar",add="mean_sd")


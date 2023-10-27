# library(RcmdrPlugin.DoE)
############   DOE设计
library(FrF2)
x <- FrF2::FrF2(nfactors=4,resolution=4, 
                factor.names=paste("x",1:4,sep=""), ncenter=3, randomize=T)
x
FrF2::aliasprint(x)
###PB筛选 DSD确定筛选
x <- daewr::DefScreen(m=8, c=0, center=0, randomize=FALSE)
x
write_xlsx(x, path="outfiles/frac_design_1.xlsx")

#######################################
# Import data
yield_data <- read_csv("datas/yield_data.csv")
# Linear model
yield_lm <- lm(Yield ~ (.)^2, data = yield_data)  #2次交互项
summary(yield_lm)
FrF2::DanielPlot(yield_lm, half = TRUE)
# Pareto chart  beta值排序 y=beta*x+Error
source("script/Pareto_plot.R")
Pareto_plot(yield_lm)
# Subsequent analysis by ANOVA
yield_lm2 <- lm(Yield ~ Catalyst*Temperature*Concentration, yield_data)
summary(yield_lm2)
#方差分析
anova(yield_lm2)
Anova(yield_lm2, type="II")

#拟合值和实际值  45对角线
yield_data %>% modelr::add_predictions(yield_lm2) %>% 
ggplot(aes(x=pred, y=Yield))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red")+
  geom_smooth(method = 'lm',formula= 'y ~ x',se=FALSE, linetype="dashed",color="black")+
  geom_text(aes(label=paste0("R2=", round(summary(yield_lm2)[["r.squared"]],4))),
            x=60, y=90)+
  theme_modern()

##诊断图  残差图
library(ggfortify)
autoplot(yield_lm2, which = c(1:3,6))+theme_bw()
#因子图
FrF2::IAPlot(yield_lm2)

#  p值排序
broom::tidy(yield_lm2) %>%
  mutate(logWorth=-log10(p.value)) %>% 
  filter(term !="(Intercept)") %>% arrange(desc(logWorth)) %>% 
  qplot(data=., x=reorder(term,logWorth), y=logWorth, geom="col")+
          coord_flip()+theme_modern()+
  geom_hline(aes(yintercept=-log10(0.05)),color="Red",linetype="dashed")
  

############混料设计
library(mixexp)
# Make a simplex-lattice design
simplex_lattice_desing <- SLD(
  fac = 3, # Number of components (factors)
  lev = 2  # Number of levels besides 0
)
# Display the design
simplex_lattice_desing
DesignPoints(simplex_lattice_desing)
# write.csv(
#   simplex_lattice_desing, file = "datas/simplex_lattice_desing.csv", 
#   row.names = FALSE
# )
simplex_centroid_design <- SCD(
  fac = 3  # Number of components (factors)
)
# Export the design
# write.csv(
#   simplex_centroid_design, file = "datas/simplex_centroid_design.csv", 
#   row.names = FALSE
# )
# Display the design
simplex_centroid_design
DesignPoints(simplex_centroid_design)

simplex_restrictions <- Xvert(
  nfac = 3,                       # Number of components (maximum 12)
  uc   = c(0.612, 0.765, 0.109),  # Upper constrains
  lc   = c(0.153, 0.306, 0.053),  # Lower constrains
  plot = FALSE,                   # Automatic plot when TRUE and nfac = 3
  ndm  = 1                        # Order of centroids included
)

# Display the design
simplex_restrictions
DesignPoints(simplex_restrictions)


#####################################################333
data_sausage <- import("datas/data_sausage.xlsx")
str(data_sausage)

hardness_model_lm <- MASS::stepAIC(lm(
  Hardness ~ -1 + MCC + RS + OF + MCC:RS + RS:OF + MCC:OF + MCC:RS:OF, # Model
  data = data_sausage
))
summary(hardness_model_lm)
Anova(hardness_model_lm, type = "II")
#Lack-of-fit test
source("script/pureErrorAnova.R")
pureErrorAnova(hardness_model_lm)

capture.output(
  summary(hardness_model_lm),
  file = "outfiles/sum_hardness_model.txt"
)

ModelPlot(
  model=hardness_model_lm,                                  # Our model
  dimensions = list(x1 = "MCC", x2 = "RS", x3 = "OF"),  # Variable names
  contour = TRUE,                                       # Add contour lines
  fill = TRUE,                                          # Add color
  axislabs = c("MCC", "RS", "OF"),                      # Axis labels
  cornerlabs = c("MCC", "RS", "OF")                     # Corner labels
)

#####################################
rm(list=ls())
library(lattice)
x.grid <- expand.grid(x1= seq(0,1,length=10),
                      x2= seq(0,1,length=10)  )
x.grid$y <- 90  - 8*x.grid$x1 - 40*x.grid$x1^2 - 8*x.grid$x2 - 80*x.grid$x2^2 + 
            80*x.grid$x1*x.grid$x2 + rnorm(nrow(x.grid),0,2)
x.grid
wireframe(y~x1*x2, data=x.grid,
          drape=TRUE, strip=TRUE,  pretty=TRUE, 
          # at=do.breaks(c(100,1200),100),
          col.regions = pals::parula(100),
          scales = list(arrows = FALSE),
          screen = list(z =80, x = -60, y=10)
          )

#############相应曲面
library(rsm)
bb_design_1 <- bbd(
  k  = 4,            # Number of factors,
  n0 = 3,            # Number of center points,
  block = FALSE,     # Consider blocks or not in the design 
  randomize = FALSE  # Randomize or not the order experiments
)
bb_design_1

cc_design_1 <- ccd(
  basis = 3,         # Number of factors
  n0    = 3,         # Number of central points
  randomize = FALSE # Not randomized
)
cc_design_1

#################
#  CodedLevel = （Level - CenterPoint） / StepSize
(bb_design_2 <- bbd(
  k  = 4,            # Four factors 
  n0 = 3,            # Three central points 
  block = FALSE,     # No blocks 
  randomize = FALSE, # Not randomized
  coding = list(
    x1 ~ (Temperature - 50) / 25,   #-1,0=50,1  
    x2 ~ (Power - 210)/ 30,
    x3 ~ (time - 30) / 15,
    x4 ~ (Ethanol - 80) / 20
  )
))

#########################################
df=bruceR::import()
str(df)
library(rsm)
df=coded.data(df, x1 ~ (Temp - 150)/50, 
                  x2 ~ (Press - 150)/25
              )
code2val(data.frame(x1=3.14, x2=-1), codings = codings(df))

m1 <- lm(Yield ~ SO(x1,x2), data = df)
summary(m1)
m2=MASS::stepAIC(lm(Yield ~ .^2+I(x1^2)+I(x2^2), data=df))  # (x1+x2)^2  2阶展开
summary(m2)
persp(m2,  ~ x1 + x2 , contours = "colors")



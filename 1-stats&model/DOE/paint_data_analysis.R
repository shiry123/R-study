# Analysis of yield data (fractional design with 8 factors and resolution IV)
library(FrF2)
# Import data
getwd()
set.wd(ask = TRUE)
paint_data <- read_csv("datas/paint_data.csv")
paint_data
# Linear model
paint_lm <- lm(Brightness ~ (.)^2, data = paint_data)
summary(paint_lm)
# Daniel plot
FrF2::DanielPlot(paint_lm)
source("script/Pareto_plot.R")
Pareto_plot(paint_lm)

# Subsequent analysis by ANOVA
paint_lm2 <- lm(Brightness ~ A*B*G, data = paint_data)
anova(paint_lm2)

# Cube plot
cubePlot(paint_lm2, eff1 = "A", eff2 = "B", eff3 = "G")


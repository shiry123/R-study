# Analysis of yield data (fractional design with 5 factors and resolution V)
library(FrF2)
# Import data
yield_data <- read_csv("datas/yield_data.csv")
yield_data
str(yield_data)
# Linear model
yield_lm <- lm(Yield ~ (.)^2, data = yield_data)
summary(yield_lm)
FrF2::DanielPlot(yield_lm)

# Pareto chart
yield_coeffs <- coefficients(yield_lm)[-1] # We discard the intercept
yield_effects <- data.frame(
  Effect = names(yield_coeffs),
  Value  = unname(yield_coeffs),
  AbsoluteValue = abs(unname(yield_coeffs)),
  Sign   = as.character(unname(sign(yield_coeffs)))
)
ggplot(yield_effects, aes(AbsoluteValue, reorder(Effect, -AbsoluteValue, abs))) +
  geom_col(aes(fill = Sign)) +
  xlab("Magnitude of effect") +
  ylab("Effect") +
  theme_minimal()
# ggsave("graphs/pareto_chart_yield_data.jpeg")

###################################
source("script/Pareto_plot.R")
Pareto_plot(yield_lm)

yield_lm2 <- MASS::stepAIC(lm(Yield ~ Catalyst*Temperature*Concentration, yield_data))
summary(yield_lm2)
anova(yield_lm2)
source("script/pureErrorAnova.R")
pureErrorAnova(yield_lm2)
FrF2::IAPlot(yield_lm2)


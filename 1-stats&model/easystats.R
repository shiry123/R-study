library(easystats)
easystats::easystats_update("core")
pkg_unload(easystats)
library(tidyfst)
pkg_unload(pkg_names=c("modelbased", "correlation", "effectsize", "parameters"))

df=data_read("rawdata/CN018/CN018-RLD DDU.xlsx",sheet="RLD-DDU")
dat=data_extract(df,1:6)
dat
dat %>% data_to_long(4:6)
data_write(dat, "outfiles/ddu.xlsx")
ggpubr::mean_sd(data$value)

data <- iris %>%
  group_by(Species) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
data
data %>%
  ggplot(aes(x = name,y = value,color = Species,group = Species,fill = Species)) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()

model <- lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)
get_parameters(model)
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point() + # This adds the points
  geom_smooth(method = "lm") # This adds a regression line


library(rstanarm)
model2 <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)
posteriors <- describe_posterior(model2)
posteriors 

posteriors <- get_parameters(model2)
head(posteriors)

mean(posteriors$Petal.Length)
median(posteriors$Petal.Length)
map_estimate(posteriors$Petal.Length)
hdi(posteriors$Petal.Length, ci = 0.89)


ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density()
ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange") +
  # The mean in blue
  geom_vline(xintercept = mean(posteriors$Petal.Length), color = "blue", linewidth = 1) +
  # The median in red
  geom_vline(xintercept = median(posteriors$Petal.Length), color = "red", linewidth = 1) +
  # The MAP in purple
  geom_vline(xintercept = map_estimate(posteriors$Petal.Length), color = "purple", linewidth = 1)
##################################
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
summary(m)
m %>% 
  model_parameters() %>% 
  plot()
check <- check_normality(m)
plot(check, type = "qq")
check_model(m)

newdf=tibble(wt=2,cyl=1,gear=4,disp=100) 
modelr::add_predictions(newdf, m)
estimate_expectation(m,data=newdf)
newdf %>% 
  mutate(pred= estimate_expectation(m,data=.)$Predicted)

model <- lmer(
  Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species),
  data = iris
)
summary(model)
select_parameters(model)

model %>% 
  model_parameters() %>% 
  plot()


model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
estimate_means(model)
estimate_means(model,fixed = "Sepal.Width") %>% 
  plot()

data <- iris
data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
data
model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
estimate_means(model)
estimate_means(model, at = "Sepal.Width", length = 3)
estimate_contrasts(model)


iris %>% 
  group_by( Species) %>% 
  describe_distribution()
standardise(iris)

###贝叶斯
library(bayestestR)
library(rstanarm)
model = stan_glm(mpg ~ wt + cyl + gear + am + hp, data = mtcars)
equivalence_test(model)
plot(equivalence_test(model))

library(rstanarm)
model <- stan_glm(Petal.Width ~ Petal.Length * Sepal.Width, data = iris)
result_pd <- p_direction(model)
plot(result_pd)

model <- stan_glm(mpg ~ wt + gear, data = mtcars)
equivalence_test(model)
plot(hdi(model,ci = .89))

describe_posterior(
  rnorm(10000),
  centrality = "median",
  test = c("p_direction", "p_significance")
)

posterior <- distribution_gamma(10000, 1.5)  # Generate a skewed distribution
centrality <- point_estimate(posterior)  # Get indices of centrality
plot(centrality)


# CCD适用于多因素多水平实验，有连续变量存在；BBD适用于因素水平较少（因素一般少于5个，水平为3个）。
# CCD比BB实验能更好的拟合相应曲面。因为CCD的设计过程中，有很多点会超出原定的水平，
# 所以在实验室条件下，最好做CCD。如果超出原定的水平，会发生危险，或者不容易达到，那就做BB。

library(rsm)
getwd()
set.wd(ask = TRUE)
yield_data <- read_csv("datas/yield_data2.csv")
str(yield_data)

# Coded units
yield_data <- coded.data(
  yield_data,                 # Experimental data
  formulas = list(            # List of coding formulas for each factor
    x1 ~ (A - 210)/30, 
    x2 ~ (B - 80)/10,
    x3 ~ (C - 30)/15,
    x4 ~ (D - 30)/10
  ))
yield_data

# Adjust a second order model
yield_model <- rsm(Yield ~ SO(x1, x2, x3, x4), data = yield_data)
summary(yield_model)
# anova(yield_model)
# Anova(yield_model, type = "III")

# FO, TWI, PQ, or SO 
# “first-order,”, “two-way interaction,” “pure quadratic,” and “second-order” 

capture.output(
  summary(yield_model),                       # Object to be exported
  file = "outfiles/model_summary.txt"         # File name 
)

# Optimal point in coded units
opt_point <- summary(yield_model)$canonical$xs
opt_point
x.solution=data.frame(rbind(opt_point))
x.solution
best_response <- predict(
  yield_model,             # Our model
  x.solution             # Data frame with points to be predicted 
)
names(best_response) = "Best yield" # A nice name to our best point
best_response

# Optimal point in original units
op_point_ru <- code2val(
  opt_point,                     # Optimal point in coded units
  codings = codings(yield_data)  # Formulas to convert to factor units
)
op_point_ru


############ 函数 
opt_f=function(model){
  opt_point = summary(model)$canonical$xs
  op_point_ru = code2val(opt_point,codings = codings(model))
  x.solution = data.frame(rbind(opt_point))
  best_response = predict(model, x.solution)
  names(best_response) = "Best yield"
  return(list(op_point_ru, best_response))
}
opt_f(yield_model)

# Contour plots
par(mfrow = c(2,3))       # 2 x 3 pictures on one plot
contour(
  yield_model,            
  ~ x1 + x2 + x3 + x4,    
  image = TRUE,               
  # at = summary(yield_model)$canonical$xs
)

# 3D plots
par(mfrow = c(2,3))       # 2 x 3 pictures on one plot
persp(
  yield_model,            # Our model 
  ~ x1 + x2 + x3 + x4,    # A formula to obtain the 6 possible graphs
  col = topo.colors(100), # Color palette
  contours = "colors"     # Include contours with the same color palette
) 


########################################################
library(rsm)
ChemReact
CR <- coded.data(ChemReact, x1~(Time-85)/5, x2~(Temp-175)/5)
CR
### 1st-order model, using only the first block
CR.rs1 <- rsm(Yield ~ FO(x1,x2), data=CR) 
summary(CR.rs1)
CR.rs1 <- update(CR.rs1, . ~ . + TWI(x1, x2))


### 2nd-order model, using both blocks
CR.rs2 <- rsm (Yield ~ Block + SO(x1,x2), data=CR) 
summary(CR.rs2)
canonical(CR.rs2)
summary(CR.rs2)$canonical$xs

steepest(CR.rs2, dist = c(0, 0.5, 1, 2))
canonical.path(CR.rs2, dist = seq(-5, 5, by = 0.5))

persp(CR.rs2, ~ x1+x2 , contours = "colors", col=pals::parula(100)) 

contour(CR.rs2, ~ x1 + x2,image = TRUE)

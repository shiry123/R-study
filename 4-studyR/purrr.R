############################################################################
df <- tibble(id = 1:6, w = runif(6), x = runif(6), y = runif(6), z = runif(6),
             grp=rep(c("a","b"),3))
df
df %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(w:z)),
    mean= mean(c_across(w:z)),
    sd = sd(c_across(w:z))
  )
df %>% mutate(sum=rowSums(across(w:z)),
              mean=rowMeans(across(w:z)),
              c=pmap_dbl(list(x,y,z),~mean(c(...))),
              xy=map2_dbl(x,y, ~sd(c(.x, .y))),
              w10=map_dbl(w, ~.x*10)
)

df=tibble(
  a=c(rep(0,2),1,NA,NA),
  b=c(NA,rep(1,3),NA),
  c=c(rep(0,2),NA,NA,NA)
)
df
myfun=function(x){
  if(1 %in% x) 1
  else if (all(is.na(x))) { NA }
  else 0
}
df %>% 
  mutate(ack=pmap_dbl(., ~myfun(c(...))))
df = iris[, 1:4]
map_dbl(df, ~mean(.x, na.rm = TRUE))
map_dbl(df, mean, na.rm = TRUE) 

height = c(1.58, 1.76, 1.64)
weight = c(52, 73, 68)
cal_BMI = function(h, w)  w/h^2     # 定义计算BMI的函数
cal_BMI = function(h, w) {
  w/h^2    # 定义计算BMI的函数
}
map2_dbl(height, weight, cal_BMI)
map2_dbl(height, weight, ~ .y / .x^2) 
df = tibble(height = height, weight = weight)
df
df %>% 
  mutate(bmi = map2_dbl(height, weight, cal_BMI)) 
df %>% 
  mutate(bmi = map2_dbl(height, weight, ~ .y / .x^2))
df %>% 
  mutate(bmi = map2_dbl(height, weight, \(x,y) y/x^2))

df <- tibble(
  n = c(1,3,5),
  mean = c(5,10,-3),
  sd = c(1,5,10)
)
df
pmap(df, rnorm)
names(df) = c("n", "m", "s")
df
pmap(df, ~ rnorm(..1, ..2, ..3))    # 结果同上(略), 或者简写为
pmap(df, ~ rnorm(...))

# pmap_*() 是另一种行化操作数据框的办法
df = crossing(a = 0.3, b = 1:3, c = 1:3, d=2:4)
df
df %>% 
  mutate(r4 = pmap_dbl(., ~ ..1 / (..2 + ..3 + ..4)),
         r3 = pmap_dbl(., ~ ..1 / (..2 + ..3)),
         m = pmap_dbl(., ~ mean(c(...))), 
         id = pmap_chr(., str_c, sep = "-"))


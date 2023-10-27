# library(installr)
# updateR()
library(tidyverse)
# tidyverse与其他包的冲突
tidyverse_conflicts()
# 列出所有tidyverse的依赖包
tidyverse_deps()
#获取tidyverse的logo
tidyverse_logo()
# 列出所有tidyverse包
tidyverse_packages()
# 更新tidyverse包
tidyverse_update()
install.packages("dplyr")

######################################################
df = tibble(x1 = 1:6, x2 = 5:10, x3 = 9:14, 
             x=c(4,5,7,NA,8,10), y=c(5,4,3,NA,6,7), z=c(5,7,NA,8,5,4),
             grp=rep(c("A","B"), each=3))
df
df %>% group_by(grp) %>% 
  get_summary_stats(x:z, type="mean_sd")

df
source("script/data_calc.R")
data_calc(df, cols=x:z, func = mean, row_calc = TRUE)
df %>% 
  mutate(mu=pmap_dbl(pick(x:z), \(...) mean(c(...), na.rm=TRUE)))
df %>% 
  rowwise() %>% 
  mutate(mu=mean(c_across(x:z), na.rm=TRUE))


df %>% group_by(grp) %>% 
  data_calc(cols=x:z, func=mean)

data_calc(df, cols=x:z, func=mean, by=grp)

df %>% 
  group_by(grp) %>% 
  summarise(across(x:z, \(x) mean(x, na.rm = TRUE)) )


data_calc(df, cols=x:z, func=list(mean,sd), by=grp)

df %>% 
  group_by(grp) %>% 
  summarise(across(x:z, list(f1=\(x) mean(x, na.rm = TRUE),
                             f2=\(x) sd(x, na.rm = TRUE))) )

df %>% 
  mutate(mu=pmap_dbl(pick(x:z), ~mean(c(...), na.rm=TRUE)))
df %>% 
  mutate(mu=pmap_dbl(list(x,y,z), \(...) mean(c(...), na.rm=TRUE)))


df %>%
  mutate(
    across(x, list(f1 = ~ .x * 2, f2 = ~ .x^2))
  )

df %>%
  mutate(x.f1=x*2,  x.f2=x.f1^2)

df %>%
  mutate(
    across(x1:x3, list(f1 = \(x) x*2, f2 =\(x) x^2))
  )

df %>% 
  group_by(grp) %>% 
  summarise(
    n=n(),
    across(x:z, list(mean= ~mean(.x, na.rm=TRUE),
                     sd= ~sd(.x, na.rm=TRUE)), .names = "{.col}_{.fn}")
  )

dat=df %>% rowid_to_column("id") 
dat
dat.r=dat %>% 
  pivot_longer(x:z, values_to = "values") %>% 
  group_by(id) %>% 
  summarise(m=mean(values, na.rm=TRUE),
            s=sd(values, na.rm=TRUE))
dat.r
dat %>% left_join(dat.r, by="id") %>% select(-id)


#########################################################################
df = tibble(x1 = 1:6, x2 = 5:10, x3 = 9:14, 
            x=c(4,5,7,NA,8,10), y=c(5,4,3,NA,6,7), z=c(5,7,NA,8,5,4),
            grp=rep(c("A","B"), each=3), gp=rep(c("a","b","c"), each=2))
df
func=c("mean","sd","var","min","max","sum","median")
funcs=map(func, ~get(.x))
funcs
funcs[[1]](df$x, na.rm=TRUE)
fun.list=map(func, ~eval(parse(text=str_glue("\\(x){.x}(x,na.rm=TRUE)"))))
fun.list
fun.list[[1]](df$x)


f=function(data,cols,fun_names,by=NULL,row_calc=FALSE,...){
  var_args = list(...)
  na.rm = if(!is.null(var_args[["na.rm"]])) var_args[["na.rm"]] else TRUE
  
  funcs=map(fun_names, \(fns) eval(parse(text=str_glue("\\(x){fns}(x,na.rm={na.rm})"))))
  names(funcs)=fun_names
  
  if(isTRUE(row.calc)){
    dat=data %>% rowid_to_column(".id") 
    dat.r=dat %>% 
      pivot_longer({{cols}}, values_to = ".values") %>% 
      group_by(.id) %>% 
      summarise(
        across(.values, .fns=funcs, .names = "{.fn}"), .groups = "drop"
      ) 
    .results=dat %>% left_join(dat.r, by=".id") %>% select(-.id)
  }else{
    .results=data %>%
      group_by(across({{by}})) %>%
      summarise(
        n=n(),
        across({{cols}}, .fns=funcs, .names = "{.col}/{.fn}"), .groups = "drop"
      ) %>% 
      pivot_longer(contains("/"), names_to = c("variable",".value"), names_sep = "/") %>%
      relocate(n, .after=variable)
  }
  
  return(.results)
}
f(df,cols=x:z, fun_names="sum", by=grp, row_calc = TRUE)
f(df,cols=x:z, fun_names="sum", by=grp)


########################################################################
calSummaryStats=function(x,...){
  var_args = list(...)
  probs = if(!is.null(var_args[["probs"]])) var_args[["probs"]] else seq(0, 1, 0.2)
  type = if(!is.null(var_args[["type"]])) var_args[["type"]] else 8
  na.rm = if(!is.null(var_args[["na.rm"]])) var_args[["na.rm"]] else TRUE
  res = rep(NA, 1 + length(probs))
  names(res) = c("sd", paste0("Q_", round(probs, 2)))
  res[1] = sd(x, na.rm=na.rm)
  res[2: length(res)] = quantile(x, probs=probs, na.rm=na.rm, type=type)
  res
}
x = c(1: 3, NA, 7: 10)
x
calSummaryStats(x)


DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],A=4:5)
l = list(DT1,DT2)
l
rbindlist(l, use.names=TRUE)

f=function(data, cols,  func=NULL, by=NULL){
  data %>%
    group_by(across({{by}})) %>%
  summarise(
      n=n(),
      across({{cols}}, .fns=func, .names = "{.col}/{.fn}")
    ) %>% 
    pivot_longer(contains("/"), names_to = c(".variable",".value"), names_sep = "/") %>% 
    relocate(n, .after=.variable)
}
f(df, x1:x3, func =list(means=\(x) mean(x,na.rm=T),sd,sum), by=grp)

###############################################################################
f=function(data, cols,  func=NULL, by=NULL){
  if(missing(func)){
    func_names=c("mean","sd")
  }else{
    if(!is.list(func)){
      func_names=deparse(substitute(func))
    }else{
      func_names=sapply(substitute(func), deparse) %>% .[-1]
    }
  }
    # funcs=map(func_names, ~eval(parse(text=str_glue("~{.x}(.x,na.rm=TRUE)"))))
    funcs=map(func_names, ~get(.x))
    names(funcs)=func_names

  data %>%
    group_by(across({{by}})) %>%
    summarise(
      n=n(),
      across({{cols}}, .fns=funcs, .names = "{.col}/{.fn}")
    ) %>% 
    pivot_longer(contains("/"), names_to = c("variable",".value"), names_sep = "/") %>%
    relocate(n, .after=variable)
}

f(df, x:z,by=grp)
f(df, x:z,mean, by=grp)
f(df, x:z, list(mean,sd,var), by=grp)

get_func=function(func){
  if(missing(func)){
    func_names=c("mean","sd")
  }else{
    if(!is.list(func)){
      func_names=deparse(substitute(func))
    }else{
      func_names=sapply(substitute(func), deparse) %>% .[-1]
    }
  }
  funcs=map(func_names, ~eval(parse(text=str_glue("~{.x}(.x,na.rm=TRUE)"))))
  names(funcs)=func_names
  return(funcs)
}
get_func(mean)
get_func(list(mean,sd))

fns=get_func(list(mean,sd))
df %>% 
  group_by(grp) %>% 
  summarise(
    n=n(),
    across(c(x:z), fns, .names = "{.col}_{.fn}")
  )



#######################################################
iris = tibble(iris)
billboard %>% select(num_range("wk", 10:15))
iris %>% 
  dplyr::filter(Species == "virginica")
iris %>% 
  arrange(desc(Sepal.Length)) # 降序
iris %>% 
  mutate(Sepal.Length = Sepal.Length * 10)
iris %>% 
  dplyr::filter(Species == "virginica") %>% 
  summarize(medianSL = median(Sepal.Length),
            maxSL = max(Sepal.Length))
iris %>% 
  group_by(Species) %>% 
  summarize(n=n(),
            average=mean(Sepal.Length),
            SD=sd(Sepal.Length),
            medianSL = median(Sepal.Length),
            maxSL = max(Sepal.Length))

ggplot(iris, aes(x = Petal.Length,y = Petal.Width)) + 
  geom_point() + 
  facet_wrap(~Species)

################################################################3
 
files = list.files("datas/read_datas", pattern = "xlsx", full.names = TRUE)
df = map_dfr(files, readxl::read_xlsx)    # 批量读取+按行堆叠合并
df
#若嵌套只需设置参数 recursive = TRUR；


df=mtcars %>% rownames_to_column("type")
df
df %>% group_by(cyl) %>% 
  summarise(
    avg=mean(mpg)
  )

dat=df %>% group_by(cyl) %>% 
  summarise(
    data=list(pick(mpg, wt))
  )
dat
dat$data[1]

df.nest=df %>% 
  select(mpg, cyl, wt) %>% 
  group_nest(cyl)      # 嵌套数据框(列表列)
df.nest
df.nest=
  df.nest %>% 
  mutate(model = map(data, ~ lm(mpg ~ wt, data = .x)),   # 分组建模
         pred = map(model, predict),                   # 计算每个样本的预测值
         tidy.m=map(model, broom::tidy)
         )                     
df.nest
df.nest$model %>% 
  map(summary) %>% 
  map_dbl("r.squared")   # 用列表的元素名做 map 相当于提取该元素
df.nest$model %>% 
  map(broom::tidy)   # 模型参数信息
df.nest$model %>% 
  map(broom::glance)   # 模型评估信息
df.nest %>% 
  unnest(c(data, pred))   # 解除嵌套

height = c(1.58, 1.76, 1.64)
weight = c(52, 73, 68)
cal_BMI = function(h, w) w/h^2     # 定义计算BMI的函数

map2_dbl(height, weight, cal_BMI)
map2_dbl(height, weight, ~ .y / .x^2)     # 结果同上(略)
df = tibble(height = height, weight = weight)
df
df %>% 
  mutate(bmi = map2_dbl(height, weight, cal_BMI)) # cal_BMI 函数 map(x,f)
df %>% 
  mutate(bmi = map2_dbl(height, weight, ~ .y / .x^2))
# pmap
df = tibble(
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

#dplyr 包中提供了 rowwise() 将数据框“行化”，可以实现按行操作数据（速度较慢）。
df = crossing(x = 0.3, y = 1:3, z = 1:3)
df
df %>% 
  mutate(r = pmap_dbl(., ~ ..1 / (..2 + ..3)), 
         m.mean = pmap_dbl(., ~ mean(c(...))), 
         a = pmap_chr(., str_c, sep = "-"))
df = mpg %>%
  group_nest(manufacturer) 
df

setwd("D:/Mywd")
df %>%
  pwalk(~ readr::write_csv(..2, paste0("datas/", ..1, ".csv")))

files = list.files("datas/", pattern = "xlsx", full.names = TRUE)
files
df = map(files, readxl::read_xlsx) %>% 
  reduce(full_join, by = "人名")                  # 读入并依次做全连接

df
# 函数 reduce() 可先对序列前两个元素应用函数，再对结果与第3个元素应用函数，
# 再对结果与第4个元素应用函数，……直到所有的元都被“reduced”。

accumulate(1:10, sum) 
cumsum(1:10)


vars = str_c("x", 2:5) 
vars
accumulate(vars, ~ str_c(.x, .y, sep = " + "),  .init = "y ~ x1")
mpg
mpg %>%
  group_by(manufacturer) %>%
  dplyr::filter(every(cty, ~ .x > 15) & some(cty, ~ .x > 25)) %>% 
  ungroup() 


### rowwise() across()
d = tibble::tribble(
  ~water, ~food,
  10.0,   10.0,
  12.1,   10.3,
  13.5,   19.1,
  17.4,   16.0,
  25.8,   15.6,
  27.4,   19.8
)
d

d %>%
  mutate(100 * across(.names = "{.col}/%") / rowSums(across(water:food))
         )

rowPercent = function(df) {
  df / rowSums(df) * 100
}
d %>%
  mutate(rowPercent(across(.names = "{.col}/%")))

d %>% 
  rowwise() %>% 
  mutate(
    100*across(everything(), .names = "{.col}/%", ~ .x / sum(c_across(water:food)))
  )


df = tibble(x1 = 1:4, x2 = 5:8, x3 = 9:12, x=4:7, y=5:8, 
             grp=rep(c("A","B"), each=2))
df
pow=function(x,n){
  x^n
}
df %>% summarise_vars(x1:x3, 
                      list(mean, sd, var), 
                      by=grp
                      )
df %>% summarise_vars(x1:x3, function(x) pow(x, 0.4), 
                      by=grp
                      )
df %>% summarise_vars(x1:x3, \(x) pow(x, 0.4), 
                      by=grp
)  #不支持purrr风格函数


df %>% summarise_vars(x1:x3, list(mean, sd, var), na.rm = TRUE,
                      by=grp)

########################
df = tibble(
  id = 1:10,
  sex = c("m", "m", "m", "f", "f", "f", "m", "f", "f", "m"),
  lds1.x = c(NA, 1, 0, 1, NA, 0, 0, NA, 0, 1),
  lds1.y = c(1, NA, 1, 1, 0, NA, 0, 3, NA, 1),
  lds2.x = c(2, 1, NA, 0, 0, NA, 1, NA, NA, 1),
  lds2.y = c(0, 2, 2, NA, NA, 0, 0, 3, 0, NA)
)
df
df %>%
  mutate(
    map2_dfr(
      .x = across(ends_with(".x"), .names = '{sub(".x","",.col)}'),
      .y = across(ends_with(".y")),
      .f = coalesce               # Vectors coalesce
    )
  )

df = tibble(iris)
df
df %>%
  mutate(total = rowSums(across(where(is.numeric))))  
df %>%
  mutate(total = rowMeans(across(where(is.numeric)))) 

df %>%
  rowwise(Species) %>%    # rowwise() 函数为数据框创建 按行方式（rowwise）
  summarise(total = sum(c_across(where(is.numeric))),   # c_across 只能选择列
            .groups = "drop")
df %>%
  mutate(ranks = min_rank(desc(Sepal.Length))) %>%
  arrange(ranks)
df %>%
  select(starts_with("m"))
df %>%
  select(ends_with("e"))
df %>%
  select(contains("a"))
df %>%
  select(matches("[p]al"))
df %>%
  select(where(is.numeric))
df[, 1:4] %>%
  select(where(~ max(.x, na.rm = TRUE) >7))
df = tibble(Class = c("1 班 ", "2 班 "),
            Name = c(" 张三 , 李四 , 王五 ", " 赵六 , 钱七 "))
df
df %>%
  separate_rows(Name, sep = ", ")
world_bank_pop
pop2 = world_bank_pop %>%
  pivot_longer(`2000`:`2017`, names_to = "year", values_to = "value")
pop2
pop2 %>%
  count(indicator)
pop3 = pop2 %>%
  separate(indicator, c(NA, "area", "variable"))
pop3

# 数据集分组随机重复抽样
df = group_by(mtcars, cyl) 
slice_sample(df, n = 3)
map_dfr(1:2, ~ slice_sample(df, n = 3), .id = "replicate") %>%
  ungroup()      # 解除分组
GroupRepSample = function(data, ..., n, r) {
  # 实现对数据集分组重复随机抽样
  # ...为一个或多个分组变量, n为随机抽样数, r为重复次数
  df = group_by(data, ...)
  map_dfr(1:r, ~ slice_sample(df, n = n), .id = "replicate") %>% 
    ungroup()  
}
GroupRepSample(mtcars, am, cyl, n = 3, r = 2)
# 注意，有一个分组只有 2 个样本，而 slice_sample 默认参数replace = FALSE 不能重复，
# 所以抽不出来 3 个样本，所以样本总数是 17 × 2 = 34。
GroupRepSample = function(data, ..., n = NULL, p = NULL, r) {
  # 实现对数据集分组重复随机抽样
  # ...为一个或多个分组变量, n为随机抽样数, p为随机抽样比例
  # r为重复次数
  df = group_by(data, ...)
  if(is.null(p)) {
    map_dfr(1:r, ~ slice_sample(df, n = n), .id = "replicate") %>% 
      ungroup()
  } else {
    map_dfr(1:r, ~ slice_sample(df, prop = p), .id = "replicate") %>% 
      ungroup()
  }
}
GroupRepSample(mtcars, am, cyl, n = 3, r = 2)       # 结果同上,略
GroupRepSample(mtcars, am, cyl, p = 0.5, r = 2) 


mtcars %>% 
  group_by(cyl) %>% 
  slice_sample(n = 3) %>% 
  slice(rep(1:n(), 2)) %>% 
  mutate(replicate = rep(1:2, each = 3)) %>%
  ungroup()

library(tidyverse)
ggplot(mpg, aes(displ, hwy, color = drv)) +
  geom_point() +
  scale_color_jco(name="驱动方式", labels = c(" 四驱", " 前驱", " 后驱"))

  scale_color_manual("驱动方式",                 # 修改图例名
                     values = c("red", "blue", "green"),
                     # breaks = c("4", "f", "r"),
                     labels = c(" 四驱", " 前驱", " 后驱"))
ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2")        # 使用Dark2 调色版
RColorBrewer::display.brewer.all() #查看所有可用的调色版
ggplot(mpg, aes(displ, hwy, color = hwy)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red")
ggplot(mpg, aes(displ, hwy, color = hwy)) +
  geom_point() +
  scale_color_distiller(palette = "Set1")
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ drv, scales = "free")
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ drv + cyl)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)
ggsave("my_plot.pdf", width = 8, height = 6, dpi = 300)
library(ggrepel)
best_in_class = mpg %>%        # 选取每种车型 hwy 值最大的样本
  group_by(class) %>%
  slice_max(hwy, n = 1)
best_in_class
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label_repel(data = best_in_class, aes(label = model))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  annotate(geom = "text", x = 6, y = 40,
           label = " 引擎越大\n燃油效率越高!", size = 4, color = "red")
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_violin(trim = FALSE, alpha = 0.5, color = "green") +     # 小提琴图
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max = function(x) {mean(x) + sd(x)},
               geom = "pointrange", color = "red")
ggplot(mpg, aes(class, fill = drv)) +
  geom_bar(position = position_dodge(preserve = "single"))
ggplot(mpg, aes(displ, hwy)) +
  geom_point(position = "jitter")             # 避免有散点重叠
library(patchwork)
p1 = ggplot(mpg, aes(displ, hwy)) +
  geom_point()
p2 = ggplot(mpg, aes(drv, displ)) +
  geom_boxplot()
p3 = ggplot(mpg, aes(drv)) +
  geom_bar()
p1 | (p2 / p3)
# 人口
pops = read_csv("datas/hljPops.csv")       
mutate(Age = as_factor(Age)) %>%             
  pivot_longer(-Age, names_to = "性别", values_to = "Pops")   # 宽变长
pops
ggplot(pops, aes(x = Age, y = ifelse(性别 == "男", -Pops, Pops), fill = 性别)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = c(-200,200)) +
  xlab("年龄段") + ylab("人口数(万)") + 
  coord_flip()
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10() # A better way to log transform

library(gapminder)
gapminder
gapminder %>%
  distinct(continent, country) %>%
  count(continent) %>% 
  mutate(coll = if_else(continent == "Asia", "red", "gray")) %>% 
  ggplot(aes(x = fct_reorder(continent, n), y = n)) +
  geom_text(aes(label = n), hjust = -0.25) +
  geom_col(width = 0.8, aes(fill = coll) ) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("#b3b3b3a0", "#D55E00")) +
  annotate("text", x = 3.8, y = 48, label = "this is important\ncase", 
           color = "#D55E00", size = 5) +
  annotate(
    geom = "curve", x = 4.1, y = 48, xend = 4.1, yend = 35, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  theme(legend.position = "none",
        axis.text = element_text(size = 11)
  ) +
  labs(title = "My title", x = "")

data1 = data.frame(
  A=c("小米","华为","苹果")
)
data1
data2 = data.frame(
  A=c("小米","小米","苹果","OPPO"),
  B=c("1","2","1","1")
)
data2
data1 %>% mutate(C=1) %>% 
  full_join(data2,by="A") %>% 
  mutate(my.merge=case_when(is.na(C)~"只在data2",
                            is.na(B)~"只在data1",
                            TRUE~"匹配上")) %>% 
  select(-C)

# case_when
x = 1:50
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
####################################################################3
library(tidyverse)
library(lubridate) # 处理日期时间
library(readxl) # 读取Excel
library(naniar) # 探索处理缺失值
df = import("datas/朝阳医院2018年销售数据.xlsx")
df
colnames(df)
summary(df)
df = df %>%
  rename(销售时间= 购药时间)
df
naniar::miss_var_summary(df)
df %>%
  filter(if_any(1:2, is.na))
df = df %>%
  filter(!if_any(1:2, is.na))
df
df = df %>%
  mutate(across(5:7, ~ naniar::impute_mean(.x)))
df = df %>%
  mutate(销售时间= ymd(销售时间))
df = df %>%
  arrange(销售时间)
df
df = df %>%
  filter(销售数量> 0)
df
df = read_xlsx("datas/朝阳医院2018年销售数据.xlsx")%>% # 读取数据
  rename(销售时间= 购药时间) %>% # 重命名列
  filter(!if_any(1:2, is.na)) %>% # 删除缺失值
  mutate(across(5:7, ~ impute_mean(.x))) %>% # 插补缺失值
  mutate(销售时间= ymd(销售时间)) %>% # 修改日期列
  drop_na(销售时间) %>% # 删除缺失行
  arrange(销售时间) %>% # 按行排序
  filter(销售数量> 0) # 删除异常值行
df %>%
  summarise(总消费次数= n_distinct(销售时间, 社保卡号),
                 总消费金额= sum(实收金额),
                 月份数= min(销售时间) %--% max(销售时间) %/% dmonths(1)) %>%
  mutate(月均消费次数= 总消费次数/ 月份数,
               客单价= 总消费金额/ 总消费次数)
df_month = df %>%
  group_by(月度= month(销售时间)) %>%
  summarise(across(5:7, sum))
df_month
df_month %>%
  ggplot(aes(月度, 销售数量)) +
  geom_line(color = "blue", size = 1.5)
df_month %>%
  pivot_longer(3:4, names_to = "类别", values_to = "金额") %>%
  ggplot(aes(月度, 金额, color = 类别)) +
  geom_line(size = 1.5) +
  theme(legend.position = "top")

x=c(1,2,3,4)
MeanStd3 = function(x, type = "sample") {
  mu = mean(x)
  n = length(x)
  switch(type,
         "sample" = {
           std = sqrt(sum((x - mu) ^ 2) / (n - 1))
         },
         "population" = {
           std = sqrt(sum((x - mu) ^ 2) / n)
         })
  list(mu = mu, std = std)
}
MeanStd3(x)
MeanStd3(x, "population")

#vlookup匹配玩家名称并返回玩家的球队来查找玩家的球队名称
#create first data frame
df1 = tibble(player=LETTERS[1:18],
              team=rep(c('Mavs', 'Lakers', 'Rockets'), each=6))
df1
#create second data frame 
df2 = tibble(player=LETTERS[1:15],
              points=c(14, 15, 15, 16, 8, 9, 16, 27, 30, 24, 14, 19, 8, 6, 5))
df2
#merge the two data frames using inner_join
inner_join(df1, df2, by="player")
left_join(df1, df2, by="player")

MeanStd3 = function(x, type = "sample") {
  mu = mean(x)
  n = length(x)
  switch(type,
         "sample" = {
           std = sqrt(sum((x - mu) ^ 2) / (n - 1))
         },
         "population" = {
           std = sqrt(sum((x - mu) ^ 2) / n)
         })
  list(mu = mu, std = std)
}
x = c(2, 6, 4, 9, 12)
MeanStd3(x,"population")

p = ggplot(mtcars, aes(wt, disp)) + geom_point()
p
wrap_by = function(...) {
  facet_wrap(vars(...), labeller = label_both)
}                  
p + wrap_by(vs)
p + wrap_by(vs, am)
p + wrap_by(drat = cut_number(drat, 3))
wrap_cut = function(var, n = 3) {
  # Let's enquote the named argument `var` to make it auto-quoting:
  var = enquo(var)
  nm = as_label(var)
  wrap_by(!!nm := cut_number(!!var, !!n))
}
p + wrap_cut(mpg)


files = list.files("datas/read_datas", pattern = "xlsx", 
                   full.names = TRUE)
files
#########################################
df = tibble(files = list.files("datas/read_datas", pattern = "xlsx", 
                               full.names = TRUE)) %>% 
  mutate(grp = str_extract(files, ".(?=\\d)")) %>% 
  group_nest(grp, .key = "files") %>% 
  mutate(data = map(files, ~ map_dfr(.x$files, read_excel)))
df
df$data
walk2(df$data, str_c(df$grp, "年级.xlsx"), write_xlsx)


test=tibble(sf = rep(c("A","B"),each = 4),
            a1 = c(1:4,2:5),
            a2 = rep(c(2,3),each = 4),
            a3 = rep(c(4,5),each = 4),
            a4 = rep(c(1,2),each = 4))
test
test %>% group_by(sf) %>% 
  summarise(
    across(a1:a4, sum)
  ) %>% 
  pivot_longer(c(a1:a4), names_to="var",values_to="a5")

test %>%
  rowwise() %>%
  mutate(
    sums = sum(c_across(a1:a4)))

test %>% mutate(a5=pmap_dbl(lst(a1,a2,a3,a4),~sum(c(...))))

df=import(as="dt")
df
df %>%
  mutate(across(everything(), ~replace(.,is.na(.), 0)))
df = tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df
replace_na_dt(df, to=0)

fruit = c("apple", "banana", "pear", "pineapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
df %>% filter(str_detect(canister_batch, "^A"))
iris %>% mutate(across(matches("Sepal"), log))
iris %>% mutate(across(contains("Sepal"), log))
iris %>% select(matches("[pt]al"))
x = c("CDK 弱(+)10%+", "CDK(+)30%-",
      "CDK(-)0+", "CDK(++)60%*")
str_view(x, "\\d+%")
str_extract("(1st) other (2nd)", "\\(.+\\)")
str_extract("(1st) other (2nd)", "\\(.+?\\)")


df = tibble(x1 = 1:6, x2 = 5:10, x3 = 9:14, 
             x=c(4,5,7,NA,8,10), y=c(5,4,3,NA,6,7), z=c(5,7,NA,8,5,4),
             grp=rep(c("A","B"), each=3))
df
l=list(f1=\(...) mean(..., na.rm = TRUE),
       f2=\(...) sd(..., na.rm = TRUE))
df %>% mutate(
  m=apply(pick(x1:z), 1, l[[1]])
  )

apply(select(df, x1:x3), 2, sum)


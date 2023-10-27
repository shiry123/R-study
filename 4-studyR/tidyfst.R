# install.packages("tidyfst")
library(tidyfst)
pkg_load()
iris
iris %>% select_dt(5)

iris %>% mutate_dt(one = 1,
                   Sepal.Length.1 = Sepal.Length + 1
                   )
iris %>% transmute_dt(one = 1,
                      Sepal.Length = Sepal.Length + 1)

# add group number with symbol `.GRP`
iris %>% mutate_dt(id = 1:.N, grp = .GRP, by = Species)

iris %>% mutate_vars(is.numeric, scale)
iris %>% mutate(across(where(is.numeric),scale))
iris %>%
  count_dt(Species) %>%
  add_prop(digits =1)
df=tibble(x=c(0.9011,0.8501))
df
df %>% mutate(x_percent=percent(x))

iris %>% mutate_vars(.func = as.character)

iris[3:8,] %>%
  mutate_when(Petal.Width == .2,
              one = 1,Sepal.Length=2)

iris %>% filter_dt(Sepal.Length == max(Sepal.Length))


iris %>% group_dt(by = Species, slice_dt(1:2))

iris %>% group_dt(by=Species,
                  mutate_dt(s.max= max(Sepal.Length)) %>% summarise_dt(sum=sum(s.max))
                  )
iris %>%
  group_dt(
    by = Species,
    rbind(.SD[1],.SD[.N])
  )

iris %>% group_dt(by=Species,summarise_dt(sum=sum(Sepal.Length)))

iris %>% summarise_dt(sum=sum(Sepal.Length), by=Species) 

mtcars %>%
  group_dt(by =.(vs,am),  #.() = list()
           summarise_dt(avg = mean(mpg),
                        n=.N))
mtcars %>% summarise_dt(avg = mean(mpg),
                        n=.N,
                        by=.(vs,am))


str(mtcars)
mtcars %>%
  group_dt(by =.(vs,am),  #.() = list()
           summarise_vars("mpg|disp", .func = list(mean,sd,var))
  )

mtcars %>% summarise_vars("mpg|disp", 
                          .func = list(mean,sd,var), 
                          by =.(vs,am)
                          )


df <- tibble(x1 = 1:4, x2 = 5:8, x3 = 9:12, x=c(4,5,7,NA), y=c(5,NA,6,7), 
             grp=rep(c("A","B"), each=2))
df
df %>% rowwise_dt(
  mutate_dt(m = mean(c(x1, x2, x3)),
            s=sd(c(x1, x2, x3))
  )
)

df %>% summarise_vars(x1:x3, .func=list(mean,sd,sum), by=grp)

df %>% summarise_vars(x1:x3, .func=mean, by=grp)
df %>% summarise_vars("x1|x3", list(mean,sd,var), by=grp)

mtcars %>% summarise_vars(is.numeric,mean,by = .(vs,am))
mtcars %>% summarise_vars(is.numeric,mean,by = "vs,am")


iris %>% summarise_vars(is.numeric, mean, by =Species)




billboard
billboard %>% longer_dt(-"wk",name = "week",value = "rank",na.rm = TRUE)
billboard %>%
  longer_dt(artist,track,date.entered,  #不包括
            name = "week",value = "rank",na.rm = TRUE)


stocks = data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
) %>%
  longer_dt(time) -> longer_stocks

longer_stocks

longer_stocks %>%
  wider_dt(time,
           name = "name",
           value = "value")

df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df
df %>% separate_dt(x, c("A", "B"))
df %>% separate_dt(x, into = c(NA,"B"))

sys_time_print({
  res = iris %>%
    mutate_dt(one = 1)
})

mtcars %>% nest_dt(cyl)
mtcars %>% nest_dt(cyl,vs)

df <- data.table(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
df
df %>% chop_dt(y,z)
iris %>% squeeze_dt(1:2,.name = "data")

dt1 = data.table(x = c("A","b"),y = 1:2)
dt1
dt2 = data.table(x = c("a","B"),z = 4:5)
dt2
sql_join_dt(dt1,dt2)
sample_n_dt(mtcars, 10)
iris %>% dummy_dt(Species)
new_mtcars=mtcars %>% rn_col("rn")
new_mtcars
new_mtcars %>% col_rn("rn")

iris %>% mutate(across(1:4, ~scale(.x, center = TRUE, scale = FALSE)))
iris %>% mutate_vars(is.numeric, function(x) scale(x,center = TRUE, scale = FALSE))
iris %>% mutate_vars(is.numeric, \(x) scale(x,center = TRUE, scale = FALSE))

pow=function(x,n){
  x^n
}
iris %>% mutate(across(1:4, ~pow(.x, 0.5)))
iris %>% mutate_vars(is.numeric, function(x) pow(x, 0.5))
iris %>% mutate_vars(is.numeric, \(x) pow(x, 0.5))

iris %>% mutate(across(1:4, ~scale(.x, center = TRUE, scale = TRUE)))
iris %>% mutate_vars(is.numeric,scale)

mtcars %>% rn_col()
iris %>% dummy_dt(Species)
mtcars %>% head() %>% dummy_dt(vs,am)

x = 1:10
nth(x, 1)
nth(x, 5)
nth(x, -2)

y = c(10,3,4,5,2,1,6,9,7,8)
maxth(y,3) #第3个最大的
minth(y,3)
df.nest=mtcars %>% nest_dt(cyl,vs)

df <- data.table(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
df
df %>% chop_dt(y,z)

df %>% chop_dt(y,z) %>% unchop_dt(y,z)


iris %>% squeeze_dt(1:2) %>% 
  mutate_dt(ndt.mean=lapply(ndt, \(x) mean(x))) %>% unnest_dt(ndt.mean)

iris %>% squeeze_dt(1:2) %>% 
  mutate_dt(ndt.mean=sapply(ndt, \(x) mean(x)))


############################## dtplyr  #######################################
library(dtplyr)
mtcars2 <- lazy_dt(mtcars)
mtcars2 %>% 
  filter(wt < 5) %>% 
  mutate(l100k = 235.21 / mpg) %>% # liters / 100 km
  group_by(cyl) %>% 
  summarise(l100k = mean(l100k)) %>% 
  as_tibble()


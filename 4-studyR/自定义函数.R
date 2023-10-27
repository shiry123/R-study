summarise_mean = function(data, vars) {
  d=data %>%
    summarise(n = n(), across({{vars}}, mean))
  return(list(d))
}
mtcars %>%
  group_by(cyl) %>%
  summarise_mean(c(mpg,disp))

my_summarise = function(data, group_var, summarise_var) {
  d=data %>%
    group_by(across( {{group_var}} )) %>%
    summarise(across( {{summarise_var}}, mean,  .names = "mean_{.col}" ))
  return(list(d))
}
mtcars %>%
  my_summarise(c(am, cyl), where(is.numeric))

var_summary = function(data, var) {
  x=enquo(var)
  d=data %>%
    summarise(n = n(),
               !!x := mean(.data[[var]]))
  return(list(d, var, x))
}
var_summary(mtcars, "mpg")


grouped_mean = function(data, dv, by) {
  summary_var = enquo(dv)
  group_var = enquo(by)
  summary_nm = str_c("mean_", as_label(summary_var))
  group_nm = str_c("group_", as_label(group_var))
  
  data %>%
    group_by(!!group_nm := !!group_var) %>%
    summarise(!!summary_nm := mean(!!summary_var))
}
grouped_mean(mtcars, mpg, cyl)

##########################################################
get_formula=function(x){
  c.string=str_detect(str_trim(x), "^c\\(")
  if(isTRUE(c.string)){
    vars=x %>% str_extract("(?<=\\().+?(?=\\))") %>% 
      str_split_1(",")
  } else {
    vars=x
  }
  var.formula=formula(paste("~",paste(vars, collapse = "+")))
  return(list(var.formula,isTRUE(c.string)))
}
var="cN"
var="c(N, P)"
get_formula(var)

f=function(x, y=NULL){
  e=isTRUE(missing(y))
  d=substitute(x)
  a=deparse(substitute(x))
  
  b=enquo(y)
  c=as_label(b)
  f=expr(y)
  return(list(d,a, b,c, e, f))
}

f(x, y=c(N,P))
f(c(N,P),p+1)
f(c(N,P))
var <- "cyl"
expr(with(mtcars, mean(!!sym(var))))
f=function(.func=list(), na.rm=TRUE){
  
  if(!is.list(.func)){
    func_names = deparse(substitute(.func))
  }else{
    func_names = sapply(substitute(.func), deparse) %>% .[-1]
  }
  funcs=map(func_names, \(fns) eval(parse(text=str_glue("\\(x){fns}(x,na.rm={na.rm})"))))
  names(funcs)=func_names
  
  return(list(func_names,funcs))
}
x=f(.func = list(mean,sd,sum))[[2]]
x



myplot=function(data,x,y, group=NULL, facet=NULL){
  x.var=substitute(x)   #enquo
  y.var=substitute(y)
  x.nm=deparse(x.var)  #as_label
  y.nm=deparse(y.var)
  group=substitute(group)

  
  get_string=function(x){
    deparse(substitute(x))
  }
  
  facet.nm=get_string(facet)
  
  p1=ggplot(data, aes(!!x.var, !!y.var, color=!!group))+
    geom_point()
  p2=ggplot(data, aes(.data[[x.nm]], .data[[y.nm]] ))+
    geom_point()
if(missing(facet)){
   p=p1
}else{
  p=p1+
    facet_grid(facet, scales = "free")
}
return(p)
}
myplot(mtcars, x=disp, y=mpg, facet = ~vs)


fdt = function(.data,...,cols = NULL,negate =FALSE){
  dt = as_dt(.data)
  a.l=substitute(list(...))
  dot_string=substitute(list(...)) %>%
                deparse() %>%
                paste0(collapse = "") %>%
                str_extract("\\(.+\\)") %>%
                str_sub(2,-2)
    return(list(dot_string,a.l))
}
fdt(mtcars,c(N,P))
fdt(mtcars,c("N","P"))

name="bob"
str_glue("My name is {name}, not {{name}}.")

#################################################################
f <- function(x, type = c("mean", "median", "power")) {
  
  pow=function(x,n) x^n

  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         power = pow(x,0.5))
}
x=c(1,2,3,4)
f(x,type = "mean")

library(rlang)
iris %>% filter(!!sym('Petal.Length') >= mean(!!sym('Petal.Length')))

mtcars %>% filter(cyl=="6")
mtcars %>% filter(!!sym("cyl") == "6")

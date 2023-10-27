data_calc=function(data, cols=NULL, func=list(), by=NULL, row_calc=FALSE, ...){
  var_args = list(...)
  na.rm = if(!is.null(var_args[["na.rm"]])) var_args[["na.rm"]] else TRUE
  
  if(!is.list(func)){
    func_names = deparse(substitute(func))
  }else{
    func_names = sapply(substitute(func), deparse) %>% .[-1]
  }
  funcs=lapply(func_names, \(fns) eval(parse(text=str_glue("\\(x){fns}(x,na.rm={na.rm})"))))
  names(funcs)=func_names
  
  if(isTRUE(row_calc)){
    dat=data %>% rowid_to_column(".id") 
    dat.r=dat %>% 
      pivot_longer({{cols}}, values_to = ".values") %>% 
      group_by(.id) %>% 
      summarise(
        across(.values, .fns=funcs, .names = "{.fn}"), .groups = "drop"
      ) 
    .results=dat %>% left_join(dat.r, by=".id") %>% select(-.id)
    
  }else{
   if(!missing(by)){
    .results=data %>%
      group_by(across({{by}})) %>%
      summarise(
        n=n(),
        across({{cols}}, .fns=funcs, .names = "{.col}/{.fn}"), .groups = "drop"
      ) %>% 
      pivot_longer(contains("/"), names_to = c("variable",".value"), names_sep = "/") %>%
      relocate(n, .after=variable)
  }else if(is_grouped_df(data)){
    .results=data %>% 
      summarise(
        n=n(),
        across({{cols}}, .fns=funcs, .names = "{.col}/{.fn}"), .groups = "drop"
      ) %>% 
      pivot_longer(contains("/"), names_to = c("variable",".value"), names_sep = "/") %>%
      relocate(n, .after=variable)
  }else{
    .results=data %>%
      summarise(
        n=n(),
        across({{cols}}, .fns=funcs, .names = "{.col}/{.fn}"), .groups = "drop"
      ) %>% 
      pivot_longer(contains("/"), names_to = c("variable",".value"), names_sep = "/") %>%
      relocate(n, .after=variable)
  }
    
 }

return(.results)
}
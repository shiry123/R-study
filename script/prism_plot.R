prism_plot = function(data,x,y, trace=NULL,facets=NULL,...){
  library(ggprism)
  library(ggbeeswarm)
  x=enquo(x)
  y=enquo(y)
  trace=enquo(trace)
  facets=enquo(facets)
  facet.nm=as_label(facets)
  x.judge=data %>% pull(!!x)
  
  get_formula=function(x){
    c.string=str_detect(str_trim(x), "^c\\(")   
    if(isTRUE(c.string)){
      vars=x %>% str_extract("(?<=\\().+?(?=\\))") %>% 
        str_split_1(",")
    } else {
      vars=x
    }
    var.formula=formula(paste("~",paste(vars, collapse = "+")))
    return(var.formula)
  }
  
  
  var_args = list(...)
  pd = if(!is.null(var_args[["pd"]])) var_args[["pd"]] else 0.6
  axis_text_angle = if(!is.null(var_args[["axis_text_angle"]])) var_args[["axis_text_angle"]] else 0
  
  if(is.numeric(x.judge[1])){
    p=ggplot(data, aes(x=!!x, y=!!y))+
      ggbeeswarm::geom_beeswarm(aes(fill = !!trace),shape = 21)+
      guides(y = guide_prism_minor())+
      ggprism::theme_prism(base_line_size = 0.7,
                           axis_text_angle=axis_text_angle)+
      theme(legend.position = "top",
            legend.title=element_text("sans"),
            strip.text = element_text(size = 14),
            legend.spacing.x = unit(0, "pt"),
            legend.text = element_text(margin = margin(r = 20)))

    if(facet.nm =="NULL"){
      p=p
    }else{
      p=p+facet_wrap(get_formula(facet.nm),  scales = "free")  
    }
    #字符型x
  } else {
      p=data %>%
        ggplot(aes(x=!!x, y=!!y, fill =!!trace))+
        ggbeeswarm::geom_beeswarm(dodge.width = pd, shape = 21)+
        stat_summary(fun = mean, geom = "crossbar",
                     position = position_dodge(pd), 
                     colour = "red", linewidth = 0.3, width =0.4,
                     show.legend = FALSE)+
        scale_x_discrete(guide = "prism_bracket")+
        scale_y_continuous(guide = guide_prism_minor())+
        ggprism::theme_prism(base_line_size = 0.7,
                             axis_text_angle=axis_text_angle)+
        theme(legend.position = "top",
              legend.title=element_text("sans"),
              strip.text = element_text(size = 14),
              legend.spacing.x = unit(0, "pt"),
              legend.text = element_text(margin = margin(r = 20)))
      if(facet.nm =="NULL"){
        p=p
      }else{
        p=p+facet_wrap(get_formula(facet.nm), scales = "free") 
      }
      
    }
    
  return(p)
}


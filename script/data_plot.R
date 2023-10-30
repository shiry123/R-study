##########多分面参数使用 facet=c(var1, var2)

data_plot = function(data, x, y, 
                     trace=NULL, 
                     facet=NULL,
                     add="none",
                     add.params = list(),
                     pd=0,
                     axis.text.angle=0,
                     labeller="label_value",
                     legend.position="right",
                     type=c("point","beeswarm","line","box","bar"),...){
  library(see)
  library(ggpubr)
  x=enquo(x)
  y=enquo(y)
  trace=enquo(trace)
  facet=enquo(facet)
  facet.nm=as_label(facet)
  
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
  
  type=match.arg(type)
# add.params
  reg.equ=if(!is.null(add.params[["reg.equ"]])) add.params[["reg.equ"]] else FALSE
  se =if(!is.null(add.params[["se"]])) add.params[["se"]] else FALSE
  label.x.npc=if(!is.null(add.params[["label.x.npc"]])) add.params[["label.x.npc"]] else "left"
  label.y.npc=if(!is.null(add.params[["label.y.npc"]])) add.params[["label.y.npc"]] else "bottom"
  position=if(!is.null(add.params[["position"]])) add.params[["position"]] else position_dodge(pd)

# ...
  var_args = list(...)
  linewidth=if(!is.null(var_args[["linewidth"]])) var_args[["linewidth"]] else 0.5

  ### 不同类型图形
  switch (type,
    "point" ={
      p=ggplot(data, aes(x=!!x, y=!!y, group=!!trace, color=!!trace))+
        geom_point2(size=2,position = position_dodge(pd))+
        scale_color_see()
      
      if(add=="mean"){
      p=p+stat_summary(fun = mean, geom = "crossbar",
                       position = position_dodge(pd), 
                       colour = "blue", linewidth = 0.3, width =0.3,
                       show.legend = FALSE)
      }else if(add=="mean_sd"){
          p=p+
            stat_summary(fun.data = "mean_sd", 
                         geom="pointrange",
                         position =position)
          # p=p+
          #   # stat_summary(fun.data = "mean_sd", 
          #   #              geom="pointrange",
          #   #              position =position_jitterdodge(jitter.width =0.2,
          #   #                                             dodge.width = 0.8))

      }else if(add=="reg.line"){
            p=p+
              geom_smooth(method="lm", formula = "y~x", se=se, linetype=2)
          if(isTRUE(reg.equ)){
            p=p+
              stat_regline_equation(aes(label =  paste(after_stat(eq.label), 
                                                         after_stat(rr.label), 
                                                         sep ="~`,`~")),
                                      label.x.npc =label.x.npc,
                                      label.y.npc = label.y.npc)
          }
       } 
    },
    "beeswarm" ={
      p=ggplot(data, aes(x=!!x, y=!!y, 
                         group=!!trace, color=!!trace))+
        ggbeeswarm::geom_beeswarm(dodge.width=pd)+
        scale_color_see()
      if(add=="mean"){
        p=p+stat_summary(fun = mean, geom = "crossbar",
                         position = position_dodge(pd), 
                         colour = "blue", linewidth = 0.3, width =0.3,
                         show.legend = FALSE)
        }
      },
    "line"={
      if(add=="mean_sd"){
        if(as_label(trace)==as_label(x) | as_label(trace)=="NULL"){
          p=ggplot(data, aes(x=!!x,y=!!y, group=1))+
            geom_point2(stat="summary",fun="mean",
                        position = position_dodge(pd))+
            geom_line(stat="summary",fun="mean",
                      position = position_dodge(pd), linewidth=linewidth)

        }else{
          p=ggplot(data, aes(x=!!x,y=!!y, group=!!trace, color=!!trace))+
            geom_point2(stat="summary",fun="mean",
                       position = position_dodge(pd))+
            geom_line(stat="summary", fun="mean",
                      position = position_dodge(pd), linewidth=linewidth)
        }
          p=p+
            stat_summary(fun.data = "mean_sd",geom="pointrange",
                           position = position_dodge(pd))+
            scale_color_see()
      }  else{

        p=ggplot(data, aes(x=!!x,y=!!y, color=!!trace))+
          geom_point2()+
          geom_line(aes(group=!!line.group), linewidth=linewidth)+

        p=ggplot(data, aes(x=!!x,y=!!y, group=!!trace, color=!!trace))+
          geom_point2()+
          geom_line(linewidth=linewidth)+
          scale_color_see()
      }
    },
    "box"={
      if(as_label(trace)==as_label(x) | as_label(trace)=="NULL"){
        p=ggplot(data, aes(x =!!x, y =!!y)) +
          stat_boxplot(geom = "errorbar", width=0.2,
                       position = position_dodge(pd))+
          geom_boxplot(width=0.5,
                       position = position_dodge(pd))
      }else{
        p=ggplot(data, aes(x =!!x, y =!!y, fill =!!trace)) +
          stat_boxplot(geom = "errorbar", width=0.2,
                       position = position_dodge(pd))+
          geom_boxplot(width=0.5,
                       position = position_dodge(pd))+
          scale_fill_see()
      }

      if(add=="jitter"){
          p=p+
            geom_jitter(position = position_jitterdodge(jitter.width = 0.1,
                                                        dodge.width=pd))

          if(as_label(trace)==as_label(x) | as_label(trace)=="NULL"){
            p=p+
              geom_jitter(width = 0.2)
          }else{
            p=p+
              geom_jitter(position = position_jitterdodge(jitter.width = 0.2,
                                                           dodge.width=pd))
          }
        }
    },
    "bar"={
          p=ggplot(data, aes(x=!!x, y=!!y,fill = !!trace))+
            geom_bar(color = "black",stat = 'identity', 
                     position =position_dodge(pd), width=0.5)+
            scale_fill_see()
          
          if(add=="mean_sd"){
          p=ggplot(data, aes(x =!!x, y =!!y,fill = !!trace)) +
            geom_bar(stat = "summary", fun = mean, 
                     color = "black", width=0.5,
                     position =position_dodge(pd)) +
            stat_summary(fun.data = 'mean_sd',
                         geom = "errorbar", color = "black",width = 0.2,
                         position =position_dodge(pd))+
            scale_fill_see()
          }
        }
)
  
  if(facet.nm =="NULL"){
    p=p+
      # guides(y = ggprism::guide_prism_minor())+
      # ggprism::theme_prism(axis_text_angle=axis.text.angle)+
      # theme(legend.position =legend.position,
      #       legend.title=element_text())
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.text=element_text(size=10, face = "bold"),
            axis.title.x=element_text(size=10),
            axis.title.y=element_text(size=10),
            legend.text=element_text(size=10),
            legend.position=legend.position,
            axis.line=element_line(colour="black", linewidth=0.6),
            axis.ticks=element_line(colour="black", linewidth=0.6),
            axis.text.x = element_text(angle =axis.text.angle, vjust =0.2))
  }else{
    p=p+
      # guides(y = ggprism::guide_prism_minor())+
      # ggprism::theme_prism(axis_text_angle=axis.text.angle)+
      # theme(legend.position =legend.position,
      #       legend.title=element_text())+
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.text=element_text(size=10, face = "bold"),
            axis.title.x=element_text(size=10),
            axis.title.y=element_text(size=10),
            legend.text=element_text(size=10),
            legend.position=legend.position,
            axis.line=element_line(colour="black", linewidth=0.6),
            axis.ticks=element_line(colour="black", linewidth=0.6),
            axis.text.x = element_text(angle =axis.text.angle, vjust =0.2))+
      facet_nested(get_formula(facet.nm), scales = "free",labeller = labeller)
  }
  
  return(p)
}



# "bar"={
#   if(bar.position=="dodge"){
#     p=ggplot(data, aes(x=!!x, y=!!y,fill = !!trace))+
#       geom_bar(color = "black",stat = 'identity', 
#                position =bar.position, width=0.5)+
#       scale_fill_see()
#     
#     if(add=="mean_sd"){
#       p=ggplot(data, aes(x =!!x, y =!!y,fill = !!trace)) +
#         geom_bar(stat = "summary", fun = mean, 
#                  color = "black", width=0.5,
#                  position = position_dodge(pd)) +
#         stat_summary(fun.data = 'mean_sd',
#                      geom = "errorbar", color = "black",width = 0.2,
#                      position = position_dodge(pd))+
#         scale_fill_see()
#     }
#     
#   } else {
#     p=ggplot(data, aes(x=!!x, y=!!y,fill = !!trace))+
#       geom_bar(color = "black",stat = 'identity', 
#                position =bar.position, width=0.5)+
#       scale_fill_see()
#     
#     if(add=="mean_sd"){
#       data_sum=function(dat, .x, .y, .trace=NULL, .facet=NULL){
#         x_var=enquo(.x)
#         y_var=enquo(.y)
#         trace_var=enquo(.trace)
#         facet_var=enquo(.facet)
#         
#         dat %>% 
#           convert_as_factor(!!trace_var) %>% 
#           group_by(!!x_var,!!facet_var,!!trace_var) %>% 
#           summarise(mean=mean(!!y_var),
#                     sd=sd(!!y_var),
#                     .groups="drop") %>% 
#           arrange(!!x_var,!!facet_var,desc(!!trace_var)) %>% 
#           group_by(!!x_var,!!facet_var) %>% 
#           mutate(label.y=cumsum(mean))
#       }
#       p=data_sum(data,!!x, !!y,!!trace, !!facet) %>% 
#         ggplot(aes(x=!!x,y=mean,fill=!!trace))+
#         geom_bar(position = "stack",stat="identity",width=0.5)+
#         geom_errorbar(aes(ymin=label.y-sd,ymax=label.y+sd),width=0.2)+
#         scale_fill_see()
#       
#     }
#   }
#   
# }
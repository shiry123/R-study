trendline_plot=function(data, x, y, model=c("line2P","line3P","log2P","exp2P","exp3P","power2P","power3P"),
                        facet.by=NULL){
  # "line2P" (formula as: y=a*x+b),
  # "line3P" (y=a*x^2+b*x+c),
  # "log2P" (y=a*ln(x)+b),
  # "exp2P" (y=a*exp(b*x)),
  # "exp3P" (y=a*exp(b*x)+c),
  # "power2P" (y=a*x^b),
  # "power3P" (y=a*x^b+c).
  model=match.arg(model)
  x=enquo(x)
  y=enquo(y)
  df.x=data %>% pull(!!x)
  df.y=data %>% pull(!!y)
  
  library(ggtrendline)
  ggtrendline(df.x, df.y, model=model, xlab=as_label(x), ylab=as_label(y)) + 
      geom_point(aes(df.x, df.y))

}
relax_opt = function(data, x.var, opt.model, stepsize, n, type="min"){
  library(Rsolnp)
  x.mean   <- sapply(data[,x.var], mean,na.rm=T)
  x.lb     <-  sapply(data[,x.var], min,na.rm=T) # LB,UB
  x.ub     <-  sapply(data[,x.var], max,na.rm=T)
  delta    <- (x.ub-x.lb)*stepsize 
  
  obj   =function(x) {      
    xx               <- data.frame(rbind(x))
    colnames(xx)     <- x.var 
    return( predict(opt.model, xx))   
  }
  
  minus.obj   =function(x) {  	
    xx               <- data.frame(rbind(x))
    colnames(xx)     <- x.var 
    return( -predict(opt.model, xx))      # max(x) = min(-x)      
  }
  ###########################
  df.collect  <- NULL 

  switch(type,
  "min" = {
  for (i in (0:n)){
             xx.lb <- x.lb - i*delta
             xx.ub <- x.ub + i*delta
             tt <- system.time(res.nlp  <- gosolnp(fun=obj, LB=xx.lb, UB=xx.ub , 
                                                   pars=x.mean, control=list(trace=0)))
             x.solution           <- rbind(res.nlp$pars)
             colnames(x.solution) <- x.var
             df.collect           <- rbind(df.collect, 
                                           data.frame(relax.step=i,
                                                    y.at.solution = obj(x.solution),
                                                    rbind(x.solution),
                                                    return.code=res.nlp$convergence,
                                                    time.s=round(tt[3],2) 
                                                                ) )
           }
         },
  
  "max" = {
  for (i in (0:n)){
             xx.lb <- x.lb - i*delta
             xx.ub <- x.ub + i*delta
             tt <- system.time(res.nlp  <- gosolnp(fun=minus.obj, LB=xx.lb, UB=xx.ub , 
                                                   pars=x.mean, control=list(trace=0)))
             x.solution         <- rbind(res.nlp$pars)
             colnames(x.solution) <- x.var
             df.collect         <- rbind(df.collect,
                                         data.frame(relax.step=i,
                                                    y.at.solution = -minus.obj(x.solution),
                                                    rbind(x.solution),
                                                    return.code=res.nlp$convergence,
                                                    time.s=round(tt[3],2)
                                                    ))
           }
         }
  )
  rownames(df.collect)  <- NULL
  return(df.collect)
}

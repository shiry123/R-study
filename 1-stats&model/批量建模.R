getwd()
rm(list=ls())
# library(randomForest)
dat <- bruceR::import("datas/all.data.xlsx",sheet="historical.data")
str(dat)
x.name=colnames(dat[,2:8])
# 响应变量 ton.dmm 和 ton.mf
df=dat %>% pivot_longer(9:10, names_to = "Respones", values_to = "y") 
df
form=formula(paste("y","~",paste(x.name, collapse="+")))
form
df.nest=df %>% select(-obsnr) %>% 
  group_nest(Respones) %>% 
  mutate(
    model=map(data, ~randomForest::randomForest(form, data = .x ,
                                  importance=TRUE ,proximity=TRUE)), #随机森林
    pred=map(model, ~predict(.x)),
    R2=map2(data, pred, ~round(cor(.y, .x$y)^2,3))
  ) %>% unnest(R2)
df.nest

df.nest$model[[1]]$importance
randomForest::varImpPlot(df.nest$model[[1]], main = "variable importance")

##绘制多个Y的拟合图
for (i in (1:length(df.nest$Respones)) )   {
plot(df.nest$pred[[i]], df.nest$data[[i]]$y, #type="n",
       xlab="predicted", ylab="observed",
       main=paste("Respones=",df.nest$Respones[i]))
abline(0,1,col="red")    # diagonal in red
abline(lm(df.nest$data[[i]]$y ~ df.nest$pred[[i]])$coef,lty=2) # linear fit in black
legend("topleft",legend=substitute(R^2 == a, list(a=df.nest$R2[i])))# linear fit in black
grid()
}

#绘图
plot.data=df.nest %>% select(Respones,data,pred,R2) %>% unnest(c(data ,pred)) 
plot.data
R2.label=plot.data %>% group_by(Respones) %>% slice_min(y)
ggplot(data=plot.data, aes(x=pred,y=y))+
  facet_wrap(~Respones)+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red")+
  geom_smooth(method = 'lm',formula= 'y ~ x',se=FALSE, linetype="dashed",color="blue")+
  geom_text(data=R2.label,aes(label=paste0("R2=",R2), x=250, y=1100))+
  theme_lucid()
####################################################################
x.pred <- expand_grid(
  m.kat = seq(min(dat$m.kat,na.rm=T), 
              max(dat$m.kat,na.rm=T), length=15),
  m.add = seq(min(dat$m.add), 
              max(dat$m.add), length=15),
  p.co2 = seq(min(dat$p.co2), 
              max(dat$p.co2), length=3),
  T     = seq(min(dat$T), 
              max(dat$T), length=3),
  t.h   = median(dat$t.h),
  p.h2  = median(dat$p.h2),
  v.ml  = median(dat$v.ml)) 
x.pred
# x.pred %>% summarise(across(everything(),range))
sapply(x.pred, range)
# x.pred$y1 <- predict(df.nest$model[[1]], x.pred)
# x.pred$y2 <- predict(df.nest$model[[2]], x.pred)
x.pred=x.pred %>% 
  mutate(map_dfc(df.nest$model, ~predict(.x, x.pred))) %>% 
  rename_with(~str_replace(.x, "...", "y"), num_range("...",1:length(df.nest$model)))
x.pred
x.pred %>% distinct(p.co2)
library(lattice)
x.pred.f=x.pred %>% mutate(across(c(T,p.co2), ~as.factor(.x), .names = "{.col}.f"))
x.pred.f
levels(x.pred.f$T.f)=cc("T:20,T:70,T:120")
levels(x.pred.f$p.co2.f)=cc("p.co2:5,p.co2:22.5,p.co2:40")

cex.set=0.7
wireframe(y1 ~ m.kat*m.add|p.co2.f*T.f, data=x.pred.f,
          zlab=list(label="y",cex=cex.set,rot=90),
          xlab=list(label="m.kat",cex=cex.set,rot=0),
          ylab=list(label="m.add",cex=cex.set,rot=0),
          drape=TRUE,      
          at=do.breaks(c(50,600),100),
          # col.regions = topo.colors(100),
          col.regions =pals::parula(100),
          strip=TRUE, pretty=TRUE,
          scales = list(arrows = FALSE,
                        x=c(cex=cex.set),y=c(cex=cex.set),z=list(cex=cex.set)),
          screen = list(z = 300, x = -60, y=0),
          par.strip.text=list(cex=cex.set)
)

x.promising=dat %>% dplyr::filter(ton.dmm > 400) %>% 
  dplyr::select(-starts_with("ton."), -obsnr) %>% distinct()
x.promising
sapply(x.promising,range)

candidate <- expand.grid(
  m.kat=seq(min(x.promising$m.kat),  
            max(x.promising$m.kat),length=3),
  v.ml=seq(min(x.promising$v.ml),  
           max(x.promising$v.ml),length=3),
  m.add=seq(min(x.promising$m.add),  
            max(x.promising$m.add),length=3))
candidate  <- rbind(x.promising[,1:3],candidate) 
candidate
# library(AlgDesign)
doe.cat <- optFederov(~ (m.kat + v.ml + m.add)^2+
                        I(m.kat^2) + I(v.ml^2) + I(m.add^2),
                      data=as.data.frame(candidate),center=T,augment=T,rows=(1:9),
                      criterion="D", nTrials=15)$design 
doe.cat
unique(doe.cat$m.kat)
# rownames(doe.cat)=NULL
kappa(model.matrix(~ (m.kat + v.ml + m.add)^2 + 
                     I(m.kat^2) + I(v.ml^2) + I(m.add^2) , 
                     data=data.frame(scale(doe.cat))))


###############     doe1    ########################################
# data %>%
#   mutate(rmse = rmse(model, data),
#          rsq = rsquare(model, data),
#          slope = coef(model)[[2]],
#          pval = glance(model)$p.value)
getwd()
doe1 <- import("datas/all.data.xlsx", sheet="DoE1")
str(doe1)
x.var=colnames(doe1[,2:4])
sapply(doe1,range)

df=doe1 %>% pivot_longer(9:10, names_to = "Respones", values_to = "y") 
df
form <-formula("y ~ (m.kat + v.ml + m.add)^2 + I(m.kat^2) + I(v.ml^2) + I(m.add^2)")
form
df.nest=df %>% select(-obsnr) %>% 
  group_nest(Respones) %>% 
  mutate(
    model=map(data, ~MASS::stepAIC(lm(form, data = .x)),trace=0,k=log(nrow(.x))),
    pred=map(model, ~predict(.x)),
    tidy=map(model, ~broom::glance(.x)),
    R2=map2(data, pred, ~round(cor(.y, .x$y)^2,3))
    ) %>% unnest(R2)
df.nest
df.nest$model %>% map(anova) 

#绘图
plot.data=df.nest %>% select(Respones,data,pred,R2) %>% unnest(c(data ,pred)) 
plot.data
R2.label=plot.data %>% group_by(Respones) %>% slice_min(y)
ggplot(plot.data, aes(x=pred,y=y))+
  facet_wrap(~Respones)+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red")+
  geom_smooth(method = 'lm',formula= 'y ~ x',se=FALSE, linetype="dashed",color="blue")+
  geom_text(data=R2.label,aes(label=paste0("R2=",R2), x=250, y=1100))


#预测
x.pred <- expand_grid(m.kat = seq(min(doe1$m.kat), 
                                  max(doe1$m.kat), length=15),
                      m.add = seq(min(doe1$m.add), 
                                  max(doe1$m.add), length=15),
                      v.ml  = seq(min(doe1$v.ml), 
                                  max(doe1$v.ml), length=3))
x.pred=x.pred %>% 
  mutate(map_dfc(df.nest$model, ~predict(.x, x.pred))) %>% 
  rename_with(~str_replace(.x, "...", "y"), num_range("...",1:length(df.nest$model)))
x.pred
x.pred %>% distinct(v.ml)
x.pred.f=x.pred %>% mutate(v.ml.f=factor(v.ml, label=c("V:0.25mL","V:0.375mL","V:0.5mL")))
x.pred.f
library(lattice)
cex.set=0.7
wireframe(y1 ~ m.kat*m.add|v.ml.f, data=x.pred.f,
          zlab=list(label="y",cex=cex.set,rot=90),
          xlab=list(label="m.kat",cex=cex.set,rot=0),
          ylab=list(label="m.add",cex=cex.set,rot=0),
          drape=TRUE, strip=TRUE,  pretty=TRUE,  
          at=do.breaks(c(100,1200),100),
          col.regions = pals::parula(100),
          scales = list(arrows = FALSE,
                        x=c(cex=cex.set),y=c(cex=cex.set),z=list(cex=cex.set)),
          screen = list(z = 300, x = -60, y=0),
          par.strip.text=list(cex=cex.set)
)

####################  opt  ###################################
source("script/relax_opt.R")
df.nest
opt_val=df.nest %>% mutate(
  resut=map2(data, model, ~relax_opt(.x, x.var, .y, 0.1, 2, type="max"))
)
opt_val$resut

###############    doe2         ###################################
# library(AlgDesign)
## full factorial candidate set
candidate <- expand.grid(T=seq(80,100, length=2),
                         p.H2=seq(80,100, length=2),
                         p.CO2=seq(15,25, length=2),
                         t.H=seq(16,20, length=2))

doe2 <- optFederov(~ T + p.H2 + p.CO2 + t.H ,
                   data=candidate, center=T,
                   criterion="D", nTrials=5)$design   
doe2
# d-optimal cat design
(kappa <- kappa(model.matrix(~ T + p.H2 + p.CO2 + t.H ,
                             data=data.frame(scale(doe2)))))

doe2 <- import("datas/all.data.xlsx", sheet="DoE2")
doe2
colnames(doe2)
sapply(doe2, range)

df=doe2 %>% pivot_longer(9:10, names_to = "Respones", values_to = "y") 
df
x.var=colnames(doe2[,5:8])
form <-formula(paste("y","~", paste(x.var,collapse="+"),sep=""))
form
df.nest=df %>% 
  group_nest(Respones) %>% 
  mutate(
    model=map(data, ~MASS::stepAIC(lm(form, data = .x)),trace=0,k=log(nrow(.x))),
    pred=map(model, ~predict(.x)),
    tidy=map(model, ~broom::glance(.x)),
    R2=map2(data, pred, ~round(cor(.y, .x$y)^2,3))
  ) %>% unnest(R2)
df.nest
df.nest$model %>% map(anova) 

#绘图
plot.data=df.nest %>% select(Respones,data,pred,R2) %>% unnest(c(data ,pred)) 
plot.data
R2.label=plot.data %>% group_by(Respones) %>% slice_min(y)
qplot(data=plot.data, x=pred,y=y, facets = ~Respones)+
  geom_abline(intercept = 0, slope = 1, color="red")+
  geom_smooth(method = 'lm',formula= 'y ~ x',se=FALSE, linetype="dashed",color="blue")+
  geom_text(data=R2.label,aes(label=paste0("R2=",R2), x=1000, y=3000))+
  theme_bw()

#预测
x.pred <- expand_grid(     T = seq(min(doe2$T), 
                                   max(doe2$T), length=15),
                           p.h2 = seq(min(doe2$p.h2), 
                                      max(doe2$p.h2), length=15 ),
                           p.co2  = seq(min(doe2$p.co2), 
                                        max(doe2$p.co2), length=3),
                           t.h    = seq(min(doe2$t.h), 
                                        max(doe2$t.h), length=3))
x.pred=x.pred %>% 
  mutate(map_dfc(df.nest$model, ~predict(.x, x.pred))) %>% 
  rename_with(~str_replace(.x, "...", "y"), num_range("...",1:length(df.nest$model)))
x.pred 

library(lattice)
cex.set=0.7
wireframe(y1 ~ T*p.h2|p.co2*t.h, data=x.pred,
          zlab=list(label="y",cex=cex.set,rot=90),
          xlab=list(label="T",cex=cex.set,rot=0),
          ylab=list(label="p.h2",cex=cex.set,rot=0),
          drape=TRUE, strip=TRUE,  pretty=TRUE,  
          at=do.breaks(c(-50,2600),100),
          col.regions = pals::parula(100),
          scales = list(arrows = FALSE,
                        x=c(cex=cex.set),y=c(cex=cex.set),z=list(cex=cex.set)),
          screen = list(z = 60, x = -60, y=0),
          par.strip.text=list(cex=cex.set)
)

source("script/relax_opt.R")
df.nest
opt_val=df.nest %>% mutate(
  resut=map2(data, model, ~relax_opt(.x, x.var, .y, 0.1, 2, type="max"))
)
opt_val$resut
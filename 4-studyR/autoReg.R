library(autoReg)
data(cancer,package="survival")
colon %>%
  select("status","rx","sex","age","obstruct","perfor","adhere","differ","surg")->data
colnames(data)<-c("Status","Tretment","Sex","Age","Obstruction","Perforation","Adherence","Differentiation","Surgery")
data[,c(3,5:9)]<-lapply(data[,c(3,5:9)],as.factor)
data %>%
  mutate(Age=ifelse(Age>=60,">=60","<60"))->data
data$Age<-as.factor(data$Age)
str(data)
fit=glm(Status~.,data=data,family="binomial")
summary(fit)
step(fit)
fit %>% tidy()
autoReg(fit) %>% myft()
autoReg(fit, uni=TRUE,threshold=1, final=TRUE) %>% myft()
modelPlot(fit)

library(rrtable)
autoReg(fit) %>% myft() %>% 
  rrtable::table2docx()  #导出到docx，可编辑数据
# table2pptx(result)  #导出到ppt，可编辑数据
p1=modelPlot(fit)
rrtable::plot2pptx(print(p1))

##########################################################################
or_ci <- exp(confint(fit))
p_values <-summary(fit)$coefficients[, 4]
forest_data <- data.frame(
  Variables =rownames(or_ci)[-1],
  OR =exp(coef(fit))[-1],
  Lower_CI = or_ci[-1, 1],
  Upper_CI = or_ci[-1, 2],
  P_Value = p_values[-1] 
)

forest_data$Variables<-factor(forest_data$Variables,levels = rev(forest_data$Variables))
forest_data%>%
  mutate(Change=case_when(
    P_Value<0.05& OR>1~"Risk factor",
    P_Value<0.05& OR<1~"Protective factor",
    P_Value>=0.05~"Not sig"
  ))->forest_data
p<-ggplot(forest_data, aes(x =OR, y = Variables)) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI,color=Change), 
                 height = 0.2,linewidth=1) +
  geom_point(alpha=0.3,aes(size=P_Value)) +
  xlim(min(or_ci), max(or_ci)) +
  labs(x = "OR (95%CI)", y = "") +
  geom_vline(xintercept = 1.0,color="grey",linetype=2,linewidth=1)+
  scale_color_manual(values = c("#377eb8","#4daf4a","#e41a1c"))+
  theme_bw()+
  theme(axis.text = element_text(size = 12,color = "black"),
        panel.border = element_rect(linewidth = 1))
p

color_mapping <- c("TretmentLev" ="#377eb8",
                   "TretmentLev+5FU" ="#4daf4a",
                   "Sex1"="#377eb8",
                   "Age>=60"="#377eb8",
                   "Obstruction1"="#377eb8",
                   "Perforation1"="#377eb8",
                   "Adherence1"="#e41a1c",
                   "Differentiation2"="#377eb8",
                   "Differentiation3"="#e41a1c",
                   "Surgery1"="#e41a1c")

p1<-p+theme(axis.text.y = element_text(color = rev(color_mapping)),
            panel.grid = element_blank())
p1

patch<-p+p1+
  plot_annotation(
    title ="Visualize regression results using ggplot2",
    tag_levels = "A",
    theme = theme(plot.title =element_text(size = 16))
  )
patch

######################################################################
data(colon, package="survival")
colon
mycolon <- colon %>% # 创建新数据集新变量
  transmute(time,
            status,
            Age = age,
            Sex = factor(sex, levels = c(0, 1),
                         labels = c("Female", "Male")),
            Obstruct = factor(colon$obstruct),
            Differ = factor(colon$differ),
            Extent = factor(colon$extent))
str(mycolon) # 查看数据集结构
gaze(~age+sex,data=acs)
fit=gaze(sex~.,data=acs,digits=1,method=1,show.p=TRUE) %>% myft()
fit
gaze(sex+DM+HBP~age,data=acs) %>% myft()
# library(rrtable)
# # table2pptx(ft) #Exported table as Report.pptx
table2docx(fit) #Exported table as Report.docx

str(acs)
mytable2flextable( mytable(Dx~.,data=acs) ,vanilla= FALSE )

fit <- glm(status ~ Age + Sex + Obstruct + Differ + Extent,
           data=mycolon,
           family="binomial")
summary(fit)
autoReg(fit)
autoReg(fit, uni=TRUE, multi = TRUE, final=TRUE)
library(survival)
fit <- coxph(Surv(time,status) ~ Age + Sex + Obstruct + Differ + Extent,
             data=mycolon)
autoReg(fit)


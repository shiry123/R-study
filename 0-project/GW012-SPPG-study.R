getwd()
# 合并一个excel中不同sheet
path = "rawdata/SP30-summary.xlsx"
df <- map(excel_sheets(path), ~ read_xlsx(path, sheet = .x)) %>%  
  reduce(bind_rows)  
df
df_sep <- df %>%  #  管道符（快捷键ctrl + shift + M）
  separate(Canister,c("Lot","ID"),sep="-",remove=FALSE,convert = TRUE) %>% 
  select(-ID)
df_sep

# 直方图
library(rstatix)
df.mean <- df %>%
  group_by(Canister) %>%
  get_summary_stats(c("Area"), type = "mean_sd")
df.mean
# across 函数
df_sep %>%
  summarise_all(~ sum(is.na(.))) #是否存在NA
df_sep %>%
  group_by(Canister) %>%
  summarise(
    across(c(Area,Ovality), list(mean = mean, sd = sd), na.rm = TRUE)
  )
# 另一个方法
df.mean <- df %>%
  dplyr::group_by(Canister) %>%
  dplyr::summarize( n=dplyr::n(),
                    mean= base::mean(Area,na.rm = TRUE),
                    sd=stats::sd(Area,na.rm = TRUE))
df.mean
write.table(df.mean, "clipboard", sep="\t", row.names=FALSE)

p1 <- ggplot(df,aes(x=Area,y=..density.., color=Canister))+
  facet_grid(Actuator~Canister)+
  geom_histogram(bins = 50,alpha=0.5,position="identity")+
  geom_density(size=0.5)+
  geom_vline(data=df.mean, aes(xintercept=mean, color=Canister),
             linetype="dashed")+
  theme_bw()+
  theme(legend.position="none")
# theme(legend.justification=c(0,0),legend.position=c(0.8,0.8))
p1
# txt1<-data.frame(Canister=c('AFS36A-082404'),
#                 label=c("Canister(AFS36A-082404) washing by methanol 40act/per"))
df=df %>% 
  mutate(Canister=fct_relevel(Canister,
                              c("AFS36A-037717","AFS36A-082125","AFS36A-082404",
                                "AFS36A-103207","AFS36A-049033"))
  )
df %>% 
ggplot(aes(x=ActuationNumber, y=Area, colour=Canister))+
  geom_point()+
  facet_grid(Canister~Actuator)+
  geom_vline(aes(xintercept=25),linetype="dashed",alpha = 0.3)+
  scale_x_continuous(limits = c(0,160),breaks = seq(0,160,5)) +
  scale_y_continuous(limits = c(200,800),breaks = seq(200,800,100)) +
  labs(x ="Actuation No.",
       y="Area [mm^2]",
       title = "Spray Pattern (30mm)",
       subtitle = "RLD(AFS36A)",
       caption ="Actuator：AFS36A-M024")+
  # geom_text(data=txt1,x=50,y=700,aes(label=label),size=3)+
  theme_bw()
library(ggpubr)
# boxplot
my_comparisons <- list(c("AFS36A-037717", "AFS36A-082125"),
                       c("AFS36A-082125", "AFS36A-082404"),
                       c("AFS36A-049033", "AFS36A-103207"))
p3<-ggplot(df,aes(x=Canister, y=Area, color=Actuator))+
  geom_boxplot()+
  stat_summary(fun = "mean",geom = "point",size = 4,
               shape = "diamond")+
  geom_text(data = df.mean, aes(label = mean, y = mean),
            position = position_dodge(0.9),
            size = 3, color = "black", vjust = -1)+
  theme_bw()+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle=0,vjust=0.5))
p3
p3+stat_compare_means(comparisons=my_comparisons,
                   # label = "p.signif",
                   method="t.test")                         #  wilcox.test

p3+ annotate(geom = "segment",x=0.6,xend=3.4,y=280,yend = 280,
             size=1,color="Red")+
  annotate(geom = "text", x=2, y=250,label="RLD dev",color="Red")


library(patchwork)
p1+p3

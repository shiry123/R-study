library(NonCompart)
Theoph
Theoph %>% mutate(Dosage=map2_dbl(Wt,Dose, ~.x*.y))
source("script/data_plot.R")
Theoph=Theoph %>% convert_as_factor(Subject)
data_plot(Theoph, x=Time, y=conc, trace = Subject)+
  geom_line(aes(group=Subject))

Unit(concUnit="ug/L", doseUnit="mg")
df.pk=tblNCA(Theoph, key = "Subject", colTime="Time", colConc = "conc", 
       adm = "Extravascular", dur = 0, down = "Linear",
       dose = 320, doseUnit= "mg", timeUnit = "h", concUnit = "mg/L") %>% 
  select(Subject,CMAX,TMAX,AUCLST,AUCIFO)
df.pk
# library(ncar)
ncar::pdfNCA(fileName = "pdfNCA-Theoph.pdf", concData=Theoph, key = "Subject", 
             colTime = "Time", colConc = "conc", dose = 320, doseUnit = "mg", 
             timeUnit = "h", concUnit = "mg/L", down = "Linear")

###############################################################################
Time = Theoph %>% filter(Subject== 8) %>% pull(Time)
Concentration = Theoph %>% filter(Subject== 8) %>% pull(conc)

AUC(Time, Concentration)
AUC(Time, Concentration, down="Log")
Res =sNCA(Time, Concentration, dose = 320, concUnit = "mg/L")
Res["AUCLST"]
Res["TMAX"]
IntAUC(Time, Concentration, t1 = 0, t2 = 2, Res)

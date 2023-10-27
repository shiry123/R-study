##批量读取+合并Excel文件，每个带多个Sheet
files = list.files("datas/read_datas", pattern = "xlsx", 
                   full.names = TRUE, recursive = TRUE)
files
# library(readxl)
readPath = function(path) {
  map_dfr(readxl::excel_sheets(path), ~ readxl::read_xlsx(path, sheet = .x))
}
df = map_dfr(files, readPath)
df
# 合并一个excel中不同sheet
path = "rawdata/GW012/SP30-summary.xlsx"
# excel_sheets(path)
df <- map_dfr(readxl::excel_sheets(path), ~ readxl::read_xlsx(path, sheet = .x))  
df

#######################################################################
sp<- import("rawdata/GW012/GW012-valve-select.xlsx",sheet="sp") 
sp
df=sp %>% select(1:2,5:6) %>% 
  mutate(
    formulation=sjmisc::rec(Canister_Batch, rec = "AFS62A=R; 
                                                   W21111204=T;
                                                   else=NA",
                             append = FALSE),
    act=as.factor(act)) %>% 
      filter(formulation %in% c("R","T")) %>% drop_na()
str(df) 
df %>% 
  group_by(formulation) %>% 
  summarise(l=n_distinct(Canister_Batch),
            n=n_distinct(Canister_Id)/l,
            m=n_distinct(act),
            .groups="drop")

source("script/PBE_calc.R")
PBE_calc(df, formulation, Canister_Batch, Canister_Id, dv=Area_mm2,
        P.level=c("R","T"), logData=FALSE, m=40)


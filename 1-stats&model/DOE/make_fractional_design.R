### Make a fractional design
library(FrF2)
FrF2::FrF2(nruns=8, nfactors=3, blocks=2, randomize = FALSE)

FrF2(nfactors=7, resolution=5, ncenter=12, 
     center.distribute=6, randomize = FALSE)

BS.ex <- FrF2(16,7,hard=4)
design.info(BS.ex)
BS.ex

FrF2::FrF2(nruns=16,nfactors=4,  
                factor.names=paste0("x",1:4), ncenter=3, randomize=F) 

# Fractional design with 7 factors and resolution IV
frac_design_1 <- FrF2::FrF2(nfactors = 5, resolution = 4, randomize = FALSE) 
frac_design_1
# Export
write_csv(frac_design_1, "outfiles/frac_design_1.csv")

###PB筛选 DSD确定筛选
pb_design1 <- daewr::DefScreen(m=8, c=0, center=0, randomize=FALSE)
pb_design1


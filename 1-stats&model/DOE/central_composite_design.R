
# Generate a central composite design -------------------------------------

library(rsm)

cc_design_1 <- ccd(
  basis = 3,         # Number of factors
  n0    = 3,         # Number of central points
  randomize = FALSE # Not randomized
)

readr::write_csv(cc_design_1, file = "data/ccd_1.csv") # Export as CSV

head(as.data.frame(cc_design_1))

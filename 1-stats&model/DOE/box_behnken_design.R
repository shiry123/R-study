
# Generate a Box-Behnken design -------------------------------------------

library(rsm)

bb_design_1 <- bbd(
  k  = 4,            # Number of factors,
  n0 = 3,            # Number of center points,
  block = FALSE,     # Consider blocks or not in the design 
  randomize = FALSE  # Randomize or not the order experiments
)

head(as.data.frame(bb_design_1))

readr::write_csv(bb_design_1, file = "data/bbd_1.csv")

# Box-Behnken desing with units

bb_design_2 <- bbd(
  k  = 4,            # Four factors 
  n0 = 3,            # Three central points 
  block = FALSE,     # No blocks 
  randomize = FALSE, # Not randomized
  coding = list(
    x1 ~ (Temperature - 50) / 25,
    x2 ~ (Power - 210)/ 30,
    x3 ~ (time - 30) / 15,
    x4 ~ (Ethanol - 80) / 20
  )
)

head(bb_design_2)

bbd(k=3, n0 = 2, 
    coding =list(x1 ~ (Force - 20)/3, 
                 x2 ~ (Rate - 50)/10, 
                 x3 ~ Polish - 4))

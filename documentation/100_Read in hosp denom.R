#read in hospital denominator for 2019

hosp_a <- read.csv("Hosp2019.csv", header=TRUE, sep=",")
colnames(hosp_a)
saveRDS(hosp_a, "hosp_a.rds")

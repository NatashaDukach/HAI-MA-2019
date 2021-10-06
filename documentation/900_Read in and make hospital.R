#read data in

#this reads in the raw data CAUTI tabs from that
#godawful spreadsheet
library(readxl)
library(dplyr)
path <- "scraped_hai_MA_2018.xlsx"
sheets <- excel_sheets(path)

#create a list from the sheets into an object
lst <- lapply(sheets, function(i) read_excel(path, sheet = i))

#iterate over the list, pull out vars
#later we will put them together into working df
Hospital.name <- sapply(lst, function(x) x[[1]][1])
length(Hospital.name) # 70

icu.types <- sapply(lst, function(x) x[["ICU Type"]])
length(icu.types) # 70

infections <- sapply(lst, function(x) x[["Infections"]])
cath.days <- sapply(lst, function(x) x[["Catheter Days"]])
hosp.type <- sapply(lst, function(x) x[["Hospital Type"]])
num.patient.days <- sapply(lst, function(x) x[["Number of Patient Days"]])
num.admissions <- sapply(lst, function(x) x[["Number of Admissions"]])
num.icu.beds <- sapply(lst, function(x) x[["Number of ICU Beds"]])
num.beds <- sapply(lst, function(x) x[["Number of Beds"]])
profit.status <- sapply(lst, function(x) x[["Profit Status"]])

#assemble the dataframe
library(dplyr)
library(tidyr)
data <- as.data.frame(Hospital.name)
data$ICU.type <- icu.types
data$CAUTI.infections <- infections
data$Cath.days <- cath.days
data$Hospital.type <- hosp.type
data$Num.patient.days <- num.patient.days
data$Num.admissions <- num.admissions
data$Num.ICU.beds <- num.icu.beds
data$Num.beds <- num.beds
data$Profit.status <- profit.status








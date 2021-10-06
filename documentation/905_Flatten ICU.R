#rename data
data_a <- data

#Natasha says we have to remove the ICUs to unlist the list
#and anyway, we do not want hospitals without ICUs in the study
#so we will remove them now.

nrow(data_a)
data_b <- data_a[!data_a$ICU.type == "NULL", ] 
nrow(data_b)

#up to now, all of the columns in data_b are lists
#because of the recursive unflat nature of the ICU table
#even when there is a one to one relationship.
#To unlist the data, we had to get rid of hospitals with no ICUs.
#This unlists the data

data_c <- data.frame(Hospital.name = rep(data_b$Hospital.name, 
	sapply(data_b$CAUTI.infections, length)), 
	ICU.type = unlist(data_b$ICU.type),
	CAUTI.infections = unlist(data_b$CAUTI.infections), 
	Cath.days = unlist(data_b$Cath.days), 
	Hospital.type = zoo::na.locf(unlist(data_b$Hospital.type),na.rm = F),
	Num.patient.days = zoo::na.locf(unlist(data_b$Num.patient.days), na.rm = F), 
	Num.admissions = zoo::na.locf(unlist(data_b$Num.admissions), na.rm = F), 
	Num.ICU.beds = zoo::na.locf(unlist(data_b$Num.ICU.beds), na.rm = F), 
	Num.beds = zoo::na.locf(unlist(data_b$Num.beds), na.rm = F), 
	Profit.status = zoo::na.locf(unlist(data_b$Profit.status), na.rm = F))

#this flattens the dataset
#the next step is to split it



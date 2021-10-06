#add keys
xwalk_a <- read.csv("Xwalk.csv", header=TRUE, sep=",")
#remove unwanted columns
xwalk <- xwalk_a[,1:2]
colnames(xwalk)
#merge onto data_c

nrow(data_c)
colnames(data_c)
data_d <- cbind(data_c, xwalk)
nrow(data_d)
colnames(data_d)
#drop second Hospital.name
data_e <- data_d[-c(11)]
colnames(data_e)
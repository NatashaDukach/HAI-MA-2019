#read in analytic dataset
hosp <- readRDS("hosp_b.rds")

#create label var
hosp$Label <- "Non-Teaching"
hosp$Label[hosp$Teaching == 1] <- "Teaching"
table(hosp$Label, hosp$Teaching)
class(hosp$Label)

#create factor variable and label it "Type"
#so the legend is labeled "Type"
hosp$Type <- as.factor(hosp$Label)

library(ggplot2)

dkbl <- c("#10077c")
prpl <- c("#37088f")
viol <- c("#5d0499")
prpl_rose <- c("#93069d")
more_rose <- c("#b40787")
brite_red <- c("#bf0546")
supernova <- c("#ab060e")

color_values <- c(dkbl, supernova)

ggplot(hosp, aes(x=Type, y=H9Beds, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0,max(hosp$H9Beds), by=50)) +
	labs(x = "Hospital Teaching Status", y = "Number of Beds") +
	guides(fill=guide_legend("Hospital Type")) +
	theme_classic()

#ggsave
ggsave("Box Plot Beds.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

color_values <- c(prpl, supernova)

ggplot(hosp, aes(x=Type, y=H9ICUBeds, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0,max(hosp$H9ICUBeds), by=25)) +
	labs(x = "Hospital Teaching Status", y = "Number of ICU Beds") +
	guides(fill=guide_legend("Hospital Type")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU Beds.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#this is for comma in y-axis labels
library(scales)

color_values <- c(viol, supernova)
admit_breaks <- seq(0,max(hosp$H9NumAdm), by=5000)

ggplot(hosp, aes(x=Type, y=H9NumAdm, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=admit_breaks, labels=comma) +
	labs(x = "Hospital Teaching Status", y = "Number of Annual Admissions") +
	guides(fill=guide_legend("Hospital Type")) +
	theme_classic()

#ggsave
ggsave("Box Plot Admissions.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300)

#patient days
color_values <- c(prpl_rose, supernova)
patday_breaks <- seq(0,max(hosp$H9NumPatDays/1000), by=25)

ggplot(hosp, aes(x=Type, y=H9NumPatDays/1000, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=patday_breaks, labels=comma) +
	labs(x = "Hospital Teaching Status", y = "Annual Patient Days (in thousands)") +
	guides(fill=guide_legend("Hospital Type")) +
	theme_classic()

#ggsave
ggsave("Box Plot Patient Days.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300)

#look at TPS
nrow(hosp)
TPS <- subset(hosp, !is.na(hosp$HospPerfScore))
nrow(TPS)
color_values <- c(dkbl, supernova)
tps_breaks <- seq(0,max(TPS$HospPerfScore), by=5)

ggplot(TPS, aes(x=Type, y=HospPerfScore, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=tps_breaks) +
	labs(x = "Hospital Teaching Status", y = "Medicare Total Performance Score") +
	guides(fill=guide_legend("Hospital Type")) +
	theme_classic()

#ggsave
ggsave("Box Plot TPS.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300)




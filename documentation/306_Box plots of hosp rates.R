#box plots of rates
hosp <- readRDS("hosp_e.rds")

#gradation from Anoud's paper
#I'm using supernova for Training when comparing to non-training
dkbl <- c("#10077c")
prpl <- c("#37088f")
viol <- c("#5d0499")
prpl_rose <- c("#93069d")
more_rose <- c("#b40787")
brite_red <- c("#bf0546")
supernova <- c("#ab060e")

#green/yellow gradation
#dark blue/green to green/yellow
blgr <- c("#11645a")
dkseafm <- c("#0a9184")
leaf <- c("#76cecf")
ygrn <- c("#5aa35c")
transition <- c("#81ae59")
mustardy <- c("#daba1a")

#create Label var
hosp$Label <- "Non-Teaching"
hosp$Label[hosp$Teaching == 1] <- "Teaching"
table(hosp$Label, hosp$Teaching)
class(hosp$Label)

#for CLABSI, just keep ones with a CLABSI rate
nrow(hosp)
CLABSI <- subset(hosp, !is.na(CLABSIRateCrude))
nrow(CLABSI)

library(ggplot2)

color_values <- c(mustardy, supernova)
ggplot(CLABSI, aes(x=Label, y=CLABSIRateCrude, fill=Label)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRateCrude), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "Yearly CLABSI Cases Divided by Catheter Days (Crude Rate)") +
	guides(fill=guide_legend("Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot Hosp CLABSI crude Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#for CAUTI, just keep ones with a CAUTI rate
nrow(hosp)
CAUTI <- subset(hosp, !is.na(CAUTIRateCrude))
nrow(CAUTI)

color_values <- c(mustardy, supernova)
ggplot(CAUTI, aes(x=Label, y=CAUTIRateCrude, fill=Label)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRateCrude), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "Yearly CAUTI Cases Divided by Catheter Days (Crude Rate)") +
	guides(fill=guide_legend("Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot Hosp CAUTI Crude Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 


#for combined, just keep ones with a combined rate
nrow(hosp)
combined <- subset(hosp, !is.na(CombinedRateCrude))
nrow(combined)

color_values <- c(mustardy, supernova)
ggplot(combined, aes(x=Label, y=CombinedRateCrude, fill=Label)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(combined$CombinedRateCrude), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "Yearly Combined CLABSI & CAUTI Cases \n Divided by Total Catheter Days (Crude Rate)") +
	guides(fill=guide_legend("Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot Hosp Combined Crude Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#adding on adjusted CAUTI rate so I can compare
#for CAUTI, just keep ones with a CAUTI rate
nrow(hosp)
CAUTI <- subset(hosp, !is.na(CAUTIRateCrude))
nrow(CAUTI)

color_values <- c(dkseafm, supernova)
ggplot(CAUTI, aes(x=Label, y=CAUTIRateAdj, fill=Label)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRateAdj), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "CAUTI Rate Adjusted by ICU Type") +
	guides(fill=guide_legend("Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot Hosp CAUTI Adjusted Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 


#box plots of ICU vars
ICU <- readRDS("ICU_d.rds")
colnames(ICU)

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

library(ggplot2)
#Let's look at the CLABSI and CAUTI rates 
#What is killing everyone?
#let's start by looking at the ICU classifications
#create Label var
ICU$Label4 <- "Unknown"
ICU$Label4[ICU$ICUGrp == 1] <- "Medical"
ICU$Label4[ICU$ICUGrp == 2] <- "Medical/Surgical"
ICU$Label4[ICU$ICUGrp == 3] <- "Surgical"
ICU$Label4[ICU$ICUGrp == 4] <- "Cardiac"
ICU$Label4[ICU$ICUGrp == 5] <- "Pediatric"
ICU$Label4[ICU$ICUGrp == 6] <- "Specialty"
table(ICU$Label4, ICU$ICUGrp)

#create factor variable and label it "ICUCat"
#so the legend is labeled "ICUCat"
ICU$ICUCat <- as.factor(ICU$Label4)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

color_values <- c(blgr, dkseafm, leaf, ygrn, transition, mustardy)
ggplot(CLABSI, aes(x=ICUCat, y=CLABSIRate, fill=ICUCat)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRate), by=0.05)) +
	labs(x = "Type of ICU", y = "Yearly CLABSI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("Type of ICU")) +
	theme_classic() +
	theme(axis.text.x=element_blank())

ggsave("Box Plot ICU CLABSI Rates-ICU Type.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#for CAUTI, just keep ones with any CAUTIdays
nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

ggplot(CAUTI, aes(x=ICUCat, y=CAUTIRate, fill=ICUCat)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRate), by=0.05)) +
	labs(x = "Type of ICU", y = "Yearly CAUTI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("Type of ICU")) +
	theme_classic() +
	theme(axis.text.x=element_blank())

ggsave("Box Plot ICU CAUTI Rates-ICU Type.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#by teaching status
max(ICU$CLABSIRate)

#create label var
ICU$Label <- "Non-Teaching"
ICU$Label[ICU$Teaching == 1] <- "Teaching"
table(ICU$Label, ICU$Teaching)
class(ICU$Label)

#create factor variable and label it "Type"
#so the legend is labeled "Type"
ICU$Type <- as.factor(ICU$Label)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

color_values <- c(mustardy, supernova)
ggplot(CLABSI, aes(x=Type, y=CLABSIRate, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRate), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "Yearly CLABSI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CLABSI Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#for CAUTI, just keep ones with any CAUTIdays
nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

color_values <- c(mustardy, supernova)
ggplot(CAUTI, aes(x=Type, y=CAUTIRate, fill=Type)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRate), by=0.05)) +
	labs(x = "Hospital Teaching Status", y = "Yearly CAUTI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CAUTI Rates-Teaching.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#let's look at other relationships to the outcome
#medical school affiliation
#create lable var
ICU$Label2 <- "No Affiliation"
ICU$Label2[ICU$MedAfilFlag == 1] <- "Affiliated"
table(ICU$Label2, ICU$MedAfilFlag)
class(ICU$Label2)

#create factor variable and label it "Affiliation"
#so the legend is labeled "Affiliation"
ICU$Affiliation <- as.factor(ICU$Label2)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

color_values <- c(supernova, leaf)
ggplot(CLABSI, aes(x=Affiliation, y=CLABSIRate, fill=Affiliation)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRate), by=0.05)) +
	labs(x = "Medical School Affiliation Status", y = "Yearly CLABSI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("Affiliation Status of Hospital with ICU")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CLABSI Rates-Affiliation.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#for CAUTI, just keep ones with any CAUTIdays
nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

color_values <- c(supernova, leaf)
ggplot(CAUTI, aes(x=Affiliation, y=CAUTIRate, fill=Affiliation)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRate), by=0.05)) +
	labs(x = "Medical School Affiliation Status", y = "Yearly CAUTI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("Affiliation Status of Hospital with ICU")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CAUTI Rates-Affiliation.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#Profit status
#create label var
ICU$Label3 <- "Not-for-Profit"
ICU$Label3[ICU$ProfitID == 1] <- "For-Profit"
table(ICU$Label3, ICU$ProfitID)
class(ICU$Label3)

#create factor variable and label it "Affiliation"
#so the legend is labeled "Profit"
ICU$Profit <- as.factor(ICU$Label3)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

color_values <- c(ygrn, supernova)
ggplot(CLABSI, aes(x=Profit, y=CLABSIRate, fill=Profit)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRate), by=0.05)) +
	labs(x = "Hospital Profit Status", y = "Yearly CLABSI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CLABSI Rates-Profit.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#for CAUTI, just keep ones with any CAUTIdays
nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

color_values <- c(ygrn, supernova)
ggplot(CAUTI, aes(x=Profit, y=CAUTIRate, fill=Profit)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRate), by=0.05)) +
	labs(x = "Hospital Profit Status", y = "Yearly CAUTI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CAUTI Rates-Profit.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

#Let's look at bad TPS score
ICU$Label5 <- "TPS Quartile 2-4"
ICU$Label5[ICU$TPSQ1== 1] <- "TPS Quartile 1"
table(ICU$Label5, ICU$TPSQ1)
class(ICU$Label5)

#factor
ICU$TPSCat <- as.factor(ICU$Label5)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

color_values <- c(supernova, viol)
ggplot(CLABSI, aes(x=TPSCat, y=CLABSIRate, fill=TPSCat)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CLABSI$CLABSIRate), by=0.05)) +
	labs(x = "Quartile of Hospital TPS", y = "Yearly CLABSI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CLABSI Rates-TPS Quartile.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 

nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

color_values <- c(supernova, viol)
ggplot(CAUTI, aes(x=TPSCat, y=CAUTIRate, fill=TPSCat)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(CAUTI$CAUTIRate), by=0.05)) +
	labs(x = "Quartile of Hospital TPS", y = "Yearly CAUTI Cases Divided by Catheter Days (Rate)") +
	guides(fill=guide_legend("ICU within this Type of Hospital")) +
	theme_classic()

#ggsave
ggsave("Box Plot ICU CAUTI Rates-TPS Quartile.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 



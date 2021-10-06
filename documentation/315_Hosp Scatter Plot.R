#focusing on hospital level CAUTI rates
#we want to see how the adjusted rates match the raw rates
hosp <- readRDS("hosp_e.rds")
colnames(hosp)

library(ggplot2)

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

#create label var
hosp$Label <- "Non-Teaching"
hosp$Label[hosp$Teaching == 1] <- "Teaching"
table(hosp$Label, hosp$Teaching)
class(hosp$Label)

#create factor variable and label it "Type"
#so the legend is labeled "Type"
hosp$Type <- as.factor(hosp$Label)

#restrict to those who have a CAUTI rate
nrow(hosp)
plot_data <- subset(hosp, !is.na(CAUTIRateCrude))
nrow(plot_data)

#I want to know the correlation
cor.test(plot_data$CAUTIRateCrude, plot_data$CAUTIRateAdj)

ggplot(plot_data, aes(x=CAUTIRateCrude, y=CAUTIRateAdj, color=Type)) +
	geom_point() +
	scale_y_continuous(breaks=seq(0, max(plot_data$CAUTIRateAdj), by=0.05)) +
	scale_x_continuous(breaks=seq(0, max(plot_data$CAUTIRateCrude), by=0.05)) +
	labs(x = "Crude CAUTI Rate", y = "CAUTI Rate Adjusted for ICU Type") +
	scale_color_manual(name = "Teaching Status of Hospital",
		labels = c("Non-Teaching", "Teaching"),
		values = c(supernova, dkbl)) +
	guides(color=guide_legend("Teaching Status of Hospital")) +
	theme_classic()
#ggsave
ggsave("Scatter Plot Hosp CAUTI Crude by CAUTI Adj.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 





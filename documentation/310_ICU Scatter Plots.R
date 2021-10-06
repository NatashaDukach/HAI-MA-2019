#Scatter plots
#Start with ICUs
ICU <- readRDS("ICU_d.rds")
colnames(ICU)

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
ICU$Label <- "Non-Teaching"
ICU$Label[ICU$Teaching == 1] <- "Teaching"
table(ICU$Label, ICU$Teaching)
class(ICU$Label)

#create factor variable and label it "Type"
#so the legend is labeled "Type"
ICU$Type <- as.factor(ICU$Label)

#restrict to those who are eligible to have a CLABSI and a CAUTI rate
nrow(ICU)
plot_data <- subset(ICU, NoCLABSIDays == 0 | NoCAUTIDays == 0)
nrow(plot_data)

#I want to know the correlation
cor.test(plot_data$CLABSIRate, plot_data$CAUTIRate)

ggplot(plot_data, aes(x=CLABSIRate, y=CAUTIRate, color=Type)) +
	geom_point() +
	scale_y_continuous(breaks=seq(0, max(plot_data$CAUTIRate), by=0.05)) +
	scale_x_continuous(breaks=seq(0, max(plot_data$CLABSIRate), by=0.05)) +
	labs(x = "Annual CLABSI cases divided by catheter days", y = "Annual CAUTI cases divided by catheter days") +
	scale_color_manual(name = "Teaching Status of Hospital",
		labels = c("Non-Teaching", "Teaching"),
		values = c(supernova, dkbl)) +
	guides(color=guide_legend("Teaching Status of Hospital")) +
	theme_classic()
#ggsave
ggsave("Scatter Plot ICU CLABSI by CAUTI.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300) 



hosp <- readRDS("hosp_graded.rds")

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

library(ggplot2)

ggplot(hosp, aes(x="", y=yhat)) +
  	geom_boxplot() + 
	scale_y_continuous(breaks=seq(0, max(hosp$yhat), by=0.005)) +
	labs(x = "All Hospitals", y = expression(yhat)) +
	theme_classic()
ggsave("Box Plot yhat All Hosp.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300)

color_values <- c(mustardy, supernova)
ggplot(hosp, aes(x=Label, y=yhat, fill=Label)) +
  	geom_boxplot() + 
	scale_fill_manual(values=color_values) +
	scale_y_continuous(breaks=seq(0, max(hosp$yhat), by=0.005)) +
	labs(x = "Hospital Teaching Status", y = "\u0177") +
	guides(fill=guide_legend("Type of Hospital")) +
	theme_classic()
ggsave("Box Plot yhat by Hosp Type.png", 
	units = c("in"),
	width = 8,
	height = 5.5,
	dpi = 300)


#try to do some ICU regressions
ICU <- readRDS("ICU_d.rds")
colnames(ICU)
class(ICU$OrgID)

#for CLABSI, just keep ones with any CLABSIdays
nrow(ICU)
CLABSI <- subset(ICU, NoCLABSIDays == 0)
nrow(CLABSI)

CLABSI_Model1 <- lm(formula = CLABSIRate ~ OrgID + WardFlag + 
		MedSurgFlag + SurgFlag + CardiacFlag + PedFlag + SpecFlag +
		TeachFlag + ProfitFlag + TPSQ1, data = CLABSI)
summary(CLABSI_Model1)

CLABSI_Model2 <- lm(formula = CLABSIRate ~ WardFlag + 
		MedSurgFlag + SurgFlag + CardiacFlag + PedFlag + SpecFlag +
		TeachFlag + ProfitFlag + TPSQ1, data = CLABSI)
summary(CLABSI_Model2)

#for CAUTI, just keep ones with any CAUTIdays
nrow(ICU)
CAUTI <- subset(ICU, NoCAUTIDays == 0)
nrow(CAUTI)

CAUTI_Model1 <- lm(formula = CAUTIRate ~ OrgID + WardFlag + 
		MedSurgFlag + SurgFlag + CardiacFlag + PedFlag + SpecFlag +
		TeachFlag + ProfitFlag + TPSQ1, data = CAUTI)
summary(CAUTI_Model1)

CAUTI_Model2 <- lm(formula = CAUTIRate ~ WardFlag + 
		MedSurgFlag + SurgFlag + CardiacFlag + PedFlag + SpecFlag +
		TeachFlag + ProfitFlag + TPSQ1, data = CAUTI)
summary(CAUTI_Model2)
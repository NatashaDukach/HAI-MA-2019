#we need to assemble ICU records from the
#CLABSI and CAUTI tables scraped by Natasha
#we will assemble an ICU table on the basis of CLABSI and add CAUTI

## Removing hidden commas from numbers - FIXED CODE ISSUE

# replaceCommas<-function(x){
#   x<-as.numeric(gsub("\\,", "", x))
# }

CLAB_a <- read.csv("CLABSI2019.csv", header = TRUE, sep = ",")
colnames(CLAB_a)

# CLAB_a$H9CLDays <- replaceCommas(CLAB_a$H9CLDays)

# save as new .csv without commas in CLDays
# write.csv(CLAB_a, "fixed_CLABSI2019.csv")

#concat index
CLAB_a$IDX <- paste(CLAB_a$OrgID, CLAB_a$H9ICUType_CLABSI, sep="") # in the original file there ius a mistake in the name H& instead of H9

#read in the CAUTI ICU table
CAUT_a <- read.csv("CAUTI2019.csv", header = TRUE, sep = ",")
colnames(CAUT_a)

# CAUT_a$H9CathDays <- replaceCommas(CAUT_a$H9CathDays)
# 
# write.csv(CAUT_a, "fixed_CAUTI2019.csv")

#concat index
CAUT_a$IDX <- paste(CAUT_a$OrgID, CAUT_a$H9ICUType_CAUTI, sep="")

#try to merge them together
nrow(CLAB_a)
nrow(CAUT_a)
ICU_a <- merge(x = CLAB_a, y = CAUT_a, by = "IDX", all.x = TRUE)
nrow(ICU_a)

#create a rowID
ICU_a$ICURowID <- seq(1, nrow(ICU_a), 1)

#get rid of one of the OrgID columns because they became duplicated
#in the merge
#need to remove the one from CAUTI because they all have records
#in CLABSI
#so that is OrgID.y

colnames(ICU_a)
ICU_b <- ICU_a[, -7]
colnames(ICU_b)

#get rid of one of the ICU Type columns because they became duplicated
#in the merge

colnames(ICU_b)
ICU_c <- ICU_b[, -c(2,7,8)]
colnames(ICU_c)

#rename columns to a standard name
names(ICU_c) <- c("IDX", "OrgID", "ICUType", "CLABSINum", "CLABSIDays", "CAUTINum", "CAUTIDays", "ICURowID")
colnames(ICU_c)

write.csv(ICU_c,"HAI_ICU_table.csv")
#export for now before messing with adding vars
saveRDS(ICU_c, "ICU_c.rds")
ICU_c$CAUTIDays

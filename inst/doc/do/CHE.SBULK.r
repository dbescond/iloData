#############################################################################################
#	# call user-defined function 
#MY_Chem <- "I:/COMMON/A0 Short term indicators/" 
#MY_COLLECTION <- "STI" # "YEARLY" "EAPEP"  "YTHSTAT" "STI"
#MY_ChemNEW <- paste(MY_Chem,"Collection/COU_CHE/",sep="")
setwd(paste0(ilo:::path$data, 'CHE/SBULK/'))

require(openxlsx)

DATA_DA <- read.xlsx(paste0("./CHE_Query.xlsx"), sheet="Database_DA")  
DATA_EA_SECTOR <- read.xlsx(paste0("./CHE_Query.xlsx"), sheet="Database_EA_SECTOR")  
DATA_EA_ISIC4 <- read.xlsx(paste0("./CHE_Query.xlsx"), sheet="Database_EA_ISIC4")  


TIME <- colnames(DATA_DA)[substr(colnames(DATA_DA),1,1)%in%"Y"]
DATA_DA <- reshape(DATA_DA,direction = "long" , v.names = "OBS_VALUE", ,idvar= "ID", varying = list(TIME), times = TIME,timevar = "TIME_PERIOD")

TIME <- colnames(DATA_EA_SECTOR)[substr(colnames(DATA_EA_SECTOR),1,1)%in%"Y"]
DATA_EA_SECTOR <- reshape(DATA_EA_SECTOR,direction = "long" , v.names = "OBS_VALUE", ,idvar= "ID", varying = list(TIME), times = TIME,timevar = "TIME_PERIOD")

TIME <- colnames(DATA_EA_ISIC4)[substr(colnames(DATA_EA_ISIC4),1,1)%in%"Y"]
DATA_EA_ISIC4 <- reshape(DATA_EA_ISIC4,direction = "long" , v.names = "OBS_VALUE", ,idvar= "ID", varying = list(TIME), times = TIME,timevar = "TIME_PERIOD")





Y <- bind_rows(DATA_EA_ISIC4,DATA_EA_SECTOR,DATA_DA) %>% mutate(NOTES_SOURCE_CODE = paste0('R1:3903_', NOTES_SOURCE_CODE))

REF <- levels(as.factor(substr(Y$SOURCE_CODE,1,2)))


for (i in 1:length(REF)){
X <- Y[substr(Y$SOURCE_CODE,1,2)%in%REF[i],]
save(X,file = paste("./0.Ready/COU_CHE_",REF[i],".Rdata",sep=""))
}


REF <- cbind(PATH = paste("J:/COMMON/STATISTICS/DPAU/DATA/CHE/SBULK/output/COU_CHE_",REF,".Rdata",sep=""),ID = NA, Types  ="CL", REF = NA)
write.csv(REF,paste("./FileToLoad.csv",sep=""),row.names = FALSE,na="")







rm(list=ls(all=TRUE)) 
q(save = "no", status = 0, runLast = FALSE)
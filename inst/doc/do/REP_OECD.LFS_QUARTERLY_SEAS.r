#############################################################################################
#
	# Workspace empty
# load the repository name
init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }; rm(cleanTemp)
require(RSelenium)
setwd(paste0(ilo:::path$data, '/REP_OECD/LFS_QUARTERLY_SEAS/'))
Sys.setenv(http_proxy="")
#source(paste("./Processing/R_Functions/A_Load_Functions.R", sep=""))
INPUT <- paste0(getwd(), '/input/')


 
Mapping_File <- readxl:::read_excel("./ReadME_OECD_BA_SA.xlsx", sheet="File")  %>% filter(!ID%in%NA) %>% as.data.frame
Mapping_Definition <- readxl:::read_excel("./ReadME_OECD_BA_SA.xlsx", sheet="Definition")  
ReadMeSource <- readxl:::read_excel("./ReadME_OECD_BA_SA.xlsx", sheet="MappingSource")  


source('./do/REP_OECD.LFS_QUARTERLY_SEAS_meta.r')


header_ILO <- c("INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE","NOTES_FREQUENCY_CODE")

##########################################################



for(i in 1:length(Mapping_File$NAME)){

shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                ,  browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/text/csv"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()   

remDr$navigate(Mapping_File$URL[i]) # go to query webpage

	Sys.sleep(10)	

remDr$executeScript("redirectToSurvey('csv');") # open csv download pop up

remDr$switchToFrame('DialogFrame')


	Sys.sleep(10)	

remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_rbCustomLayout')$clickElement()
	Sys.sleep(3)	
remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_cbLabel')$clickElement()
	Sys.sleep(3)	
remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_btnExportCSV')$clickElement()
	Sys.sleep(350)	
	
	


remDr$close()
remDr$closeServer()


test <- list.files("C:\\temp\\")
test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
file.rename(paste0("C:\\temp\\",test),paste0(INPUT, Mapping_File$NAME[i],'.csv'))
invisible(gc(reset = TRUE))

Base <- read_csv(paste0(INPUT,Mapping_File$NAME[i],".csv"))


colnames(Base) <- gsub(' ', '.', colnames(Base))

Base <- Base %>% select(-Unit.Code, -PowerCode.Code, -Reference.Period.Code) %>% rename(Flags = Flag.Codes)


header_NAT <- colnames(Base)[!colnames(Base)%in%c("TIME","Value","Flags")]
REF_MAPPING <- Mapping_Definition[Mapping_Definition$File%in%Mapping_File[i,"ID"],]
header_NAT <- colnames(REF_MAPPING[,colnames(REF_MAPPING)%in%header_NAT])
REF_MAPPING <- REF_MAPPING[,c(colnames(REF_MAPPING)[colnames(REF_MAPPING)%in%header_ILO],header_NAT)]

Base <- Base %>% filter(!Flags%in%"M") %>% select(-contains('.Code'))


invisible(gc(reset = TRUE))


My_list <- vector("list", length(header_NAT))
MY_NEW <- cbind(ID_PASS = Base[,colnames(Base)%in%colnames(REF_MAPPING)[3]],
				rep(NA,nrow(Base)),
				rep(NA,nrow(Base)),
				Base)

				
				rm(Base)
colnames(MY_NEW)[1:3] <- c('ID_PASS', header_ILO)


for (k in 4:ncol(REF_MAPPING)){
MY_NEW$ID_PASS <- paste(MY_NEW$ID_PASS,MY_NEW[,colnames(MY_NEW)%in%colnames(REF_MAPPING)[k]],sep=";")
}


MY_MATRIX <- MY_NEW[1,c("ID_PASS","INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE","LOCATION","NOTES_FREQUENCY_CODE","TIME","Value","Flags")]
MY_MATRIX <- as.data.frame(MY_MATRIX)

for (j in 1:nrow(REF_MAPPING)){

	MY_NEW[,2]<- REF_MAPPING[j,"INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE"]
	MY_NEW[,3]<- REF_MAPPING[j,"NOTES_FREQUENCY_CODE"]


	for (k in 3:ncol(REF_MAPPING)){
		test <- levels(as.factor(unlist(str_split(REF_MAPPING[j,colnames(REF_MAPPING)[k]],";"))))
		My_list[[k]] <- test
	}
	Y <- My_list[[3]]


	for(k in 4:ncol(REF_MAPPING)){
		Y <- paste(Y,My_list[[k]],sep=";")
	}


MY_MATRIX <- as.data.frame(MY_MATRIX) %>% bind_rows(
			MY_NEW[MY_NEW$ID_PASS%in%Y,colnames(MY_NEW)%in%c("ID_PASS","INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE","LOCATION","NOTES_FREQUENCY_CODE","TIME","Value","Flags")])
}
rm(MY_NEW)

MY_MATRIX <- MY_MATRIX[-1,]

MY_MATRIX <- MY_MATRIX[!MY_MATRIX$Value%in%NA,]
MY_MATRIX <- as.data.frame(MY_MATRIX) %>% as.tbl

######################### NEXT STEP

# MY_MATRIX$ID_PASS <- paste(MY_MATRIX$LOCATION,MY_MATRIX[,"INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE"],MY_MATRIX$TIME,sep="/")

Y <- MY_MATRIX %>% 
					select(-ID_PASS) %>% 
					unite_("ID_PASS", c("LOCATION","INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE", 'TIME'), sep = "/", remove = FALSE) %>% 
					mutate(Value = as.numeric(Value)) %>% 
					separate_('INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE/CLASSIF2_CODE', c("INDICATOR_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE"), sep = "/", remove = TRUE) %>% 
					mutate( TIME = gsub('-Q', '_Q0', TIME, fixed = TRUE), 
							TIME = paste0('Y', TIME), 
							TIME = gsub("-","_M",TIME, fixed = TRUE)) %>%
					rename(	ID = ID_PASS, 
							COUNTRY_CODE = LOCATION, 
							TIME_PERIOD = TIME, 
							OBS_VALUE = Value, 
							OBS_STATUS = Flags
							) %>% 
					mutate(	SOURCE_CODE = Mapping_File$SOURCE_CODE[i], 
							NOTES_CLASSIF_CODE = as.character(NA), 
							NOTES_INDICATOR_CODE = as.character(NA), 
							NOTES_SOURCE_CODE = as.character(NA), 
							ADD_STATUS = 'B', 
							ADD_REPOSITORY = 'OECD', 
							OBS_STATUS = tolower(as.character(OBS_STATUS))
							)

rm(MY_MATRIX)

invisible(gc(reset = TRUE))

# mapping source and survey



Y$ID <- paste(Y$COUNTRY_CODE,Y$SOURCE_CODE,sep="/")

Y <- Y %>% select(-SOURCE_CODE) %>% left_join(select(ReadMeSource, ID, SOURCE_CODE = REF), by = 'ID')



Y$ID <- paste(Y$COUNTRY_CODE,Y$SOURCE_CODE,Y$INDICATOR_CODE,Y$SEX_CODE,Y$CLASSIF1_CODE,Y$CLASSIF2_CODE,sep="/")

# Y$OBS_VALUE <- as.numeric(Y$OBS_VALUE)

Y[substr(Y$INDICATOR_CODE,nchar(Y$INDICATOR_CODE)-1,nchar(Y$INDICATOR_CODE))%in%"NB","OBS_VALUE"] <- Y[substr(Y$INDICATOR_CODE,nchar(Y$INDICATOR_CODE)-1,nchar(Y$INDICATOR_CODE))%in%"NB","OBS_VALUE"]

			
load(file = paste0(INPUT, '/META_OECD_BA_SA.Rdata'))
	

MY_MATRIX <- MY_MATRIX %>% separate_("ID",c("COUNTRY_CODE","SOURCE_CODE","INDICATOR_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE"), sep = "/", remove = TRUE)


MY_MATRIX <- MY_MATRIX %>% mutate(INDICATOR_CODE = paste0(substr(INDICATOR_CODE,1,7), "1", substr(INDICATOR_CODE, nchar(INDICATOR_CODE)-2, nchar(INDICATOR_CODE))))

MY_MATRIX$ID <- paste(MY_MATRIX$COUNTRY_CODE, MY_MATRIX$SOURCE_CODE, MY_MATRIX$INDICATOR_CODE, MY_MATRIX$SEX_CODE, MY_MATRIX$CLASSIF1_CODE, MY_MATRIX$CLASSIF2_CODE,sep="/")

Y <- Y %>% select(-NOTES_CLASSIF_CODE) %>% left_join(select(MY_MATRIX,ID,NOTES_CLASSIF_CODE), by = "ID")






Y$ID <- paste(Y$COUNTRY_CODE,Y$SOURCE_CODE,Y$INDICATOR_CODE,sep="/")
MY_MATRIX$ID <- paste(MY_MATRIX$COUNTRY_CODE,MY_MATRIX$SOURCE_CODE,MY_MATRIX$INDICATOR_CODE,sep="/")

MY_MATRIX <- distinct(MY_MATRIX, ID, .keep_all = TRUE)



Y <- Y %>% select(-NOTES_FREQUENCY_CODE) %>% left_join(select(MY_MATRIX,ID,NOTES_FREQUENCY_CODE), by = "ID")
Y <- Y %>% select(-NOTES_INDICATOR_CODE) %>% left_join(select(MY_MATRIX,ID,NOTES_INDICATOR_CODE), by = "ID")
Y <- Y %>% select(-NOTES_SOURCE_CODE) %>% left_join(select(MY_MATRIX,ID,NOTES_SOURCE_CODE), by = "ID")


Y <- Y %>% mutate(NOTES_SOURCE_CODE = ifelse(NOTES_SOURCE_CODE%in%NA, "R1:2382", paste0("R1:2382_", NOTES_SOURCE_CODE)))









Y[Y$NOTES_FREQUENCY_CODE%in%NA,"NOTES_FREQUENCY_CODE"] <- "P"





Y$ID <- NA
X <- Y
rm(Y, MY_MATRIX,REF_MAPPING)

save(X,file = paste(INPUT,Mapping_File$ID[i],".Rdata",sep=""))
rm(X)

invisible(gc(reset = TRUE))

print(Mapping_File$NAME[i])

}



# group all OECD SA


for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste0(INPUT,Mapping_File$ID[i],".Rdata"))
PASS <- X
if(!empty(PASS)){
ifelse(i==1,Y <- PASS, Y <- bind_rows(Y,PASS))
}
rm(PASS)


}



REF <- levels(as.factor(Y$COUNTRY_CODE))

 Y <- Y %>% as.tbl %>%  
			mutate(COLLECTION_CODE = 'STI' ) %>%
		select(	collection = COLLECTION_CODE,  
				ref_area = COUNTRY_CODE, 
				source = SOURCE_CODE, 
				indicator = INDICATOR_CODE, 
				sex = SEX_CODE, 
				classif1 = CLASSIF1_CODE, 
				classif2 = CLASSIF2_CODE, 
				time = TIME_PERIOD, 
				obs_value = OBS_VALUE, 
				obs_status  = OBS_STATUS, 
				freq_code = NOTES_FREQUENCY_CODE, 
				note_classif = NOTES_CLASSIF_CODE, 
				note_indicator = NOTES_INDICATOR_CODE, 
				note_source = NOTES_SOURCE_CODE ) %>% 
		mutate(	time = paste0(str_sub(time, 2,5), str_sub(time,7,9)), 
				time = ifelse(str_sub(time,5,5) %in% 'Q', paste0(str_sub(time, 1,5), str_sub(time, -1,-1)), time)) %>%  
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE)))
		

 
# split and save by country
for (i in 1:length(REF)){
X <- Y %>% filter(ref_area %in%REF[i])
save(X,file = paste("./output/REP_OECD_",REF[i],".Rdata",sep=""))
print(REF[i])
}


LOAD <- cbind(PATH = paste0(getwd(), "/output/REP_OECD_",REF,".Rdata"),ID = NA, Types  ="OECD_ilostat", REF = REF)
write.csv(LOAD,"./FileToLoad.csv",row.names = FALSE,na="")

rm(X, Y, LOAD, REF)

final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
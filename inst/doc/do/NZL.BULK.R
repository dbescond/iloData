#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  May 2016. last update May 2017
#############################################################################
Target <- "NZL"
init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(Ariane,quietly =TRUE)
require(lubridate, quietly =TRUE)
require(RSelenium, quietly =TRUE)
require(readxl,quietly =TRUE)
setwd(paste0(ilo:::path$data, '/',Target,'/BULK/'))
Sys.setenv(http_proxy="")
Sys.setenv(ftp_proxy="")
source(paste0(ilo:::path$data, '/',Target,'/BULK/','do/',Target,'.BULK_functions.r'))

INPUT <- paste0(ilo:::path$data, '/',Target,'/BULK/input/')

Mapping_File <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="File", guess_max = 1000)  %>% filter(!ID%in%NA) %>% as.data.frame
Mapping_Definition <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="Definition", guess_max = 21474836) %>% as.data.frame

key 		<- c("COLLECTION_CODE","COUNTRY_CODE", "SOURCE_CODE", "INDICATOR_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE","CLASSIF3_CODE","CLASSIF4_CODE", "CLASSIF5_CODE")


# step 0 DOWNLOAD FILE

{

URL <- 'http://www.stats.govt.nz/tools_and_services/releases_csv_files/csv-files-for-infoshare.aspx'

require(RSelenium)
shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = TRUE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                ,  browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/x-zip-compressed"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()    
	

	
	remDr$navigate(URL)
	
	Sys.sleep(10)

remDr$findElement('class name', 'content')$findChildElement('partial link text', 'Labour Market Statistics')$highlightElement()	
remDr$findElement('class name', 'content')$findChildElement('partial link text', 'Labour Market Statistics')$clickElement()

	Sys.sleep(100)
	


remDr$close()
remDr$closeServer()
	

rm(URL)	
	
}

test <- list.files("C:\\temp\\")
test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.zip']
file.rename(paste0("C:\\temp\\",test),paste0(INPUT, unique(Mapping_File$ZIP_NAME),'.zip'))


# STEP 1 CLEAN UP AND REDUCE ORIGINAL FILE


for (i in 1:length(Mapping_File$NAME)){

print(Mapping_File$NAME[i])



REF_MAPPING <- Mapping_Definition[Mapping_Definition$ILO_file%in%Mapping_File[i,"ID"],]
REF_MAPPING <- REF_MAPPING[,-1]
REF_MAPPING$series_id <- gsub("\n","",REF_MAPPING$series_id)


REF_ID <- My_unsplit_KEY(REF_MAPPING,"series_id",ref=";")
n <- length(colnames(REF_ID)[substr(colnames(REF_ID),1,4)%in%"PASS"])
REF_ID <- REF_ID[,substr(colnames(REF_ID),1,4)%in%"PASS"]
if(n>1){REF_ID <- reshape(REF_ID,direction = "long",v.names = "ID", varying = list(substr(colnames(REF_ID),1,4)%in%"PASS"));REF_ID <- as.vector(REF_ID$ID)}
REF_ID <- unique(REF_ID[!REF_ID%in%NA])





##################################
#################################
ref <- readr::read_csv(paste0(INPUT,unique(Mapping_File$ZIP_NAME[i]),'.zip'), col_names = TRUE, n_max = 1, progress = FALSE)	
	
ref <- paste0(rep('c', ncol(ref)), collapse = '')
#con <- unz(paste0('C:/temp/',Mapping_File$ZIP_NAME[i],'.zip'), test$Name )
X <- readr::read_csv(paste0(INPUT,unique(Mapping_File$ZIP_NAME[i]),'.zip'), col_types = ref,  col_names = TRUE, progress = FALSE)
rm(ref)


X <- X %>% mutate(TIME_PERIOD = paste0("Y",substr(Period,1,4),"_M",substr(Period,6,7)), value = as.numeric(Data_value), series_id = Series_reference) %>% 
		select(TIME_PERIOD, value, series_id) %>%
		filter(series_id%in%REF_ID)
		
invisible(gc(reset = TRUE))
	  
save(X,file = paste(INPUT,Mapping_File$NAME[i],".Rdata",sep=""))
rm(X)



invisible(gc(reset = TRUE))

}


# STEP 2 MAP to ILO CODE
i <- 1
for (i in 1:length(Mapping_File$NAME)){

print(Mapping_File$NAME[i])
load(paste0(INPUT,Mapping_File$NAME[i],".Rdata"))

X <- X %>% as.data.frame

REF_MAPPING <- Mapping_Definition[Mapping_Definition$ILO_file%in%Mapping_File[i,"ID"],]
REF_MAPPING <- REF_MAPPING[,-1]

My_list <- vector("list", 1)

MY_NEW <- cbind(ID_PASS = X[,colnames(X)%in%colnames(REF_MAPPING)],
				rep(NA,nrow(X)),
				rep(NA,nrow(X)), X)
colnames(MY_NEW)[1:3] <- c('ID_PASS', colnames(REF_MAPPING)[1:2])

rm(X)

MY_MATRIX <- MY_NEW[1,c("ID_PASS","ILO_key","ILO_note","TIME_PERIOD","value")]

for (j in 1:nrow(REF_MAPPING)){

MY_NEW[,2]<- REF_MAPPING[j,1]
MY_NEW[,3]<- REF_MAPPING[j,2]

k <- 3
My_REF <- levels(as.factor(unlist(strsplit(REF_MAPPING[j,colnames(REF_MAPPING)[k]],";"))))


MY_MATRIX <-rbind(MY_MATRIX,
			MY_NEW[MY_NEW$ID_PASS%in%My_REF,colnames(MY_NEW)%in%c("ID_PASS","ILO_key","ILO_note","TIME_PERIOD","value")])
}


MY_MATRIX <- MY_MATRIX[-1,]

######################### NEXT STEP
MY_MATRIX$ID_PASS <- paste(MY_MATRIX$ILO_key,MY_MATRIX$TIME_PERIOD,sep="/")



MY_MATRIX$value <- as.numeric(MY_MATRIX$value)

MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"ILO_key",key[!key%in%c("CLASSIF3_CODE","CLASSIF5_CODE","CLASSIF5_CODE")])
MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"ILO_note",note)
MY_MATRIX <- MY_MATRIX[,!colnames(MY_MATRIX)%in%c("ILO_key","ILO_note")]

My_REF <- as.data.frame(cbind(ID=levels(as.factor(MY_MATRIX$ID_PASS)),
				COLLECTION_CODE = "",
				COUNTRY_CODE = "",
				SOURCE_CODE = "",
				INDICATOR_CODE = "",
				SEX_CODE = "",
				CLASSIF1_CODE = "",
				CLASSIF2_CODE = "",
				TIME_PERIOD = "",
				OBS_VALUE =as.vector(by(MY_MATRIX$value,as.factor(MY_MATRIX$ID_PASS),sum,simplify=TRUE)),
				OBS_STATUS ="",
				NOTES_FREQUENCY_CODE = "",
				NOTES_CLASSIF_CODE = "",
				NOTES_INDICATOR_CODE = "",
				NOTES_SOURCE_CODE = "",
				ADD_STATUS = "B",
				ADD_REPOSITORY = "NSO"),stringsAsFactors=FALSE)



My_REF <- My_Vlookup(My_REF,"ID","COLLECTION_CODE",MY_MATRIX,"ID_PASS","COLLECTION_CODE")
My_REF <- My_Vlookup(My_REF,"ID","COUNTRY_CODE",MY_MATRIX,"ID_PASS","COUNTRY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","SOURCE_CODE",MY_MATRIX,"ID_PASS","SOURCE_CODE")

My_REF <- My_Vlookup(My_REF,"ID","INDICATOR_CODE",MY_MATRIX,"ID_PASS","INDICATOR_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF1_CODE",MY_MATRIX,"ID_PASS","CLASSIF1_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF2_CODE",MY_MATRIX,"ID_PASS","CLASSIF2_CODE")
My_REF <- My_Vlookup(My_REF,"ID","SEX_CODE",MY_MATRIX,"ID_PASS","SEX_CODE")
My_REF$SEX_CODE <- paste("SEX_",My_REF$SEX_CODE,sep="")

My_REF <- My_Vlookup(My_REF,"ID","TIME_PERIOD",MY_MATRIX,"ID_PASS","TIME_PERIOD")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_FREQUENCY_CODE",MY_MATRIX,"ID_PASS","NOTES_FREQUENCY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_CLASSIF_CODE",MY_MATRIX,"ID_PASS","NOTES_CLASSIF_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_INDICATOR_CODE",MY_MATRIX,"ID_PASS","NOTES_INDICATOR_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_SOURCE_CODE",MY_MATRIX,"ID_PASS","NOTES_SOURCE_CODE")


My_REF$ID <- NA
X <- My_REF
rm(My_REF,MY_MATRIX,MY_NEW,REF_MAPPING)




save(X,file = paste(INPUT,Mapping_File$ID[i],".Rdata",sep=""))
rm(X)

invisible(gc(reset = TRUE))

print(Mapping_File$ID[i])

}





# STEP 3 Combined BY COUNTRY and EXCEPTION
i <- 3

for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste0(INPUT,Mapping_File$ID[i],".Rdata"))


#########

if(!Mapping_File$Unit[i]%in%'1'){
X <- X %>% mutate(OBS_VALUE = ifelse(substr(INDICATOR_CODE,1,3)%in%c('EAR','HOW') | substr(INDICATOR_CODE,10,11) %in% c('RT'), OBS_VALUE, as.numeric(OBS_VALUE) / 1000)) %>% as.tbl

}


ifelse(i==1,Y <- as.tbl(X), Y <- bind_rows(Y,X) %>% as.tbl)
rm(X)
invisible(gc(reset = TRUE))
}



REF <- levels(as.factor(substr(Y$SOURCE_CODE,1,2)))


 
Y <- Y %>% 
		as.tbl %>%  
		select(	collection = COLLECTION_CODE,  
				ref_area = COUNTRY_CODE, 
				source = SOURCE_CODE, 
				indicator = INDICATOR_CODE, 
				sex = SEX_CODE, 
				classif1 = CLASSIF1_CODE, 
				classif2 = CLASSIF2_CODE, 
				time = TIME_PERIOD, 
				obs_value = OBS_VALUE, 
				obs_status  =OBS_STATUS, 
				freq_code = NOTES_FREQUENCY_CODE, 
				note_classif = NOTES_CLASSIF_CODE, 
				note_indicator = NOTES_INDICATOR_CODE, 
				note_source = NOTES_SOURCE_CODE ) %>% 
		mutate(	time = paste0(str_sub(time, 2,5), str_sub(time,7,9)), 
				time = ifelse(str_sub(time,5,5) %in% 'Q', paste0(str_sub(time, 1,5), str_sub(time, -1,-1)), time)) %>% 
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE)))
 




# split and save by country
for (i in 1:length(REF)){
X <- Y[substr(Y$source,1,2)%in%REF[i],] %>% as.tbl
save(X,file = paste0('./output/',Target,'_',REF[i],'.Rdata'))
print(REF[i])
}

REF <- cbind(PATH = paste0(getwd(), "/output/",Target,"_",REF,".Rdata"),ID = NA, Types  ="NSO_ilostat", REF = '')
write.csv(REF,paste0("./FileToLoad.csv"),row.names = FALSE,na="")




ref <- Mapping_File$NAME %>% unique

for (i in 1:length(ref)){
# file.remove(paste0('C:/temp/',ref[i]))
print(paste0(Mapping_File$NAME[i], ' : OK'))
}







final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
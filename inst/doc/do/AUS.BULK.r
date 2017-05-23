#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  Jan 2009. last update May 2017
#############################################################################
init_time <- Sys.time() 

cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(lubridate)
require(ilo)		
require(Ariane,quietly =TRUE)
require(stringr,quietly =TRUE)
require(readxl,quietly =TRUE)
require(RSelenium)
# set work directory 
setwd(paste0(ilo:::path$data, '/AUS/BULK/'))
Sys.setenv(http_proxy="")
Sys.setenv(ftp_proxy="")		 
source(paste0(ilo:::path$data, '/AUS/BULK/','do/AUS.BULK_functions.r'))
Target <- "AUS"
INPUT <- paste0(ilo:::path$data, '/AUS/BULK/input/')
#######################  
key 		<- c("COLLECTION_CODE","COUNTRY_CODE", "SOURCE_CODE", "INDICATOR_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE","CLASSIF3_CODE","CLASSIF4_CODE", "CLASSIF5_CODE")
Mapping_File <- read_excel("./ReadME_AUS.xlsx", sheet="File") %>% filter(!ID %in% NA) %>% as.data.frame
Mapping_Definition <- read_excel("ReadME_AUS.xlsx", sheet="Definition")   %>% as.data.frame
zip_file <- Mapping_File %>% select(ZIP_NAME) %>% distinct %>% t %>% as.character


# STEP 0 Download

for (i in seq_along(zip_file)){

download_data_AUS(Mapping_File %>% filter(ZIP_NAME %in% zip_file[i]), Drop  = FALSE)

	print(i)

}

# STEP 1 open, CLEAN UP AND REDUCE ORIGINAL FILE

for (i in seq_along(Mapping_File$NAME)){

print(Mapping_File$NAME[i])



REF_MAPPING <- Mapping_Definition[Mapping_Definition$ILO_file%in%Mapping_File[i,"ID"],]
REF_MAPPING <- REF_MAPPING[,-1]
REF_MAPPING$series_id <- gsub("\n","",REF_MAPPING$series_id)


REF_ID <- My_unsplit_KEY(REF_MAPPING,"series_id",ref=";")
n <- length(colnames(REF_ID)[substr(colnames(REF_ID),1,4)%in%"PASS"])
REF_ID <- REF_ID[,substr(colnames(REF_ID),1,4)%in%"PASS"]
if(n>1){REF_ID <- reshape(REF_ID,direction = "long",v.names = "ID", varying = list(substr(colnames(REF_ID),1,4)%in%"PASS"));REF_ID <- as.vector(REF_ID$ID)}
REF_ID <- unique(REF_ID[!REF_ID%in%NA])



ref_sheet <- excel_sheets(paste0(INPUT,Mapping_File$NAME[i], '.xls')) 
ref_sheet <- ref_sheet[substr(ref_sheet,1,4) %in% 'Data' ]



##################################
##################################
X <- NULL
for (k in 1:length(ref_sheet)){
try(tmp <- read_excel(paste0(INPUT, Mapping_File$NAME[i], '.xls'), sheet = ref_sheet[k] , col_names = TRUE, skip = 9) , silent = TRUE)
tmp <- tmp %>% 	gather(key  = Series, value = value, -1, na.rm = TRUE) %>% 
				select(TIME_PERIOD = ends_with('ID'), series_id = Series, value)
X <- bind_rows(X, tmp )
rm(tmp)
invisible(gc(reset = TRUE))
}
rm(ref_sheet)

X <- X %>% mutate(TIME_PERIOD = paste0("Y",substr(TIME_PERIOD,1,4),"_M",substr(TIME_PERIOD,6,7)), value = as.numeric(value)) %>% 
	  filter(series_id%in%REF_ID)
invisible(gc(reset = TRUE))
	  
save(X,file = paste0(INPUT,Mapping_File$NAME[i],".Rdata"))
rm(X)

# file.remove(paste0(INPUT,Mapping_File$NAME[i], '.xls'))
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
colnames(MY_NEW)[2:3] <- colnames(REF_MAPPING)[1:2]

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

save(X,file = paste0(INPUT,Mapping_File$ID[i],".Rdata"))
rm(X)

#  file.remove(paste0(INPUT,Mapping_File$NAME[i], '.Rdata'))
invisible(gc(reset = TRUE))

print(Mapping_File$ID[i])

}


# STEP 3 Combined BY COUNTRY and EXCEPTION
i <- 3

for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste0(INPUT,Mapping_File$ID[i],".Rdata"))


ifelse(i==1,Y <- X, Y <- rbind.fill(Y,X))
rm(X)
# file.remove(paste0('C:/temp/',Mapping_File$ID[i], '.Rdata'))
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
save(X,file = paste0("./output/AUS_",REF[i],".Rdata"))
print(REF[i])
}

REF <- cbind(PATH = paste0(getwd(), "/output/AUS_",REF,".Rdata"),ID = NA, Types  ="NSO_ilostat", REF = 'AUS')
write.csv(REF,paste0("./FileToLoad.csv"),row.names = FALSE,na="")




ref <- Mapping_File$ZIP_NAME %>% unique

for (i in 1:length(ref)){
# file.remove(paste0('C:/temp/',ref[i]))
print(paste0(Mapping_File$NAME[i], ' : OK'))
}





final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
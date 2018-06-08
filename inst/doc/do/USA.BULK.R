#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################
Target <- "USA"
init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(Ariane,quietly =TRUE)
require(lubridate, quietly =TRUE)
require(RSelenium, quietly =TRUE)
require(readxl,quietly =TRUE)
require(ilo)
setwd(paste0(ilo:::path$data, '/',Target,'/BULK/'))
Sys.setenv(http_proxy="")
Sys.setenv(ftp_proxy="")
Sys.setenv(http_proxy="proxyos.ilo.org:8080")
Sys.setenv(htts_proxy="proxyos.ilo.org:8080")
Sys.setenv(ftp_proxy="proxyos.ilo.org:8080")

source(paste0(ilo:::path$data, '/',Target,'/BULK/','do/',Target,'.BULK_functions.r'))

INPUT <- paste0(ilo:::path$data, '/',Target,'/BULK/input/')

Mapping_File <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="File", guess_max = 1000)  %>% filter(!ID%in%NA) %>% as.data.frame  %>% filter(IsValidate %in% 'Yes')
Mapping_Definition <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="Definition", guess_max = 21474836) %>% as.data.frame



key 		<- c("COLLECTION_CODE","COUNTRY_CODE", "SOURCE_CODE", "INDICATOR_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE")
note 		<- c("NOTES_FREQUENCY_CODE","NOTES_CLASSIF_CODE","NOTES_INDICATOR_CODE","NOTES_SOURCE_CODE","CURRENCY_CODE")




# STEP 0 prepare compare with MICRO

load(paste0(ilo:::path$data, "REP_ILO/MICRO/output/", Target, "/CPS/", Target, "_CPS_ilostat.Rdata"))
X <- X %>%  switch_ilo(version) %>% 
			mutate(	indicator = paste0(str_sub(indicator, 1,9),str_sub(indicator, -2,-1)),
					classif1_version = str_sub(classif1_version,1,3),
					classif2_version = str_sub(classif2_version,1,3)			) %>% 
			distinct( source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
			mutate(micro = 'yes')

X %>% saveRDS(file = paste0(INPUT, Target, "_CPS_ilostat.rds"))

rm(X)

# STEP 1 Download, open, CLEAN UP AND REDUCE ORIGINAL FILE



for (i in 1:length(Mapping_File$NAME)){

test <- try( download.file(Mapping_File$URL[i], paste0(INPUT,Mapping_File$NAME[i], '.txt' , mode = 'wb'),  quiet = T), silent = T)

print(paste0(Mapping_File$NAME[i], '/ download -> ',ifelse(test%in% 0, 'OK', 'ERROR') ))


REF_MAPPING <- Mapping_Definition[Mapping_Definition$ILO_file%in%Mapping_File[i,"ID"],]
REF_MAPPING <- REF_MAPPING[,-1]
REF_MAPPING$series_id <- gsub("\n","",REF_MAPPING$series_id)
REF_ID <- My_unsplit_KEY(REF_MAPPING,"series_id",ref=";")
n <- length(colnames(REF_ID)[substr(colnames(REF_ID),1,4)%in%"PASS"])
REF_ID <- REF_ID[,substr(colnames(REF_ID),1,4)%in%"PASS"]
if(n>1){REF_ID <- reshape(REF_ID,direction = "long",v.names = "ID", varying = list(substr(colnames(REF_ID),1,4)%in%"PASS"));REF_ID <- as.vector(REF_ID$ID)}

REF_ID <- unique(REF_ID[!REF_ID%in%NA])
invisible(gc(reset = TRUE))

ref <- readr::read_delim(paste0(INPUT,Mapping_File$NAME[i],'.txt'), col_names = TRUE, n_max = 3, quote ='\t', delim='\t')
ref <- paste0(rep('c', ncol(ref)), collapse = '')
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
X <- readr::read_delim(paste0(INPUT,Mapping_File$NAME[i],'.txt'), col_types = ref,  col_names = TRUE, quote ='\t', delim='\t') %>% 
		mutate(		series_id = str_trim(series_id, side = c("both")), 
					year = str_trim(year, side = c("both")), 
					period = str_trim(period, side = c("both")), 
					value = str_trim(value, side = c("both")),
					footnote_codes = str_trim(footnote_codes, side = c("both"))
		) %>%
		filter(	!series_id %in% NA, 
				!period %in% 'A01' ) %>% 
		mutate(	value = as.numeric(value), 
				TIME_PERIOD = stringr::str_c('Y',year, '_',period )) %>%
		select(-year, -period) %>% 
		mutate(	TIME_PERIOD = stringr::str_replace(TIME_PERIOD, '_M13',''),
				TIME_PERIOD = stringr::str_replace(TIME_PERIOD, '_Q05',''),
				TIME_PERIOD = stringr::str_replace(TIME_PERIOD, '_S03','')		) %>%
		filter(series_id %in% c(REF_ID, paste0(REF_ID,'Q')))
		
		

save(X,file = paste0(INPUT,Mapping_File$NAME[i],'.Rdata'))
rm(ref, X)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
}




# STEP 2 MAP to ILO CODE

for (i in 1:length(Mapping_File$NAME)){

print(Mapping_File$NAME[i])

load(paste0(INPUT,Mapping_File$NAME[i],".Rdata"))

REF_MAPPING <- Mapping_Definition[Mapping_Definition$ILO_file%in%Mapping_File[i,"ID"],]
REF_MAPPING <- REF_MAPPING[,-1]
REF_MAPPING$series_id <- gsub("\n","",REF_MAPPING$series_id)

if(i %in% 1){
pass <- REF_MAPPING
pass$series_id <- paste(pass$series_id,"Q",sep="")
pass$series_id <- gsub(";","Q;",pass$series_id)
REF_MAPPING <- rbind(REF_MAPPING,pass)
rm(pass)
}

invisible(gc(reset = TRUE))

My_list <- vector("list", 1)



MY_NEW <- cbind(ID_PASS = X[,colnames(X)%in%colnames(REF_MAPPING)],
				rep(NA,nrow(X)),
				rep(NA,nrow(X)), X)
colnames(MY_NEW)[1:3] <- c('ID_PASS', colnames(REF_MAPPING)[1:2])

rm(X)

MY_MATRIX <- MY_NEW[1,c("ID_PASS","ILO_key","ILO_note","TIME_PERIOD","value","footnote_codes")]

for (j in 1:nrow(REF_MAPPING)){

MY_NEW[,2]<- REF_MAPPING[j,1]
MY_NEW[,3]<- REF_MAPPING[j,2]


k <- 3
My_REF <- levels(as.factor(unlist(strsplit(REF_MAPPING[j,colnames(REF_MAPPING)[k]],";"))))


MY_MATRIX <-rbind(MY_MATRIX,
			MY_NEW[MY_NEW$ID_PASS%in%My_REF,colnames(MY_NEW)%in%c("ID_PASS","ILO_key","ILO_note","TIME_PERIOD","value","footnote_codes")])
}


MY_MATRIX <- MY_MATRIX[-1,]

######################### NEXT STEP
MY_MATRIX$ID_PASS <- paste(MY_MATRIX$ILO_key,MY_MATRIX$TIME_PERIOD,sep="/")



MY_MATRIX$value <- as.numeric(MY_MATRIX$value)

MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"ILO_key",key)
MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"ILO_note",note)


MY_MATRIX <- MY_MATRIX[,!colnames(MY_MATRIX)%in%c("ILO_key","ILO_note")]
invisible(gc(reset = TRUE))
My_REF <- as.data.frame(cbind(ID=levels(as.factor(MY_MATRIX$ID_PASS)),
				COLLECTION_CODE = "",
				COUNTRY_CODE = "",
				SOURCE_CODE = "",
				INDICATOR_CODE = "",
				CLASSIF1_CODE = "",
				CLASSIF2_CODE = "",
				SEX_CODE = "",
				TIME_PERIOD = "",
				OBS_VALUE =as.vector(by(MY_MATRIX$value,as.factor(MY_MATRIX$ID_PASS),sum,simplify=TRUE)),
				OBS_STATUS ="",
				NOTES_FREQUENCY_CODE = "",
				NOTES_CLASSIF_CODE = "",
				NOTES_INDICATOR_CODE = "",
				NOTES_SOURCE_CODE = "",
				CURRENCY_CODE = "",
				ADD_STATUS = "B",
				ADD_REPOSITORY = "NSO"),stringsAsFactors=FALSE)



My_REF <- My_Vlookup(My_REF,"ID","COLLECTION_CODE",MY_MATRIX,"ID_PASS","COLLECTION_CODE")
My_REF <- My_Vlookup(My_REF,"ID","COUNTRY_CODE",MY_MATRIX,"ID_PASS","COUNTRY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","SOURCE_CODE",MY_MATRIX,"ID_PASS","SOURCE_CODE")
invisible(gc(reset = TRUE))
My_REF <- My_Vlookup(My_REF,"ID","INDICATOR_CODE",MY_MATRIX,"ID_PASS","INDICATOR_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF1_CODE",MY_MATRIX,"ID_PASS","CLASSIF1_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF2_CODE",MY_MATRIX,"ID_PASS","CLASSIF2_CODE")
My_REF <- My_Vlookup(My_REF,"ID","SEX_CODE",MY_MATRIX,"ID_PASS","SEX_CODE")
My_REF$SEX_CODE <- paste("SEX_",My_REF$SEX_CODE,sep="")
invisible(gc(reset = TRUE))
My_REF <- My_Vlookup(My_REF,"ID","TIME_PERIOD",MY_MATRIX,"ID_PASS","TIME_PERIOD")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_FREQUENCY_CODE",MY_MATRIX,"ID_PASS","NOTES_FREQUENCY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_CLASSIF_CODE",MY_MATRIX,"ID_PASS","NOTES_CLASSIF_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_INDICATOR_CODE",MY_MATRIX,"ID_PASS","NOTES_INDICATOR_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_SOURCE_CODE",MY_MATRIX,"ID_PASS","NOTES_SOURCE_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CURRENCY_CODE",MY_MATRIX,"ID_PASS","CURRENCY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","OBS_STATUS",MY_MATRIX,"ID_PASS","footnote_codes")
invisible(gc(reset = TRUE))
if(i %in% c(1,2,3,4,5,6)){My_REF$OBS_STATUS <- NA}



My_REF$ID <- NA
X <- My_REF
invisible(gc(reset = TRUE))


########### Exception
if(Mapping_File$NAME[i]%in%"la.data.2.AllStatesU"){
X[substr(X$INDICATOR_CODE,nchar(X$INDICATOR_CODE)-1,nchar(X$INDICATOR_CODE))%in%"NB","OBS_VALUE"] <- as.numeric(X[substr(X$INDICATOR_CODE,nchar(X$INDICATOR_CODE)-1,nchar(X$INDICATOR_CODE))%in%"NB","OBS_VALUE"]) / 1000
}

invisible(gc(reset = TRUE))
save(X,file = paste0(INPUT,Mapping_File$ID[i],".Rdata"))
rm(X,My_REF,MY_MATRIX,MY_NEW,REF_MAPPING)
invisible(gc(reset = TRUE))
print(Mapping_File$ID[i])

}





# STEP 3 Combined BY COUNTRY and EXCEPTION
i <- 3

for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste(INPUT,Mapping_File$ID[i],".Rdata",sep=""))


ifelse(i==1,Y <- X, Y <- bind_rows(Y,X))
rm(X)

invisible(gc(reset = TRUE))
}


Y <- Y %>% filter(!(str_detect(CLASSIF2_CODE ,  'EDU') & as.numeric(str_sub(TIME_PERIOD,2,5))<1992 ) ) %>% as.tbl

REF <- levels(as.factor(Y$SOURCE_CODE)) %>% str_replace(':','')
 
 
 
 
 
 
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
				time = ifelse(str_sub(time,5,5) %in% 'Q', paste0(str_sub(time, 1,5), str_sub(time, -1,-1)), time), 
				note_source = 'R1:3903') %>% 
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE))) %>% 
		switch_ilo(version) %>% 
		filter(!(str_detect(source, 'BA') & str_detect(classif1_version, 'ECO') & as.numeric(str_sub(time, 1,4)) > 1999)) %>% 
		filter(!(str_detect(indicator, 'LUU_XLU4_RT') & as.numeric(str_sub(time, 1,4)) > 1993)) %>% 
		mutate(	classif1_version = str_sub(classif1_version,1,3),
				classif2_version = str_sub(classif2_version,1,3))
 
############## remove if available with MICRO

micro <- readRDS(paste0(INPUT, Target, "_CPS_ilostat.rds"))

Y <- Y %>% 	left_join(micro, by = c("source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
			filter(!micro %in% 'yes') %>% 
			select(-micro, -sex_version, -classif1_version, -classif2_version)


# split and save by country
for (i in 1:length(REF)){
X <- Y %>% filter(str_replace(source,':', '') %in% REF[i])
save(X,file = paste0('./output/',Target,'_',REF[i],'.Rdata'))
print(REF[i])
}

REF <- cbind(PATH = paste0(getwd(), "/output/",Target,"_",str_replace(REF,':', ''),".Rdata"),ID = NA, Types  ="NSO_ilostat", REF = '')
write.csv(REF,paste0("./FileToLoad.csv"),row.names = FALSE,na="")

rm(Y)


ref <- Mapping_File$NAME %>% unique

for (i in 1:length(ref)){
# file.remove(paste0('C:/temp/',ref[i]))
print(paste0(Mapping_File$NAME[i], ' : OK'))
}



 
 

final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)



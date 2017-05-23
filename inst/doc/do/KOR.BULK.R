#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################
Target <- "KOR"
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

Mapping_File <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="File", guess_max = 1000)  %>% filter(IsValidate %in% 'Yes')
Mapping_Definition <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="Definition", guess_max = 21474836) 



# STEP 1 Download, open, CLEAN UP AND REDUCE ORIGINAL FILE
for (i in 1:length(Mapping_File$NAME)){

	X <- download_data_KOR(Mapping_File %>% slice(i))

	print(paste0(Mapping_File$NAME[i], '/ download -> ', nrow(X)))

	save(X,file = paste0(INPUT,Mapping_File$NAME[i],'.Rdata'))
	
	rm(X)
	
	print(i)
	invisible(gc(reset = TRUE))

}


# STEP 2 MAP to ILO CODE
for (i in 1:length(Mapping_File$NAME)){

print(Mapping_File$NAME[i])
load(paste(INPUT,Mapping_File$NAME[i],".Rdata",sep=""))

# avoid white space in column header			
colnames(X) <- gsub(' ', '.', colnames(X), fixed = TRUE)

try(X <- X %>% select(-contains('X')), silent = TRUE)
# try(X <- X %>% select(-contains('X298')), silent = TRUE)
# try(X <- X %>% select(-contains('X670')), silent = TRUE)

# get mapping frame File should be filled and File name correspond to Mapping_File ID 
REF_MAPPING <- Mapping_Definition %>% filter(!File %in% NA, File %in% Mapping_File$ID[i]) %>% select(-File)
# reduce mapping frame to columns ILO KEY + columns available on the dataset 
colnames(REF_MAPPING) <- gsub(' ', '.', colnames(REF_MAPPING), fixed = TRUE)



REF_MAPPING <- REF_MAPPING %>% 
					select(contains('_CODE')) %>% 
					bind_cols(REF_MAPPING %>% select_(.dots = colnames(X)[!colnames(X)%in% c('TIME_PERIOD','OBS_VALUE') ]))

# split columns to avail mapping redondancy					

SplitCol <- Mapping_File$SplitCol[i]
if(!is.na(SplitCol)){
SplitCol <- stringr::str_split(SplitCol, ' = ') %>% unlist
	SplitCol[1] <- gsub(' ', '.', SplitCol[1], fixed = TRUE)
	ref <- stringr::str_split(unique(REF_MAPPING[,SplitCol[[1]]]), ';') %>% unlist
	MAP <- NULL
	for ( j in seq_along(ref)){
		MAP <- bind_rows(MAP,
					bind_cols(REF_MAPPING %>% select(-contains(SplitCol[1])), data_frame(pass = 1:nrow(REF_MAPPING), ToChange = ref[j])) 
				)
	}					
	REF_MAPPING <- MAP %>% select(-pass) 
# map sex 
test <- try(
	REF_MAPPING <- REF_MAPPING %>% 	mutate(ToChangeCode = mapvalues(ToChange, c('Total','Male','Female'),  c('SEX_T','SEX_M','SEX_F'), warn_missing = FALSE)) 
, silent = TRUE )

colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChange'] <- SplitCol[1]
colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChangeCode'] <- SplitCol[2]
} else {REF_MAPPING <- REF_MAPPING %>% mutate(SEX_CODE = 'SEX_T')}

rm(SplitCol)

#create ilo key	of ref_mapping
ref_key_ilo <- REF_MAPPING %>% slice(1) %>% select(contains('_CODE')) %>% colnames
REF_MAPPING <- REF_MAPPING %>% unite_('KEY_ILO', ref_key_ilo , remove = TRUE, sep = '/') 
ref_key_ilo <-  paste(ref_key_ilo, collapse = '/')

# clean
REF_MAPPING <- REF_MAPPING %>% 	mutate_each(funs(gsub('&amp;','&', ., fixed = TRUE)), -KEY_ILO) 

#create key	of X in national language
ref_key_nat <- X %>% slice(1) %>% select(-TIME_PERIOD, -OBS_VALUE) %>% colnames
X <- X %>% unite_('KEY_NAT', ref_key_nat , remove = TRUE, sep = '/') 	
ref <- c('KEY_ILO', ref_key_nat)

REF_MAPPING <- REF_MAPPING %>% select_(.dots = ref) 


# REF_MAPPING <- REF_MAPPING %>% rename(SEX = By.gender)
My_list <- vector("list", length(2:ncol(REF_MAPPING)))


MY_NEW <- X %>% mutate(KEY_ILO = as.character(NA))

rm(X)


invisible(gc(reset = TRUE))
MY_MATRIX <- NULL 

j <- 1
for (j in 1:nrow(REF_MAPPING)){

MY_NEW$KEY_ILO <-  {REF_MAPPING %>% slice(j) %>% select(KEY_ILO) %>% as.character}


for (k in 2:ncol(REF_MAPPING)){
	My_list[[k]] <- levels(as.factor(unlist(strsplit(REF_MAPPING[j,colnames(REF_MAPPING)[k]] %>% as.character,";"))))
}
My_REF <- My_list[[2]]

if(ncol(REF_MAPPING)>2){
	for(k in 3:ncol(REF_MAPPING)){
		My_REF <- paste(sort(rep(My_REF,length(My_list[[k]]))),My_list[[k]],sep="/")
	}
}


MY_MATRIX <-bind_rows(MY_MATRIX,
			MY_NEW[MY_NEW$KEY_NAT%in%My_REF,colnames(MY_NEW)%in%c("KEY_NAT","KEY_ILO","TIME_PERIOD","OBS_VALUE")])
			
			
}

invisible(gc(reset = TRUE))



######################### NEXT STEP

X <- MY_MATRIX  %>%
				mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>% 
				select(-KEY_NAT) %>% 
				group_by(KEY_ILO, TIME_PERIOD) %>% 
				summarise(OBS_VALUE = sum(OBS_VALUE, na.rm = TRUE)) %>% 
				ungroup %>%
				rename(ID = KEY_ILO) %>%
				mutate(	COLLECTION_CODE = Mapping_File$COLLECTION_CODE[i],
						COUNTRY_CODE = Mapping_File$COUNTRY_CODE[i],
						SOURCE_CODE = Mapping_File$SOURCE_CODE[i],
						ADD_STATUS = "B",
						ADD_REPOSITORY = "NSO"	)  %>% 
				separate(ID, stringr::str_split(ref_key_ilo, '/') %>% unlist, remove = FALSE, sep = '/') %>%
				mutate(ID = NA)

rm(My_REF,MY_MATRIX,MY_NEW,REF_MAPPING, ref_key_ilo, ref_key_nat)
save(X,file = paste(INPUT,Mapping_File$ID[i],".Rdata",sep=""))
rm(X)
invisible(gc(reset = TRUE))

print(Mapping_File$ID[i])

}



# STEP 3 Combined BY COUNTRY and manage exception
for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste(INPUT,Mapping_File$ID[i],".Rdata",sep=""))

X <- X %>% mutate(OBS_STATUS = as.character(NA))

if({colnames(Mapping_File)[colnames(Mapping_File) %in% 'Manip'] %>% length ==1} ){
		if(!Mapping_File$Manip[i]%in%NA){
			try(	X 	<- 	eval(parse(text= paste0('X %>% ',Mapping_File$Manip[i]))))
		}
}

if(i==1) Y <- X else Y <- bind_rows(Y,X)
rm(X)
# file.remove(paste0('C:/temp/',Mapping_File$ID[i],'.Rdata'))
invisible(gc(reset = TRUE))

}


REF <- levels(as.factor(substr(Y$SOURCE_CODE,1,2)))

Y <- Y %>% # converge to ilostat format
		as.tbl %>%  mutate(OBS_STATUS  =as.character(NA), NOTES_SOURCE_CODE = as.character(NA)) %>% 
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
 



for (i in 1:length(REF)){
X <- Y %>% filter(substr(source,1,2)%in%REF[i])
save(X,file = paste(getwd(),'/output/',Target,'_',REF[i],".Rdata",sep=""))
rm(X)
invisible(gc(reset = TRUE))
}



REF <- cbind(PATH = paste0(getwd(), '/output/',Target,'_',REF,'.Rdata'),ID = NA, Types  ='NSO_ilostat', REF = Target)
# add historical data

write.csv(REF,paste("./FileToLoad.csv",sep=""),row.names = FALSE,na="")



final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE) 
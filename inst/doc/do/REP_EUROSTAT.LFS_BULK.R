#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  May 2010. last update May 2017
#############################################################################
require(lubridate)
require(Ariane,quietly =TRUE)
require(stringr,quietly =TRUE)
require(readxl,quietly =TRUE)
require(readr,quietly =TRUE)
require(lazyeval,quietly =TRUE)
setwd(paste0(ilo:::path$data, '/REP_EUROSTAT/LFS_BULK'))
source(paste0( './do/REP_EUROSTAT.LFS_BULK_functions.r'))

init_time <- Sys.time()

CODE_ORA <- Ariane:::CODE_ORA
Target <- "EUROSTAT"

header_ILO <- c("INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE","NOTES_FREQUENCY_CODE","NOTES_CLASSIF_CODE","NOTES_INDICATOR_CODE")

Mapping_File <- read_excel(paste0("./ReadME_",Target,".xlsx"), sheet="File", col_names =FALSE) %>% 
					cleanDf(header = FALSE) %>% 
					filter(Process %in% "Yes") %>% 
					select(-Process) %>% as.data.frame
				
# open definition for mapping
Mapping_Definition <- read_excel(paste0("./ReadME_",Target,".xlsx"), sheet="Mapping_Definition", col_names =FALSE)  %>% 
					cleanDf(header = FALSE) %>%				
					filter(Is_Validate %in% "Yes") %>% 
					select(-Is_Validate) %>% as.data.frame

					

# STEP 1 CLEAN UP AND REDUCE ORIGINAL FILE
for(i in 1:length(Mapping_File$NAME)){

X <- eurostat::get_eurostat(Mapping_File$NAME[i],   time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
invisible(gc(reset = TRUE))

X <- X	%>%	as.tbl %>% 
			# %>% 
			cleanDf %>% 
			mutate( time = ifelse(str_sub(time,5,5) %in% 'Q', paste0('Y', str_sub(time,1,4), '_Q0', str_sub(time,6,6)), time), 
					time = ifelse(str_sub(time,5,5) %in% 'M', paste0('Y', str_sub(time,1,4), '_', str_sub(time,5,7)), time), 
					time = ifelse(str_sub(time,5,5) %in% '', paste0('Y', str_sub(time,1,4)), time))

					
if(i %in% 3:5)	{X <- X %>% mutate(var_unit = unit)}	
	X <- X %>% select(-one_of('unit'))		
print(paste0(Mapping_File$NAME[i], '/ download -> ',nrow(X)))


REF_MAPPING <- Mapping_Definition[Mapping_Definition$File%in%Mapping_File[i,"ID"],]

header <- colnames(X)[!colnames(X)%in%c('time','values','flags')]
header <- colnames(REF_MAPPING[,colnames(REF_MAPPING)%in%header])
REF_MAPPING <- REF_MAPPING[,c(colnames(REF_MAPPING)[1:5],header)]




invisible(gc(reset = TRUE))



for( j in 1:length(header)){
test <- REF_MAPPING[,colnames(REF_MAPPING)%in%header[j]]
sbt <- strsplit(test,";")
n <- max(sapply(sbt, length))
l <- lapply(sbt, function(test) c(test, rep(NA, n - length(test))))
sbt <- as.data.frame(t(do.call(cbind, l)),stringsAsFactors =FALSE)
test <- reshape(sbt,direction = "long",varying = list(colnames(sbt)),v.names = "VALUE")
test <- unique(as.character(test$VALUE))
ref <- header[j]
.dots <- list(interp(~y %in% x, 
                     .values = list(y = as.name(ref), x = test)))
X <- X %>% filter_( .dots=.dots)
rm(ref, test, .dots)
}



X <- X %>% 	rename(	TIME_PERIOD = time, OBS_VALUE = values, OBS_STATUS = flags) %>%
			mutate(	OBS_STATUS = ifelse(OBS_STATUS %in% 'c', 'f', OBS_STATUS)) %>%
			filter(	!(OBS_VALUE%in%NA & OBS_STATUS%in%NA)) %>% 
			mutate(	OBS_STATUS = ifelse(!OBS_STATUS %in% c("b","u","f"),NA,  OBS_STATUS), 
					OBS_STATUS = ifelse(OBS_STATUS %in% 'f', 'u', OBS_STATUS)) %>% 
			select_(.dots = c(header,"TIME_PERIOD","OBS_VALUE","OBS_STATUS")) %>%
			as.data.frame
			

save(X,file = paste0("./input/",Mapping_File$NAME[i],".Rdata"))

print(paste0(i,"/",length(Mapping_File$NAME)," : ",Mapping_File$NAME[i]))
rm(X)
invisible(gc(reset = TRUE))
}





# STEP 2 MAP to ILO CODE
for (i in 1:length(Mapping_File$NAME)){
load(paste0("./input/",Mapping_File$NAME[i],".Rdata"))

REF_MAPPING <- Mapping_Definition[Mapping_Definition$File%in%Mapping_File[i,"ID"],]


header_NAT <- colnames(X)[!colnames(X)%in%c("TIME_PERIOD","OBS_VALUE","OBS_STATUS")]
header_NAT <- colnames(REF_MAPPING[,colnames(REF_MAPPING)%in%header_NAT])

REF_MAPPING <- REF_MAPPING[,c(colnames(REF_MAPPING)[colnames(REF_MAPPING)%in%header_ILO],header_NAT)]

invisible(gc(reset = TRUE))

My_list <- vector("list", length(header_NAT))
MY_NEW <- cbind(ID_PASS = X[,colnames(X)%in%colnames(REF_MAPPING)[5]],
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				X)
colnames(MY_NEW)[2:5] <- colnames(REF_MAPPING)[1:4]
for (k in 6:ncol(REF_MAPPING)){
MY_NEW$ID_PASS <- paste(MY_NEW$ID_PASS,MY_NEW[,colnames(MY_NEW)%in%colnames(REF_MAPPING)[k]],sep=";")
}
MY_MATRIX <- MY_NEW[1,c("ID_PASS","INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE","geo","sex","NOTES_FREQUENCY_CODE","NOTES_CLASSIF_CODE","NOTES_INDICATOR_CODE","TIME_PERIOD","OBS_VALUE","OBS_STATUS")]

invisible(gc(reset = TRUE))
j <- 1
for (j in 1:nrow(REF_MAPPING)){

MY_NEW[,2]<- REF_MAPPING[j,"INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE"]
MY_NEW[,3]<- REF_MAPPING[j,"NOTES_FREQUENCY_CODE"]
MY_NEW[,4]<- REF_MAPPING[j,"NOTES_CLASSIF_CODE"]
MY_NEW[,5]<- REF_MAPPING[j,"NOTES_INDICATOR_CODE"]




for (k in 5:ncol(REF_MAPPING)){
test <- levels(as.factor(unlist(strsplit(REF_MAPPING[j,colnames(REF_MAPPING)[k]],";"))))
My_list[[k]] <- test
}
My_REF <- My_list[[5]]

for(k in 6:ncol(REF_MAPPING)){
My_REF <- paste(sort(rep(My_REF,length(My_list[[k]]))),My_list[[k]],sep=";")
}

MY_MATRIX <-rbind(MY_MATRIX,
			MY_NEW[MY_NEW$ID_PASS%in%My_REF,colnames(MY_NEW)%in%c("ID_PASS","INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE","geo","sex","NOTES_FREQUENCY_CODE","NOTES_CLASSIF_CODE","NOTES_INDICATOR_CODE","TIME_PERIOD","OBS_VALUE","OBS_STATUS")])
}


MY_MATRIX <- MY_MATRIX[-1,]


#MY_MATRIX <- My_Vlookup(MY_MATRIX,"geo","geo",as.data.frame(CODE_ORA$T_COU_COUNTRY),"COU_ISO2_CODE","COU_ISO3_CODE")
######################### NEXT STEP
MY_MATRIX$ID_PASS <- paste(MY_MATRIX$geo,MY_MATRIX[,"INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE"],MY_MATRIX$sex,MY_MATRIX$TIME_PERIOD,sep="/")

MY_MATRIX$OBS_VALUE <- as.numeric(MY_MATRIX$OBS_VALUE)

MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE",c("INDICATOR_CODE","CLASSIF1_CODE","CLASSIF2_CODE"))
MY_MATRIX <- MY_MATRIX[,!colnames(MY_MATRIX)%in%"INDICATOR_CODE/CLASSIF1_CODE/CLASSIF2_CODE"]

My_REF <- as.data.frame(cbind(ID=levels(as.factor(MY_MATRIX$ID_PASS)),
				COUNTRY_CODE = NA,
				SOURCE_CODE = Mapping_File$SOURCE_CODE[i],
				INDICATOR_CODE = "",
				CLASSIF1_CODE = "",
				CLASSIF2_CODE = "",
				SEX_CODE = "",
				TIME_PERIOD = "",
				OBS_VALUE = as.vector(by(as.numeric(MY_MATRIX$OBS_VALUE),as.factor(MY_MATRIX$ID_PASS),sum,simplify=TRUE)),
				OBS_STATUS = "",
				NOTES_FREQUENCY_CODE = "",
				NOTES_CLASSIF_CODE = "",
				NOTES_INDICATOR_CODE = "",
				NOTES_SOURCE_CODE = "", #"R1:2383_R1:3903",
				ADD_STATUS = "B",
				ADD_REPOSITORY = "EUROSTAT"),stringsAsFactors=FALSE)


invisible(gc(reset = TRUE))

My_REF <- My_Vlookup(My_REF,"ID","COUNTRY_CODE",MY_MATRIX,"ID_PASS","geo")
My_REF <- My_Vlookup(My_REF,"ID","INDICATOR_CODE",MY_MATRIX,"ID_PASS","INDICATOR_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF1_CODE",MY_MATRIX,"ID_PASS","CLASSIF1_CODE")
My_REF <- My_Vlookup(My_REF,"ID","CLASSIF2_CODE",MY_MATRIX,"ID_PASS","CLASSIF2_CODE")

My_REF <- My_Vlookup(My_REF,"ID","SEX_CODE",MY_MATRIX,"ID_PASS","sex")
My_REF$SEX_CODE <- paste("SEX_X_",My_REF$SEX_CODE,sep="")
My_REF <- My_Vlookup(My_REF,"ID","TIME_PERIOD",MY_MATRIX,"ID_PASS","TIME_PERIOD")
My_REF <- My_Vlookup(My_REF,"ID","OBS_STATUS",MY_MATRIX,"ID_PASS","OBS_STATUS")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_FREQUENCY_CODE",MY_MATRIX,"ID_PASS","NOTES_FREQUENCY_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_CLASSIF_CODE",MY_MATRIX,"ID_PASS","NOTES_CLASSIF_CODE")
My_REF <- My_Vlookup(My_REF,"ID","NOTES_INDICATOR_CODE",MY_MATRIX,"ID_PASS","NOTES_INDICATOR_CODE")

invisible(gc(reset = TRUE))

My_REF$ID <- NA
X <- My_REF
# transfert code country
X <- X %>% mutate(	COUNTRY_CODE = ifelse(COUNTRY_CODE %in% "EL" , "GR", COUNTRY_CODE), 
					COUNTRY_CODE = ifelse(COUNTRY_CODE %in% "UK" , "GB", COUNTRY_CODE) 
						)
X <- My_Vlookup(X,"COUNTRY_CODE","COUNTRY_CODE",as.data.frame(CODE_ORA$T_COU_COUNTRY),"COU_ISO2_CODE","COU_ISO3_CODE") %>% as.tbl

rm(My_REF,MY_MATRIX,MY_NEW,REF_MAPPING)

save(X,file = paste0("./input/",Mapping_File$ID[i],".Rdata"))#,na = "",row.names = FALSE)

print(paste0(i,"/",length(Mapping_File$NAME)," : ",Mapping_File$ID[i]))
rm(X)
invisible(gc(reset = TRUE))

}



# STEP 3 DEVIDED BY COUNTRY and EXCEPTION
# mapping source and survey
ReadMeSource <- read.xlsx(paste0("./ReadME_EUROSTAT.xlsx"), sheet="MappingSource")        
REF <- as.vector(unique(ReadMeSource$COUNTRY_CODE))  
ReadMeSource$COUNTRY_CODE <- paste0(ReadMeSource$COUNTRY_CODE,ReadMeSource$SOURCE_CODE)
ReadMeSource$SOURCE_CODE <- paste0(ReadMeSource$SOURCE_CODE,":",ReadMeSource$SURVEY_CODE)


invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

for (j in 1:length(REF)){


print(REF[j])
for (i in 1:length(Mapping_File$ID)){
	print(Mapping_File$ID[i])
	load(paste0("./input/",Mapping_File$ID[i],".Rdata"))
	
	X <- X %>% filter(COUNTRY_CODE%in%REF[j])
	
	test <- c("ILO_lfsq_eegana","ILO_lfsa_eegana","ILO_lfsq_egana","ILO_lfsa_egana","ILO_lfsq_esgana","ILO_lfsa_esgana","ILO_lfsq_ewhana","ILO_lfsa_ewhana")
	if(Mapping_File$ID[i]%in%test){
			X <- X %>% filter(!str_sub(TIME_PERIOD,2,5)%in%"2008")
		}
	test <- c("ILO_lfsq_eegan2","ILO_lfsa_eegan2","ILO_lfsq_egan2","ILO_lfsa_egan2","ILO_lfsq_esgan2","ILO_lfsa_esgan2","ILO_lfsq_ewhan2","ILO_lfsa_ewhan2")
	if(Mapping_File$ID[i]%in%test){
			X <- X  %>% mutate(OBS_STATUS = ifelse(TIME_PERIOD%in%"Y2008", 'b', OBS_STATUS), 
							OBS_STATUS = ifelse(TIME_PERIOD%in%"Y2008_Q01", 'b', OBS_STATUS))
			}

	if(!empty(X)){
		ifelse(i==1,Y <- X, Y <- bind_rows(Y,X))
	}

invisible(gc(reset = TRUE))

invisible(gc(reset = TRUE))


rm(X)
}


invisible(gc(reset = TRUE))

X <- as.data.frame(Y) %>% cleanDf
rm(Y)

invisible(gc(reset = TRUE))

X <- X %>% mutate(	NOTES_INDICATOR_CODE = ifelse(SOURCE_CODE%in%"BE" & COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & NOTES_INDICATOR_CODE%in%"T2:84_T3:104" ,"T2:85_T3:104" , NOTES_INDICATOR_CODE))
X <- X %>% mutate(	NOTES_CLASSIF_CODE = ifelse(SOURCE_CODE%in%"BE" & COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & CLASSIF1_CODE%in%c("AGE_AGGREGATE_15-24","AGE_10YRBANDS_Y15-24") ,"C6:1058" , NOTES_CLASSIF_CODE), 
					NOTES_CLASSIF_CODE = ifelse(SOURCE_CODE%in%"BE" & COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & CLASSIF1_CODE%in%"AGE_5YRBANDS_Y15-19" ,"C6:1058" , NOTES_CLASSIF_CODE), 
					NOTES_CLASSIF_CODE = ifelse(SOURCE_CODE%in%"BE" ,"C9:2949" , NOTES_CLASSIF_CODE))
					

# replace ISCO 08 by ISCO 88

X <- X %>% mutate(	CLASSIF1_CODE = ifelse(SOURCE_CODE%in%"BA" & CLASSIF1_CODE%in%"OCU_ISCO08_TOTAL" & as.numeric(substr(TIME_PERIOD,2,5)) < 2011 , "OCU_ISCO88_TOTAL" , CLASSIF1_CODE) )

X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_TOTAL" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_TOTAL"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_1" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_1"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_2" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_2"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_3" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_3"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_4" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_4"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_5" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_5"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_6" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_6"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_7" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_7"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_8" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_8"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_9" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_9"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_0" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_0"
X[X$SOURCE_CODE%in%"BA" & X$CLASSIF1_CODE%in%"OCU_ISCO08_X" & as.numeric(substr(X$TIME_PERIOD,2,5))<2011,"CLASSIF1_CODE" ] <- "OCU_ISCO88_X"


# add GEO classif break
X[X$SOURCE_CODE%in%"BA" & substr(X$CLASSIF1_CODE,1,3)%in%c("GEO") & X$TIME_PERIOD%in%c("Y2012","Y2012_Q01","Y2012_M02"),"OBS_STATUS" ] <- "c"
# add EDU classif break
X[X$SOURCE_CODE%in%"BA" & substr(X$CLASSIF1_CODE,1,3)%in%c("EDU") & X$TIME_PERIOD%in%c("2014","Y2014_Q01","Y2014_M02"),"OBS_STATUS" ] <- "c"


X <- X[!(X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"CHE" & substr(X$CLASSIF1_CODE,1,7)%in%"GEO_COV" & as.numeric(substr(X$TIME_PERIOD,2,5))<2010),]
X <- X[!(X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"CZE" & substr(X$CLASSIF1_CODE,1,3)%in%c("AGE","GEO") & as.numeric(substr(X$TIME_PERIOD,2,5))<1998),]

X <- X[!(X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"CYP" & as.numeric(substr(X$TIME_PERIOD,2,5))<2000),]


X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"FRA" & substr(X$TIME_PERIOD,6,6)%in%"","OBS_VALUE"] <- NA
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & X$NOTES_INDICATOR_CODE%in%"T2:84_T3:89","NOTES_INDICATOR_CODE"] <- "T2:85_T3:89"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & X$NOTES_INDICATOR_CODE%in%"T2:84_T3:104","NOTES_INDICATOR_CODE"] <- "T2:85_T3:104"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & X$CLASSIF1_CODE%in%c("AGE_AGGREGATE_15-24","AGE_10YRBANDS_Y15-24"),"NOTES_CLASSIF_CODE"] <- "C6:1058"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%c("ESP","ISL","NOR","GBR") & X$CLASSIF1_CODE%in%"AGE_5YRBANDS_Y15-19","NOTES_CLASSIF_CODE"] <- "C6:1058"



X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"ISL" & X$TIME_PERIOD%in%c("Y2003","Y2003_Q01"),"OBS_STATUS"] <- "b"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"DNK" & X$NOTES_INDICATOR_CODE%in%"T2:84_T3:104","NOTES_INDICATOR_CODE"] <- "T2:84_T3:96"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"BEL" & X$NOTES_INDICATOR_CODE%in%"T2:84_T3:104","NOTES_INDICATOR_CODE"] <- "T2:84_T3:94"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"FIN" & X$NOTES_INDICATOR_CODE%in%"T2:84_T3:89","NOTES_INDICATOR_CODE"] <- "T2:84_T3:104"

X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"TUR","NOTES_FREQUENCY_CODE"] <- "S"

X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"LUX" & X$TIME_PERIOD%in%c("Y2003","Y2003_Q01") & substr(X$CLASSIF1_CODE,1,7)%in%"GEO_COV","OBS_STATUS"] <- "b"   # BUGs
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"PRT" & X$TIME_PERIOD%in%c("Y1998","Y1998_Q01") & substr(X$CLASSIF1_CODE,1,3)%in%"DUR","OBS_STATUS"] <- "b"
X[X$SOURCE_CODE%in%"BA" & X$COUNTRY_CODE%in%"NLD" & X$TIME_PERIOD%in%c("Y2010","Y2010_Q01") & substr(X$CLASSIF1_CODE,1,3)%in%"DUR","OBS_STATUS"] <- "b"


invisible(gc(reset = TRUE))


X <- X %>% as.tbl %>% mutate(TEST = paste0(COUNTRY_CODE, SOURCE_CODE))%>% select(-SOURCE_CODE)%>%
			left_join( select(as.tbl(ReadMeSource), TEST = COUNTRY_CODE, SOURCE_CODE), by = 'TEST') %>% 
			select(-TEST) %>%  
			mutate(COLLECTION_CODE = 'STI' ) %>%
			filter( COUNTRY_CODE%in%REF[j], 
					!(X$OBS_VALUE%in%NA & X$OBS_STATUS%in%NA)) %>% 
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
		





invisible(gc(reset = TRUE))
save(X,file = paste("./output/REP_EUROSTAT_",REF[j],".Rdata",sep=""))
rm(X)
invisible(gc(reset = TRUE))
print(paste0(j,"/",length(REF),":",REF[j]))

}


# split and save by country


LOAD <- cbind(PATH = paste0(getwd(), "/output/REP_EUROSTAT_",REF,".Rdata"),ID = NA, Types  ="EUROSTAT_ilostat", REF = REF)
write.csv(LOAD,"./FileToLoad.csv",row.names = FALSE,na="")

rm(REF)


final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
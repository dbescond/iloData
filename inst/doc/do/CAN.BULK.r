#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  March 2010. last update May 2017
#############################################################################

init_time <- Sys.time()

require(lubridate)
require(ilo)
setwd(paste0(ilo:::path$data, '/CAN/BULK/'))

source(paste0( './do/CAN.BULK_functions.r'))

MY_COLLECTION <- "STI" # "YEARLY" "EAPEP"  "YTHSTAT" "STI"
Target <- 'CAN'

Mapping_File <- readxl:::read_excel("./ReadME_CAN.xlsx", sheet="File")  %>% filter(!ID %in% NA) %>% filter(IsValidate %in% 'Yes') %>% select(-IsValidate) %>% as.data.frame
Mapping_Definition <- readxl:::read_excel("ReadME_CAN.xlsx", sheet="Definition")  %>% filter(IsValidate %in% 'Yes') %>% select(-IsValidate)  %>% as.data.frame


INPUT <- paste0(ilo:::path$data, '/',Target,'/BULK/input/')




# STEP 0 prepare compare with MICRO

load(paste0(ilo:::path$data, "REP_ILO/MICRO/output/", Target, "/LFS/", Target, "_LFS_ilostat.Rdata"))
X <- X %>%  switch_ilo(version) %>% 
			mutate(	indicator = paste0(str_sub(indicator, 1,9),str_sub(indicator, -2,-1)),
					classif1_version = str_sub(classif1_version,1,3),
					classif2_version = str_sub(classif2_version,1,3)			) %>% 
			distinct( source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
			mutate(micro = 'yes')

X %>% saveRDS(file = paste0(INPUT, Target, "_LFS_ilostat.rds"))

rm(X)





# STEP 1 Download, open, CLEAN UP AND REDUCE ORIGINAL FILE


Sys.setenv(http_proxy="")
Sys.setenv(https_proxy="")
Sys.setenv(ftp_proxy="")
getOption('timeout')

Sys.setenv(http_proxy="proxyos.ilo.org:8080")
Sys.setenv(htts_proxy="proxyos.ilo.org:8080")
Sys.setenv(ftp_proxy="proxyos.ilo.org:8080")

options(timeout=4000)
setwd('./input/')

for (i in 1:length(Mapping_File$NAME)){

try(unlink(paste0(basename(Mapping_File$URL[i])), recursive = TRUE, force = TRUE), silent = TRUE)

download.file(Mapping_File$URL[i], paste0(basename(Mapping_File$URL[i])), quiet = TRUE, mode = 'wb')

closeAllConnections()

if(file.info(paste0(basename(Mapping_File$URL[i])))$size / 1000 < 50){

try(unlink(paste0(basename(Mapping_File$URL[i])), recursive = TRUE, force = TRUE), silent = TRUE)

download.file(Mapping_File$URL[i], paste0(basename(Mapping_File$URL[i])), quiet = TRUE, mode = 'wb')

closeAllConnections()

}


# unzip(paste0(basename(Mapping_File$URL[i])), files = paste0(stringr::str_replace(basename(Mapping_File$URL[i]),'-eng.zip' ,'.csv' )))

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
}


for (i in 1:length(Mapping_File$NAME)){

ref <- Mapping_File$type[i] 
ref 		<- 	gsub("\n"	," ", ref,fixed = TRUE)	
ref 		<- 	gsub("\r"	," ", ref,fixed = TRUE)	
ref 		<- 	gsub("   "	," ", ref,fixed = TRUE)	
ref 		<- 	gsub("  "	," ", ref,fixed = TRUE)	



invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))


con <- unz(paste0(basename(Mapping_File$URL[i])), stringr::str_replace(basename(Mapping_File$URL[i]),'-eng.zip' ,'.csv' ))
X <- NULL
test <- try(X <- eval(parse(text = paste0("readr::read_csv(con , col_types =  ", ref, ", n_max = Inf, guess_max = 1, trim_ws = FALSE, progress  = FALSE)"))) %>% rename(Ref_Date = REF_DATE, Value = VALUE), silent = TRUE)

if(!class(test)[1] == 'tbl_df'){

unzip(paste0(basename(Mapping_File$URL[i])),  stringr::str_replace(basename(Mapping_File$URL[i]),'-eng.zip' ,'.csv' ))

X <- eval(parse(text = paste0("readr::read_csv(stringr::str_replace(basename(Mapping_File$URL[i]),'-eng.zip' ,'.csv' ), col_types =  ", ref, ", n_max = Inf, guess_max = 1, trim_ws = FALSE, progress  = FALSE)"))) %>% rename(Ref_Date = REF_DATE, Value = VALUE)

unlink(stringr::str_replace(basename(Mapping_File$URL[i]),'-eng.zip' ,'.csv' ), recursive = TRUE, force = TRUE)
}




rm(ref)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

colnames(X) <- gsub(' ', '_', colnames(X), fixed = TRUE)
colnames(X) <- gsub('-', '_', colnames(X), fixed = TRUE)
colnames(X) <- gsub('_(NAICS)', '', colnames(X), fixed = TRUE)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

X <- X %>% filter(GEO %in% 'Canada') # X[X[,Condition[1]]%in%Condition[2],]


 if(Mapping_File$NAME[i]%in%c("14100063", '14100064')){
 X <- X %>% mutate(North_American_Industry_Classification_System = gsub("Total employees, all industries","Total employees", North_American_Industry_Classification_System, fixed = TRUE))
}

# if(Mapping_File$NAME[i]%in%"02820048-eng"){
# colnames(X)[colnames(X)%in%"GOGRAPHIE"] <- "GEOGRAPHY"
# }


# if(Mapping_File$NAME[i]%in%"02820072-eng"){
# X[X$TYPEOFWORK%in%"Total employees","TYPEOFWORK"] <- "Both full- and part-time employees"
# X[X$INDUSTRY%in%c("Total employees, all industries","Total employees"),"INDUSTRY"] <- "Total employed, all industries"
# }

# if(Mapping_File$NAME[i]%in%c("02820118-eng", '02820218-eng', '02820219-eng',"02820119-eng", '02820138-eng', '02820137-eng')){
# colnames(X)[colnames(X)%in%"GEO"] <- "GEOGRAPHY"
# colnames(X)[colnames(X)%in% c("URBANRURAL", 'POPANDRURAL')] <- "URBANANDRURAL"
# }

print(Mapping_File$URL[i])
print(paste0(colnames(X), collapse = ' / '))



save(X,file = paste(Mapping_File$NAME[i],".Rdata",sep=""))
rm(X)
#file.remove(paste0('C:/temp/',basename(Mapping_File$URL[i])))
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))


}





setwd(paste0(ilo:::path$data, '/CAN/BULK/'))

# STEP 2 MAP to ILO CODE

for (i in 1:length(Mapping_File$NAME)){

print(Mapping_File$NAME[i])
load(paste('./input/',Mapping_File$NAME[i],".Rdata",sep=""))


X <- X %>% filter(!Value%in%"x") %>% 
	mutate(Sex = Sex %>% plyr:::mapvalues(from = c("Both sexes","Males", "Females", "Both sexes (x 1,000)", "Males (x 1,000)", "Females (x 1,000)"), to = c("T","M", "F", "T", "M", "F"),  warn_missing = FALSE)) %>% 
	filter(!Value %in% NA)


X <- as.data.frame(X)

REF_MAPPING <- Mapping_Definition[Mapping_Definition$File%in%substr(Mapping_File[i,"ID"],1,12),]
REF_MAPPING <- cbind(REF_MAPPING[,colnames(REF_MAPPING)[2:5]],REF_MAPPING[,colnames(REF_MAPPING)%in%colnames(X)])



My_list <- vector("list", length(7:ncol(REF_MAPPING)))


MY_NEW <- cbind(ID_PASS = X[,colnames(X)%in%colnames(REF_MAPPING)[6]],
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				rep(NA,nrow(X)),
				X)
rm(X)
invisible(gc(reset = TRUE))
colnames(MY_NEW)[1:5] <- c('ID_PASS', colnames(REF_MAPPING)[1:4])

for (k in 7:ncol(REF_MAPPING)){
MY_NEW$ID_PASS <- paste(MY_NEW$ID_PASS,MY_NEW[,colnames(MY_NEW)%in%colnames(REF_MAPPING)[k]],sep=";")
}
MY_MATRIX <- MY_NEW[1,c("ID_PASS","indicator/classif1/classif2","Sex","freq_code","note_classif","note_indicator","Ref_Date","Value")]


j <- 1
for (j in 1:nrow(REF_MAPPING)){

MY_NEW[,2]<- REF_MAPPING[j,1]
MY_NEW[,3]<- REF_MAPPING[j,2]
MY_NEW[,4]<- REF_MAPPING[j,3]
MY_NEW[,5]<- REF_MAPPING[j,4]




for (k in 6:ncol(REF_MAPPING)){
My_list[[k]] <- levels(as.factor(unlist(strsplit(REF_MAPPING[j,colnames(REF_MAPPING)[k]],";"))))
}
My_REF <- My_list[[6]]

for(k in 7:ncol(REF_MAPPING)){
My_REF <- paste(sort(rep(My_REF,length(My_list[[k]]))),My_list[[k]],sep=";")
}

MY_MATRIX <-rbind(MY_MATRIX,
			MY_NEW[MY_NEW$ID_PASS%in%My_REF,colnames(MY_NEW)%in%c("ID_PASS","indicator/classif1/classif2","Sex","freq_code","note_classif","note_indicator","Ref_Date","Value")])
}


MY_MATRIX <- MY_MATRIX[-1,]

invisible(gc(reset = TRUE))
ifelse(unique(substr(MY_MATRIX$Ref_Date,5,5))%in%"-",MY_MATRIX$Ref_Date <- paste("Y",substr(MY_MATRIX$Ref_Date,1,4),"_M",substr(MY_MATRIX$Ref_Date,6,7),sep=""),MY_MATRIX$Ref_Date <- paste("Y",substr(MY_MATRIX$Ref_Date,1,4),sep=""))

######################### NEXT STEP
MY_MATRIX$ID_PASS <- paste(MY_MATRIX[,"indicator/classif1/classif2"],MY_MATRIX$Sex,MY_MATRIX$Ref_Date,sep="/")



MY_MATRIX$Value <- as.numeric(MY_MATRIX$Value)

MY_MATRIX <- My_unsplit_KEY(MY_MATRIX,"indicator/classif1/classif2",c("indicator","classif1","classif2"))
MY_MATRIX <- MY_MATRIX[,!colnames(MY_MATRIX)%in%"indicator/classif1/classif2"]
My_REF <- as.data.frame(cbind(ID=levels(as.factor(MY_MATRIX$ID_PASS)),
				collection = Mapping_File$collection[i],
				ref_area = Mapping_File$ref_area[i],
				source = Mapping_File$source[i],
				indicator = "",
				classif1 = "",
				classif2 = "",				
				sex = "",
				time = "",
				obs_value =as.vector(by(MY_MATRIX$Value,as.factor(MY_MATRIX$ID_PASS),sum,simplify=TRUE)),
				freq_code = "",
				note_classif = "",
				note_indicator = ""),stringsAsFactors=FALSE)





My_REF <- My_Vlookup(My_REF,"ID","indicator",MY_MATRIX,"ID_PASS","indicator")
My_REF <- My_Vlookup(My_REF,"ID","classif1",MY_MATRIX,"ID_PASS","classif1")
My_REF <- My_Vlookup(My_REF,"ID","classif2",MY_MATRIX,"ID_PASS","classif2")
My_REF <- My_Vlookup(My_REF,"ID","sex",MY_MATRIX,"ID_PASS","Sex")
My_REF$sex <- paste0("SEX_",My_REF$sex)
My_REF <- My_Vlookup(My_REF,"ID","time",MY_MATRIX,"ID_PASS","Ref_Date")
My_REF <- My_Vlookup(My_REF,"ID","freq_code",MY_MATRIX,"ID_PASS","freq_code")
My_REF <- My_Vlookup(My_REF,"ID","note_classif",MY_MATRIX,"ID_PASS","note_classif")
My_REF <- My_Vlookup(My_REF,"ID","note_indicator",MY_MATRIX,"ID_PASS","note_indicator")



My_REF$ID <- NA
X <- as.tbl(My_REF) %>% select(-ID)
rm(My_REF,MY_MATRIX,MY_NEW,REF_MAPPING)
save(X,file = paste('./input/',Mapping_File$ID[i],".Rdata",sep=""))
rm(X)
invisible(gc(reset = TRUE))

print(Mapping_File$ID[i])

}

# STEP 3 Combined BY COUNTRY 
Y <- NULL
for (i in 1:length(Mapping_File$ID)){
print(Mapping_File$ID[i])
load(paste('./input/',Mapping_File$ID[i],".Rdata",sep=""))

Y <- bind_rows(Y, as.tbl(X))
rm(X)
invisible(gc(reset = TRUE))
}






REF <- levels(as.factor(substr(Y$source,1,2)))

Y <- Y %>% 
		as.tbl %>%  
		mutate(	obs_status  = as.character(NA), 
				note_source = 'R1:3903', # add tag Bulk
				obs_value = as.numeric(obs_value)) %>%
		select(	collection,  
				ref_area , 
				source , 
				indicator , 
				sex , 
				classif1 ,
				classif2 , 
				time , 
				obs_value , 
				obs_status  , 
				freq_code , 
				note_classif , 
				note_indicator , 
				note_source  ) %>% 
		mutate(	time = paste0(str_sub(time, 2,5), str_sub(time,7,9)), 
				time = ifelse(str_sub(time,5,5) %in% 'Q', paste0(str_sub(time, 1,5), str_sub(time, -1,-1)), time)) %>% 
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE))) %>% 
		switch_ilo(version) %>% 
		mutate(	classif1_version = str_sub(classif1_version,1,3),
				classif2_version = str_sub(classif2_version,1,3))
 

############## remove if available with MICRO

micro <- readRDS(paste0(INPUT, Target, "_LFS_ilostat.rds")) %>% distinct()

Y <- Y %>% 	left_join(micro , by = c("source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
			filter(!micro %in% 'yes') %>% 
			select(-micro, -sex_version, -classif1_version, -classif2_version)


for (i in 1:length(REF)){
X <- Y[substr(Y$source,1,2)%in%REF[i],]
save(X,file = paste0('./output/CAN_',REF[i],'.Rdata'))
rm(X)
invisible(gc(reset = TRUE))
}


REF <- cbind(PATH = paste0(getwd(), "/output/CAN_",REF,".Rdata"),ID = NA, Types  ="NSO_ilostat", REF = 'CAN')
write.csv(REF,paste0("./FileToLoad.csv"),row.names = FALSE,na="")





final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)



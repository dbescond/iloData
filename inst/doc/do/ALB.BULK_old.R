#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################
Target <- "ALB"
init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))

if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(Ariane,quietly =TRUE)
require(lubridate, quietly =TRUE)
require(readxl,quietly =TRUE)
setwd(paste0(ilo:::path$data, '/',Target,'/BULK/'))
Sys.setenv(http_proxy="") #
Sys.setenv(https_proxy="")
Sys.setenv(ftp_proxy="") # 

INPUT <- paste0(ilo:::path$data, '/',Target,'/BULK/input/')

Mapping_File <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="File", guess_max = 1000)  %>% filter(IsValidate %in% 'Yes')
Mapping_Definition <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="Definition", guess_max = 21474836) 



# STEP 1 Download, open, CLEAN UP AND REDUCE ORIGINAL FILE
{ 
require(RSelenium, quietly =TRUE)
shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                ,  browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                                #, browser.helperApps.neverAsk.saveToDisk = "application/vnd.ms-excel"))
                                #, browser.helperApps.neverAsk.saveToDisk = "application/octet-stream"))
								# application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
#RSelenium::startServer()
remDr <- remoteDriver( extraCapabilities = fprof )
remDr$open()    
	
	
	
	
	
	remDr$navigate('http://www.instat.gov.al/en/themes/labour-market-and-education/employment-and-unemployment-from-lfs/#tab2' )
	
Sys.sleep(3)


	
	webElem <- remDr$findElement(value = '//a[@href = "#tab2"]')
	webElem$clickElement()
	
	webElem <- remDr$findElement(value = '//a[@href = "/media/2567/t1.xlsx"]')
	webElem$clickElement()
	Sys.sleep(2)
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.xlsx'))
	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', newfile$value[1]))

	webElem <- remDr$findElement(value = '//a[@href = "/media/2570/t2.xlsx"]')
	webElem$clickElement()
	Sys.sleep(2)
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.xlsx'))
	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', newfile$value[1]))

	webElem <- remDr$findElement(value = '//a[@href = "/media/3119/t3.xlsx"]')
	webElem$clickElement()
	Sys.sleep(2)
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.xlsx'))
	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', newfile$value[1]))

	webElem <- remDr$findElement(value = '//a[@href = "/media/3120/t4.xlsx"]')
	webElem$clickElement()
	Sys.sleep(2)
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.xlsx'))
	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', newfile$value[1]))

	invisible(gc(reset = TRUE))

remDr$closeServer()




for (i in 1:length(Mapping_File$NAME)){


if(i %in% 1:3){
X <- readxl:::read_excel(paste0('./input/', Mapping_File$NAME[i], '.xlsx'), skip=5)%>% 
		select(-c(1,2)) %>% 
		gather(Time, Value, -Sex, -Age) %>% 
		fill(Age) %>% 
		mutate(Time = paste0(str_sub(Time,-4,-1), str_sub(Time,-7,-6))) 
}

if(i %in% 4){
X <- readxl:::read_excel(paste0('./input/', Mapping_File$NAME[i], '.xlsx'), skip=3)%>% 
		select(-c(1)) %>% 
		rename(Indicator = X__2) %>%
		gather(Time, Value, -Indicator) %>% 
		mutate(Time = paste0(str_sub(Time,-4,-1), 'Q',str_sub(Time,-7,-7))) %>% 
		filter(!Indicator %in% 'of which') %>% 
		mutate(	
			Value = Value / 1000,
			Indicator = gsub('15 years old and over', '15-++ years', Indicator), 
			Age = str_sub(Indicator, -11, -1), 
			Indicator = ifelse(nchar(Indicator)> 12, str_sub(Indicator, 1, -13), NA), 
			Age = gsub('15-++ years', '15 years old and over', Age, fixed = TRUE)) %>% 
		fill(Indicator)
		
}

save(X, file = paste0('./input/', Mapping_File$NAME[i], '.Rdata'))

unlink(paste0('./input/', Mapping_File$NAME[i], '_COLNAMES.csv'))

rm(X)
invisible(gc(reset = TRUE))

}

}


# STEP 2 MAP to ILO CODE
for (i in 1:length(Mapping_File$NAME)){

	print(Mapping_File$NAME[i])
	load(paste0(INPUT,Mapping_File$NAME[i],".Rdata"))




	# get mapping frame File should be filled and File name correspond to Mapping_File ID 
	REF_MAPPING <- Mapping_Definition %>% filter(!File %in% NA, File %in% Mapping_File$ID[i]) %>% select(-File)
	# reduce mapping frame to columns ILO KEY + columns available on the dataset 

	REF_MAPPING <- REF_MAPPING %>% 
					select(contains('_Code')) %>% 
					bind_cols(REF_MAPPING %>% select_(.dots = colnames(X)[!colnames(X)%in% c('Time','Value') ]))

	# split columns to avail mapping redondancy					

	# SplitCol <- Mapping_File$SplitCol[i]
	# if(!is.na(SplitCol)){
		# SplitCol <- str_split(SplitCol, ' = ') %>% unlist
		# SplitCol[1] <- gsub(' ', '.', SplitCol[1], fixed = TRUE)
		# ref <- str_split(unique(REF_MAPPING[,SplitCol[[1]]]), ';') %>% unlist
		# MAP <- NULL
		# for ( j in seq_along(ref)){
			# MAP <- bind_rows(MAP,
							# bind_cols(REF_MAPPING %>% select(-contains(SplitCol[1])), data_frame(pass = 1:nrow(REF_MAPPING), ToChange = ref[j])) 
						# )
		# }					
		# REF_MAPPING <- MAP %>% select(-pass) 
		## map sex 
		# test <- try(
					# REF_MAPPING <- REF_MAPPING %>% 	mutate(ToChangeCode = mapvalues(ToChange, c('Both sexes','Female','Male'),  c('SEX_T','SEX_F','SEX_M'), warn_missing = FALSE)) 
				# , silent = TRUE )

		# colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChange'] <- SplitCol[1]
		# colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChangeCode'] <- SplitCol[2]
	# } else {
		# REF_MAPPING <- REF_MAPPING %>% mutate(Sex_Code = 'SEX_T')
	# }
	# rm(SplitCol)

	#create ilo key	of ref_mapping
	ref_key_ilo <- REF_MAPPING %>% slice(1) %>% select(contains('_Code')) %>% colnames
	REF_MAPPING <- REF_MAPPING %>% unite_('KEY_ILO', ref_key_ilo , remove = TRUE, sep = '/') 
	ref_key_ilo <-  paste(ref_key_ilo, collapse = '/')

	# clean
	REF_MAPPING <- REF_MAPPING %>% 	mutate_all(funs(gsub('&amp;','&', ., fixed = TRUE))) 

	#create key	of X in national language
	ref_key_nat <- X %>% slice(1) %>% select(-Time, -Value) %>% colnames
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
					MY_NEW[MY_NEW$KEY_NAT%in%My_REF,colnames(MY_NEW)%in%c("KEY_NAT","KEY_ILO","Time","Value")])
	}

	invisible(gc(reset = TRUE))

	######################### NEXT STEP

	X <- MY_MATRIX  %>%
				mutate(Value = as.numeric(Value)) %>% 
				select(-KEY_NAT) %>% 
				group_by(KEY_ILO, Time) %>% 
				summarise(Value = sum(Value, na.rm = TRUE)) %>% 
				ungroup %>%
				rename(ID = KEY_ILO) %>%
				mutate(	Collection_Code = Mapping_File$Collection_Code[i],
						Country_Code = Mapping_File$Country_Code[i],
						Source_Code = Mapping_File$Source_Code[i])  %>% 
				separate(ID, stringr::str_split(ref_key_ilo, '/') %>% unlist, remove = FALSE, sep = '/') %>%
				select(-ID) %>% 
		mutate(Value = as.numeric(Value))

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

X <- X %>% mutate_all(as.character)
if(i==1) Y <- X else Y <- bind_rows(Y,X)
rm(X)
invisible(gc(reset = TRUE))

}

REF <- levels(as.factor(substr(Y$Source_Code,1,2)))


Y <- Y %>% # converge to ilostat format
		as.tbl %>%  
		mutate(	obs_status  = as.character(NA), 
				note_source = 'R1:3903', # add tag Bulk
				obs_value = as.numeric(Value)) %>% 
		select(	collection = Collection_Code,  
				ref_area = Country_Code, 
				source = Source_Code, 
				indicator = Indicator_Code, 
				sex = Sex_Code, 
				classif1 = Classif1_Code, 
				classif2 = Classif2_Code, 
				time = Time, 
				obs_value , 
				obs_status, 
				freq_code = Notes_Frequency_Code, 
				note_classif = Notes_Classif_Code, 
				note_indicator = Notes_Indicator_Code, 
				note_source
				 )  %>%  
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE))) %>%
		filter(as.numeric(str_sub(time,1,4)) > 2013) %>% 
		mutate(obs_status = ifelse(time %in% '2014Q1', 'B', obs_status))
 
			
			
			
			
for (i in 1:length(REF)){
X <- Y %>% filter(substr(source,1,2)%in%REF[i])
save(X,file = paste(getwd(),'/output/',Target,'_',REF[i],".Rdata",sep=""))
rm(X)
invisible(gc(reset = TRUE))
}



REF <- cbind(PATH = paste0(getwd(), '/output/',Target,'_',REF,".Rdata"),ID = NA, Types  ="NSO_ilostat", REF = Target)
# add historical data

write.csv(REF,paste("./FileToLoad.csv",sep=""),row.names = FALSE,na="")






final_time <- Sys.time(); final_time - init_time
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################
Target <- "MAC"
require(tidyverse,quietly =TRUE)
init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))

if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(Ariane,quietly =TRUE)

require(lubridate, quietly =TRUE)
require(readxl,quietly =TRUE)
setwd(paste0(ilo:::path$data, '/',Target,'/BULK/'))
Sys.setenv(http_proxy="")
Sys.setenv(ftp_proxy="")

INPUT <- paste0(ilo:::path$data, '/',Target,'/BULK/input/')

Mapping_File <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="File", guess_max = 1000)  %>% filter(IsValidate %in% 'Yes')
Mapping_Definition <- read_excel(paste0('./ReadME_',Target,'.xlsx'), sheet="Definition", guess_max = 21474836) 



# STEP 1 Download, open, CLEAN UP AND REDUCE ORIGINAL FILE
require(RSelenium, quietly =TRUE)
shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\temp"
                                ,  browser.download.folderList = 2L
                                ,  browser.download.autohideButton = TRUE
                                ,  browser.download.animateNotification = TRUE
								, network.proxy.autoconfig_url = 'http://proxyos.ilo.org:8080'
								, network.proxy.http = 'proxyos.ilo.org'
								, network.proxy.http_port = 8080L
								, network.proxy.ftp = 'proxyos.ilo.org'
								, network.proxy.ftp_port = 8080L
								, network.proxy.socks = 'proxyos.ilo.org'
								, network.proxy.socks_port = 8080L
								, network.proxy.ssl = 'proxyos.ilo.org'
								, network.proxy.ssl_port = 8080L                                
								, network.proxy.type = 4L                                
								, browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = 'application/octet-stream' 
								
								))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof, browserName = 'firefox', remoteServerAddr = '127.0.0.1')
remDr$open()    

	
for (i in 1:length(Mapping_File$NAME)){

remDr$navigate('http://www.dsec.gov.mo/Statistic/LabourAndEmployment/EmploymentSurvey.aspx?lang=en-US')
		Sys.sleep(3)
remDr$getTitle()[[1]]
	

	records <- Mapping_File %>% slice(i)
	remDr$navigate(records$URL)
		
		remDr$getTitle()[[1]]
	
		Sys.sleep(6)
	webElem <- remDr$findElements('class name', 'rtbText')
	# webElem[[1]]$highlightElement()
	webElem[[1]]$clickElement()
	
		Sys.sleep(15)
	
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.xls'))

	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', records$NAME, '.xls'))

	

	print(i)
	invisible(gc(reset = TRUE))


	
}
invisible(try(remDr$close()	, silent = TRUE))
invisible(try(remDr$closeServer(), silent = TRUE))


for (i in 1:length(Mapping_File$NAME)){



X <- readxl:::read_excel(paste0('./input/', Mapping_File$NAME[i], '.xls'), sheet = 2)

for (j in 1:ncol(X)){
	if(length(unique(X[[j]])) ==1){ if (unique(X[[j]]) %in% NA) { colnames(X)[j] <- paste0('DELETE_', j)}}
}

X <- X %>% select(-contains('DELETE'))

header <- eval(parse(text = Mapping_File$header[i]))

new_header <- X %>% select(-c(1,2)) %>% slice(1:length(header)) %>% t %>% as_data_frame
new_header <- new_header %>% fill(V1)

colnames(new_header) <- header


new_header <- new_header %>% select(-contains('DELETE')) %>% mutate(ID = paste0('ID_',1:n()))

colnames(X) <- c('year', 'period', new_header$ID)

X <- X %>% 	slice(-c(1:length(header))) %>% 
			gather(key = "ID", value = "Value", -year, -period) %>% 
			left_join(new_header, by = "ID")  %>% 
			select(-ID)

for (j in 1990:2030){

		X <-  X %>% mutate( period = period %>% str_replace_all(fixed(paste0('/',j )), ''))
}	

X <- X %>% filter(!Value %in% NA) %>% 
			mutate(period = period %>% 
							plyr:::mapvalues(
								from 	= c(NA, "7 - 9", "8 - 10", "9 - 11", "10 - 12", "11 - 1", "12 - 2", "1 - 3", "2 - 4", "3 - 5", "4 - 6", "5 - 7", "6 - 8", "Qtr.1", "Qtr.2", "Qtr.3", "Qtr.4"),
								to 		= c('', "M08", "M09", "M10", "M11", "M12", "M01", "M02", "M03", "M04", "M05", "M06", "M07", "Q1", "Q2", "Q3", "Q4"), warn_missing = FALSE)) %>% 
			fill(year) %>% mutate(year = as.numeric(year))
			
for (j in 1:nrow(X)){

if(X$period[j] %in% 'M01'){X$year[j] <- X$year[j] + 1}

}			
			
X <- X %>% unite(Time, year, period, sep = '') %>% as.tbl %>% 
			filter(!Value %in% c('', NA, '~')) %>% 
			mutate(Value = as.numeric(Value))

if(Mapping_File$NAME[i] %in% 'ReportID5') {
X <- X %>% mutate(sex = ifelse(sex %in% NA, 'Total', sex))
}




save(X, file = paste0('./input/', Mapping_File$NAME[i], '.Rdata'))
print(Mapping_File$NAME[i])
rm(X)
invisible(gc(reset = TRUE))

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
				select(-ID) 

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
		as.tbl %>%  mutate(obs_status  =as.character(NA), note_source = as.character(NA), Value = as.numeric(Value)) %>% 
		select(	collection = Collection_Code,  
				ref_area = Country_Code, 
				source = Source_Code, 
				indicator = Indicator_Code, 
				sex = Sex_Code, 
				classif1 = Classif1_Code, 
				classif2 = Classif2_Code, 
				time = Time, 
				obs_value = Value, 
				obs_status, 
				freq_code = Notes_Frequency_Code, 
				note_classif = Notes_Classif_Code, 
				note_indicator = Notes_Indicator_Code, 
				note_source
				 )  %>%  
		mutate_all(funs(plyr::mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE))) 
 
############################################ exception

####### calculate ECO_ISIC3_M (MNP) with TOTAL - sum

TEST <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_ECO_NB')) %>% 
		group_by(collection, ref_area, source, indicator, time) %>% 
		tally() %>% ungroup %>% filter(n == 3)

Y <- Y %>% left_join(TEST, by = c("collection", "ref_area", "source", "indicator", "time"))	%>% 
		filter(n  %in%NA) %>% 
		select(-n)	
		
EMP_ECO_NAG <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_ECO_NB'), !classif1 %in% 'ECO_ISIC3_TOTAL') %>% 
		mutate(classif1 = 'ECO_NAG') %>% 
		group_by(collection, ref_area, source, indicator, sex, classif1, time) %>% 
		summarise(	obs_value = sum(obs_value)) %>% 
		ungroup 
EMP_ECO_TOTAL <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_ECO_NB'), classif1 %in% 'ECO_ISIC3_TOTAL') %>% 
		left_join(EMP_ECO_NAG %>% mutate(classif1 = 'ECO_ISIC3_TOTAL') %>% rename(NAG = obs_value), by = c("collection", "ref_area", "source", "indicator", "sex", "classif1", "time")) %>% 
		filter(!NAG %in% NA) %>% 
		mutate(obs_value = obs_value - NAG, 
			   classif1 = 'ECO_ISIC3_M', 
			   note_classif = 'C5:1010_C5:1029') %>%
		select(-NAG) 
		
Y <- bind_rows(Y, EMP_ECO_TOTAL)	
rm(EMP_ECO_TOTAL, EMP_ECO_NAG, TEST)	
		
####### calculate COU_ISCO88_X with TOTAL - sum

			
TEST <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_OCU_NB')) %>% 
		group_by(collection, ref_area, source, indicator, time) %>% 
		tally() %>% ungroup %>% filter(n == 3)
			

Y <- Y %>% left_join(TEST, by = c("collection", "ref_area", "source", "indicator", "time"))	%>% 
		filter(n  %in%NA) %>% 
		select(-n)	
					
			
EMP_OCU_X <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_OCU_NB'), !classif1 %in% 'OCU_ISCO88_TOTAL') %>% 
		mutate(classif1 = 'OCU_X') %>% 
		group_by(collection, ref_area, source, indicator, sex, classif1, time) %>% 
		summarise(	obs_value = sum(obs_value)) %>% 
		ungroup 
		
EMP_OCU_TOTAL <- Y %>% filter(indicator %in% c('EMP_TEMP_SEX_OCU_NB'), classif1 %in% 'OCU_ISCO88_TOTAL') %>% 
		left_join(EMP_OCU_X %>% mutate(classif1 = 'OCU_ISCO88_TOTAL') %>% rename(OCU_X = obs_value), by = c("collection", "ref_area", "source", "indicator", "sex", "classif1", "time")) %>% 
		filter(!OCU_X %in% NA) %>% 
		mutate(obs_value = obs_value - OCU_X, 
			   classif1 = 'OCU_ISCO88_X') %>%
		select(-OCU_X) 
Y <- bind_rows(Y, EMP_OCU_TOTAL) %>% mutate(note_source = 'R1:3903')
	
rm(EMP_OCU_TOTAL, EMP_OCU_X, TEST)			
			
			
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
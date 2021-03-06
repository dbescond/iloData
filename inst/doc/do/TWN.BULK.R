#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################
Target <- "TWN"
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
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                , browser.download.folderList = 2L
                                , browser.download.autohideButton = TRUE
                                , browser.download.animateNotification = TRUE
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
                                , browser.helperApps.alwaysAsk.force = FALSE
								, browser.download.manager.alertOnEXEOpen = FALSE
								, browser.download.manager.focusWhenStarting = FALSE
								, browser.download.manager.useWindow = FALSE
								, browser.download.manager.showWhenStarting = FALSE
								, browser.download.manager.showAlertOnComplete = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "text/csv"))
                                #, browser.helperApps.neverAsk.saveToDisk = "application/octet-stream"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()    
	
for (i in 1:length(Mapping_File$NAME)){


records <- Mapping_File %>% slice(i)
	remDr$navigate(records$URL )
	
	Sys.sleep(10)

	
	webElem <- remDr$findElements('class name', 'tdtop')
	# webElem$highlightElement()
	
	test <- webElem[[1]]$findChildElements(value = '//a[@alt = "Select all"]')
	
	
	ref <- NULL
	
	for (j in 1:length(test)){
		
		test[[j]]$clickElement()
	
	}
		
	remDr$findElement(value = '//input[@value = "Continue"]')$highlightElement()
	remDr$findElement(value = '//input[@value = "Continue"]')$clickElement()
	
		Sys.sleep(10)
	remDr$findElement(value = '//input[@src = "includes/prnfiless.gif"]')$highlightElement()
	remDr$findElement(value = '//input[@src = "includes/prnfiless.gif"]')$clickElement()
	

		Sys.sleep(10)


		

	
	newfile <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.')) %>% filter(str_detect(value, '.csv'))

	file.rename(paste0("C:\\temp\\",newfile$value[1]),paste0('./input/', records$NAME, '.csv'))

	

	print(i)
	invisible(gc(reset = TRUE))


	
}
remDr$closeServer()
# remDr$close()


for (i in 1:length(Mapping_File$NAME)){

n_ref <- ncol(read_csv(paste0('./input/', Mapping_File$NAME[i], '.csv'), col_names = FALSE, skip = 10))


ref <- read_lines(paste0('./input/', Mapping_File$NAME[i], '.csv'))


 if(!str_detect(ref[3], 'Education')){

	ref_title <- paste0(str_sub(ref[1],2,-1), str_sub(ref[2],2,-1), collapse = ' ')%>% 
				str_replace(fixed('\"\"'), '') %>% 
				str_replace(fixed('\"'), '') %>% 
				str_split('by', simplify = TRUE) 
	}  else{
				
		if(ref[2] %in% ''){

			ref_title <- paste0(str_sub(ref[1],2,-1), collapse = ' ')%>% 
						str_replace_all(fixed('"'), '') %>%
						str_replace(fixed('and\"'), 'and ') %>% 
						str_split('by', simplify = TRUE) 
		} else {			
				
			ref_title <- paste0(str_sub(ref[1],2,-1), str_sub(ref[2],2,-1), str_sub(ref[3],2,-1), collapse = ' ')%>% 
				str_replace(fixed('\"\"'), '') %>% 
				str_replace(fixed('\"'), '') %>% 
				str_replace(fixed('and\"'), 'and ') %>% 
				str_sub(1,-2) %>% str_split('by', simplify = TRUE) 
								
				
		}
	}
				
ref_title <- ref_title[length(ref_title)]	 %>%  
				str_split(',|and', simplify = TRUE) %>% t %>% as_data_frame %>% 
				mutate(V1 = str_trim(V1)) %>% 
				filter(!tolower(V1) %in% 'period') %>% t %>% as.character %>%
				str_replace(fixed('"'), '') 
ref_title <- str_replace(ref_title, 'Class ofWorkers', 'Class of Workers')

		
test <- ref %>% as_data_frame %>% mutate(		line = 1:n(), 
										start = ifelse(str_sub(value,1,1) %in% "", line + 1, NA),
										start =   ifelse(str_sub(value,2,5) %in% Mapping_File$Start_Year[i], line-1, start)) 
										
test <- test %>% filter(!start %in% NA) %>% slice(1:2) %>% select(start) %>% t %>% as.numeric										
write_lines(ref[test[2]:test[1]], path = paste0('./input/', Mapping_File$NAME[i], '_COLNAMES.csv'), na = "NA", append = FALSE)

NEW <-  read_csv(paste0('./input/', Mapping_File$NAME[i], '_COLNAMES.csv'), col_names = FALSE)  %>% select(-X1) %>% t

if(length(ref_title) == 2){
	NEW <- NEW %>% as_data_frame %>% fill(V1, V2, .direction = 'down') %>% select(2:1)
	new_colnames <- NEW %>% mutate(ref = paste(V2, V1, sep = ' / ')) %>% select(ref) %>% t %>% as.character
}
if(length(ref_title) == 3){
	NEW <- NEW %>% as_data_frame %>% fill(V1, V2, V3, .direction = 'down') %>% select(3:1)
	new_colnames <- NEW %>% mutate(ref = paste(V3, V2, V1, sep = ' / ')) %>% select(ref) %>% t %>% as.character
}
if(length(ref_title) == 4){
	NEW <- NEW %>% as_data_frame %>% fill(V1, V2, V3, V4, .direction = 'down') %>% select(4:1)
	new_colnames <- NEW %>% mutate(ref = paste(V4, V3, V2, V1, sep = ' / ')) %>% select(ref) %>% t %>% as.character
}
colnames(NEW) <- ref_title

print(Mapping_File$NAME[i])
print(paste0(ref_title))

X <- read_csv(paste0('./input/', Mapping_File$NAME[i], '.csv'), col_names = TRUE, skip = test[2]-1 ) 

colnames(X) <- c('Time', new_colnames)

X <- X %>% gather(key = 'key', value = 'Value', na.rm = TRUE, -Time) %>% separate(key, ref_title, sep = ' / ') %>% 
			filter(!Value %in% c(NA, '-', '--')) %>% 
			mutate(Value = as.numeric(Value))

save(X, file = paste0('./input/', Mapping_File$NAME[i], '.Rdata'))

unlink(paste0('./input/', Mapping_File$NAME[i], '_COLNAMES.csv'))

rm(X)
invisible(gc(reset = TRUE))

}




# STEP 2 MAP to ILO CODE
for (i in 1:length(Mapping_File$NAME)){

	print(Mapping_File$NAME[i])
	load(paste0(INPUT,Mapping_File$NAME[i],".Rdata"))



	# avoid white space in column header			
	colnames(X) <- gsub(' ', '.', colnames(X), fixed = TRUE)
	colnames(X) <- gsub('5', '', colnames(X), fixed = TRUE)

	# get mapping frame File should be filled and File name correspond to Mapping_File ID 
	REF_MAPPING <- Mapping_Definition %>% filter(!File %in% NA, File %in% Mapping_File$ID[i]) %>% select(-File)
	# reduce mapping frame to columns ILO KEY + columns available on the dataset 

	REF_MAPPING <- REF_MAPPING %>% 
					select(contains('_Code')) %>% 
					bind_cols(REF_MAPPING %>% select_(.dots = colnames(X)[!colnames(X)%in% c('Time','Value') ]))

	# split columns to avail mapping redondancy					

	SplitCol <- Mapping_File$SplitCol[i]
	if(!is.na(SplitCol)){
		SplitCol <- str_split(SplitCol, ' = ') %>% unlist
		SplitCol[1] <- gsub(' ', '.', SplitCol[1], fixed = TRUE)
		ref <- str_split(unique(REF_MAPPING[,SplitCol[[1]]]), ';') %>% unlist
		MAP <- NULL
		for ( j in seq_along(ref)){
			MAP <- bind_rows(MAP,
							bind_cols(REF_MAPPING %>% select(-contains(SplitCol[1])), data_frame(pass = 1:nrow(REF_MAPPING), ToChange = ref[j])) 
						)
		}					
		REF_MAPPING <- MAP %>% select(-pass) 
		# map sex 
		test <- try(
					REF_MAPPING <- REF_MAPPING %>% 	mutate(ToChangeCode = mapvalues(ToChange, c('Both sexes','Female','Male'),  c('SEX_T','SEX_F','SEX_M'), warn_missing = FALSE)) 
				, silent = TRUE )

		colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChange'] <- SplitCol[1]
		colnames(REF_MAPPING)[colnames(REF_MAPPING) %in% 'ToChangeCode'] <- SplitCol[2]
	} else {
		REF_MAPPING <- REF_MAPPING %>% mutate(Sex_Code = 'SEX_T')
	}
	rm(SplitCol)

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
		as.tbl %>%  mutate(obs_status  =as.character(NA), note_source = 'R1:3903', Value = as.numeric(Value)) %>% 
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
		mutate_all(funs(mapvalues(.,c('XXX_XXX_XXX', 'NaN', '', ' ', 'NA'), c(NA, NA, NA, NA, NA), warn_missing = FALSE)))
 



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

#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################

download_data_KOR <- function(Mapping_File,  TIME_WAIT = 2){
# Mapping_File <- Mapping_File %>% slice(i)


NAME <- Mapping_File$NAME
URL <- Mapping_File$URL
Dimension <-  Mapping_File$Dimension
Pivot <- Mapping_File$Pivot
BulkDownload <- Mapping_File$BulkDownload
#Popup <- Mapping_File$Popup
# Delete <- Mapping_File$DeleteTime


#   i <- 7 ;  URL <- Mapping_File$URL[i]; NAME <- Mapping_File$NAME[i]; Dimension <-  Mapping_File$Dimension[i] ; Pivot <- Mapping_File$Pivot[i] ; BulkDownload <- Mapping_File$BulkDownload[i]; Popup <- Mapping_File$Popup[i]


#test <- list.files(INPUT)
#test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
#if(length(test)!=0){
#for(j in 1:length(test)){file.remove(paste0(INPUT, test[j]))}
#}
require(RSelenium)
shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(TIME_WAIT * 2)

# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)

fprof <- makeFirefoxProfile(list( browser.download.dir = "C:\\temp"
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
                                , browser.helperApps.neverAsk.saveToDisk = "text/html"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()    
	

	remDr$navigate(URL)
	
	Sys.sleep(TIME_WAIT * 15)

	MainWindow <- remDr$getCurrentWindowHandle() %>% unlist 

	
# go on pivot frame
if(Pivot) {
	webElem <- remDr$findElement('id', 'ico_swap')
	webElem$highlightElement()
	webElem$clickElement()
	Sys.sleep(TIME_WAIT * 2)
	
	webElem <- remDr$findElement('id', 'pop_pivotfunc2')$findChildElement('id', 'Ri1')
	webElem$highlightElement()
	webElem$clickElement()

	webElem <- remDr$findElement('id', 'pop_pivotfunc2')$findChildElement(value = '//a[@href = "javascript:fn_remove();"]')
	webElem$highlightElement()
	webElem$clickElement()

	webElem <- remDr$findElement('id', 'pop_pivotfunc2')$findChildElement('class name', 'confirmBtn')
	webElem$highlightElement()
	webElem$clickElement()
	
	# webElem <- remDr$findElement('id', 'pop_pivotfunc2')$findChildElement('class name', 'closeBtn')
	# webElem$highlightElement()
	# webElem$clickElement()
	Sys.sleep(TIME_WAIT * 80)
}
# go on add function frame	
	webElem <- remDr$findElement('id', 'ico_addfunc')
	webElem$highlightElement()
	webElem$clickElement()
	Sys.sleep(TIME_WAIT * 2)
	
	#webElem <- remDr$findElement('id', 'pop_addfunc')$findChildElement('name', 'dataOpt2')
	#webElem$highlightElement()
	#webElem$sendKeysToElement(list("Code+English"))
	
	webElem <- remDr$findElement('id', 'pop_addfunc')$findChildElement('name', 'periodCo')
	webElem$highlightElement()
	webElem$sendKeysToElement(list("5"))
	
	webElem <- remDr$findElement('id', 'pop_addfunc')$findChildElement(value = '//a[@href = "javascript:fn_apply();"]')
	webElem$clickElement()
	Sys.sleep(TIME_WAIT * 80)

# setting criteria	
	# open box 'Setting Criteria'
	webElem <- remDr$findElement('class name', 'leftBtn')
	webElem$highlightElement()
	webElem$clickElement()
	Sys.sleep(TIME_WAIT * 30)
	
	#
	
	remDr$switchToFrame('ifrSearchDetail')

	# pass all items on the left by clicking on all button '>>'
	
	webElem <- remDr$findElement('id', 'ifr_pop_selectAll2')$findChildElement('class name' , 'detailSelect')$findChildElements(using = 'xpath', value = "//*/img[@src = 'images/rangeDetail/fw1.gif']")
	
	for (j in 1:Dimension){
	try(webElem[[j]]$highlightElement(), silent = TRUE)
	try(webElem[[j]]$clickElement()	, silent = TRUE)
	}


	# click on tab 'Quartely' and then select all with '>>'
	DivTab <- remDr$findElement('id', 'ifr_pop_selectAll2')$findChildElement('css selector' , 'div.detailTabDiv')
	
	webElem <- DivTab$findChildElement(using = 'css selector', value = "li#Q")
	webElem$highlightElement()
	webElem$clickElement()
	webElem <- remDr$findElement('id', 'ifr_pop_selectAll2')$findChildElement('class name' , 'detailSelect')$findChildElements(using = 'xpath', value = "//*/img[@src = 'images/rangeDetail/fw1.gif']")
	webElem[[Dimension]]$highlightElement()
	webElem[[Dimension]]$clickElement()
	
	# click on tab 'Annual' and then select all with '>>'
	webElem <- DivTab$findChildElement(using = 'css selector', value = "li#Y")
	webElem$highlightElement()
	webElem$clickElement()
	webElem <- remDr$findElement('id', 'ifr_pop_selectAll2')$findChildElement('class name' , 'detailSelect')$findChildElements(using = 'xpath', value = "//*/img[@src = 'images/rangeDetail/fw1.gif']")
	webElem[[Dimension]]$highlightElement()
	webElem[[Dimension]]$clickElement()
	rm(DivTab)
	
	# click on 'Apply' and validate the Alert
	webElem <- remDr$findElement('id', 'ifr_pop_selectAll2')$findChildElement(value = '//a[@href = "javascript:fn_definite();"]')
	webElem$highlightElement()
	webElem$clickElement()
	
	
	Sys.sleep(TIME_WAIT * 40)
#if(Popup){
	try(remDr$acceptAlert(), silent = TRUE)
			Sys.sleep(TIME_WAIT * 8)
	try(remDr$acceptAlert(), silent = TRUE)
		Sys.sleep(TIME_WAIT * 8)
	try(remDr$acceptAlert(), silent = TRUE)
			Sys.sleep(TIME_WAIT * 8)
	try(remDr$acceptAlert(), silent = TRUE)
#}		
	Sys.sleep(TIME_WAIT * 130)
	
# download database	
	# open box
if(BulkDownload){	


	# back to main frame
	remDr$switchToFrame(NULL)
	
	webElem <- remDr$findElement(using = 'id', value = "tabMenu")$findChildElement(using = 'xpath', value = "//*/img[@src = 'images/btn_downLoad_en.gif']")
	webElem$highlightElement()	
	webElem$clickElement()
	

	webElem <- remDr$findElement(using = 'id', value = "pop_downlarge")$findChildElement(using = 'id', value = "downLargeCSV")	
	webElem$clickElement()
	#webElem <- remDr$findElement(using = 'id', value = "pop_downlarge")$findChildElement(using = 'id', value = "exprYn")	
	#webElem$clickElement()
	webElem <- remDr$findElement(using = 'id', value = "pop_downlarge")$findChildElement(value = '//a[@href = "javascript:fn_downLargeSubmit();"]')
	webElem$clickElement()	
	Sys.sleep(TIME_WAIT * 50)

	webElem <- remDr$findElement('id', 'pop_downglarge2')$findChildElement('link text' , 'X Close')
	webElem$highlightElement()	
	webElem$clickElement()	
} else{

	# back to main frame
	remDr$switchToFrame(NULL)
	
	webElem <- remDr$findElement(using = 'id', value = "ico_download")
	webElem$highlightElement()	
	webElem$clickElement()

	webElem <- remDr$findElement(using = 'id', value = "pop_downgrid")$findChildElement(using = 'id', value = "csvradio")	
	webElem$clickElement()
	#webElem <- remDr$findElement(using = 'id', value = "pop_downgrid")$findChildElement(using = 'id', value = "codeYn")	
	#webElem$clickElement()
	webElem <- remDr$findElement(using = 'id', value = "pop_downgrid")$findChildElement(using = 'id', value = "PtypeScr")	
	webElem$clickElement()
	webElem <- remDr$findElement(using = 'id', value = "pop_downgrid")$findChildElement(value = '//a[@href = "javascript:fn_downGridSubmit();"]')
	webElem$clickElement()	
	Sys.sleep(TIME_WAIT * 40)
	
	
	
	
	webElem <- remDr$findElement('id', 'pop_downgrid2')$findChildElement('link text' , 'X Close')
	webElem$highlightElement()	
	webElem$clickElement()	

}	

	Sys.sleep(TIME_WAIT * 80)
	

try(remDr$close(), silent = TRUE)
remDr$closeServer()

rm(remDr, webElem) 
	invisible(gc(reset = TRUE))

test <- list.files("C:\\temp\\")
test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
file.rename(paste0("C:\\temp\\",test),paste0(INPUT, NAME,'.csv'))




ref <- readr::read_csv(paste0(INPUT, NAME,'.csv'), col_names = TRUE, n_max = 1)
ref <- paste0(rep('c', ncol(ref)), collapse = '')
X <- readr::read_csv(paste0(INPUT, NAME,'.csv'), col_types = ref,  col_names = TRUE)


X <- X[, !colnames(X) %in% c(NA, 'UNIT')]
colnames(X) <- gsub(' ', '_' , colnames(X))
X <- X %>% select(-starts_with('X'))

if(NAME %in% c('DT_1DA7089')){ X <- X %>% mutate(By_gender = 'Total')}

########################

ref_id <- X %>% slice(1) %>% colnames()
ref_id <- ref_id[!substr(ref_id,1,2) %in% c('19', '20') ]


invisible(gc(reset = TRUE))


X <- X 	%>% 	unite_('ID', ref_id, remove = TRUE, sep = '@') %>% 
			gather(key = TIME_PERIOD, value = OBS_VALUE, -ID, na.rm = TRUE) %>%
			separate(ID, ref_id, remove = TRUE, sep = '@') %>% 
			filter(!OBS_VALUE %in% c('NA', NA, '-'))%>% 
			mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>% 
			filter(!OBS_VALUE %in% NA) %>% 
			mutate( TIME_PERIOD  = 	gsub('_Month', '', TIME_PERIOD  , fixed = TRUE),
					TIME_PERIOD  = 	gsub('_Quarter', '', TIME_PERIOD  , fixed = TRUE),
					TIME_PERIOD  = 	gsub('_Year', '', TIME_PERIOD  , fixed = TRUE),
					TIME_PERIOD  = 	gsub('/4', '', TIME_PERIOD, fixed = TRUE),
					TIME_PERIOD  = 	gsub('._', '', TIME_PERIOD, fixed = TRUE), 
					TIME_PERIOD  = 	gsub('_', '', TIME_PERIOD, fixed = TRUE),
					TIME_PERIOD  = 	gsub('_', '', TIME_PERIOD, fixed = TRUE),
					TIME_PERIOD  =  ifelse(nchar(TIME_PERIOD) %in% 6, paste0('Y', substr(TIME_PERIOD, 1, 4), '_M', substr(TIME_PERIOD, 5, 6)), TIME_PERIOD), 
					TIME_PERIOD  =  ifelse(nchar(TIME_PERIOD) %in% 5, paste0('Y', substr(TIME_PERIOD, 1, 4), '_Q0', substr(TIME_PERIOD, 5, 5)), TIME_PERIOD), 
					TIME_PERIOD  =  ifelse(nchar(TIME_PERIOD) %in% 4, paste0('Y', substr(TIME_PERIOD, 1, 4)), TIME_PERIOD)
			) %>% 	
			mutate_all(funs(gsub('&amp;','&', ., fixed = TRUE))) %>% 
			mutate_all(funs(gsub(';','@', ., fixed = TRUE))) 

if(NAME %in%  'DT_1DA7013'){

	X <- X %>% group_by(By_gender , By_education_level,  Item, TIME_PERIOD) %>% 
			summarise(	OBS_VALUE = first(OBS_VALUE)) %>% ungroup
}	

if(NAME %in% c('DT_1DA7103', 'DT_1DA7087')){

	X <- X %>% group_by(By_gender , By_education_level, TIME_PERIOD) %>% 
			summarise(	OBS_VALUE = first(OBS_VALUE)) %>% ungroup
}
			
			
return(X)			
			
			
			
			
			
}

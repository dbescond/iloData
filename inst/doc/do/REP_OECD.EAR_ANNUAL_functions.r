

REP_OECD.EAR_ANNUAL_input_ANNUAL_EAR_XTLP_SEX_RT  <- function (check = TRUE) {
  
  #rm(list=setdiff(ls(), c("ilo")))
  #require(ilo)
  
  input_path	<- paste0(getwd(),'/input/ANNUAL_EAR_XTLP_SEX_RT.csv')
  X 	<- read_delim(input_path, delim = ',')
  
  Source.Map  <- read.csv('./input/maps/MapSource.csv', stringsAsFactors = FALSE)
  
  X <- X %>% select(	ref_area = COUNTRY,	sex = Sex,	time = Time,	obs_value = Value)
  
  Xp <- X %>% 
    mutate( sex= sex %>% recode("All persons"="SEX_T",
                                            "Men"="SEX_M",
                                            "Women"="SEX_F")) %>%
    left_join(Source.Map, by="ref_area") %>%
    mutate(
      collection = "STI",
      obs_status = NA_character_,
      note_classif=  NA_character_,
      note_indicator =  NA_character_,
      classif1 = NA_character_,
      classif2 = NA_character_,
      note_source= "R1:2382_R1:3903",
      indicator="EAR_XTLP_SEX_RT"
    )
  
  write_csv(Xp, file = './input/Output.ANNUAL_EAR_XTLP_SEX_RT.csv')

  
}


REP_OECD_EAR_ANNUAL_download <- function(REF){

# REF <- Mapping_File %>% slice(1)

cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }; rm(cleanTemp)

shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                ,  browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/text/csv"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()   

remDr$navigate(REF$URL) # go to query webpage

	Sys.sleep(10)	

remDr$executeScript("redirectToSurvey('csv');") # open csv download pop up

remDr$switchToFrame('DialogFrame')


	Sys.sleep(10)	

remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_rbCustomLayout')$clickElement()
	Sys.sleep(3)	
#remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_cbLabel')$clickElement() ########## keep label
# 	Sys.sleep(3)	
remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_btnExportCSV')$clickElement()
	Sys.sleep(100)	
	
	


try(remDr$close(), silent = TRUE)
remDr$closeServer()

test <- list.files("C:\\temp\\")
test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
file.rename(paste0("C:\\temp\\",test),paste0(INPUT, REF$NAME,'.csv'))
invisible(gc(reset = TRUE))


}
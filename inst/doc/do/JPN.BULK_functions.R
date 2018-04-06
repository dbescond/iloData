#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  April 2016. last update May 2017
#############################################################################

download_data_JPN <- function(Mapping_File){


Sys.setenv(http_proxy="proxyos.ilo.org:8080")
Sys.setenv(htts_proxy="proxyos.ilo.org:8080")
Sys.setenv(ftp_proxy="proxyos.ilo.org:8080")


	require(estatapi)
	JPNappId <- 'b493ff64ec275aa2b8b9918040c01aea5e46525c'



	for (i in 1:nrow(Mapping_File)){
		X <- estat_getStatsData(statsDataId= Mapping_File$statsDataId[i], appId = JPNappId, lang='E', metaGetFlg='Y', cntGetFlg='N', sectionHeaderFlg=1) %>% 
			select(-contains('_code'))
		colnames(X) <- gsub(' ', '.', colnames(X), fixed = TRUE)
		colnames(X) <- gsub('/', '_', colnames(X), fixed = TRUE)
		colnames(X) <- gsub(',', '', colnames(X), fixed = TRUE)
		try(colnames(X) <- gsub('Time.(Monthly)', 'Time', colnames(X), fixed = TRUE), silent = TRUE)
		try(colnames(X) <- gsub('Time.(Quarterly)', 'Time', colnames(X), fixed = TRUE), silent = TRUE)
		try(colnames(X) <- gsub('Time.(Yearly)', 'Time', colnames(X), fixed = TRUE), silent = TRUE)
		try(colnames(X) <- gsub('Time.(Yearly)', 'Time', colnames(X), fixed = TRUE), silent = TRUE)
		
		try(colnames(X) <- gsub("(Detailed.Tabulation)",'', colnames(X), fixed = TRUE)		, silent = TRUE)
			
		
		X <- X %>% mutate(Time =  gsub("Jan.-Mar. ",'Q1', Time, fixed = TRUE),
					Time =  gsub("Apr.-Jun. ",'Q2', Time, fixed = TRUE),
					Time =  gsub("Jul.-Sep. ",'Q3', Time, fixed = TRUE),
					Time =  gsub("Oct.-Dec. ",'Q4', Time, fixed = TRUE),
					Time =  gsub("Jan. ",'M01', Time, fixed = TRUE),
					Time =  gsub("Feb. ",'M02', Time, fixed = TRUE),
					Time =  gsub("Mar. ",'M03', Time, fixed = TRUE),
					Time =  gsub("Apr. ",'M04', Time, fixed = TRUE),
					Time =  gsub("May ",'M05', Time, fixed = TRUE),
					Time =  gsub("Jun. ",'M06', Time, fixed = TRUE),
					Time =  gsub("Jul. ",'M07', Time, fixed = TRUE),
					Time =  gsub("Aug. ",'M08', Time, fixed = TRUE),
					Time =  gsub("Sep. ",'M09', Time, fixed = TRUE),
					Time =  gsub("Oct. ",'M10', Time, fixed = TRUE),
					Time =  gsub("Nov. ",'M11', Time, fixed = TRUE),
					Time =  gsub("Dec. ",'M12', Time, fixed = TRUE), 
					Time = ifelse(nchar(Time)>4, paste0(str_sub(Time,-4,-1), str_sub(Time,1,nchar(Time) -4)), Time)
				) %>% 
		rename(Value = value) %>% 
		filter(!Value %in% NA)
		
		save(X,file = paste0(INPUT,Mapping_File$NAME[i],'.Rdata'))
			print(paste0('   --- ',paste0(Mapping_File$NAME[i], ' : ', nrow(X))))
			rm(X)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))

	}
}


download_data_JPN_OLD <- function(Mapping_File){



  shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone-3.8.1.jar', wait   = FALSE)
  Sys.sleep(2)
  
  action <- startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
  
  fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                , browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/octet-stream"))
  #RSelenium::startServer()
  remDr <- remoteDriver(extraCapabilities = fprof)
  action <- remDr$open()   
  URL <- 'http://www.e-stat.go.jp/SG1/estat/GL38020101.do?_toGL38020101_&tstatCode=000000110001'
  action <- remDr$navigate(URL)
  Sys.sleep(15)
  
  
  ref_tree <- unique(Mapping_File$TreeID)
 

 for (t1 in seq_along(ref_tree)){ # segement on the tree (quarterly, monthy place 1, 2, ...)
        
    webElem <- remDr$findElement('id', 'disPlayRange_1')$findChildElements('link text' , stringr::str_sub(ref_tree[t1], 1, -1L-1))
    action <- webElem[[stringr::str_sub(ref_tree[t1], -1) %>% as.numeric]]$highlightElement()
    action <- webElem[[stringr::str_sub(ref_tree[t1], -1) %>% as.numeric]]$clickElement()
    
	ref_Frame <- Mapping_File %>% filter(Mapping_File$TreeID %in% ref_tree[t1]) %>% select(FrameID) %>% distinct %>% t %>% as.character
	ref_Frame <- eval(parse(text = ref_Frame))
	
    webElem <- remDr$findElement('id', 'contents_main')$findChildElement('link text' , ref_Frame) # get the first time available
    action <- webElem$highlightElement()
    action <- webElem$clickElement()

    ref_table <- Mapping_File %>% filter(Mapping_File$TreeID %in% ref_tree[t1]) %>% select(TableID) %>% distinct


	print(ref_tree[t1])
	
	
    for (t2 in seq_along(ref_table$TableID)){ # segment by table (I-1, II-2 ...)
	
		webElem <- remDr$findElements('css selector', 'tr')


		l1 <- ldply(	webElem, 
						function(x){
							test <- try(x$findChildElement('class name', 'left_right_bottom_borderonly')$getElementText(), silent = TRUE)
							test <- ifelse(class(test) %in% 'list',x$findChildElement('class name', 'left_right_bottom_borderonly')$getElementText() %>% unlist, NA)
							test %in% ref_table$TableID[t2]
							}) %>% 
						mutate(ref = 1:n()) %>% 
						filter(V1 %in% TRUE) %>% 
						.$ref	
		
		
		l2 	 <-	ldply(	webElem[[l1]]$findChildElements(using = 'css selector', value = "td.vtopborder_r > a > img"), 
						function(x){
							x$getElementAttribute('src') %>% 
							unlist %>% 
							stringr::str_detect('images/db.gif')
							}) %>% 
						mutate(ref = 1:n()) %>% 
						filter(V1 %in% TRUE) %>% 
						.$ref	
						
						
						
		action <- webElem[[l1]]$findChildElements(using = 'css selector', value = "td.vtopborder_r > a > img")[[l2]]$highlightElement()
		action <- webElem[[l1]]$findChildElements(using = 'css selector', value = "td.vtopborder_r > a > img")[[l2]]$clickElement()
		Sys.sleep(5)
		rm(l1, l2)

 		ref_File  <- Mapping_File %>% filter(Mapping_File$TreeID %in% ref_tree[t1], Mapping_File$TableID %in% ref_table$TableID[t2]) 
		
		print(paste0('--- ', ref_table$TableID[t2]))

		for (t3 in seq_along(ref_File$FileID)){
		
		print(paste0('node: ', t1, ' / ', t2, ' / ', t3))
			
			test <- list.files("C:\\temp\\")
			test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
			if(length(test)!=0){
				for(j in 1:length(test)){file.remove(paste0("C:\\temp\\", test[j]))}
			}
		
			if(ref_File$LevelID[t3] %in% '2'){
					
				webElem <- remDr$findElements('css selector', 'tr')
				l1 <- ldply(	webElem, 
						function(x){
							test <- try(x$findChildElement('css selector', 'td')$getElementText(), silent = TRUE)
							test <- ifelse(class(test) %in% 'list',x$findChildElement('css selector', 'td')$getElementText() %>% unlist, NA)
							test %in% ref_File$FileID[t3]
							}) %>% 
						mutate(ref = 1:n()) %>% 
						filter(V1 %in% TRUE) %>% 
						.$ref
				action <- webElem[[l1]]$findChildElement(using = 'css selector', value = "td > a ")$highlightElement()
				action <- webElem[[l1]]$findChildElement(using = 'css selector', value = "td > a ")$clickElement()
				Sys.sleep(5)
				rm(l1)		
			}	
			
			# Select all items
			webElem <- remDr$findElement('id', 'showSelectedAll')$highlightElement()
			webElem <- remDr$findElement('id', 'showSelectedAll')$clickElement()
			Sys.sleep(5)
			# identify the table tr minus the header
			webElem <- remDr$findElement('class name', 'list')$findChildElements(using = 'css selector', value = "tr ")
			webElem[[1]] <- NULL 
			
			ref_Para <- eval(parse(text = ref_File$ParameterID[t3]))
			ref_Var <- eval(parse(text = ref_File$ValiableID[t3]))
			MainWindow <- remDr$getCurrentWindowHandle() %>% unlist 
			colnamesID	<- NULL				
			for (i in seq_along(ref_Para)){		
				action <- webElem[[i]]$findChildElement('name', paste0('selectDispItemOption', i-1))$sendKeysToElement(list(ref_Para[i]))
				if(substr(ref_Para[i],1,3) %in% 'Row'){colnamesID <- rbind(colnamesID, 
				c(
				action <- webElem[[i]]$findChildElements(using = 'css selector', value = "td ")[[2]]$getElementText() %>% unlist , 
				ref_Para[i])
				)}
			}
			colnamesID <- as.data.frame(colnamesID) %>% arrange(desc(V2)) %>% select(V1) %>% t %>% as.character
	
			for (i in seq_along(ref_Var)){

			
				if(!is.na(unlist(ref_Var[i])[1])){
					webElem <- remDr$findElement('class name', 'list')$findChildElements(using = 'css selector', value = "tr ")
					webElem[[1]] <- NULL 
					action <- webElem[[i]]$findChildElement('css selector', 'input.button-normal.auto.sub')$highlightElement()
					action <- webElem[[i]]$findChildElement('css selector', 'input.button-normal.auto.sub')$clickElement()
					AllWindow <- remDr$getWindowHandles() %>% unlist 
					SelectWindow <- AllWindow[!AllWindow %in% MainWindow]
					action <- remDr$switchToWindow(SelectWindow)
					# remove all elements
					Sys.sleep(10)
					action <- remDr$findElement(value = "//input[@title = 'Remove all items']")$clickElement()
					Sys.sleep(5)
					ref <- ref_Var[i] %>% unlist
					
					for (j in seq_along(ref)){
						action <- remDr$findElement('css selector', 'div#treeDiv1 > select.itemListSelect > option.itemListSelect')$findChildElement(value = paste0("//option[@title = '",ref[j],"']"))$highlightElement()
						action <- remDr$findElement('css selector', 'div#treeDiv1 > select.itemListSelect > option.itemListSelect')$findChildElement(value = paste0("//option[@title = '",ref[j],"']"))$clickElement()
						Sys.sleep(7)
						action <- remDr$findElement(value = "//input[@title = 'Add selected items']")$clickElement()
						Sys.sleep(7)
					}
					action <- remDr$findElement('id', 'doOkBtn')$clickElement() # validate
					Sys.sleep(8)
			
					action <- remDr$switchToWindow(MainWindow)	
					Sys.sleep(5)	
					
				}
				# rm(SelectWindow, AllWindow)
				
					
			}
			
			webElem <- remDr$findElements('css selector', 'div.action > p.f-right > input')
			action <- webElem[[1]]$findChildElement(value = "//input[@value = 'Download']")$highlightElement()
			action <- webElem[[1]]$findChildElement(value = "//input[@value = 'Download']")$clickElement()
			Sys.sleep(5)	
	
			AllWindow <- remDr$getWindowHandles() %>% unlist 
			SelectWindow <- AllWindow[!AllWindow %in% MainWindow]
			action <- remDr$switchToWindow(SelectWindow)		
			rm(SelectWindow, AllWindow)
			Sys.sleep(6)
				
				
			action <- remDr$switchToFrame('downloadTable')
			# remDr$findElement('id', 'ZIPRadio')$highlightElement()
			# remDr$findElement('id', 'ZIPRadio')$clickElement()
						Sys.sleep(5)
			
			action <- remDr$findElement(value = "//input[@value = 'OK']")$highlightElement()
			action <- remDr$findElement(value = "//input[@value = 'OK']")$clickElement()
			Sys.sleep(60)
		# close
			action <- remDr$findElement(value = "//input[@value = 'close']")$clickElement() # close frame
			action <- remDr$switchToWindow(MainWindow)		
						Sys.sleep(5)
			action <- remDr$findElement(value = "//input[@value = 'Back to Database']")$highlightElement()			
			action <- remDr$findElement(value = "//input[@value = 'Back to Database']")$clickElement()
			
			if(t3 %in% max(seq_along(ref_File$FileID)) & ref_File$LevelID[t3] %in% '2'){
				action <- remDr$findElement(value = "//input[@value = 'Back to the result of statistics']")$highlightElement()
				action <- remDr$findElement(value = "//input[@value = 'Back to the result of statistics']")$clickElement()
			}
			
 
			invisible(gc(reset = TRUE))

			test <- list.files("C:\\temp\\")
			test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
			file.rename(paste0("C:\\temp\\",test),paste0(INPUT, ref_File$FileID[t3],'.csv'))
			
			Sys.sleep(2)
			SkipCols <- 5 + ref_Para[ref_Para %in% 'Page' ] %>% length + 2

			try(ref <- readr::read_csv(paste0(INPUT, ref_File$FileID[t3],'.csv'), col_names = FALSE, n_max = 1, skip = SkipCols), silent = TRUE )
			ref <- paste0(rep('c', ncol(ref)), collapse = '')
			try(X <- readr::read_csv(paste0(INPUT, ref_File$FileID[t3],'.csv'), col_types = ref,  col_names = TRUE, skip = SkipCols) , silent = TRUE )
			
			for (i in seq_along(colnamesID)){
				
				colnames(X)[(i*3-2):(i*3)] <- c(paste0('DELETE', (i*3-2)), paste0('DELETE', (i*3-1)), colnamesID[i])
			
			}
			X <- X %>% select(-contains('DELETE'))
			for (i in seq_along(colnamesID)){
			
				X <- X %>% fill(i, .direction = c("down")) 
			
			}
			
			colnames(X) <- gsub("Jan.-Mar. ",'Q1', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Apr.-Jun. ",'Q2', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Jul.-Sep. ",'Q3', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Oct.-Dec. ",'Q4', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Jan. ",'M01', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Feb. ",'M02', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Mar. ",'M03', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Apr. ",'M04', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("May ",'M05', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Jun. ",'M06', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Jul. ",'M07', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Aug. ",'M08', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Sep. ",'M09', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Oct. ",'M10', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Nov. ",'M11', colnames(X), fixed = TRUE)
			colnames(X) <- gsub("Dec. ",'M12', colnames(X), fixed = TRUE)

			X <- X %>% 
					unite_('ID', colnamesID, remove = TRUE, sep = '@') %>% 
					gather(key = Time, value = Value, -ID, na.rm = TRUE) %>% 
					separate(ID, colnamesID, remove = TRUE, sep = '@') %>% 
					
					mutate(Value = as.numeric(Value)) %>% 
					filter(!Value %in% NA) %>% 
					mutate(	Time = ifelse(nchar(Time) %in% 7,paste0(stringr::str_sub(Time, 4,7), stringr::str_sub(Time, 1,3)), Time), 
							Time = ifelse(nchar(Time) %in% 6,paste0(stringr::str_sub(Time, 3,6), stringr::str_sub(Time, 1,2)), Time)  
								)
			colnames(X) <- gsub(' ','.', colnames(X), fixed = TRUE)
			for (i in seq_along(colnames(X))){
				test <- str_locate(colnames(X)[i], '[(]')[1]
				if(!is.na(test)){
					colnames(X)[i] <- str_sub(colnames(X)[i], 1, test-1)
				}
			}
								
		
			colnames(X) <- gsub("(Detailed Tabulation)",'', colnames(X), fixed = TRUE)		
					
					
			save(X,file = paste0(INPUT,paste0(ref_File$FileID[t3], '_', substr(ref_File$TreeID[t3], 1,1)),'.Rdata'))
			print(paste0('   --- ',paste0(ref_File$FileID[t3], '_', substr(ref_File$TreeID[t3], 1,1), ' : ', nrow(X))))
			rm(X, colnamesID)
		}
	}

  action <- remDr$navigate(URL)
  Sys.sleep(2)


  }
		
		
	
action <- remDr$close()
action <- remDr$closeServer()
invisible(gc(reset = TRUE))
  	
}



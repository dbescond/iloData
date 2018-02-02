init_time <- Sys.time() 

 
 
setwd(paste0(ilo:::path$data, '/REP_ILO/STI_QUARTERLY_SEAS/'))
 
 
require(ilo)
require(seasonal)
require(xts)
init_ilo()



ref_break <- "([I][1][1])[-:.]([0-9]+)"    
ref_freq <- "([I][1][2])[-:.]([0-9]+)" 
key <- c("ref_area", "source", "indicator", "sex", "classif")
CODE_ORA <- Ariane:::CODE_ORA

# detect all ilo SA series:
# X <- get_ilo(collection = 'STI', freq ='Q', indicator = '1_', info) %>% filter(!str_detect(note_source, 'R1:2382'),!str_detect(source,'BE') )

# rebuilt only few indicator
# OLD <- get_ilo(collection = 'STI', freq ='Q', indicator = '1_', info) %>% filter(!str_detect(note_source, 'R1:2382'),!str_detect(source,'BE') ) %>% 
				# filter(indicator %in% c('EAP_DWA1_SEX_AGE_RT', 'UNE_DEA1_SEX_AGE_RT'))


# OLD <- OLD %>% select(collection, ref_area, source, indicator, sex, classif1,  time) %>% mutate(year = str_sub(time, 1,4) , indicator = indicator %>% str_replace('1_','P_')) %>% select(-time) %>% distinct(collection, ref_area, source, indicator, sex, classif1, year) %>% mutate(test = 1)



# 'EAP_TEA1_SEX_AGE_NB', 'EAP_DWA1_SEX_AGE_RT', 'EMP_TEM1_SEX_AGE_NB', 'EMP_TEM1_SEX_ECO_NB', 'UNE_TUN1_SEX_AGE_NB', 'UNE_DEA1_SEX_AGE_RT'
X1 <- 	get_ilo(	collection = 'STI', freq ='Q', 
				ref_area = c('ARG', 'AZE', 'BGR', 'BRB', 'COL', 'CYP', 'EGY', 'HKG', 'HRV', 'JAM', 'JOR', 'KAZ', 'LKA', 'LTU', 'LVA', 'MAC', 'MAR', 'MDA', 'MKD', 'MLT', 'MNE', 'MUS', 'MYS', 'PER', 'PHL', 'PSE', 'ROU', 'SGP', 'THA', 'TTO', 'TWN', 'UKR', 'URY', 'VEN', 'ZAF'), 
				query = "filter(indicator %in% c('EAP_TEAP_SEX_AGE_NB', 'EAP_DWAP_SEX_AGE_RT', 'EMP_TEMP_SEX_AGE_NB', 'EMP_TEMP_SEX_ECO_NB', 'UNE_TUNP_SEX_AGE_NB', 'UNE_DEAP_SEX_AGE_RT'), !stringr:::str_detect(note_source, 'R1:2382'),!stringr:::str_detect(source,'BE'), !stringr:::str_sub(source,1,1) %in% 'F' )") 
X2 <- 	get_ilo(	collection = 'STI', freq ='Q', 
				query = "filter(indicator %in% c('UNE_TUNE_SEX_AGE_DUR_NB'), !stringr:::str_detect(source,'BE') , !stringr:::str_sub(source,1,1) %in% 'F')") %>% 
		arrange(classif2) %>% 
		mutate(classif2 = ifelse(classif2 %in% c('DUR_DETAILS_MGE1LT3','DUR_DETAILS_MLT1'), 'DUR_DETAILS_MLT3', classif2)) %>% 
		filter(	str_detect(classif1, 'AGE_AGGREGATE|AGE_YTHADULT|ECO_SECTOR|ECO_AGGREGATE'), 
				classif2 %in% c('DUR_DETAILS_TOTAL', 'DUR_DETAILS_MGE3LT6', 'DUR_DETAILS_MGE6LT12','DUR_DETAILS_MGE12LT24', 'DUR_DETAILS_MGE24', 'DUR_DETAILS_MLT3' )) %>%	
		group_by(collection, ref_area, source, indicator, sex, classif1, classif2, time) %>% 
		summarise(	obs_value = sum(obs_value, na.tm = TRUE), 
					obs_status = first(obs_status), 
					note_classif = first(note_classif), 
					note_indicator = first(note_indicator), 
					note_source = first(note_source)) %>% 
		ungroup %>% 
		group_by(collection, ref_area, source, indicator, sex, classif1, time) %>% 
		mutate(test = paste0(classif2, collapse = '/ ')) %>% 
		ungroup  %>% 
		filter(!test %in% 'DUR_DETAILS_TOTAL') %>% 
		select(-test) %>% 
		mutate(	
				note_classif = note_classif %>% str_replace('_C7:2942', ''),
				note_classif = note_classif %>% str_replace('C7:2942_', ''),
				note_classif = gsub('C7:2942', NA, note_classif), 
				note_classif = note_classif %>% str_replace('_C7:3007', ''),
				note_classif = note_classif %>% str_replace('C7:3007_', ''),
				note_classif = gsub('C7:3007', NA, note_classif)
				) %>% 
		filter(!ref_area %in% 'LUX')
				
			
			
RAW <- bind_rows(X1, X2) %>%			
		mutate(year = str_sub(time, 1,4)) %>% 
		filter(str_detect(classif1, 'AGE_AGGREGATE|AGE_YTHADULT|ECO_SECTOR|ECO_AGGREGATE'))  %>% 
		mutate(	Break =  str_extract(note_indicator, ref_break),
				TEST_N_BRK_P = ifelse(Break%in%NA,NA, time))%>%
		filter(	as.numeric(str_sub(time,1,4))>1999) %>%
		arrange(ref_area, source, time) %>%
		mutate(Freq = str_extract(note_indicator, ref_freq)) %>% 
		left_join(select(CODE_ORA$T_FRQ_FREQUENCY, Freq = NEW_CODE_ORACLE,FRQ_CODE), by="Freq") %>%
		filter( !FRQ_CODE %in% c("G", "C","W","D", "o" , "n","E","F","a","b","c","d","e","f","g","h","i","U","j","k","m"), 
				!(ref_area %in% "CYP" & as.numeric(str_sub(time,1,4))<2004), 
				!source %in% 'EA:1308', #KAZ
				!source %in% 'BA:501', # MKD
				!(ref_area %in% "BGR" & as.numeric(str_sub(time,1,4))<2001), 
				!(ref_area %in% "HRV" & as.numeric(str_sub(time,1,4))<2007), 
				!(ref_area %in% "LTU" & as.numeric(str_sub(time,1,4))<2002), 
				!(ref_area %in% "LVA" & as.numeric(str_sub(time,1,4))<2002), 
				!(ref_area %in% "ARG" & as.numeric(str_sub(time,1,4))<2004), 
				!(ref_area %in% "COL" & as.numeric(str_sub(time,1,4))<2008), 
				!(ref_area %in% "BRB" & as.numeric(str_sub(time,1,4))<2001), 
				!(ref_area %in% "JAM" & as.numeric(str_sub(time,1,4))<2005), 
				!(ref_area %in% "PSE" & as.numeric(str_sub(time,1,4))<2008), 
				!(ref_area %in% "ROU" & as.numeric(str_sub(time,1,4))<2003), 
				!(ref_area %in% "TTO" & as.numeric(str_sub(time,1,4))<2001), 
				!(ref_area %in% "VEN" & as.numeric(str_sub(time,1,4))<2003), 
				!(ref_area %in% "VEN" & time %in% c("2003Q1","2003Q2")), 
				!(ref_area %in% "PHL" & as.numeric(str_sub(time,1,4))<2005), 
				!(ref_area %in% "PHL" & time %in% "2005Q1"), 
				!(ref_area %in% "IRN" ), 
				!(ref_area %in% "MLT" & as.numeric(str_sub(time,1,4))<2002)) %>% 
			mutate(ID = paste(ref_area, source, indicator, sex, classif1, classif2, sep = '.')) %>% 
			select(-Freq, -FRQ_CODE) %>% 
			mutate(Stime = as.Date(as.yearqtr(time))) %>% 
			group_by(ID) %>% 
			mutate(	TEST2011 = max(as.numeric(year)), 
					countnb = n()) %>% 
			ungroup %>% 
			filter(	TEST2011>2010,
					countnb > 24) %>%
			select(-TEST2011, -countnb ) %>% 
			filter( 
					!str_detect(classif1 , 'AGE_AGGREGATE'), 
					!str_detect(classif1 , 'ECO_AGGREGATE'), 
					!str_detect(classif1 , 'ECO_SECTOR_NAG'), 
					!str_detect(classif1 , 'ECO_SECTOR_X'), 
					!str_detect(classif1 , 'AGE_YTHADULT_Y15-64')
					)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))					
					
# rm(X1, X2)			

				
 
 
 ts_model <- list()
 ref <- unique(RAW$ID)
 
 for (i in 1:length(ref)){
 
	check <- RAW %>% filter(ID %in% ref[i]) %>% select(Stime, obs_value)
	ref_quarter <- seq.Date(min(check$Stime),max(check$Stime),"quarter") %>% as_data_frame %>% rename(Stime  = value)
	check <- check  %>% full_join(ref_quarter, by = 'Stime') %>% arrange(Stime)  
	ser <- xts(check$obs_value, check$Stime) %>% ts(., start= min(as.numeric(str_sub(check$Stime,1,4))), freq = 4)

    m <- try(seas(ser, arima.model = "(1 1 0)(1 0 0)", transform.function = 'none', na.action = na.x13), silent = TRUE) 


if(class(m) %in% 'seas'){
ts_model[[ref[i]]] <- m
rm(check, ref_quarter, ser, m)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
print(paste0(i, '/',length(ref),' : ', ref[i]))
} else{print(paste0(i, '/',length(ref),' : ', ref[i], " ::: failed"))}


 }
 
 
 ######################################################################################################################## test 2001
 ########################################################################################################################
 ############################################################ check graph ############################################################
 ######################################################################################################################################################
# library(shiny)
# library(dygraphs)


# ui <- fluidPage(
		# titlePanel(title=h4("var", align="center")),
		# sidebarPanel( 
			# sliderInput("num", "id:",min = 1, max = length(ts_model), value=1, step = 1)),
# mainPanel(dygraphOutput("plot2"))
# )

  
  
# server <- function(input,output){
  
  # output$plot2<-renderDygraph({

	# dygraph(cbind(original = original(ts_model[[input$num]]),
			# final =  final(ts_model[[input$num]])), main = names(ts_model[input$num])) %>%
	# dyOptions(colors = RColorBrewer::brewer.pal(2, "Set2"))
   # })
# }
	
	
	
# shinyApp(ui, server)

 ####################################################################################################################################################################################
 ####################################################################################################################################################################################
 
 
 
 
 
 NEW <- NULL
 for (i in 1:length(ts_model)){
 
 NEW <- bind_rows(	NEW, 
					data.frame(Stime=index(final(ts_model[[i]])), ts_value = coredata(final(ts_model[[i]]))) %>% as.tbl %>% 
						mutate(	ID = names(ts_model[i]), 
								Stime = as.Date(as.yearqtr(Stime))))
 
 }
 
 
 res <- left_join(RAW, NEW,  by = c("ID", "Stime")) %>% 
			mutate(ts_value = ifelse(ts_value %in% NA,obs_value, ts_value )) %>%
			filter(!ts_value %in% NA) %>% 
			select(-obs_value) %>% 
			rename(obs_value = ts_value) %>%
			mutate(indicator = str_c(str_sub(indicator, 1, 7), '1', str_sub(indicator, 9, -1))) %>% 
			select(collection, ref_area, source, indicator, sex, classif1, classif2,   time, obs_value, obs_status, note_classif, note_indicator, note_source) %>% 
			mutate(note_indicator = ifelse(note_indicator %in% NA, 'I13:3922', paste0(note_indicator, '_I13:3922')))
 
 
 
 
 
ref_freq <- "([I][1][2])[-:.]([0-9]+)" 
res <-  res %>% mutate(Freq = str_extract(note_indicator, ref_freq)) %>% 
		left_join(select(Ariane:::CODE_ORA$T_FRQ_FREQUENCY, Freq = NEW_CODE_ORACLE,freq_code = FRQ_CODE), by="Freq")

ref <- unique(res$Freq)		
for (i in 1:length(ref)){

		res <- res %>% mutate(note_indicator = ifelse(Freq %in% ref[i], str_replace(note_indicator, paste0(ref[i], '_'), ''), note_indicator))
}
rm(ref, ref_freq)		
		
 res <- res %>% select(-Freq)
  ref_file <- unique(res$ref_area)
  X <- res
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  for (i in 1:length(ref_file)){
  X <- res %>% filter(ref_area %in% ref_file[i])
    save(X, file = paste0("./output/",ref_file[i],".Rdata"))
	rm(X)
  }
  
	ref <- cbind(PATH = paste0(getwd(),"/output/",ref_file,".Rdata"),ID = NA, Types  ="ILO_ilostat", REF = paste0(ref_file)) %>% as_data_frame
	data.table:::fwrite(ref,paste0("./FileToLoad.csv"))
  rm(res, ref_file)
  
  
  
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
  
  
  
  
  
  
  
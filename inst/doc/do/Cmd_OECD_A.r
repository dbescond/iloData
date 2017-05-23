

setwd(paste0(ilo:::path$data,'REP_OECD/LFS_ANNUAL/'))


################### 
# processing of EMP_TEMP_SEX_AGE_NB", "EAP_TEAP_SEX_AGE_NB", "POP_XWAP_SEX_AGE_NB", "UNE_TUNE_SEX_AGE_NB
# from oecd

#############################################
# open oecd by 5 yrbands data

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_5YRBANDS <- function(){ 
	
	n_row_ref <- readr::read_csv("./input/ANNUAL_AGE_5YRBANDS.csv", col_names = FALSE, n_max = 1) %>% ncol
	X <- readr::read_csv("./input/ANNUAL_AGE_5YRBANDS.csv", col_types = paste0(rep('c', n_row_ref), collapse =''), guess_max = 1)
	colnames(X)[1] <- 'country' #fix bad UTF8 encoding issue for first character/column

	rm(n_row_ref)
	
	X <- X %>%  
			select(country,sex = SEX, classif1 = Age, indicator = Series, time = TIME, value =  Value, Flags) %>% 
			filter(!country %in% 'FTFR') %>% 
			mutate(value = as.numeric(value))

# test if age classification is complete and total is accurate
	Y <- X %>% 	
			mutate(value = as.numeric(value)) %>% spread(classif1, value)  %>% mutate(TOT = rowSums(.[6:16], na.rm = TRUE)) 

# remove inaccurate data 
	Y <- Y %>% 	
			mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% 
			filter(test %in% FALSE, Unknown %in% NA, !country %in% c('GBR', 'RUS')) %>% 
			distinct(country, sex, indicator, time) %>% mutate(delete = TRUE) 
	X <- X %>% 	
			left_join(Y, by = c("country", "sex", "indicator", "time")) %>% 
			filter(!delete %in% TRUE) %>% 
			select(-Flags, - delete)
rm(Y)
 

# create the Total AGE
	Total <- X %>% 
			filter(!classif1 %in% c("Total", "Unknown")) %>% 
			group_by(country, sex, indicator, time) %>% 
			summarise(classif1 = 'Total', NEW = sum(as.numeric(value))) %>% 
			ungroup

# test
# X %>% filter(!classif1 %in% c("Total", "Unknown")) %>% mutate(value = as.numeric(value)) %>% 
#			spread(classif1, value)  %>% mutate(TOT = rowSums(.[5:15], na.rm = TRUE)) %>% left_join(Total) %>% fix
 
	AGE_5 <- X %>%
			mutate(value = as.numeric(value)) %>% filter(!classif1 %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, value = NEW)) %>% 
			mutate(classif1 = classif1 %>% 
									plyr::mapvalues(	
											from = 	c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+", "Total"), 
											to = 	c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65", "AGE_5YRBANDS_TOTAL")
											)	)

saveRDS(AGE_5,file = './tmp/ANNUAL_AGE_5YRBANDS.rds' )
#	X <- readRDS('./tmp/ANNUAL_AGE_5YRBANDS.rds')

rm(Total, X, AGE_5)	
invisible(gc(reset = TRUE))											
}												
												
#############################################
# open oecd by 10 yrbands data

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_10YRBANDS <- function(){ 
	readr::read_csv("./input/ANNUAL_AGE_10YRBANDS.csv", col_types = 'c_c__c_c__c_______d_c') -> X
	# test if age classification is complete and total accurante
	colnames(X)[1] <- 'COUNTRY'

	X %>% filter(!COUNTRY %in% 'FTFR')-> X

X %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[6:11], na.rm = TRUE)) -> Y

# remove inaccurate data 
Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA, !COUNTRY %in% c('POL', 'RUS')) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
X %>% left_join(Y, by = c("COUNTRY", "SEX", "Series", "TIME")) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
rm(Y)
 

# create the Total AGE
X %>% filter(!Age %in% c("Total", "Unknown")) %>% group_by(COUNTRY, SEX, Series, TIME) %>% summarise(Age = 'Total', NEW = sum(as.numeric(Value))) %>% ungroup -> Total

# test
# X %>% filter(!Age %in% c("Total", "Unknown")) %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[5:9], na.rm = TRUE)) %>% left_join(Total) %>% fix
 
AGE_10 <- X %>% mutate(Value = as.numeric(Value)) %>% filter(!Age %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, Value = NEW)) 

AGE_10 <- AGE_10  %>% 
					rename(country = COUNTRY, sex = SEX, indicator = Series, time = TIME, value = Value, classif1 = Age) %>% 
					mutate(classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+", "Total"), 
												to = 	c("AGE_10YRBANDS_Y15-24", "AGE_10YRBANDS_Y25-34", "AGE_10YRBANDS_Y35-44", "AGE_10YRBANDS_Y45-54", "AGE_10YRBANDS_Y55-64", "AGE_10YRBANDS_YGE65", "AGE_10YRBANDS_TOTAL")))


saveRDS(AGE_10,file = './tmp/ANNUAL_AGE_10YRBANDS.rds' )
#	X <- readRDS('./tmp/ANNUAL_AGE_10YRBANDS.rds')

												
rm(Total, X, AGE_10)
invisible(gc(reset = TRUE))

}


#############################################
# open oecd by AGGREGATE and YTHADULT AGE data

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_AGGREGATE <- function(){ 
	readr::read_csv("./input/ANNUAL_AGE_AGGREGATE.csv", col_types = 'c_c__c_c__c_______d_c') -> X
	colnames(X)[1] <- 'COUNTRY'

	X %>% filter(!COUNTRY %in% 'FTFR')-> X


# test if age classification is complete and total accurante
X %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[6:9], na.rm = TRUE)) -> Y

# remove inaccurate data #####no deletion
# Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
# X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
X %>% select(-Flags)-> X
# rm(Y)
 

# create the Total AGE
X %>% filter(!Age %in% c("Total", "Unknown")) %>% group_by(COUNTRY, SEX, Series, TIME) %>% summarise(Age = 'Total', NEW = sum(as.numeric(Value))) %>% ungroup -> Total

# test
# X %>% filter(!Age %in% c("Total", "Unknown")) %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[5:9], na.rm = TRUE)) %>% left_join(Total) %>% fix
 
AGE_AGGREGATE <- X %>% mutate(Value = as.numeric(Value)) %>% filter(!Age %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, Value = NEW)) 
AGE_AGGREGATE <- AGE_AGGREGATE  %>% 
					rename(country = COUNTRY, sex = SEX, indicator = Series, time = TIME, value = Value, classif1 = Age) %>% 
					mutate(classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 54", "55 to 64", "65+", "Total"), 
												to = 	c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_YGE65", "AGE_AGGREGATE_TOTAL")))

AGE_YTHADULT <- AGE_AGGREGATE %>% mutate(classif1 = classif1 %>% plyr::mapvalues(
									from= c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_YGE65", "AGE_AGGREGATE_TOTAL"), 
									to = c("AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE15")
									)) %>% 
					group_by(country, indicator, sex, classif1, time) %>% 
					summarise(	value = sum(value, na.rm = TRUE)) %>% 
					ungroup 
					
saveRDS(bind_rows(AGE_AGGREGATE, AGE_YTHADULT),file = './tmp/ANNUAL_AGE_AGGREGATE.rds' )
#	X <- readRDS('./tmp/ANNUAL_AGE_AGGREGATE.rds')
					
					
rm(Total, X, AGE_YTHADULT, AGE_AGGREGATE)
invisible(gc(reset = TRUE))
					
}												


######################################################
######################################################
######################################################
# back AGE with result combine and add notes create emp_rt, eap_rt, une_rt

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE <- function(){




X <- bind_rows( readRDS('./tmp/ANNUAL_AGE_5YRBANDS.rds'), 
				readRDS('./tmp/ANNUAL_AGE_10YRBANDS.rds'),  
				readRDS('./tmp/ANNUAL_AGE_AGGREGATE.rds')) %>% 
				mutate(	sex = 	sex %>% recode(	"MW" = "SEX_T", 
												"MEN" = "SEX_M", 
												"WOMEN" = "SEX_F"), 
						indicator = indicator %>% plyr::mapvalues(	from = 	c("Employment", "Labour Force", "Population", "Unemployment"), 
															to = c("EMP_TEMP_SEX_AGE_NB", "EAP_TEAP_SEX_AGE_NB", "POP_XWAP_SEX_AGE_NB", "UNE_TUNE_SEX_AGE_NB")	)) %>% 
		filter(!country %in% c('IND','CHN'))


# test total all equal 
# X %>% filter(classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_10YRBANDS_TOTAL', 'AGE_AGGREGATE_TOTAL')) %>%
# spread(classif1, value)															
															
# add source and exception for Brazil
															
ReadMeSource <- readxl::read_excel(paste0("./ReadME_OECD_LFS.xlsx"), sheet="MappingSource") %>%
				mutate(ID = substr(ID,1,3)) %>% rename(country = ID, source = REF)
															
															
X <- X %>% left_join(ReadMeSource) %>% mutate(source = ifelse(country %in% 'BRA', 'BA:358', source))															
															
															
															
															
															
########## backup formaer mapping from ODR done by YZ in 2013															
# require(ilo)
# init_ilo()
# Y <- get_ilo(collection = 'ODR')													
# Y <- Y %>% filter(indicator %in% c("EMP_TEMP_SEX_AGE_NB", "EAP_TEAP_SEX_AGE_NB", "POP_XWAP_SEX_AGE_NB", "UNE_TUNE_SEX_AGE_NB"))			
# Y <- Y %>% group_by(country, indicator, time) %>% summarise(note_source = first(note_source))	%>% ungroup 

# Y %>% readr::write_csv(paste0("./input\\backupnote.csv"))


Y <- readr::read_csv(paste0("./input/backupnote.csv"))
# cleaning unuseful notes
Y <- Y %>% mutate(
			note_source = gsub('R1:2383_','',note_source,fixed = TRUE),
			note_source = gsub('S3:5_','',note_source,fixed = TRUE),
			note_source = gsub('S3:9_','',note_source,fixed = TRUE),
			note_source = gsub('S3:14_','',note_source,fixed = TRUE),
			note_source = gsub('S3:1490_','',note_source,fixed = TRUE),
			note_source = gsub('S3:19_','',note_source,fixed = TRUE),
			note_source = gsub('S3:20_','',note_source,fixed = TRUE),
			note_source = gsub('S3:16_','',note_source,fixed = TRUE),
			note_source = gsub('S3:2882_','',note_source,fixed = TRUE),
			note_source = gsub('S3:2883_','',note_source,fixed = TRUE),
			note_source = gsub('S3:2885_','',note_source,fixed = TRUE),
			note_source = gsub('S4:29_','',note_source,fixed = TRUE),
			note_source = gsub('S4:34_','',note_source,fixed = TRUE),
			note_source = gsub('S5:37_','',note_source,fixed = TRUE),
			note_source = gsub('S5:38_','',note_source,fixed = TRUE),
			note_source = gsub('S5:39_','',note_source,fixed = TRUE),
			note_source = gsub('S5:40_','',note_source,fixed = TRUE),
			note_source = gsub('S5:41_','',note_source,fixed = TRUE),
			note_source = gsub('S5:352_','',note_source,fixed = TRUE),
			note_source = gsub('S5:356_','',note_source,fixed = TRUE)
			)


Y <- Y %>% distinct(country, indicator, .keep_all = TRUE) %>% mutate(time = as.character(time))	
LAST_NOTE <- Y %>% arrange(time) %>% group_by(country, indicator) %>% summarise(time = first(time), note_source = first(note_source)) %>% ungroup

X <- left_join(X, Y, by = c("country", "indicator", "time")) %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:2382_T2:84_T3:89', note_source), 
							  note_source = ifelse(country %in% 'RUS','R1:2382_T2:84_T3:102',note_source ), 
							  note_source = ifelse(country %in% 'JPN' & time %in% '2011','R1:2382_S4:34_T2:84_T3:102',note_source )
							  ) 


ANNUAL_AGE <- X %>% select(country, source, sex:note_source) %>% mutate(classif2 = NA) %>% ilo:::switch_ilo(version)


#calculate indicator rate

UNE_DEAP_SEX_AGE_RT  <- 	bind_rows(
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"UNE_TUNE_SEX_AGE_NB"), 
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"EAP_TEAP_SEX_AGE_NB")  %>% select(-contains('notes'))
				) %>% spread (indicator, value) %>% 
				mutate(value = round(UNE_TUNE_SEX_AGE_NB / EAP_TEAP_SEX_AGE_NB * 100,3)) %>% 
				filter(!value %in% NA) %>%
				select(-UNE_TUNE_SEX_AGE_NB, -EAP_TEAP_SEX_AGE_NB) %>% 
				mutate(indicator = 'UNE_DEAP_SEX_AGE_RT')
				


EAP_DWAP_SEX_AGE_RT  <- 	bind_rows(
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"EAP_TEAP_SEX_AGE_NB"), 
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"POP_XWAP_SEX_AGE_NB")  %>% select(-contains('notes'))
				) %>% spread (indicator, value) %>% 
				mutate(value = round(EAP_TEAP_SEX_AGE_NB / POP_XWAP_SEX_AGE_NB * 100,3)) %>% 
				filter(!value %in% NA) %>%
				select(-EAP_TEAP_SEX_AGE_NB, -POP_XWAP_SEX_AGE_NB) %>% 
				mutate(indicator = 'EAP_DWAP_SEX_AGE_RT')
				
				
		
EMP_DWAP_SEX_AGE_RT  <- 	bind_rows(
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"EMP_TEMP_SEX_AGE_NB"), 
				ANNUAL_AGE %>% 	
				filter(	indicator%in%"POP_XWAP_SEX_AGE_NB")  %>% select(-contains('notes'))
				) %>% spread (indicator, value) %>% 
				mutate(value = round(EMP_TEMP_SEX_AGE_NB / POP_XWAP_SEX_AGE_NB * 100,3)) %>% 
				filter(!value %in% NA) %>%
				select(-EMP_TEMP_SEX_AGE_NB, -POP_XWAP_SEX_AGE_NB) %>% 
				mutate(indicator = 'EMP_DWAP_SEX_AGE_RT')
								
				
	

saveRDS(bind_rows(ANNUAL_AGE,EAP_DWAP_SEX_AGE_RT, UNE_DEAP_SEX_AGE_RT, EMP_DWAP_SEX_AGE_RT ) %>% select(-contains('_version')),file = './tmp/ANNUAL_AGE.rds' )
#	X <- readRDS('./tmp/ANNUAL_AGE.rds')
	
	
rm(EAP_DWAP_SEX_AGE_RT, UNE_DEAP_SEX_AGE_RT, EMP_DWAP_SEX_AGE_RT, X, Y,   ANNUAL_AGE, LAST_NOTE, ReadMeSource)
	invisible(gc(reset = TRUE))			
}				
				
				
################### 
# processing of "UNE_TUNE_SEX_AGE_DUR_NB
# from oecd


REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_AGE_DUR <- function(){
#############################################
# open oecd by unemployment by age and duration
 				
				
				
		 
X <- readr::read_csv("./input/ANNUAL_UNE_AGE_DUR.csv") 

colnames(X)[1] <- 'COUNTRY'
X <- X %>% select(COUNTRY, SEX, Age, Duration,  TIME,  Value)


X %>% filter(!COUNTRY %in% c('FTFR', 'CHN', 'IND'))-> X

########### no erro found 
# test if age classification is complete and total accurante
# X %>% mutate(Value = as.numeric(Value)) %>% spread(Duration, Value)  %>% mutate(TOT = rowSums(.[c(6:10,13)], na.rm = TRUE)) -> Y

# # remove inaccurate data 
# Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA, !COUNTRY %in% c('GBR', 'RUS')) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
# X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
# rm(Y)
 		
				
				
				
				
													
X <- X  %>% 		filter(!Duration %in% 'Total Declared') %>%
					rename(country = COUNTRY, sex = SEX, time = TIME, value = Value, classif1 = Age, classif2 = Duration) %>% 
					mutate(	indicator = 'UNE_TUNE_SEX_AGE_NB',
							classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 54", "55+", "Total"), 
												to = 	c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_TOTAL")), 
							sex = 	sex %>% plyr::mapvalues(	from = 	c("MW", "MEN", "WOMEN"), 
																to = c("SEX_T", "SEX_M", "SEX_F")	))
																
																
DUR_AGGREGATE <- X %>% mutate(classif2 = classif2 %>% plyr::mapvalues(
									from= c("< 1 month", "> 1 month and < 3 months", "> 3 month and < 6 months", "> 6 month and < 1 year", "1 year and over", "Total", "Unknown"), 
									to = c("DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MGE6LT12", "DUR_AGGREGATE_MGE12", "DUR_AGGREGATE_TOTAL", "DUR_AGGREGATE_X")
									))	%>%
				group_by(country, indicator, sex, classif1, classif2, time) %>% 
					summarise(	value = sum(value, na.rm = TRUE)) %>% 
					ungroup
										

DUR_DETAILS <- X %>% mutate(classif2 = classif2 %>% plyr::mapvalues(
									from= c("< 1 month", "> 1 month and < 3 months", "> 3 month and < 6 months", "> 6 month and < 1 year", "1 year and over", "Total", "Unknown"), 
									to = c("DUR_DETAILS_MLT1", "DUR_DETAILS_MGE1LT3", "DUR_DETAILS_MGE3LT6", "DUR_DETAILS_MGE6LT12", "DUR_DETAILS_MGE12LT24", "DUR_DETAILS_TOTAL", "DUR_DETAILS_X")
									))										

DUR_DETAILS <- DUR_DETAILS %>% mutate(note_classif = ifelse(classif2 %in% 'DUR_DETAILS_MGE12LT24', 'C7:3716',NA ))

DUR_AGE_AGGREGATE <- bind_rows(DUR_DETAILS, DUR_AGGREGATE)


DUR_AGE_YTHADULT <- DUR_AGE_AGGREGATE %>% 
					mutate(classif1 = classif1 %>% plyr::mapvalues(
									from= c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_TOTAL"), 
									to = c("AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE15")
									)) %>% 
					group_by(country, indicator, sex, classif1, classif2, time) %>% 
					summarise(	value = sum(value, na.rm = TRUE), 
								note_classif = first(note_classif)) %>% 
					ungroup 

DUR_AGE_AGGREGATE <- DUR_AGE_AGGREGATE %>% 
					mutate(note_classif = ifelse(classif1 %in% 'AGE_AGGREGATE_Y55-64', paste0(note_classif, '_C6:2343'), note_classif), 
							note_classif = gsub('NA_', '', note_classif, fixed = TRUE))


ANNUAL_UNE_AGE_DUR <- bind_rows(DUR_AGE_YTHADULT, DUR_AGE_AGGREGATE) %>% mutate(time = as.character(time))

############ add note and source 

ANNUAL_UNE_AGE_DUR <- left_join(	ANNUAL_UNE_AGE_DUR, 
									readRDS('./tmp/ANNUAL_AGE.rds') %>% filter(indicator %in% 'UNE_TUNE_SEX_AGE_NB') %>% distinct(country, indicator, time, source, note_source), by = c('country', 'indicator', 'time')) %>% 
									mutate(indicator = 'UNE_TUNE_SEX_AGE_DUR_NB')

									
saveRDS(ANNUAL_UNE_AGE_DUR,file = './tmp/ANNUAL_UNE_AGE_DUR.rds' )
#	X <- readRDS('./tmp/ANNUAL_AGE.rds')
										
									
rm(DUR_AGE_YTHADULT, DUR_AGE_AGGREGATE, DUR_DETAILS, DUR_AGGREGATE, X, ANNUAL_UNE_AGE_DUR)	
invisible(gc(reset = TRUE))		
}

			
			
################### 
# processing of EMP_TEMP_SEX_STE_NB

REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_EMP_STE <- function(){		
	
readr::read_csv("./input/ANNUAL_EMP_STE.csv", col_types = 'c__c_c__c_______d_c') -> X
colnames(X)[1] <- 'country'



X <- X %>% 	rename( sex = Sex, classif1 = Subject, time = TIME, value = Value, note_value = Flags) %>% 
			mutate(
					classif1 = classif1 %>% 
									plyr::mapvalues(	from = c("Civil Employment all status, all activities", "Employees in all activities", "Employers and persons working on own account all activities",  "Unpaid family workers all activities"), 
														to = c('STE_ICSE93_TOTAL','STE_ICSE93_1','STE_ICSE93_3','STE_ICSE93_5')),
					sex = sex %>% 
									plyr::mapvalues(	from = c("Males", 'Females', 'All persons'), 
														to = c('SEX_M','SEX_F','SEX_T'))
					
					
					)
					
					
														
														
													
# test if classification is complete and total accurate
# X %>% select(-note_value) %>% spread(classif1, value)  %>% mutate(TOT = rowSums(.[4:6], na.rm = TRUE)) %>% filter(!TOT == 0, as.numeric(time) > 1959) %>% mutate(STE_ICSE93_6 = STE_ICSE93_TOTAL - TOT) -> Y




#Y %>% mutate(test = (round(as.numeric(STE_ICSE93_TOTAL),0) - round(as.numeric(TOT),0) ) > 1) %>% as.tbl -> Y

# remove inaccurate data 
#Y %>% left_join(TEST) %>% filter(!value %in% NA)  %>% mutate(test2 = abs((round(as.numeric(STE_ICSE93_TOTAL),0) - round(as.numeric(value),0)) ) > 3) %>% filter(test2 %in% TRUE) %>% distinct(country, sex, time) %>% mutate(delete = TRUE) -> Y
#X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select( -delete)-> X
X <- X %>% mutate(value = as.numeric(as.character(value))) %>% filter(!classif1 %in% c(0, '0'))


######### delete obvious error the reclaculate them

X <- X %>%   filter(
					!(country %in% 'TUR' & time %in% c('1960','1961', '1965')),
					!(country %in% 'USA' & time %in% c('1955','1956', '1957', '1958', '1959', '1960', '1961', '1962') & sex %in% c('SEX_F','SEX_M')),					
					!(country %in% 'NZL' & time %in% c('1957','1958', '1959', '1960') & classif1 %in% 'STE_ICSE93_1') 
				)




X <- X %>% arrange(desc(sex)) %>% group_by(country, classif1, time) %>% mutate(test = paste0(sex, collapse = ' ')) %>% ungroup 

unique(X$test)

ADD1 <- X %>% filter(test %in% 'SEX_T SEX_F') %>% group_by(country, classif1, time) %>% summarise(sex = 'SEX_M', value = first(value) - last(value), note_value = first(note_value)) %>% ungroup
ADD2 <- X %>% filter(test %in% 'SEX_M SEX_F') %>% group_by(country, classif1, time) %>% summarise(sex = 'SEX_T', value = first(value) + last(value), note_value = first(note_value)) %>% ungroup

X <- bind_rows(X, ADD1, ADD2) %>% arrange(country, time)



X %>% arrange(desc(classif1)) %>% group_by(country, sex, time) %>% mutate(test = paste0(classif1, collapse = ' ')) %>% ungroup -> X

unique(X$test)

ADD1 <- X %>% filter(test %in% 'STE_ICSE93_3 STE_ICSE93_1') %>% group_by(country, sex, time) %>% summarise(classif1 = 'STE_ICSE93_TOTAL', value = first(value) + last(value), note_value = first(note_value)) %>% ungroup
ADD2 <- X %>% filter(test %in% 'STE_ICSE93_TOTAL STE_ICSE93_1') %>% group_by(country, sex, time) %>% summarise(classif1 = 'STE_ICSE93_3', value = first(value) - last(value), note_value = first(note_value)) %>% ungroup





X <- bind_rows(X, ADD1) %>% arrange(country, time)


X %>% arrange(desc(classif1), desc(sex)) %>% mutate(REF = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% group_by(country, time) %>% mutate(test = paste0(REF, collapse = ' ')) %>% ungroup %>% select(-REF) -> X

X %>% filter(!test %in% c(	'T_TOTAL M_TOTAL F_TOTAL',
							'T_TOTAL',
							'T_1')) %>% select(-test)-> X



STE_AGGREGATEPLUS <- X %>% 
			mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3", 'STE_ICSE93_5'), 
															to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF", 'STE_AGGREGATE_SLF'))) %>% 
			group_by(country,sex,time, classif1) %>% 
			summarise(value = sum(value, na.rm = TRUE), note_value = first(note_value)) %>% ungroup
			

X %>% arrange(desc(classif1), desc(sex)) %>% mutate(REF = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% group_by(country, time) %>% mutate(test = paste0(REF, collapse = ' ')) %>% ungroup %>% select(-REF) -> X

X %>% filter(!test %in% c(	'T_TOTAL M_TOTAL F_TOTAL T_1 M_1 F_1',
							'T_TOTAL M_TOTAL F_TOTAL T_1',
							'T_TOTAL T_1', 
							'T_TOTAL M_TOTAL F_TOTAL T_3 T_1', 
							'T_TOTAL T_3 T_1', 
							'T_TOTAL M_TOTAL F_TOTAL T_3 M_3 F_3 T_1 M_1 F_1', 
							'T_TOTAL M_TOTAL F_TOTAL T_3 T_1 M_1 F_1')) %>% select(-test)-> X

							
							
							
							
STE <- bind_rows(X,STE_AGGREGATEPLUS )


# STE %>%  mutate(test = paste0(substr(sex,5,5), '_', stringr::str_sub(classif1,12,-1))) %>% select(country:value, test) %>% select(-sex, -classif1) %>% spread(test , value) %>% save_ilo()


STE <- STE %>% arrange(country, time, note_value) %>% group_by(country, time) %>% mutate(note_value = first(note_value)) %>% ungroup() %>%arrange(country, time, desc(sex), classif1)
	


# X %>% select(-note_value) %>% spread(classif1, value) -> Y

REF <- readRDS('./tmp/ANNUAL_AGE.rds')  %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB') %>% distinct(country, source,note_source) %>% group_by(country, source ) %>% summarise(note_source = last(note_source)) %>% ungroup 

STE <- STE %>% left_join( REF, by = "country")

STE <- STE %>% mutate(indicator = 'EMP_TEMP_SEX_STE_NB')

saveRDS(STE,file = './tmp/ANNUAL_STE.rds' )



# STE %>% select(country:value) %>% mutate(test  = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% select(-sex, -classif1) %>% spread(test, value)


#	X <- readRDS('./tmp/ANNUAL_STE.rds')
		




# test with emp by age and add missing on age

# Y %>% left_join(
# ANNUAL_AGE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB', classif1 %in% 'AGE_AGGREGATE_TOTAL') %>% select(country, sex, time, value) %>% rename(AGE = value)
# ) %>% 	filter(AGE %in% NA) %>% 
		# select(country, sex,  time, STE_ICSE93_TOTAL) %>% 
		# rename(value = STE_ICSE93_TOTAL) %>% 
		# mutate(classif1 = 'AGE_AGGREGATE_TOTAL', indicator = 'EMP_TEMP_SEX_AGE_NB') %>% 
		# left_join(LAST_NOTE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB') %>% distinct(country, note_source)) %>% 
		# left_join(ANNUAL_AGE %>% distinct(country, source)) -> NEW

# ANNUAL_AGE <- bind_rows(ANNUAL_AGE, NEW) %>% filter(!note_source %in% NA) %>% filter(!value %in% NA)
# rm(NEW)





# TEST <- Y %>% filter((STE_ICSE93_3 %in% NA & STE_ICSE93_5 %in% NA) | (STE_ICSE93_3 %in% 0 & STE_ICSE93_5 %in% 0) | STE_ICSE93_3 %in% c(0, NA) | STE_ICSE93_TOTAL %in% NA) %>% select(country, sex, time) %>% mutate(delete = TRUE)

# X <- X %>% left_join(TEST) %>% filter(!delete %in% TRUE) %>% select( -delete) %>% filter(!value %in% 0)

# rm(TEST)

# X  %>% select(-note_value) %>% spread(classif1, value) -> Y

# TEST <- Y %>% filter(STE_ICSE93_5 %in% NA) %>% distinct(country,   sex,  time) %>% mutate(keep = TRUE)

# STE_AGGREGATE <- X %>% left_join(TEST) %>% filter(keep %in% TRUE) %>% select(-keep) %>% 
			# mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3"), 
															# to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF")))


# STE_ISCE93 <- X %>% left_join(TEST) %>% filter(!keep %in% TRUE) %>% select(-keep)

# STE_AGGREGATEPLUS <- STE_ISCE93 %>% 
			# mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3", 'STE_ICSE93_5'), 
															# to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF", 'STE_AGGREGATE_SLF'))) %>% 
			# group_by(country,sex,time, classif1) %>% 
			# summarise(value = sum(value), note_value = first(note_value)) %>% ungroup
			
# STE_ISCE93 <- STE_ISCE93 %>% mutate(note_classif = ifelse(classif1 %in% 'STE_ICSE93_3', 'C2:975', NA))			


# STE <- bind_rows(STE_ISCE93, STE_AGGREGATEPLUS, STE_AGGREGATE) %>% left_join(
		# ANNUAL_AGE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB', classif1 %in% 'AGE_AGGREGATE_TOTAL') %>% distinct(country, time, source, note_source) 
		# ) %>% filter(!note_source %in% NA) %>% 
		# mutate(indicator = 'EMP_TEMP_SEX_STE_NB')





rm( STE_AGGREGATEPLUS)
 	invisible(gc(reset = TRUE))
			
}				
			

		
			
			
			
			
REP_OECD.LFS_ANNUAL_input <- function(){			
######### result			
			
res <- bind_rows(ANNUAL_UNE_AGE_DUR, ANNUAL_AGE,STE ) %>% 
			mutate(	value = round(value, 3), 
					
					note_value = ifelse(note_value %in% 'Break', 'B', NA), 
					note_source = ifelse(note_value %in% 'B' & note_source %in% NA, 'I11:264', note_source), 
					note_source = ifelse(note_value %in% 'B' & !note_source %in% NA, paste0(note_source, '_I11:264'), note_source)
					)	%>% select(country, indicator, source, sex:value, note_value, note_classif, note_source)  %>% switch_ilo(version)
				
			
res <- res %>% filter(as.numeric(time) >= 1990)				
					


ref_file <- unique(res$country)


for (i in 1:length(ref_file)){
saveRDS(res %>% filter(country %in% ref_file[i]), file = paste0("./output/",ref_file[i],".rds"))
}

ref <- cbind(PATH = paste0(getwd(),"/output/",ref_file,".rds"),ID = NA, Types  ="CL", REF = paste0(ref_file))
write.csv(ref,paste("./FileToLoad.csv",sep=""),row.names = FALSE,na="")

}



rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)	



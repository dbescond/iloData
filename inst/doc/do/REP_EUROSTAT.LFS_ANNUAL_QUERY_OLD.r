#' 
#' on quarterly basis , email is send to eurostat lfs team to get self made tabulation bases on Eurostat_Query_2016.xlsx request
#' EU LFS data at annual level is request
#' Current contact is Frank.BAUER@ec.europa.eu, team contact : ESTAT-LFS-user-support@ec.europa.eu
#' query built base on eulfs database userguide.pdf (check help foler)
#' This Data is used to get the relation between the Country code and the Country Name used by AMECO

#' @return csv files compare with ilostat database on output folder
#'
#' @examples
#' ## Not run:
#'
#'  source(paste0("J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/LFS_ANNUAL_QUERY/do/REP_EUROSTAT.LFS_ANNUAL_QUERY_input.r") )   
#'
#' ## End(**Not run**)



REP_EUROSTAT.LFS_ANNUAL_QUERY_input <- function(){
setwd(paste0(ilo:::path$data, '/REP_EUROSTAT/LFS_ANNUAL_QUERY/'))

inputFile <- list.files('./input/')	%>% as_data_frame %>% filter(str_detect(value, '.csv'))	


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_141()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_142()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_143()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EAP_6()

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_15()

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21()

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24()

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29()

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40()
}
		


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_141 		<- 	function(){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE, 5, 10, agg, yth 
ref_file <- 'ILO_POP_141.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	ILOSTAT = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX, 
					indicator = ILOSTAT, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL',
											'AGE_5YRBANDS_Y15-19' 	= 'AGE_5YRBANDS_Y15-19', 
											'AGE_5YRBANDS_Y20-24' 	= 'AGE_5YRBANDS_Y20-24', 
											'AGE_5YRBANDS_Y25-29'	= 'AGE_5YRBANDS_Y25-29', 
											'AGE_5YRBANDS_Y30-34'	= 'AGE_5YRBANDS_Y30-34', 
											'AGE_5YRBANDS_Y35-39' 	= 'AGE_5YRBANDS_Y35-39', 
											'AGE_5YRBANDS_Y40-44' 	= 'AGE_5YRBANDS_Y40-44', 
											'AGE_5YRBANDS_Y45-49' 	= 'AGE_5YRBANDS_Y45-49', 
											'AGE_5YRBANDS_Y50-54' 	= 'AGE_5YRBANDS_Y50-54', 
											'AGE_5YRBANDS_Y55-59' 	= 'AGE_5YRBANDS_Y55-59', 
											'AGE_5YRBANDS_Y60-64' 	= 'AGE_5YRBANDS_Y60-64', 
											'AGE_5YRBANDS_YGE65' 	= 'AGE_5YRBANDS_YGE65', 
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 5 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'POP_XWAP_SEX_AGE_NB')	, file = paste0('./input/indicator/POP_XWAP_SEX_AGE_NB.AGE_5YRBANDS.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EAP_TEAP_SEX_AGE_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_NB.AGE_5YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EMP_TEMP_SEX_AGE_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_NB.AGE_5YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'UNE_TUNE_SEX_AGE_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_NB.AGE_5YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EIP_TEIP_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_NB.AGE_5YRBANDS.rds'))	

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'POP_XWAP_SEX_AGE_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_NB.AGE_10YRBANDS.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EAP_TEAP_SEX_AGE_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EMP_TEMP_SEX_AGE_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'UNE_TUNE_SEX_AGE_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EIP_TEIP_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_NB.AGE_10YRBANDS.rds'))
	
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))

											
		
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'POP_XWAP_SEX_AGE_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_NB.AGE_AGGREGATE.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EAP_TEAP_SEX_AGE_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EMP_TEMP_SEX_AGE_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'UNE_TUNE_SEX_AGE_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EIP_TEIP_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_NB.AGE_AGGREGATE.rds'))
	
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
			
		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'POP_XWAP_SEX_AGE_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_NB.AGE_YTHADULT.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EAP_TEAP_SEX_AGE_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EMP_TEMP_SEX_AGE_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'UNE_TUNE_SEX_AGE_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EIP_TEIP_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_NB.AGE_YTHADULT.rds'))
	
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
		
	rm(X)

	print(ref_file)
	
	
}
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_142 		<- 	function(){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE_EDU, 10, agg, yth, isced11, isced97, agg 
ref_file <- 'ILO_POP_142.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	HAT97LEV = col_character(),
																	HAT11LEV = col_character(),
																	ILOSTAT = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	# COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX, 
					indicator = ILOSTAT, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
	X 	<-  bind_rows(
					X %>% filter(time < 2014) %>% rename(classif2 = HAT97LEV) %>% select(-HAT11LEV),															
					X %>% filter(time > 2013) %>% rename(classif2 = HAT11LEV) %>% select(-HAT97LEV)) 

	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															

	
	################## add remapping EDU aggregate
	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/AGE_10YRBANDS.EDU_ISCED11.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/AGE_10YRBANDS.EDU_ISCED97.rds'))														
	saveRDS(X %>% 	mutate(classif2 = classif2 %>% recode('EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_10YRBANDS'), str_detect(classif2, 'AGGREGATE')) , file = paste0('./input/indicator/AGE_10YRBANDS.EDU_AGGREGATE.rds'))														

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/AGE_AGGREGATE.EDU_ISCED11.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/AGE_AGGREGATE.EDU_ISCED97.rds'))														
	saveRDS(X %>% 	mutate(classif2 = classif2 %>% recode('EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_AGGREGATE'), str_detect(classif2, 'AGGREGATE')) , file = paste0('./input/indicator/AGE_AGGREGATE.EDU_AGGREGATE.rds'))														

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/AGE_YTHADULT.EDU_ISCED11.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/AGE_YTHADULT.EDU_ISCED97.rds'))														
	saveRDS(X %>% 	mutate(classif2 = classif2 %>% recode('EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_YTHADULT'), str_detect(classif2, 'AGGREGATE')) , file = paste0('./input/indicator/AGE_YTHADULT.EDU_AGGREGATE.rds'))														

	rm(X)

	print(ref_file)
	
	
}
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_143 		<- 	function(){	# WAP, EAP, EMP, UNE, EIP, SEX_AGE_GEO, 5, 10, agg, yth 
ref_file <- 'ILO_POP_143.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																		COUNTRY = col_character(),
																		YEAR = col_integer(),
																		#QUARTER = col_character(),
																		SEX = col_character(),
																		DEGURBA = col_character(),
																		AGE = col_character(),
																		ILOSTAT = col_character(),
																		VALUE = col_double(),
																		FLAG = col_character(),
																		FLAG_BREAK = col_character()
																		#COUNTRY_ORDER = col_character()
																	)) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					classif2 = DEGURBA, 
					sex = SEX, 
					indicator = ILOSTAT, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"), 
					classif2 = classif2 %>% recode('GEO_COV_X' 	= 'GEO_COV_NRESP'))
	

	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															

	
	################## add remapping EDU aggregate
	
	
	
	

	############ prepare indicator file by 10 years bands

	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'POP_XWAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_GEO_NB.AGE_10YRBANDS.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EAP_TEAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_GEO_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EMP_TEMP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_GEO_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'UNE_TUNE_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_GEO_NB.AGE_10YRBANDS.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EIP_TEIP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_GEO_NB.AGE_10YRBANDS.rds'))
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'POP_XWAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_GEO_NB.AGE_AGGREGATE.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EAP_TEAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_GEO_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EMP_TEMP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_GEO_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'UNE_TUNE_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_GEO_NB.AGE_AGGREGATE.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EIP_TEIP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_GEO_NB.AGE_AGGREGATE.rds'))
	
		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'POP_XWAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/POP_XWAP_SEX_AGE_GEO_NB.AGE_YTHADULT.rds'))														
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EAP_TEAP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EAP_TEAP_SEX_AGE_GEO_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EMP_TEMP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_GEO_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'UNE_TUNE_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/UNE_TUNE_SEX_AGE_GEO_NB.AGE_YTHADULT.rds'))	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EIP_TEIP_SEX_AGE_GEO_NB') , file = paste0('./input/indicator/EIP_TEIP_SEX_AGE_GEO_NB.AGE_YTHADULT.rds'))
	
	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EAP_6 			<- 	function(){ # LFPR, SEX_AGE, 5, 10, agg, yth 
ref_file <- 'ILO_EAP_6.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	# POP_Target = col_double(),
																	# POP_NoAnswer = col_integer(),
																	# POP = col_double(),
																	ILOSTAT_Rate = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	# COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EAP_DWAP_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX,  
					obs_value = ILOSTAT_Rate, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL',
											'AGE_5YRBANDS_Y15-19' 	= 'AGE_5YRBANDS_Y15-19', 
											'AGE_5YRBANDS_Y20-24' 	= 'AGE_5YRBANDS_Y20-24', 
											'AGE_5YRBANDS_Y25-29'	= 'AGE_5YRBANDS_Y25-29', 
											'AGE_5YRBANDS_Y30-34'	= 'AGE_5YRBANDS_Y30-34', 
											'AGE_5YRBANDS_Y35-39' 	= 'AGE_5YRBANDS_Y35-39', 
											'AGE_5YRBANDS_Y40-44' 	= 'AGE_5YRBANDS_Y40-44', 
											'AGE_5YRBANDS_Y45-49' 	= 'AGE_5YRBANDS_Y45-49', 
											'AGE_5YRBANDS_Y50-54' 	= 'AGE_5YRBANDS_Y50-54', 
											'AGE_5YRBANDS_Y55-59' 	= 'AGE_5YRBANDS_Y55-59', 
											'AGE_5YRBANDS_Y60-64' 	= 'AGE_5YRBANDS_Y60-64', 
											'AGE_5YRBANDS_YGE65' 	= 'AGE_5YRBANDS_YGE65', 
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 5 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EAP_DWAP_SEX_AGE_RT')	, file = paste0('./input/indicator/EAP_DWAP_SEX_AGE_RT.AGE_5YRBANDS.rds'))														

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EAP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EAP_DWAP_SEX_AGE_RT.AGE_10YRBANDS.rds'))														

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EAP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EAP_DWAP_SEX_AGE_RT.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EAP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EAP_DWAP_SEX_AGE_RT.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_15 		<- 	function(){ # DIS, SEX_AGE, 10, agg, yth 
ref_file <- 'ILO_EMP_15.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EMP_TEMP_SEX_STE_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = STAPRO, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	# test OK	X %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% select(-obs_status) %>% spread(sex, obs_value) %>% ilo:::save_ilo()
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'STE_ICSE93')	, indicator %in% 'EMP_TEMP_SEX_STE_NB')	, file = paste0('./input/indicator/EMP_TEMP_SEX_STE_NB.STE_ICSE93.rds'))														


	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'STE_ICSE93_1' 	=  'STE_AGGREGATE_EES', 
														'STE_ICSE93_6'  =  'STE_AGGREGATE_X',
														'STE_ICSE93_TOTAL' 	=  'STE_AGGREGATE_TOTAL'
														)) %>% filter(!str_detect(classif1, 'STE_ICSE93'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'STE_AGGREGATE')	, indicator %in% 'EMP_TEMP_SEX_STE_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_STE_NB.STE_AGGREGATE.rds'))														

	rm(X)

	print(ref_file)
	
	
}
	
	
	
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12 		<- 	function(){ # EPR, SEX_AGE, 5, 10, agg, yth 
ref_file <- 'ILO_EMP_12.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	# POP_Target = col_double(),
																	# POP_NoAnswer = col_integer(),
																	# POP = col_double(),
																	ILOSTAT_Rate = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	# COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EMP_DWAP_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX,  
					obs_value = ILOSTAT_Rate, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL',
											'AGE_5YRBANDS_Y15-19' 	= 'AGE_5YRBANDS_Y15-19', 
											'AGE_5YRBANDS_Y20-24' 	= 'AGE_5YRBANDS_Y20-24', 
											'AGE_5YRBANDS_Y25-29'	= 'AGE_5YRBANDS_Y25-29', 
											'AGE_5YRBANDS_Y30-34'	= 'AGE_5YRBANDS_Y30-34', 
											'AGE_5YRBANDS_Y35-39' 	= 'AGE_5YRBANDS_Y35-39', 
											'AGE_5YRBANDS_Y40-44' 	= 'AGE_5YRBANDS_Y40-44', 
											'AGE_5YRBANDS_Y45-49' 	= 'AGE_5YRBANDS_Y45-49', 
											'AGE_5YRBANDS_Y50-54' 	= 'AGE_5YRBANDS_Y50-54', 
											'AGE_5YRBANDS_Y55-59' 	= 'AGE_5YRBANDS_Y55-59', 
											'AGE_5YRBANDS_Y60-64' 	= 'AGE_5YRBANDS_Y60-64', 
											'AGE_5YRBANDS_YGE65' 	= 'AGE_5YRBANDS_YGE65', 
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	########### prepare indicator file by 5 years bands
	# saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT')	, file = paste0('./input/indicator/EMP_DWAP_SEX_AGE_RT.AGE_5YRBANDS.rds'))														

	########### prepare indicator file by 10 years bands
	# X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														# 'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														# )) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EMP_DWAP_SEX_AGE_RT.AGE_AGGREGATE.rds'))														

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EMP_DWAP_SEX_AGE_RT.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT') , file = paste0('./input/indicator/EMP_DWAP_SEX_AGE_RT.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_20 		<- 	function(){ # EMP, ftpt, SEX_AGE_JOB, 10, agg, yth, time
ref_file <- 'ILO_EMP_20.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	FTPT = col_character(),
																	AGE = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EMP_TEMP_SEX_AGE_JOB_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					classif2 = FTPT,
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands

	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EMP_TEMP_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_JOB_NB.AGE_10YRBANDS.rds'))														

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EMP_TEMP_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_JOB_NB.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EMP_TEMP_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EMP_TEMP_SEX_AGE_JOB_NB.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21 		<- 	function(){ # EES, SEX_AGE_JOB, 10, agg, yth, contract 
ref_file <- 'ILO_EES_21.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	TEMP = col_character(),
																	AGE = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EES_TEES_SEX_AGE_JOB_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					classif2 = TEMP,
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands

	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EES_TEES_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EES_TEES_SEX_AGE_JOB_NB.AGE_10YRBANDS.rds'))														

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EES_TEES_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EES_TEES_SEX_AGE_JOB_NB.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EES_TEES_SEX_AGE_JOB_NB') , file = paste0('./input/indicator/EES_TEES_SEX_AGE_JOB_NB.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}
		

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24 		<- 	function(){ # TRU, EX_AGE, 10, agg, yth
ref_file <- 'ILO_TRU_24.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	AGE = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'TRU_TTRU_SEX_AGE_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'TRU_TTRU_SEX_AGE_NB')	, file = paste0('./input/indicator/TRU_TTRU_SEX_AGE_NB.AGE_10YRBANDS.rds'))														

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'TRU_TTRU_SEX_AGE_NB') , file = paste0('./input/indicator/TRU_TTRU_SEX_AGE_NB.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'TRU_TTRU_SEX_AGE_NB') , file = paste0('./input/indicator/TRU_TTRU_SEX_AGE_NB.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}

		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29 		<- 	function(){ # UR, SEX_AGE, 5, 10, agg, yth 
ref_file <- 'ILO_UNE_29.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	# POP_UNP = col_double(),
																	# POP_ACT = col_double(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	# COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_DEAP_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL',
											'AGE_5YRBANDS_Y15-19' 	= 'AGE_5YRBANDS_Y15-19', 
											'AGE_5YRBANDS_Y20-24' 	= 'AGE_5YRBANDS_Y20-24', 
											'AGE_5YRBANDS_Y25-29'	= 'AGE_5YRBANDS_Y25-29', 
											'AGE_5YRBANDS_Y30-34'	= 'AGE_5YRBANDS_Y30-34', 
											'AGE_5YRBANDS_Y35-39' 	= 'AGE_5YRBANDS_Y35-39', 
											'AGE_5YRBANDS_Y40-44' 	= 'AGE_5YRBANDS_Y40-44', 
											'AGE_5YRBANDS_Y45-49' 	= 'AGE_5YRBANDS_Y45-49', 
											'AGE_5YRBANDS_Y50-54' 	= 'AGE_5YRBANDS_Y50-54', 
											'AGE_5YRBANDS_Y55-59' 	= 'AGE_5YRBANDS_Y55-59', 
											'AGE_5YRBANDS_Y60-64' 	= 'AGE_5YRBANDS_Y60-64', 
											'AGE_5YRBANDS_YGE65' 	= 'AGE_5YRBANDS_YGE65', 
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 5 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'UNE_DEAP_SEX_AGE_RT')	, file = paste0('./input/indicator/UNE_DEAP_SEX_AGE_RT.AGE_5YRBANDS.rds'))														

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'UNE_DEAP_SEX_AGE_RT') , file = paste0('./input/indicator/UNE_DEAP_SEX_AGE_RT.AGE_10YRBANDS.rds'))														

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'UNE_DEAP_SEX_AGE_RT') , file = paste0('./input/indicator/UNE_DEAP_SEX_AGE_RT.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'UNE_DEAP_SEX_AGE_RT') , file = paste0('./input/indicator/UNE_DEAP_SEX_AGE_RT.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39 		<- 	function(){ # DIS, SEX_AGE, 10, agg, yth 
ref_file <- 'ILO_EIP_39.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	AGE = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EIP_WDIS_SEX_AGE_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										),
					classif1 = classif1 %>% recode(
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL',
											'AGE_10YRBANDS_Y15-24'	= 'AGE_10YRBANDS_Y15-24', 
											'AGE_10YRBANDS_Y25-34' 	= 'AGE_10YRBANDS_Y25-34', 
											'AGE_10YRBANDS_Y35-44'	= 'AGE_10YRBANDS_Y35-44', 
											'AGE_10YRBANDS_Y45-54' 	= 'AGE_10YRBANDS_Y45-54', 
											'AGE_10YRBANDS_Y55-64' 	= 'AGE_10YRBANDS_Y55-64', 
											'AGE_10YRBANDS_YGE65' 	= 'AGE_10YRBANDS_YGE65',
											'AGE_AGGREGATE_Y25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_Y15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')	, indicator %in% 'EIP_WDIS_SEX_AGE_NB')	, file = paste0('./input/indicator/EIP_WDIS_SEX_AGE_NB.AGE_10YRBANDS.rds'))														
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(sex, obs_value) %>% ilo:::save_ilo()


	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')	, indicator %in% 'EIP_WDIS_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_WDIS_SEX_AGE_NB.AGE_AGGREGATE.rds'))														

		############ prepare indicator file by yth years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	saveRDS(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')	, indicator %in% 'EIP_WDIS_SEX_AGE_NB') , file = paste0('./input/indicator/EIP_WDIS_SEX_AGE_NB.AGE_YTHADULT.rds'))														

	rm(X)

	print(ref_file)
	
	
}
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40 		<- 	function(){ # NEET, SEX 
ref_file <- 'ILO_YTH_40.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EIP_NEET_SEX_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
																
	
	saveRDS(X, file = paste0('./input/', str_replace(ref_file, '.csv', '.rds')))															
	
	############ prepare indicator file by 10 years bands
	saveRDS(X , file = paste0('./input/indicator/EIP_NEET_SEX_NB.rds'))														

	rm(X)

	print(ref_file)
	
	
}
	





















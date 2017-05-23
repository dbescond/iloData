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
#' # set up work directory
#' setwd(paste0(ilo:::path$data, '/REP_EUROSTAT/LFS_ANNUAL_QUERY/'))
#' # get input files list
#' 
#'
#'    source("D:/NEW_EUROSTAT/do/Cmd.R")         
#'              
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
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_95_NaceRev1()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_96_NaceRev2()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev1()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev2()


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco88()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco08()


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_105()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_20()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev1()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev2()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_31()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_34()




REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_145_NaceRev1()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_146_NaceRev2()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_147_NaceRev1()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_148_NaceRev2_NACE2D()


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_151_ISCO88()
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_152_ISCO08()
}
		

		
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_141 					<- 	function(){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE, 5, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS.csv")))															
	
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))

											
		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
			
		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															
	
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
		
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_142 					<- 	function(){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE_EDU, 10, agg, yth, isced11, isced97, agg 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
	X 	<-  bind_rows(
					X %>% filter(time < 2014) %>% rename(classif2 = HAT97LEV) %>% select(-HAT11LEV),															
					X %>% filter(time > 2013) %>% rename(classif2 = HAT11LEV) %>% select(-HAT97LEV)) 

	
	
	################## add remapping EDU aggregate
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_ISCED11.csv")))															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_ISCED97.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_10YRBANDS'), str_detect(classif2, 'EDU_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_AGGREGATE.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_ISCED11.csv")))															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_ISCED97.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_AGGREGATE'), str_detect(classif2, 'EDU_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_AGGREGATE.csv")))															

			############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_ISCED11.csv")))															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_ISCED97.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_YTHADULT'), str_detect(classif2, 'EDU_AGGREGATE')), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_AGGREGATE.csv")))															


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_143 					<- 	function(){	# WAP, EAP, EMP, UNE, EIP, SEX_AGE_GEO, 5, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"), 
					classif2 = classif2 %>% recode('GEO_COV_X' 	= 'GEO_COV_NRESP'))
	

	############ prepare indicator file by 10 years bands

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															
	
	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EAP_6 						<- 	function(){ # LFPR, SEX_AGE, 5, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																

	############ prepare indicator file by 5 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															
	
		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_15 					<- 	function(){ # EMP, SEX_STE icse93, agg 
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
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'STE_ICSE93')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_STE_ICSE93.csv")))															
	

	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'STE_ICSE93_1' 	=  'STE_AGGREGATE_EES', 
														'STE_ICSE93_6'  =  'STE_AGGREGATE_X',
														'STE_ICSE93_TOTAL' 	=  'STE_AGGREGATE_TOTAL'
														)) %>% filter(!str_detect(classif1, 'STE_ICSE93'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'STE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_STE_AGGREGATE.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_95_NaceRev1 			<- 	function(){ # EMP, SEX_ECO isic3 agg sector 
ref_file <- 'ILO_EMP_95_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA111D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA111D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_96_NaceRev2 			<- 	function(){ # EMP, SEX_ECO isic4 agg sector 
ref_file <- 'ILO_EMP_96_NaceRev2.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "ECO_ISIC4.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev1 			<- 	function(){ # EMP, SEX_ECO2 isic3 2 digits agg sector 
ref_file <- 'ILO_EMP_97_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA113D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA113D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_EQISIC4ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_EQISIC4ISIC3.csv")))															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev2 			<- 	function(){ # EMP, SEX_ECO2 isic4 2 digits agg sector 
ref_file <- 'ILO_EMP_97_NaceRev2.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE2D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE2D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_2digit.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_100_Isco88 			<- 	function(){ # EMP, SEX_OCU isco 88 skill 					############ PENDING 
ref_file <- 'ILO_EMP_100_Isco88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add OCU_ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO88.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_1'
														)) %>% filter(!str_detect(classif1, 'OCU_ISCO88')) %>% # calculate SKILL_X
			 group_by(ref_area, time, sex, indicator, classif1)
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco88 			<- 	function(){ # EMP, SEX_OCU2 isco88 2 digits agg sector 
ref_file <- 'ILO_EMP_102_Isco88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO3D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO3D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add OCU_isco88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_EQISCO08ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_EQISCO08ISCO88.csv")))															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco08		 		<- 	function(){ # EMP, SEX_OCU2 isco08 2 digits agg sector 
ref_file <- 'ILO_EMP_102_Isco08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO2D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO2D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add OCU_isco88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO08_2digit.csv")))															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_105 					<- 	function(){ # UNE, SEX_CAT
ref_file <- 'ILO_EMP_105.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	HWACTUAL = col_character(),
																	STAPRO = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = HWACTUAL, 
					sex = SEX,  
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	

	############ prepare segment previous and other
	data.table:::fwrite(X , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), ".csv")))				

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12 					<- 	function(){ # EPR, SEX_AGE, 5, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	
	########### prepare indicator file by 5 years bands
	# saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT')	, file = paste0('./input/indicator/indicator/EMP_DWAP_SEX_AGE_RT.AGE_5YRBANDS.rds'))														
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS.csv")))															

	########### prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														 'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														 )) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_20 					<- 	function(){ # EMP, ftpt, SEX_AGE_JOB, 10, agg, yth, time
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																

	############ prepare indicator file by 10 years bands
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															
	
		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21 					<- 	function(){ # EES, SEX_AGE_JOB, 10, agg, yth, contract 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	
	############ prepare indicator file by 10 years bands

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}
		

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24 					<- 	function(){ # TRU, SEX_AGE, 10, agg, yth
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev1 			<- 	function(){ # TRU, SEX_ECO, agg <2008, yth
ref_file <- 'ILO_TRU_26_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	NA111D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'TRU_TTRU_SEX_AGE_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA111D, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
																

	################## add ECO_AGGREGATE
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															

	############ prepare indicator file by eco sector
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
		
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev2 			<- 	function(){ # TRU, SEX_ECO, agg >2008, yth
ref_file <- 'ILO_TRU_26_NaceRev2.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	NACE1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'TRU_TTRU_SEX_AGE_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
																

	################## add ECO_AGGREGATE
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															

	############ prepare indicator file by eco sector
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
		
	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29 					<- 	function(){ # UR, SEX_AGE, 5, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																
	
	
	############ prepare indicator file by 5 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_31 					<- 	function(){ # UNE SEX_AGE_DUR agg, yth, dur_details, agg 
ref_file <- 'ILO_UNE_31.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	AGE = col_character(),
																	SEX = col_character(),
																	DURUNE = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	# COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_AGE_DUR_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = AGE, 
					classif2 = DURUNE, 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
	
	
	################## add remapping EDU aggregate
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_DUR_DETAILS.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'DUR_DETAILS_TOTAL' = 'DUR_AGGREGATE_TOTAL', 
												'DUR_DETAILS_MGE6LT12' = 'DUR_AGGREGATE_MGE6LT12', 
												'DUR_DETAILS_X' = 'DUR_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_10YRBANDS'), str_detect(classif2, 'DUR_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_DUR_AGGREGATE.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_DUR_DETAILS.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'DUR_DETAILS_TOTAL' = 'DUR_AGGREGATE_TOTAL', 
												'DUR_DETAILS_MGE6LT12' = 'DUR_AGGREGATE_MGE6LT12', 
												'DUR_DETAILS_X' = 'DUR_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_AGGREGATE'), str_detect(classif2, 'DUR_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_DUR_AGGREGATE.csv")))															

			############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_DUR_DETAILS.csv")))															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_YTHADULT'), str_detect(classif2, 'DUR_AGGREGATE')), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_DUR_AGGREGATE.csv")))															


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_34 					<- 	function(){ # UNE, SEX_CAT
ref_file <- 'ILO_UNE_34.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	EXISTPR = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_CAT_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = EXISTPR, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	# delete table empty
	X <- X %>% arrange(desc(classif1)) %>% group_by(ref_area,  time,   sex) %>% mutate(ref = paste0(classif1, collapse = ' / ')) %>% ungroup %>% 
				filter(!ref %in% 'CAT_UNE_TOTAL / CAT_UNE_NRESP') %>% select(-ref)

	############ prepare segment previous and other
	data.table:::fwrite(X %>% filter(!classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OTHER.csv")))				

	data.table:::fwrite(X %>% filter(classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_CAT_UNE_PRE.csv")))				
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev1 			<- 	function(){ # UNE, SEX_ECO prev isic3, aggr, sector ###############################
ref_file <- 'ILO_UNE_35_NacePRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	NA11PR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_ECO_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA11PR1D, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))

										
										
	############ prepare segment previous and other
	data.table:::fwrite(X %>% filter(!classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OTHER.csv")))				

	data.table:::fwrite(X %>% filter(classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_CAT_UNE_PRE.csv")))				
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev2 			<- 	function(){ # UNE, SEX_ECO prev isic3, aggr, sector  ################################
ref_file <- 'ILO_UNE_35_NacePRev2.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	#QUARTER = col_character(),
																	SEX = col_character(),
																	NACEPR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_ECO_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACEPR1D, 
					sex = SEX,  
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))

										
										
	############ prepare segment previous and other
	data.table:::fwrite(X %>% filter(!classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OTHER.csv")))				

	data.table:::fwrite(X %>% filter(classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_CAT_UNE_PRE.csv")))				
	
	rm(X)

	print(ref_file)
	
	
}



REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39 					<- 	function(){ # DIS, SEX_AGE, 10, agg, yth 
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
											'AGE_AGGREGATE_25-54' 	= 'AGE_AGGREGATE_Y25-54', 
											'AGE_YTHADULT_15-64'	= 'AGE_YTHADULT_Y15-64' , 
											'AGE_YTHADULT_YGE25'	= 'AGE_YTHADULT_YGE25', 
											.default = "ERROR", 
											.missing = "ERROR"
					))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS.csv")))															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(sex, obs_value) %>% ilo:::save_ilo()


	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE.csv")))															

		############ prepare indicator file by yth years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15	', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40 					<- 	function(){ # NEET, SEX 
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
													
																								
	data.table:::fwrite(X , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), ".csv")))															

													
	rm(X)

	print(ref_file)
	
	
}
	

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_145_NaceRev1 			<- 	function(){ # HOW, SEX_ECO isic3 agg sector 
ref_file <- 'ILO_HOW_145_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(), 
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA111D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA111D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_146_NaceRev2 			<- 	function(){ # HOW, SEX_ECO isic4 agg sector 
ref_file <- 'ILO_HOW_146_NaceRev2.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(), 
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "ECO_ISIC4.csv")))															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE.csv")))															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL	', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR.csv")))															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_147_NaceRev1 			<- 	function(){ # HOW, SEX_ECO2 isic3 2 digits agg sector 
ref_file <- 'ILO_HOW_147_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(), 
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA113D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA113D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_EQISIC4ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_EQISIC4ISIC3.csv")))															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_148_NaceRev2_NACE2D 	<- 	function(){ # HOW, SEX_ECO2 isic4 2 digits agg sector 
ref_file <- 'ILO_HOW_148_NaceRev2_NACE2D.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(), 
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE2D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG))%>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE2D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_2digit.csv")))															

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_151_ISCO88 			<- 	function(){ # HOW, SEX_OCU2 isic4 2 digits agg sector 
ref_file <- 'ILO_HOW_151_ISCO88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO3D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO3D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add OCU_EQISCO08ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_EQISCO08ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_EQISCO08ISCO88.csv")))															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_152_ISCO08				<- 	function(){ # HOW, SEX_OCU2 isic4 2 digits agg sector 
ref_file <- 'ILO_HOW_152_ISCO08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO2D = col_character(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO2D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
	
	
	################## add OCU_ISCO08
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "OCU_ISCO08_2digit.csv")))															

	rm(X)

	print(ref_file)
	
	
}

















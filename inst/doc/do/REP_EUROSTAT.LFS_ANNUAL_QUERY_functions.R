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
#' ## End(**Not run**)

	
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_141 					<- 	function(timeto = 2005){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE, 5, 10, agg, yth 
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
																), progress = FALSE ) %>% 
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL'						
					))
																
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS_",timeto,".csv")), showProgress = FALSE)															
	
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))

											
		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
			
		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															
	
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(indicator, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
		
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_142 					<- 	function(timeto = 2005){ # WAP, EAP, EMP, UNE, EIP, SEX_AGE_EDU, 10, agg, yth, isced11, isced97, agg 
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
															
																	
																), progress = FALSE ) %>% 
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL' 
											))
	X 	<-  bind_rows(
					X %>% filter(time < 2014) %>% rename(classif2 = HAT97LEV) %>% filter(HAT11LEV %in% 'EDU_ISCED11_NAPPL') %>% select(-HAT11LEV),															
					X %>% filter(time > 2013) %>% rename(classif2 = HAT11LEV) %>% filter(HAT97LEV %in% 'EDU_ISCED97_NAPPL') %>% select(-HAT97LEV)) %>% 
			mutate(classif2 = classif2 %>% recode('EDU_ISCED11_UNK'		= 'EDU_ISCED11_X'))

	
	
	################## add remapping EDU aggregate
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_ISCED11_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_ISCED97_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_X' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_10YRBANDS'), str_detect(classif2, 'EDU_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_EDU_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_ISCED11_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_ISCED97_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_X' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_AGGREGATE'), str_detect(classif2, 'EDU_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_EDU_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

			############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'EDU_ISCED11')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_ISCED11_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'EDU_ISCED97')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_ISCED97_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_X' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_YTHADULT'), str_detect(classif2, 'EDU_AGGREGATE')), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_EDU_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_143 					<- 	function(timeto = 2005){	# WAP, EAP, EMP, UNE, EIP, SEX_AGE_GEO, 5, 10, agg, yth 
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
																	), progress = FALSE ) %>% 
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'
											), 
					classif2 = classif2 %>% recode('GEO_COV_NRESP' 	= 'GEO_COV_X'))
	

	############ prepare indicator file by 10 years bands

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															
	
	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EAP_6 						<- 	function(timeto = 2005){ # LFPR, SEX_AGE, 5, 10, agg, yth 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL'
											))
																

	############ prepare indicator file by 5 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_15 						<- 	function(timeto = 2005){ # EMP, SEX_STE icse93, agg 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'STE_ICSE93')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_STE_ICSE93_",timeto,".csv")), showProgress = FALSE)															
	

	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'STE_ICSE93_1' 	=  'STE_AGGREGATE_EES', 
														'STE_ICSE93_6'  =  'STE_AGGREGATE_X',
														'STE_ICSE93_TOTAL' 	=  'STE_AGGREGATE_TOTAL'
														)) %>% filter(!str_detect(classif1, 'STE_ICSE93'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'STE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_STE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_95_NaceRev1 			<- 	function(timeto = 2005){ # EMP, SEX_ECO isic3 agg sector OK
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>% 	mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL', # ok
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 
														

														
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_96_NaceRev2 			<- 	function(timeto = 2005){ # EMP, SEX_ECO isic4 agg sector IND
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "ECO_ISIC4_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

														
														
														
														
														
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev1 			<- 	function(timeto = 2005){ # EMP, SEX_ECO2 isic3 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
										), 
					classif1 = classif1 %>% recode('ECO_EQISIC4ISIC3_64_65*67' = 'ECO_EQISIC4ISIC3_64_65-67'))
	
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_EQISIC4ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_EQISIC4ISIC3_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev2 			<- 	function(timeto = 2005){ # EMP, SEX_ECO2 isic4 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_2digit_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_100_Isco88 				<- 	function(timeto = 2005){ # EMP, SEX_OCU isco 88 skill 					
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO88_NAPPL' = 'OCU_ISCO88_X') ) %>% 
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	
	################## add OCU_ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															

	
	
	
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
			filter(YEAR < (timeto) + 1) %>%
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
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_L1', 
														'OCU_ISCO88_NAPPL' = 'OCU_SKILL_X') ) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO88')) %>%
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
		

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_100_Isco08 				<- 	function(timeto = 2005){ # EMP, SEX_OCU isco 08 skill 					
ref_file <- 'ILO_EMP_100_Isco08.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															

	
	
	
	X <-	X %>%
		mutate(classif1 = classif1 %>% recode('OCU_ISCO08_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO08_9' =  'OCU_SKILL_L1' )) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO08')) 

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco88 				<- 	function(timeto = 2005){ # EMP, SEX_OCU2 isco88 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
										), 
					classif1 = classif1 %>% recode(
										'OCU_EQISCO08ISCO88_11_11*121'  = 'OCU_EQISCO08ISCO88_11_11-121', 
										'OCU_EQISCO08ISCO88_23_23*33'  = 'OCU_EQISCO08ISCO88_23_23-33' 
										))
	
	
	################## add OCU_isco88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_EQISCO08ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_EQISCO08ISCO88_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco08		 		<- 	function(timeto = 2005){ # EMP, SEX_OCU2 isco08 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO08_2digit_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev1_Isco88 	<- 	function(timeto = 2005){ # EMP, ECO_OCU isic3, agg sectpr & isco 88 skill 	IND				
ref_file <- 'ILO_EMP_109_NaceRev1.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA111D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA111D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					)  %>% 
			filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA, 	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif2 = classif2 %>% recode('OCU_ISCO88_NAPPL' = 'OCU_ISCO88_X') ) %>% 
		group_by(ref_area, time, sex, indicator, classif1, classif2) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	
	################## add OCU_ISCO88 ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_ISCO88'), str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															

	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_ISCO88')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															
	
	
	
	
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NA111D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																) ) %>% 
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NA111D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA,  obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif2 = classif2 %>% recode('OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_L1', 
														'OCU_ISCO88_NAPPL' = 'OCU_SKILL_X') ) %>% 
		filter(!str_detect(classif2, 'OCU_ISCO88')) %>%
		group_by(ref_area, time, sex, indicator, classif1, classif2) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
		

	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_SKILL') , str_detect(classif1, 'ECO_ISIC3')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_SKILL')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

														
														
														
												
		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev2_Isco88 	<- 	function(timeto = 2005){ # EMP, ECO_OCU isic4, agg sectpr & isco 88 skill 					
ref_file <- 'ILO_EMP_109_NaceRev2_ISCO88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA, obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
										
	################## add OCU_ISCO88 ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_ISCO88'), str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															

	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_ISCO88')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR' , 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															
	
	
	
	
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA, obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif2 = classif2 %>% recode('OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_L1') ) %>% 
		filter(!str_detect(classif2, 'OCU_ISCO88')) 
		

	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_SKILL') , str_detect(classif1, 'ECO_ISIC4')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_SKILL')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',  
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

											
																
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev2_Isco08 	<- 	function(timeto = 2005){ # EMP, ECO_OCU isic4, agg sectpr & isco 08 skill 					
ref_file <- 'ILO_EMP_109_NaceRev2_ISCO08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA, 	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										))
										
	################## add OCU_ISCO08 ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_ISCO08'), str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															

	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_ISCO08')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))  

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															
	
	
	
	
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	NACE1D = col_character(),
																	ISCO1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = NACE1D, 
					classif2 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			filter(sex %in% 'SEX_T') %>%
			mutate(	sex = NA, 	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif2 = classif2 %>% recode('OCU_ISCO08_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO08_9' =  'OCU_SKILL_L1') ) %>% 
		filter(!str_detect(classif2, 'OCU_ISCO08')) 
		

	data.table:::fwrite(X %>% filter(	str_detect(classif2, 'OCU_SKILL') , str_detect(classif1, 'ECO_ISIC4')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file 
	X <- X %>%		filter(	str_detect(classif2, 'OCU_SKILL')) %>% 
					mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
															'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

		data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_105 					<- 	function(timeto = 2005){ # UNE, SEX_CAT
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	data.table:::fwrite(X , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_",timeto,".csv")), showProgress = FALSE)				

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12 						<- 	function(timeto = 2005){ # EPR, SEX_AGE, 5, 10, agg, yth 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL'
											))
																
	
	
	########### prepare indicator file by 5 years bands
	# saveRDS(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')	, indicator %in% 'EMP_DWAP_SEX_AGE_RT')	, file = paste0('./input/indicator/indicator/EMP_DWAP_SEX_AGE_RT.AGE_5YRBANDS.rds'))														
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	########### prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														 'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														 )) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_20 						<- 	function(timeto = 2005){ # EMP, ftpt, SEX_AGE_JOB, 10, agg, yth, time
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21 						<- 	function(timeto = 2005){ # EES, SEX_AGE_JOB, 10, agg, yth, contract 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																
	
	
	############ prepare indicator file by 10 years bands

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24 						<- 	function(timeto = 2005){ # TRU, SEX_AGE, 10, agg, yth
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev1 			<- 	function(timeto = 2005){ # TRU, SEX_ECO, agg <2008, yth
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'TRU_TTRU_SEX_ECO_NB') %>% select(-FLAG_BREAK) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by eco sector
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

														

		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
		
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev2 			<- 	function(timeto = 2005){ # TRU, SEX_ECO, agg >2008, yth
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'TRU_TTRU_SEX_ECO_NB') %>% select(-FLAG_BREAK) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by eco sector
	X <- X %>% filter(!classif1 %in% 'ECO_SECTOR_IND') %>% 
				mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

														
											
		
		data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
		
	rm(X)

	print(ref_file)
	
	
}
		
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29 						<- 	function(timeto = 2005){ # UR, SEX_AGE, 5, 10, agg, yth 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_5YRBANDS_YGE15' 	= 'AGE_5YRBANDS_TOTAL' ))
																
	
	
	############ prepare indicator file by 5 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_5YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_5YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_5YRBANDS_TOTAL' 	=  'AGE_10YRBANDS_TOTAL', 
														'AGE_5YRBANDS_YGE65' 	=  'AGE_10YRBANDS_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_5YRBANDS'))
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_31 						<- 	function(timeto = 2005){ # UNE SEX_AGE_DUR agg, yth, dur_details, agg 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL')) %>% 
			filter(!classif2 %in% 'DUR_DETAILS_MLT3') ########## pending
	
	
	################## add remapping EDU aggregate
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_DUR_DETAILS_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'DUR_DETAILS_TOTAL' = 'DUR_AGGREGATE_TOTAL', 
												'DUR_DETAILS_MGE6LT12' = 'DUR_AGGREGATE_MGE6LT12', 
												'DUR_DETAILS_X' = 'DUR_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_10YRBANDS'), str_detect(classif2, 'DUR_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_DUR_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_DUR_DETAILS_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'DUR_DETAILS_TOTAL' = 'DUR_AGGREGATE_TOTAL', 
												'DUR_DETAILS_MGE6LT12' = 'DUR_AGGREGATE_MGE6LT12', 
												'DUR_DETAILS_X' = 'DUR_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_AGGREGATE'), str_detect(classif2, 'DUR_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_DUR_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

			############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT') , str_detect(classif2, 'DUR_DETAILS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_DUR_DETAILS_",timeto,".csv")), showProgress = FALSE)															
	data.table:::fwrite(X %>% 	mutate(classif2 = classif2 %>% recode(
												'EDU_ISCED11_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED97_TOTAL' = 'EDU_AGGREGATE_TOTAL', 
												'EDU_ISCED11_UNK' = 'EDU_AGGREGATE_X', 
												'EDU_ISCED97_UNK' = 'EDU_AGGREGATE_X')) %>%
					filter(	str_detect(classif1, 'AGE_YTHADULT'), str_detect(classif2, 'DUR_AGGREGATE')), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_DUR_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_34 						<- 	function(timeto = 2005){ # UNE, SEX_CAT
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
										), 
					classif1 = classif1 %>% recode('CAT_UNE_NRESP'  = 'CAT_UNE_UNK'))
										
	
	
	# delete table empty
	X <- X %>% arrange(desc(classif1)) %>% group_by(ref_area,  time,   sex) %>% mutate(ref = paste0(classif1, collapse = ' / ')) %>% ungroup %>% 
				filter(!ref %in% 'CAT_UNE_TOTAL / CAT_UNE_UNK') %>% select(-ref)

	############ prepare segment previous and other
	data.table:::fwrite(X %>% filter(!classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OTHER_",timeto,".csv")), showProgress = FALSE)				

	data.table:::fwrite(X %>% filter(classif1 %in% 'CAT_UNE_PRE'), file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_CAT_UNE_PRE_",timeto,".csv")), showProgress = FALSE)				
	
	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev1 			<- 	function(timeto = 2005){ # UNE, SEX_ECO prev isic3, aggr, sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'ECO_ISIC3_TOTAL') %>% 
				left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 

	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
	
	
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV) %>% 
				mutate(indicator = unique(X$indicator)) 
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'ECO_ISIC3_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
						NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW)  %>% 
				mutate(indicator = unique(X$indicator)) 
	
	X <- X %>% 
				filter(!classif1 %in% c('ECO_ISIC3_TOTAL', 'ECO_ISIC3_X')) %>% 
				bind_rows(TOTAL, CAT_X)
	
	
	
	
	

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%   mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 

												
		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev2 			<- 	function(timeto = 2005){ # UNE, SEX_ECO prev isic4, aggr, sector  
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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

										
										

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'ECO_ISIC4_TOTAL') %>% 
				left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 

	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
		
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV)%>% 
				mutate(indicator = unique(X$indicator)) 
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'ECO_ISIC4_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
						NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW)%>% 
				mutate(indicator = unique(X$indicator)) 
					
					
	X <- X %>% 
				filter(!classif1 %in% c('ECO_ISIC4_TOTAL', 'ECO_ISIC4_X')) %>% 
				bind_rows(TOTAL, CAT_X)
	
	
	
################## add ECO_ISIC4
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "ECO_ISIC4_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	X <- X %>%  mutate(classif1 = classif1  %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR', 
														'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														)) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE')) 
											
		
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_36_Isco88 				<- 	function(timeto = 2005){ # UNE, SEX_OCU isco 88 skill 			 		
ref_file <- 'ILO_UNE_36_Isco88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	ISCOPR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_OCU_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCOPR1D, 
					sex = SEX, 
					
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO88_NAPPL' = 'OCU_ISCO88_X') ) %>% 
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	
	
	############ prepare segment previous and other

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'OCU_ISCO88_TOTAL') %>% left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 
	
	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
	
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV) %>% 
				mutate(indicator = unique(X$indicator))
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'OCU_ISCO88_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
					NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW) %>% 
				mutate(indicator = unique(X$indicator)) 
				
	X <- X %>% 
				filter(!classif1 %in% c('OCU_ISCO88_TOTAL', 'OCU_ISCO88_X')) %>% 
				bind_rows(TOTAL, CAT_X)
	
	################## add OCU_ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															

	
	
	
		X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	ISCOPR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_OCU_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCOPR1D, 
					sex = SEX, 
					
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_L1', 
														'OCU_ISCO88_NAPPL' = 'OCU_SKILL_X') ) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO88')) %>%
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	############ prepare segment previous and other

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'OCU_SKILL_TOTAL') %>% 
				left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 

	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
		
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV) %>% 
				mutate(indicator = unique(X$indicator)) 
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'OCU_SKILL_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
						NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW)%>% 
				mutate(indicator = unique(X$indicator)) 
				
	X <- X %>% 
				filter(!classif1 %in% c('OCU_SKILL_TOTAL', 'OCU_SKILL_X')) %>% 
				bind_rows(TOTAL, CAT_X)

		
		
		
		
		

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_36_Isco08 				<- 	function(timeto = 2005){ # UNE, SEX_OCU isco 08 skill 			 		
ref_file <- 'ILO_UNE_36_Isco08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	ISCOPR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_OCU_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCOPR1D, 
					sex = SEX, 
					
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO08_NAPPL' = 'OCU_ISCO08_X') ) %>% 
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	
	test <- X %>% filter(time %in% '2015', ref_area %in% 'BEL', str_detect(classif1, 'ISCO'))
	
	
	############ prepare segment previous and other

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'OCU_ISCO08_TOTAL') %>% 
				left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 

	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
		
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV) %>% 
				mutate(indicator = unique(X$indicator)) 
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'OCU_ISCO08_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
						NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW) %>% 
				mutate(indicator = unique(X$indicator)) 
				
	X <- X %>% 
				filter(!classif1 %in% c('OCU_ISCO08_TOTAL', 'OCU_ISCO08_X')) %>% 
				bind_rows(TOTAL, CAT_X)
	
	################## add OCU_ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															

	
	
	
		X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	SEX = col_character(),
																	ISCOPR1D = col_character(),
																	VALUE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'UNE_TUNE_SEX_OCU_NB') %>% select(-FLAG_BREAK) %>%
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCOPR1D, 
					sex = SEX, 
					
					obs_value = VALUE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										)) %>%  
		mutate(classif1 = classif1 %>% recode('OCU_ISCO08_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO08_9' =  'OCU_SKILL_L1', 
														'OCU_ISCO08_NAPPL' = 'OCU_SKILL_X') ) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO08')) %>%
		group_by(ref_area, time, sex, indicator, classif1) %>% 
		summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = last(obs_status)) %>% 
		ungroup
	
	############ prepare segment previous and other

	REF <- read_csv(paste0('./input/indicator/ILO_UNE_34_CAT_UNE_PRE_',timeto,'.csv'), 	col_types = cols(
																					ref_area = col_character(),
																					time = col_integer(),
																					sex = col_character(),
																					classif1 = col_character(),
																					obs_value = col_double(),
																					obs_status = col_character(),
																					indicator = col_character())) %>% 
			rename(PREV = obs_value) 
	
	
	check <- X %>% 
				filter(classif1 %in% 'OCU_SKILL_TOTAL') %>% 
				left_join(select(REF, -indicator, -classif1, -obs_status), by = c("ref_area", "time", "sex"))  %>% 
				filter(!PREV %in% NA) %>% 
				mutate(reduce = obs_value - PREV, keep = 1) 
	
	X <- X %>% 
				left_join(select(check, ref_area, time, sex, keep), by = c("ref_area", "time", "sex")) %>% 
				filter(keep %in% 1 ) %>% 
				select(-keep)
		
	TOTAL <- check %>% 
				select(ref_area, time, sex, classif1, PREV, obs_status) %>% 
				rename(obs_value = PREV) %>% 
				mutate(indicator = unique(X$indicator)) 
	
	CAT_X <- X %>% 
				filter(classif1 %in% 'OCU_SKILL_X') %>% 
				left_join(select(check, ref_area, time, sex, reduce), by = c("ref_area", "time", "sex")) %>% 
				mutate(	NEW =  obs_value - reduce, 
						NEW = round(NEW, 5)) %>% 
				select(-obs_value, -reduce)  %>% 
				rename(obs_value  = NEW) %>% 
				mutate(indicator = unique(X$indicator)) 
				
	X <- X %>% 
				filter(!classif1 %in% c('OCU_SKILL_TOTAL', 'OCU_SKILL_X')) %>% 
				bind_rows(TOTAL, CAT_X)

		
		
		
		
		

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39 						<- 	function(timeto = 2005){ # DIS, SEX_AGE, 10, agg, yth 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL' ))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(classif1, obs_value) %>% ilo:::save_ilo()
	# test OK	X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) %>% select(-obs_status) %>% spread(sex, obs_value) %>% ilo:::save_ilo()


	############ prepare indicator file by agg years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by yth years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40 						<- 	function(timeto = 2005){ # NEET, SEX 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
													
																								
	data.table:::fwrite(X , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_",timeto,".csv")), showProgress = FALSE)															

													
	rm(X)

	print(ref_file)
	
	
}
	
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_145_NaceRev1 			<- 	function(timeto = 2005){ # HOW, SEX_ECO isic3 agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC3_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC3_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC3_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC3'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file 
	 X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														 'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														 'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														 )) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	 data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_146_NaceRev2 			<- 	function(timeto = 2005){ # HOW, SEX_ECO isic4 agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "ECO_ISIC4_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_ISIC4_TOTAL' 	=  'ECO_AGGREGATE_TOTAL', 
														'ECO_ISIC4_X'  =  'ECO_AGGREGATE_X'
														)) %>% filter(!str_detect(classif1, 'ECO_ISIC4'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_AGGREGATE') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															
	
	############ prepare indicator file by 10 years bands
	 X <- X %>% mutate(classif1 = classif1 %>% recode(	'ECO_AGGREGATE_TOTAL' 	=  'ECO_SECTOR_TOTAL',
														 'ECO_AGGREGATE_AGR'	 	= 'ECO_SECTOR_AGR',
														 'ECO_AGGREGATE_X'  =  'ECO_SECTOR_X'
														 )) %>% filter(!str_detect(classif1, 'ECO_AGGREGATE'))

	 data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_SECTOR') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SECTOR_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_147_NaceRev1 			<- 	function(timeto = 2005){ # HOW, SEX_ECO2 isic3 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
										), 
					classif1 = classif1 %>% recode('ECO_EQISIC4ISIC3_64_65*67' = 'ECO_EQISIC4ISIC3_64_65-67'))
	
	################## add ECO_ISIC3
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_EQISIC4ISIC3') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_EQISIC4ISIC3_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by 10 years bands


	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_148_NaceRev2_NACE2D 	<- 	function(timeto = 2005){ # HOW, SEX_ECO2 isic4 2 digits agg sector 
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'ECO_ISIC4') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC4_2digit_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_149_Isco88 				<- 	function(timeto = 2005){ # HOW, SEX_OCU isco 88 skill 					
ref_file <- 'ILO_HOW_149_Isco88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO1D = col_character(),
																	# pop = col_double(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
						mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										), 
					indicator = indicator %>% recode('HOW_XEES_SEX_OCO_NB' = 'HOW_XEES_SEX_OCU_NB' )) 
	
	################## add OCU_ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO88_",timeto,".csv")), showProgress = FALSE)															

	
	
	X <- X %>%	mutate(classif1 = classif1 %>% recode('OCU_ISCO88_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO88_9' =  'OCU_SKILL_L1') ) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO88'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_150_Isco08 				<- 	function(timeto = 2005){ # HOW, SEX_OCU isco 08 skill 					
ref_file <- 'ILO_HOW_150_Isco08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO1D = col_character(),
																	# pop = col_double(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
			rename(	ref_area 	= 	COUNTRY, 
					time 		=  	YEAR, 
					classif1 = ISCO1D, 
					sex = SEX, 
					indicator = STAPRO, 
					obs_value = AVRGE, 
					obs_status = FLAG
					) %>% 
			mutate(	obs_status = obs_status %>% recode(
											'a' 	 = 'U', 
											'b' 	 = 'U', 
											'c' 	 = 'U' 
										), 
					indicator = indicator %>% recode('HOW_XEES_SEX_OCO_NB' = 'HOW_XEES_SEX_OCU_NB' ))  
	
	################## add OCU_ISCO08
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO08_",timeto,".csv")), showProgress = FALSE)															

	
	
	X <- X %>%	mutate(classif1 = classif1 %>% recode('OCU_ISCO08_TOTAL' 	=  'OCU_SKILL_TOTAL', 
														'OCU_ISCO08_9' =  'OCU_SKILL_L1') ) %>% 
		filter(!str_detect(classif1, 'OCU_ISCO08'))

	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_SKILL') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SKILL_",timeto,".csv")), showProgress = FALSE)															
	

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_151_ISCO88 				<- 	function(timeto = 2005){ # HOW, SEX_OCU2 isic4 2 digits agg sector 
ref_file <- 'ILO_HOW_151_ISCO88.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO3D = col_character(),
																	# pop = col_double(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
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
										), 
					classif1 = classif1 %>% recode(
										'OCU_EQISCO08ISCO88_11_11*121'  = 'OCU_EQISCO08ISCO88_11_11-121', 
										'OCU_EQISCO08ISCO88_23_23*33'  = 'OCU_EQISCO08ISCO88_23_23-33' 
										))
	
	
	################## add OCU_EQISCO08ISCO88
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_EQISCO08ISCO88') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_EQISCO08ISCO88_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_152_ISCO08				<- 	function(timeto = 2005){ # HOW, SEX_OCU2 isic4 2 digits agg sector 
ref_file <- 'ILO_HOW_152_ISCO08.csv'
	
	X 	<- 	read_csv(paste0('./input/', ref_file), col_types = cols_only(
																	COUNTRY = col_character(),
																	YEAR = col_integer(),
																	# QUARTER = col_character(),
																	HWACTUAL = col_character(),
																	SEX = col_character(),
																	STAPRO = col_character(),
																	ISCO2D = col_character(),
																	# pop = col_double(),
																	AVRGE = col_double(),
																	FLAG = col_character(),
																	FLAG_BREAK = col_character()
																	#COUNTRY_ORDER = col_character()
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%																
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG)) %>% filter(HWACTUAL %in% '1hr or more') %>% select(-FLAG_BREAK, -HWACTUAL) %>%   
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
	
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'OCU_ISCO08') ) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "OCU_ISCO08_2digit_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_41 						<- 	function(timeto = 2005){ # OLF, SEX_AGE, 10, agg, yth
ref_file <- 'ILO_EIP_41.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%																
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'EIP_WPLF_SEX_AGE_NB') %>% select(-FLAG_BREAK) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_42 						<- 	function(timeto = 2005){ # LUU_XLU2_SEX_AGE_RT, SEX_AGE, 10, agg, yth
ref_file <- 'ILO_EIP_42.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%																
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'LUU_XLU2_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_43 						<- 	function(timeto = 2005){ # LUU_XLU3_SEX_AGE_RT, SEX_AGE, 10, agg, yth
ref_file <- 'ILO_EIP_43.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'LUU_XLU3_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_44 						<- 	function(timeto = 2005){ # LUU_XLU4_SEX_AGE_RT, SEX_AGE, 10, agg, yth
ref_file <- 'ILO_EIP_44.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
			mutate(FLAG = ifelse(FLAG_BREAK %in% 'b', 'B', FLAG), indicator = 'LUU_XLU4_SEX_AGE_RT') %>% select(-FLAG_BREAK) %>%
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
											'AGE_10YRBANDS_YGE15' 	= 'AGE_10YRBANDS_TOTAL'))
																

	############ prepare indicator file by 10 years bands
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_10YRBANDS')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_10YRBANDS_",timeto,".csv")), showProgress = FALSE)															

	############ prepare indicator file by aggregate years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_10YRBANDS_TOTAL' 	=  'AGE_AGGREGATE_TOTAL', 
														'AGE_10YRBANDS_Y15-24'  =  'AGE_AGGREGATE_Y15-24',
														'AGE_10YRBANDS_Y55-64' 	=  'AGE_AGGREGATE_Y55-64',
														'AGE_10YRBANDS_YGE65'	=  'AGE_AGGREGATE_YGE65'
														)) %>% filter(!str_detect(classif1, 'AGE_10YRBANDS'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_AGGREGATE')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_AGGREGATE_",timeto,".csv")), showProgress = FALSE)															

		############ prepare indicator file by ythadult years bands
	X <- X %>% mutate(classif1 = classif1 %>% recode(	'AGE_AGGREGATE_TOTAL' 	=  'AGE_YTHADULT_YGE15', 
														'AGE_AGGREGATE_Y15-24'  =  'AGE_YTHADULT_Y15-24'
														)) %>% filter(!str_detect(classif1, 'AGE_AGGREGATE'))
			
	data.table:::fwrite(X %>% filter(	str_detect(classif1, 'AGE_YTHADULT')) , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_AGE_YTHADULT_",timeto,".csv")), showProgress = FALSE)															

	rm(X)

	print(ref_file)
	
	
}


REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NEW 			<- 	function(timeto = 2017){ # EMP, SEX_EC2 isic 2 digits agg sector 
ref_file <- 'ILO_EMP_97.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
	ref_file_total <- list.files('./input/indicator/') %>% as_data_frame() %>% filter(str_detect(value, 'ILO_EMP_95_NaceRev1|ILO_EMP_96_NaceRev2'), str_detect(value, 'ECO_SECTOR')) %>% .$value
	
	TOTAL <- as.list(ref_file_total) %>% 
						plyr:::ldply(function(x) {read_csv( paste0('./input/indicator/', x), 
															col_types = cols(
																ref_area 	= col_character(),						
																indicator 	= col_character(),
																sex 		= col_character(),
																classif1 	= col_character(),	
																
																time 		= col_double(),
																obs_value 	= col_double(),	
																obs_status 	= col_character()), 
															progress = FALSE) 
													
												 })%>% 
						as.tbl %>% filter(classif1 %in% 'ECO_SECTOR_TOTAL') %>% 
						mutate(indicator =  gsub("_SEX_ECO_NB", "_SEX_EC2_NB", indicator)) %>% 
						select(-classif1)
		
	test_version_time <- X %>% mutate(classif2 = NA) %>% switch_ilo(version) %>% distinct(classif1_version, time) %>% mutate(classif1 = paste0(classif1_version, '_TOTAL'))	%>% select(-classif1_version)
	TOTAL <- TOTAL %>% left_join(test_version_time, by = 'time') 
	
	TEST_YEAR <- X %>% distinct(ref_area, time, indicator) %>% mutate(keep = 1)
	TOTAL <- TOTAL %>% left_join(TEST_YEAR, by = c("ref_area", "time", "indicator")) %>% filter(keep %in% 1) %>% select(-keep)
	
	X <- bind_rows(X, TOTAL)
	
	data.table:::fwrite(X  , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_ISIC_2digit_",timeto,".csv")), showProgress = FALSE)															


	
	X <- X %>% filter(classif1 %in% c("EC2_ISIC4_A01", "EC2_ISIC4_A03", "EC2_ISIC4_C10", "EC2_ISIC4_C11", "EC2_ISIC4_C13", "EC2_ISIC4_C14", "EC2_ISIC4_C21", "EC2_ISIC4_C31", "EC2_ISIC4_E36", "EC2_ISIC4_F41", "EC2_ISIC4_G46", "EC2_ISIC4_G47", "EC2_ISIC4_H49", "EC2_ISIC4_I55", "EC2_ISIC4_I56", "EC2_ISIC4_J61", "EC2_ISIC4_K64", "EC2_ISIC4_K65", "EC2_ISIC4_N78", "EC2_ISIC4_Q86")) %>% 	
				mutate(classif1 = classif1 %>% mapvalues(from = c("EC2_ISIC4_A01", "EC2_ISIC4_A03", "EC2_ISIC4_C10", "EC2_ISIC4_C11", "EC2_ISIC4_C13", "EC2_ISIC4_C14", "EC2_ISIC4_C21", "EC2_ISIC4_C31", "EC2_ISIC4_E36", "EC2_ISIC4_F41", "EC2_ISIC4_G46", "EC2_ISIC4_G47", "EC2_ISIC4_H49", "EC2_ISIC4_I55", "EC2_ISIC4_I56", "EC2_ISIC4_J61", "EC2_ISIC4_K64", "EC2_ISIC4_K65", "EC2_ISIC4_N78", "EC2_ISIC4_Q86"), 
																	to = c("ECO_ISIC4_A_01", "ECO_ISIC4_A_03", "ECO_ISIC4_C_10", "ECO_ISIC4_C_11", "ECO_ISIC4_C_13", "ECO_ISIC4_C_14", "ECO_ISIC4_C_21", "ECO_ISIC4_C_31", "ECO_ISIC4_E_36", "ECO_ISIC4_F_41", "ECO_ISIC4_G_46", "ECO_ISIC4_G_47", "ECO_ISIC4_H_49", "ECO_ISIC4_I_55", "ECO_ISIC4_I_56", "ECO_ISIC4_J_61", "ECO_ISIC4_K_64", "ECO_ISIC4_K_65", "ECO_ISIC4_N_78", "ECO_ISIC4_Q_86"), warn_missing =  FALSE))  %>% 
				mutate(indicator= gsub("SEX_EC2_NB", "SEX_ECO2_NB", indicator))

		data.table:::fwrite(X  , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_ECO_SELECTED_ISIC_2digit_",timeto,".csv")), showProgress = FALSE)															

	
	
	rm(X, TOTAL, ref_file_total)

	print(ref_file)
	
	
}

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_NEW		 		<- 	function(timeto = 2017){ # EMP, SEX_OC2 isco 2 digits agg sector 
ref_file <- 'ILO_EMP_102.csv'
	
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
																), progress = FALSE ) %>%
			filter(YEAR < (timeto) + 1) %>%
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
	
		
	ref_file_total <- list.files('./input/indicator/') %>% as_data_frame() %>% filter(str_detect(value, 'ILO_EMP_100_Isco08|ILO_EMP_100_Isco88'), str_detect(value, 'OCU_SKILL')) %>% .$value
	
	TOTAL <- as.list(ref_file_total) %>% 
						plyr:::ldply(function(x) {read_csv( paste0('./input/indicator/', x), 
															col_types = cols(
																ref_area 	= col_character(),						
																indicator 	= col_character(),
																sex 		= col_character(),
																classif1 	= col_character(),	
																
																time 		= col_double(),
																obs_value 	= col_double(),	
																obs_status 	= col_character()), 
															progress = FALSE) 
													
												 })%>% 
						as.tbl %>% filter(classif1 %in% 'OCU_SKILL_TOTAL') %>% 
						mutate(indicator =  gsub("_SEX_OCU_NB", "_SEX_OC2_NB", indicator)) %>% 
						select(-classif1)
		
	test_version_time <- X %>% mutate(classif2 = NA) %>% switch_ilo(version) %>% distinct(classif1_version, time) %>% mutate(classif1 = paste0(classif1_version, '_TOTAL'))	%>% select(-classif1_version)
	TOTAL <- TOTAL %>% left_join(test_version_time, by = 'time') 
	
	TEST_YEAR <- X %>% distinct(ref_area, time, indicator) %>% mutate(keep = 1)
	TOTAL <- TOTAL %>% left_join(TEST_YEAR, by = c("ref_area", "time", "indicator")) %>% filter(keep %in% 1) %>% select(-keep)
	
	X <- bind_rows(X, TOTAL)
	

	
	data.table:::fwrite(X   , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_ISCO_2digit_",timeto,".csv")), showProgress = FALSE)															

	X <- X %>% filter(classif1 %in% c("OC2_ISCO08_11", "OC2_ISCO08_12", "OC2_ISCO08_14", "OC2_ISCO08_22", "OC2_ISCO08_23", "OC2_ISCO08_25", "OC2_ISCO08_32", "OC2_ISCO08_33", "OC2_ISCO08_41", "OC2_ISCO08_42", "OC2_ISCO08_51", "OC2_ISCO08_52", "OC2_ISCO08_61", "OC2_ISCO08_63", "OC2_ISCO08_71", "OC2_ISCO08_74", "OC2_ISCO08_81", "OC2_ISCO08_83", "OC2_ISCO08_91", "OC2_ISCO08_92", "OC2_ISCO08_93")) %>% 	
				mutate(classif1 = classif1 %>% mapvalues(from = c("OC2_ISCO08_11", "OC2_ISCO08_12", "OC2_ISCO08_14", "OC2_ISCO08_22", "OC2_ISCO08_23", "OC2_ISCO08_25", "OC2_ISCO08_32", "OC2_ISCO08_33", "OC2_ISCO08_41", "OC2_ISCO08_42", "OC2_ISCO08_51", "OC2_ISCO08_52", "OC2_ISCO08_61", "OC2_ISCO08_63", "OC2_ISCO08_71", "OC2_ISCO08_74", "OC2_ISCO08_81", "OC2_ISCO08_83", "OC2_ISCO08_91", "OC2_ISCO08_92", "OC2_ISCO08_93"), 
																	to = c("OCU_ISCO08_1_11", "OCU_ISCO08_1_12", "OCU_ISCO08_1_14", "OCU_ISCO08_2_22", "OCU_ISCO08_2_23", "OCU_ISCO08_2_25", "OCU_ISCO08_3_32", "OCU_ISCO08_3_33", "OCU_ISCO08_4_41", "OCU_ISCO08_4_42", "OCU_ISCO08_5_51", "OCU_ISCO08_5_52", "OCU_ISCO08_6_61", "OCU_ISCO08_6_63", "OCU_ISCO08_7_71", "OCU_ISCO08_7_74", "OCU_ISCO08_8_81", "OCU_ISCO08_8_83", "OCU_ISCO08_9_91", "OCU_ISCO08_9_92", "OCU_ISCO08_9_93"), warn_missing =  FALSE)) %>% 
				mutate(indicator= gsub("SEX_OC2_NB", "SEX_OCU2_NB", indicator))

		data.table:::fwrite(X  , file = paste0('./input/indicator/', paste0(str_replace(ref_file, '.csv', ''), "_OCU_SELECTED_ISCO_2digit_",timeto,".csv")), showProgress = FALSE)															

	
	rm(X, ref_file_total, TOTAL)

	print(ref_file)
	
	
}




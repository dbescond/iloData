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

require(tidyverse)
require(stringr)
require(ilo)
init_ilo()

setwd(paste0(ilo:::path$data, '/REP_EUROSTAT/LFS_ANNUAL_QUERY/'))
source('./do/REP_EUROSTAT.LFS_ANNUAL_QUERY_functions.r')
inputFile <- list.files('./input/')	%>% as_data_frame %>% filter(str_detect(value, '.csv'))	




REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_141(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_142(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_POP_143(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EAP_6(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_15(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_95_NaceRev1(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_96_NaceRev2(timeto = 2017) # ok
# REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev1(timeto = 2017) # ok
# REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NaceRev2(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_100_Isco88(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_100_Isco08(timeto = 2017) # ok
# REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco88(timeto = 2017) # ok
# REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_Isco08(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev1_Isco88(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev2_Isco88(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_109_NaceRev2_Isco08(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_105(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_12(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_20(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EES_21(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_24(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev1(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_TRU_26_NaceRev2(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_31(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_34(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev1(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_35_NacePRev2(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_36_Isco88(timeto = 2017) #ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_36_Isco08(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_UNE_29(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_39(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_YTH_40(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_145_NaceRev1(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_146_NaceRev2(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_147_NaceRev1(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_148_NaceRev2_NACE2D(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_149_Isco88(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_150_Isco08(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_151_ISCO88(timeto = 2017) # ok
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_HOW_152_ISCO08(timeto = 2017) # ok

REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_41(timeto = 2017)
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_42(timeto = 2017)
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_43(timeto = 2017)
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EIP_44(timeto = 2017)



REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_97_NEW(timeto = 2017)
REP_EUROSTAT.LFS_ANNUAL_QUERY_input_ILO_EMP_102_NEW(timeto = 2017)


ref_cou <- readxl:::read_excel('./help/ReadMe.xlsx', sheet = 'ref_area') %>% select(ref_area) %>% t %>% as.character
ref_file <- list.files('./input/indicator/')
ref_note <- readxl:::read_excel('./help/ReadMe.xlsx', sheet = 'mapping_note') %>% select(indicator, note_indicator, note_source)



require(ilo)
init_ilo(-cl)

NSW <- NULL

for (i in 1:length(ref_cou)){

		ref_source <- ilo$code$cl_survey %>% filter(str_detect(code, 'BA'), str_detect(label_en, 'LFS - EU Labour Force Survey'), str_sub(label_en,1,3 ) %in% ref_cou[i] ) %>% .$code %>% as.character

		
		X <- as.list(ref_file) %>% 
						plyr:::ldply(function(x) {Y <- read_csv( paste0('./input/indicator/', x), 
															col_types = cols(
																ref_area 	= col_character(),						
																indicator 	= col_character(),
																sex 		= col_character(),
																classif1 	= col_character(),	
																classif2 	= col_character(),
																time 		= col_double(),
																obs_value 	= col_double(),	
																obs_status 	= col_character()), 
															progress = FALSE) %>% 
													filter(	ref_area %in% ref_cou[i])
													#print(paste0(unique(Y$indicator), x))
													Y
												 }
									) %>% 
						as.tbl %>% 
						mutate(source = ref_source) %>% 
						select(ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status) %>% 
						left_join(ref_note, by = 'indicator') %>% 
						mutate(collection = 'STI') %>% 
						mutate(obs_value = ifelse(obs_value %in% 0 & obs_status %in% c('U', 'B'), NA, obs_value  )) %>% 
						mutate(note_indicator = ifelse(str_detect(indicator, 'ECO2_') & as.numeric(time) < 2008, paste0(note_indicator, '_I13:4160'), note_indicator)) %>%
						mutate(note_indicator = ifelse(str_detect(indicator, 'OCU2_') & as.numeric(time) < 2011, paste0(note_indicator, '_I13:4160'), note_indicator)) %>% 
						mutate(note_indicator = gsub('NA_', '', note_indicator, fixed = TRUE))
		
		
		
		NSW <-  bind_rows(NSW, X %>% filter(indicator %in% 'EES_TEES_SEX_AGE_JOB_NB', classif1 %in% 'AGE_10YRBANDS_TOTAL', classif2 %in% c('JOB_CONTRACT_TEMP','JOB_CONTRACT_TOTAL'))) %>% mutate(collection = 'NSW')
		
		X <- X %>% 	filter(!indicator %in% 'EES_TEES_SEX_AGE_JOB_NB')  %>% 
				filter(!(ref_area %in% 'CYP' & time %in% '1999')) %>%
				mutate(obs_status = ifelse(!obs_status %in% c(NA, 'U', 'B', 'S'), 'U',obs_status ))
		# Exception ITA unemployment 2 criteria
		
		if(ref_cou[i] %in% 'ITA'){X <- X %>% mutate(note_indicator = 'T5:1429')}
				
				
		save(X,file = paste0(getwd(), '/output/QUERY_EUROSTAT_',ref_cou[i],'.Rdata'))				
		rm(X, ref_source)		
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))

		
		
		# add repo note
		
##### TEST


# X %>% 	filter(		time %in% '1998') %>% 
		# mutate(time = '1940', collection = 'YI') %>% 
		# switch_ilo(version) %>% 
		# Ariane:::MicroTestClassifVersionNEW( collection = 'YI') %>%
		# select(-contains('version')) %>% 
		# save_ilo(format = 'csv')
		
		
		
}

REF <- cbind(PATH = paste0(getwd(), "/output/QUERY_EUROSTAT_",ref_cou,".Rdata"),ID = NA, Types  ="EUROSTAT_ilostat", REF = ref_cou)
write.csv(REF,paste0("./FileToLoad.csv"),row.names = FALSE,na="")


NSW %>% arrange(classif2) %>% 
		select(-classif1, classif2) %>% 
		group_by(collection, ref_area, source, sex, time) %>% 
		summarise(	obs_value = first(obs_value) / last(obs_value) * 100, 
					obs_status = first(obs_status), 
					note_indicator = first(note_indicator), 
					note_source = first(note_source)
					) %>% 
		ungroup %>% 
		mutate(indicator = 'EES_XTMP_SEX_RT') %>%
		data.table:::fwrite(. , file = './output/_NSW_QUERY.csv', showProgress = FALSE)			










}
		




rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)



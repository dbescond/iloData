
add_aggregate <- function(X, use_SDG = FALSE){

ilo:::init_ilo(-cl)

REF_AGG <- ilo$code$cl_country_by_group %>% 					# prepare mapping for country group 'region'
					filter(	substr(code,1,7) %in% c('ILO_GEO', 'ECO_G20', 'ECO_EU2', 'ECO_BRI', 'ECO_ASE', 'ECO_WMB')) %>% 
					select(region = code, country = code_country) %>% 
					mutate(	region = gsub('ECO_EU28', 'ECO_EU28_X82', region, fixed = TRUE), 
							region = gsub('ECO_G20', 'ECO_G20_X83', region, fixed = TRUE), 
							region = gsub('ECO_ASEAN', 'ECO_ASEAN_X84', region, fixed = TRUE),
							region = gsub('ECO_WMBRICS', 'ECO_WMBRICS_X87', region, fixed = TRUE),
							region = gsub('ECO_BRICS', 'ECO_BRICS_X85', region, fixed = TRUE), 
							region = str_sub(region, -3,-1)
							) %>% 
					arrange(region, country)
			
if(use_SDG) REF_AGG <- readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds'))
	
AGG <- NULL														# set result dataset to empty
test <- unique(REF_AGG$region) %>% sort							# prepare loop on region
for (i in 1:length(test)){
ref <- REF_AGG %>% 
		filter(region %in% test[i]) %>% 
		select(country) %>% t %>% as.character					# store relevant country list
		

	AGG <-  X %>% 
			filter(country %in% ref) %>% 							# filter country i
			group_by( indicator, time, sex, classif1, classif2) %>%	# group 	
			summarise(	country = test[i], 							# rename country = region
						value = sum(value, na.rm = TRUE)) %>% 		# sum
			ungroup %>% bind_rows(AGG) 								# ungroup
	
		
		
rm(ref)
}	
rm(test)

AGG


}


clean_csv <- function(X){


X %>% mutate(value = round(value, round_val), time = as.integer(time)) %>% 
rename(ref_area = country, obs_value = value) 

}

###############################################################
######## prepare file_lfep for processing:  reshape, map, create nb & ratio, 
##################################################################

reshape_lfep <- function(){
 LFEP <- read_dta(file_lfep)  %>% 	
			filter( !age_group %in% c('Total (0+)', 'Total (15+)')) %>% 
			select(iso3code,  year, age_group,   POP_MF,    POP_M,    POP_F, LF_MF,  LF_M,  LF_F, real_M, real_F) %>%
			group_by(iso3code, year, age_group) %>% 
			summarise(	POP_MF =  sum(POP_MF), 
						POP_M =  sum(POP_M), 
						POP_F =  sum(POP_F), 
						LF_MF =  sum(LF_MF),
						LF_M =  sum(LF_M),
						LF_F =  sum(LF_F),
						real_M = first(real_M),		# take real from France 
						real_F = first(real_F)
					) %>% 
			ungroup %>% 
			filter(year >= min(time_lfep), year <= max(time_lfep))

LFEP <- LFEP %>% 
			bind_rows(
				LFEP %>% 
					group_by(iso3code, year) %>% 							# create gage group 0+ only for population
					summarise(	
						POP_MF =  sum(POP_MF, na.rm = TRUE), 
						POP_M =  sum(POP_M, na.rm = TRUE), 
						POP_F =  sum(POP_F, na.rm = TRUE), 
						age_group = '0+'
					) %>% ungroup , 
				LFEP %>% 
					filter(!age_group %in% c("0-4", "5-9", "10-14")) %>%	# create 15+ for all
					group_by(iso3code, year) %>% 
					summarise(	
						POP_MF =  sum(POP_MF, na.rm = TRUE), 
						POP_M =  sum(POP_M, na.rm = TRUE), 
						POP_F =  sum(POP_F, na.rm = TRUE),
						LF_MF =  sum(LF_MF),
						LF_M =  sum(LF_M),
						LF_F =  sum(LF_F),
						real_M = NaN, 
						real_F = NaN,
						age_group = '15+'
					) %>% ungroup 
				) %>% 
			mutate(		PR_MF = LF_MF / POP_MF * 100, 
						PR_M = LF_M / POP_M * 100, 
						PR_F = LF_F / POP_F * 100) %>% 
			mutate(	PR_M = ifelse(real_M %in% 1, paste0(PR_M, ' R'), as.character(PR_M)), 
					PR_F = ifelse(real_F %in% 1, paste0(PR_F, ' R'), as.character(PR_F))) %>% 
			select(-real_M, -real_F) %>% 
			rename(country = iso3code, time = year, classif1 = age_group) %>% 
			mutate_all(as.character) %>%
			gather(ref, val, -country, -time, -classif1, na.rm = TRUE) %>%
			separate(val, c('value','note_value'), sep = ' ', remove = TRUE, convert = TRUE, extra = "warn", fill = "right") %>% 
			separate(ref, c('indicator','sex'), sep = '_', remove = TRUE) %>% 
			arrange(time, country, indicator, sex, classif1) %>%
			mutate(classif2 = as.character(NA)) %>%
			mutate(time = as.numeric(time)) %>%  
		filter(!value %in% NaN)	%>%	
		mutate(	classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('0-4', '0+', '10-14', '15-19', '15+', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '5-9', '50-54', '55-59', '60-64', '65+'), 
								to = c('AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_0+', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_Y15-19', 'AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'),
									warn_missing = FALSE), 
				sex = sex %>% plyr:::mapvalues(
								from = c('F', 'M', 'MF'), 
								to = c('SEX_F', 'SEX_M', 'SEX_T'), 
									warn_missing = FALSE), 
				indicator = indicator %>% plyr:::mapvalues(
								from = c('LF', 'POP', 'PR'), 
								to = c('EAP_2EAP_SEX_AGE_NB', 'POP_2POP_SEX_AGE_NB', 'EAP_2WAP_SEX_AGE_RT'), 
									warn_missing = FALSE)
				) %>% filter(time >= min(time_lfep), time <= max(time_lfep))
				
save(LFEP, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/LFEP.Rdata'))


rm(LFEP)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				

}

##################################################################
######## prepare pop_2wap_sex_age_nb 
##################################################################

POP_2POP_SEX_AGE_NB <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/LFEP.Rdata'))

POP_2POP_SEX_AGE_NB_ythadu <- LFEP %>%
			filter(indicator %in% 'POP_2POP_SEX_AGE_NB', !classif1 %in% c('AGE_5YRBANDS_0+', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14')) %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_TOTAL','AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to =  c('AGE_YTHADULT_YGE15', "AGE_YTHADULT_Y15-24", "AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25",     "AGE_YTHADULT_YGE25",   "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25" , "AGE_YTHADULT_YGE25"  , "AGE_YTHADULT_YGE25"), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 		

POP_2POP_SEX_AGE_NB_ythadu1564 <- LFEP %>%
			filter(indicator %in% 'POP_2POP_SEX_AGE_NB', !classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_0+', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_YGE65')) %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to =  c( "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64",   "AGE_YTHADULT_Y15-64",   "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64" , "AGE_YTHADULT_Y15-64"  , "AGE_YTHADULT_YGE25"), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup
POP_2POP_SEX_AGE_NB_ythadu <- bind_rows(POP_2POP_SEX_AGE_NB_ythadu, POP_2POP_SEX_AGE_NB_ythadu1564)
rm(POP_2POP_SEX_AGE_NB_ythadu1564)
			
POP_2POP_SEX_AGE_NB_5 <- LFEP %>%
			filter(indicator %in% 'POP_2POP_SEX_AGE_NB', !classif1 %in% c('AGE_5YRBANDS_TOTAL')) %>% 
			mutate(classif1 = ifelse(classif1 %in% 'AGE_5YRBANDS_0+', 'AGE_5YRBANDS_TOTAL', classif1))

POP_2POP_SEX_AGE_NB_10 <- POP_2POP_SEX_AGE_NB_5 %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_TOTAL','AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to = c('AGE_10YRBANDS_TOTAL','AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24',  'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'),
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 		
					
									
POP_2POP_SEX_AGE_NB_agg <- POP_2POP_SEX_AGE_NB_10 %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_10YRBANDS_TOTAL',  'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'), 
								to =  c('AGE_AGGREGATE_TOTAL',  'AGE_AGGREGATE_YLT15', 'AGE_AGGREGATE_Y15-24', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y55-64', 'AGE_AGGREGATE_YGE65'), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 		


					
############### calculate regional aggregate

POP <- bind_rows(POP_2POP_SEX_AGE_NB_5, POP_2POP_SEX_AGE_NB_10, POP_2POP_SEX_AGE_NB_agg, POP_2POP_SEX_AGE_NB_ythadu); rm(POP_2POP_SEX_AGE_NB_5, POP_2POP_SEX_AGE_NB_10, POP_2POP_SEX_AGE_NB_agg, POP_2POP_SEX_AGE_NB_ythadu)
		

POP <- POP %>% bind_rows(add_aggregate(POP))  %>%  
		arrange(country, time, sex, classif1)	 # resort



############### add ilo source XA:...


POP <- POP %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country')  %>% filter(time >= min(time_lfep), time <= max(time_lfep))

								
if(save_file) {								
POP %>%  filter(substr(classif1,1,8) %in% 'AGE_5YRB') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% mutate(note_source = 'R1:2474') %>% 
		# filter( country %in% c('X86','X87', 'X45')) %>%
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '1a_POP_2POP_SEX_AGE_NB_5YRB.csv'), na = "")
								
POP %>%  filter(substr(classif1,1,8) %in% 'AGE_10YR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% mutate(note_source = 'R1:2474') %>%
# filter( country %in% c('X86','X87', 'X45')) %>%
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '1b_POP_2POP_SEX_AGE_NB_10YR.csv'), na = "")
			
POP %>%  filter(substr(classif1,1,8) %in% 'AGE_AGGR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time,  value) %>% mutate(note_source = 'R1:2474') %>%
# filter( country %in% c('X86','X87', 'X45')) %>%
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '1c_POP_2POP_SEX_AGE_NB_AGGR.csv'), na = "")
			
POP %>%  filter(substr(classif1,1,8) %in% 'AGE_YTHA') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% mutate(note_source = 'R1:2474') %>%
# filter( country %in% c('X86','X87', 'X45')) %>%
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '1d_POP_2POP_SEX_AGE_NB_YTHA.csv'), na = "")

 }
 
save(POP, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/POP.Rdata'))
 
WAP <- POP %>% filter( !classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_10YRBANDS_TOTAL', 'AGE_10YRBANDS_YLT15', 'AGE_AGGREGATE_TOTAL', 'AGE_AGGREGATE_YLT15'))
addTotal <-  WAP %>% filter(classif1 %in% 'AGE_YTHADULT_YGE15')
WAP <- WAP %>% bind_rows(addTotal %>% mutate(classif1 = 'AGE_5YRBANDS_TOTAL'), addTotal %>% mutate(classif1 = 'AGE_10YRBANDS_TOTAL'), addTotal %>% mutate(classif1 = 'AGE_AGGREGATE_TOTAL'))
rm(addTotal)

save(WAP, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/WAP.Rdata'))
rm(WAP, POP)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				

 
}

##################################################################
######## prepare eap_2eap_sex_age_nb
##################################################################

EAP_2EAP_SEX_AGE_NB <- function(){ 

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/LFEP.Rdata'))

EAP_2EAP_SEX_AGE_NB_5 <- LFEP  %>%	
					filter(	indicator %in% 'EAP_2EAP_SEX_AGE_NB', 
							!classif1 %in% c('AGE_5YRBANDS_0+', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14'))

	
EAP_2EAP_SEX_AGE_NB_10 <- EAP_2EAP_SEX_AGE_NB_5 %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_TOTAL','AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to = c('AGE_10YRBANDS_TOTAL','AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24',  'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'),
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 		
					
									
EAP_2EAP_SEX_AGE_NB_agg <- EAP_2EAP_SEX_AGE_NB_10 %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_10YRBANDS_TOTAL',  'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'), 
								to =  c('AGE_AGGREGATE_TOTAL',  'AGE_AGGREGATE_YLT15', 'AGE_AGGREGATE_Y15-24', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y55-64', 'AGE_AGGREGATE_YGE65'), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 		

EAP_2EAP_SEX_AGE_NB_ythadu <- EAP_2EAP_SEX_AGE_NB_agg %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_AGGREGATE_TOTAL', "AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_YGE65", 'AGE_AGGREGATE_YLT15'), 
								to =  c('AGE_YTHADULT_YGE15', "AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", NA), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 	%>% filter(!classif1 %in% NA)		
				
EAP_2EAP_SEX_AGE_NB_ythadu1564 <- EAP_2EAP_SEX_AGE_NB_agg %>% 
			filter(classif1 %in% c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64")) %>%
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c( "AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64"), 
								to =  c( "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64"), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1) %>%
			summarise(value = sum(value))	%>% ungroup 	%>% filter(!classif1 %in% NA)		
EAP_2EAP_SEX_AGE_NB_ythadu <- bind_rows(EAP_2EAP_SEX_AGE_NB_ythadu, EAP_2EAP_SEX_AGE_NB_ythadu1564)			
rm(EAP_2EAP_SEX_AGE_NB_ythadu1564)	
				
				
EAP <- bind_rows(EAP_2EAP_SEX_AGE_NB_5, EAP_2EAP_SEX_AGE_NB_10, EAP_2EAP_SEX_AGE_NB_agg, EAP_2EAP_SEX_AGE_NB_ythadu); rm(EAP_2EAP_SEX_AGE_NB_5, EAP_2EAP_SEX_AGE_NB_10, EAP_2EAP_SEX_AGE_NB_agg, EAP_2EAP_SEX_AGE_NB_ythadu)
		

EAP <- EAP %>% bind_rows(add_aggregate(EAP))  %>%  
		arrange(country, time, sex, classif1)	 # resort



############### add ilo source XA:...


EAP <- EAP %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') 

								
if(save_file) {								
	EAP %>%  filter(substr(classif1,1,8) %in% 'AGE_5YRB') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% 
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '4a_EAP_2EAP_SEX_AGE_NB_5YRB.csv'), na = "")
								
	EAP %>%  filter(substr(classif1,1,8) %in% 'AGE_10YR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% 
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '4b_EAP_2EAP_SEX_AGE_NB_10YR.csv'), na = "")
			
	EAP %>%  filter(substr(classif1,1,8) %in% 'AGE_AGGR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% 
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '4c_EAP_2EAP_SEX_AGE_NB__AGGR.csv'), na = "")
			
	EAP %>%  filter(substr(classif1,1,8) %in% 'AGE_YTHA') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value) %>% 
		clean_csv %>% data.table:::fwrite(paste0(dir_file, '4d_EAP_2EAP_SEX_AGE_NB_YTHA.csv'), na = "")

	}
	
save(EAP, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EAP.Rdata'))
rm(EAP) 
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 
	
}

##################################################################
######## prepare EAP_2WAP_SEX_AGE_RT, EIP_2EIP_SEX_AGE_NB, EIP_2WAP_SEX_AGE_RT 
##################################################################

all_sex_age <- function(){ 

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EAP.Rdata'))
load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/WAP.Rdata'))
load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/LFEP.Rdata'))


ALL_SEX_AGE <- EAP %>% 
		rename(EAP = value) %>% select(-indicator) %>% 
			left_join(WAP %>% select(country, sex, classif1, classif2, time, WAP = value), by = c("country", "time", "classif1", "sex", "classif2")) %>% 
			mutate(EIP = WAP - EAP, 
				   LFPR = EAP / WAP * 100, 
				   INAR = EIP / WAP * 100) %>% 
			select(-note_value) 
			
		
if(save_file){								

ALL_SEX_AGE %>% mutate(indicator = 'EAP_2WAP_SEX_AGE_RT') %>% rename(value = LFPR) %>% select(-EAP, -WAP, -EIP, -INAR) %>% 
				left_join(LFEP %>% filter(indicator %in% 'EAP_2WAP_SEX_AGE_RT'), by = c("country", "time", "classif1", "sex", "classif2", "value", "indicator") ) %>%
			filter(substr(classif1,1,8) %in% 'AGE_5YRB') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '5a_EAP_2WAP_SEX_AGE_RT_5YRB.csv'), na = "")
								
ALL_SEX_AGE %>% mutate(indicator = 'EAP_2WAP_SEX_AGE_RT') %>% rename(value = LFPR) %>% select(-EAP, -WAP, -EIP, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_10YR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '5b_EAP_2WAP_SEX_AGE_RT_10YR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EAP_2WAP_SEX_AGE_RT') %>% rename(value = LFPR) %>% select(-EAP, -WAP, -EIP, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_AGGR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '5c_EAP_2WAP_SEX_AGE_RT_AGGR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EAP_2WAP_SEX_AGE_RT') %>% rename(value = LFPR) %>% select(-EAP, -WAP, -EIP, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_YTHA') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '5d_EAP_2WAP_SEX_AGE_RT_YTHA.csv'), na = "")
###################

ALL_SEX_AGE %>% mutate(indicator = 'EIP_2EIP_SEX_AGE_NB') %>% rename(value = EIP) %>% select(-EAP, -WAP, -LFPR, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_5YRB') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '18a_EIP_2EIP_SEX_AGE_NB_5YRB.csv'), na = "")
								
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2EIP_SEX_AGE_NB') %>% rename(value = EIP) %>% select(-EAP, -WAP, -LFPR, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_10YR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '18b_EIP_2EIP_SEX_AGE_NB_10YR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2EIP_SEX_AGE_NB') %>% rename(value = EIP) %>% select(-EAP, -WAP, -LFPR, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_AGGR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '18c_EIP_2EIP_SEX_AGE_NB_AGGR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2EIP_SEX_AGE_NB') %>% rename(value = EIP) %>% select(-EAP, -WAP, -LFPR, -INAR) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_YTHA') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '18d_EIP_2EIP_SEX_AGE_NB_YTHA.csv'), na = "")		

###################
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2WAP_SEX_AGE_RT') %>% rename(value = INAR) %>% select(-EAP, -WAP, -LFPR, -EIP) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_5YRB') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '19a_EIP_2WAP_SEX_AGE_RT_5YRB.csv'), na = "")
								
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2WAP_SEX_AGE_RT') %>% rename(value = INAR) %>% select(-EAP, -WAP, -LFPR, -EIP) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_10YR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '19b_EIP_2WAP_SEX_AGE_RT_10YR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2WAP_SEX_AGE_RT') %>% rename(value = INAR) %>% select(-EAP, -WAP, -LFPR, -EIP) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_AGGR') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '19c_EIP_2WAP_SEX_AGE_RT_AGGR.csv'), na = "")
			
ALL_SEX_AGE %>% mutate(indicator = 'EIP_2WAP_SEX_AGE_RT') %>% rename(value = INAR) %>% select(-EAP, -WAP, -LFPR, -EIP) %>% mutate(note_value = as.character(NA)) %>%
			filter(substr(classif1,1,8) %in% 'AGE_YTHA') %>% select(collection, country, source, indicator, sex, classif1, -classif2, time, value, obs_status = note_value) %>% 
			clean_csv %>% data.table:::fwrite(paste0(dir_file, '19d_EIP_2WAP_SEX_AGE_RT_YTHA.csv'), na = "")	
		
}

save(ALL_SEX_AGE, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/ALL_SEX_AGE.Rdata'))

rm(EAP, LFEP)	
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 		
}			
	
##################################################################
######## prepare POP_2POP_SEX_AGE_GEO_NB 
##################################################################

POP_2POP_SEX_AGE_GEO_NB <- function(){  

POP_2POP_SEX_AGE_GEO_NB <- readxl:::read_excel(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/URPAS_2014_ALL.xlsx')) %>% filter(LocationType %in% 4)  %>% 
			left_join(	select(ilo$code$cl_country, country = code, LocationID = code_iso3n) %>% mutate(LocationID = as.numeric(as.character(LocationID))), by = 'LocationID')	%>% 
						select(-RowID, -LocationName, -LocationType, -IsSmallCountry, -ParentID, -SortOrder)

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/ALL_SEX_AGE.Rdata'))

						
test <- ALL_SEX_AGE %>% distinct(country) %>% mutate(ref = 1)	

save(test, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/test_country.Rdata'))

MASTER <- ALL_SEX_AGE %>% filter(substr(classif1,1,8) %in% 'AGE_YTHA') ; rm(ALL_SEX_AGE)

save(MASTER, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/MASTER.Rdata')); rm(MASTER)

POP_2POP_SEX_AGE_GEO_NB <- POP_2POP_SEX_AGE_GEO_NB %>% 
		left_join(test,  by = "country") %>% 
		filter(ref %in% 1) %>%  
		select(-LocationID,-ref) %>% 
		select(country, Year:`80+`) %>% 
		rename(time = Year, sex = Sex, classif2 = AreaType) %>% 
		gather(classif1, value, -country, -time, -sex, -classif2, na.rm = TRUE) %>% mutate(value = round(as.numeric(value), 4)) %>%
		filter(time >= min(time_lfep), time <= max(time_lfep))

POP_2POP_SEX_AGE_GEO_NB <- 
		bind_rows(	POP_2POP_SEX_AGE_GEO_NB %>% 	
							group_by(country, classif1, classif2, time) %>% 
							summarise(value = sum(value, na.rm = TRUE)) %>% 
							ungroup %>% 
							mutate(sex = 'Total'), 
					POP_2POP_SEX_AGE_GEO_NB) %>% 
		mutate(time = as.character(time)) %>% 	
		mutate(	classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('00-04', 'Total', '10-14', '15-19',  '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '05-09', '50-54', '55-59', '60-64', "65-69", "70-74", "75-79", "80+"), 
								to = c('AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_Y15-19', 'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65', 'AGE_5YRBANDS_YGE65', 'AGE_5YRBANDS_YGE65', 'AGE_5YRBANDS_YGE65'),
									warn_missing = FALSE), 
				sex = sex %>% plyr:::mapvalues(
								from = c('Female', 'Male', 'Total'), 
								to = c('SEX_F', 'SEX_M', 'SEX_T'), 
								warn_missing = FALSE), 
				classif2 = classif2 %>% plyr:::mapvalues(
								from = c('Rural', 'Urban', 'Total'), 
								to = c('GEO_COV_RUR', 'GEO_COV_URB', 'GEO_COV_NAT'), 
									warn_missing = FALSE)) %>% 	
			mutate(indicator = 'POP_2POP_SEX_AGE_GEO_NB') %>% 
			group_by(country, time, indicator, sex, classif1, classif2) %>%
			summarise(value = sum(value))	%>% ungroup 
		
		
POP_2POP_SEX_AGE_GEO_NB_ythadu <- POP_2POP_SEX_AGE_GEO_NB %>% 
			filter(indicator %in% 'POP_2POP_SEX_AGE_GEO_NB', !classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14')) %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to =  c( "AGE_YTHADULT_Y15-24", "AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25",     "AGE_YTHADULT_YGE25",   "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25" , "AGE_YTHADULT_YGE25"  , "AGE_YTHADULT_YGE25"), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1, classif2) %>%
			summarise(value = sum(value))	%>% ungroup 		
		
POP_2POP_SEX_AGE_GEO_NB_ythadu <- bind_rows(POP_2POP_SEX_AGE_GEO_NB_ythadu %>% 
												group_by(country, time, indicator, sex, classif2) %>% 
												summarise(value = sum(value))	%>% ungroup %>% 
												mutate(classif1 = 'AGE_YTHADULT_YGE15'),
											POP_2POP_SEX_AGE_GEO_NB_ythadu)	
											
POP_2POP_SEX_AGE_GEO_NB_ythadu1564 <- POP_2POP_SEX_AGE_GEO_NB %>% 
			filter(indicator %in% 'POP_2POP_SEX_AGE_GEO_NB', !classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_YGE65')) %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64'), 
								to =  c( "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64",  "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64", "AGE_YTHADULT_Y15-64" , "AGE_YTHADULT_Y15-64"), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1, classif2) %>%
			summarise(value = sum(value))	%>% ungroup 		
POP_2POP_SEX_AGE_GEO_NB_ythadu <- bind_rows(POP_2POP_SEX_AGE_GEO_NB_ythadu,POP_2POP_SEX_AGE_GEO_NB_ythadu1564 )		
rm(POP_2POP_SEX_AGE_GEO_NB_ythadu1564, test)		
	


POP_2POP_SEX_AGE_GEO_NB_10 <- POP_2POP_SEX_AGE_GEO_NB %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_5YRBANDS_TOTAL','AGE_5YRBANDS_Y00-04', 'AGE_5YRBANDS_Y05-09', 'AGE_5YRBANDS_Y10-14', 'AGE_5YRBANDS_Y15-19',  'AGE_5YRBANDS_Y20-24', 'AGE_5YRBANDS_Y25-29', 'AGE_5YRBANDS_Y30-34', 'AGE_5YRBANDS_Y35-39', 'AGE_5YRBANDS_Y40-44', 'AGE_5YRBANDS_Y45-49', 'AGE_5YRBANDS_Y50-54', 'AGE_5YRBANDS_Y55-59', 'AGE_5YRBANDS_Y60-64', 'AGE_5YRBANDS_YGE65'), 
								to = c('AGE_10YRBANDS_TOTAL','AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24',  'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'),
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1, classif2) %>%
			summarise(value = sum(value))	%>% ungroup 		
					
									
POP_2POP_SEX_AGE_GEO_NB_agg <- POP_2POP_SEX_AGE_GEO_NB_10 %>% 
			mutate(classif1 = classif1 %>% 
							plyr:::mapvalues(
								from = c('AGE_10YRBANDS_TOTAL',  'AGE_10YRBANDS_YLT15', 'AGE_10YRBANDS_Y15-24', 'AGE_10YRBANDS_Y25-34', 'AGE_10YRBANDS_Y35-44', 'AGE_10YRBANDS_Y45-54', 'AGE_10YRBANDS_Y55-64', 'AGE_10YRBANDS_YGE65'), 
								to =  c('AGE_AGGREGATE_TOTAL',  'AGE_AGGREGATE_YLT15', 'AGE_AGGREGATE_Y15-24', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y25-54', 'AGE_AGGREGATE_Y55-64', 'AGE_AGGREGATE_YGE65'), 
									warn_missing = FALSE)) %>% 
			group_by(country, time, indicator, sex, classif1, classif2) %>%
			summarise(value = sum(value))	%>% ungroup 		



POP_2POP_SEX_AGE_GEO_NB <- bind_rows(POP_2POP_SEX_AGE_GEO_NB, POP_2POP_SEX_AGE_GEO_NB_10, POP_2POP_SEX_AGE_GEO_NB_agg, POP_2POP_SEX_AGE_GEO_NB_ythadu)
rm(POP_2POP_SEX_AGE_GEO_NB_10, POP_2POP_SEX_AGE_GEO_NB_agg, POP_2POP_SEX_AGE_GEO_NB_ythadu)
		

POP_2POP_SEX_AGE_GEO_NB <- POP_2POP_SEX_AGE_GEO_NB %>% bind_rows(add_aggregate(POP_2POP_SEX_AGE_GEO_NB))  %>%  
		arrange(country, time, sex, classif1, classif2)	 # resort


DT_POP_SEX_AGE_GEO <- POP_2POP_SEX_AGE_GEO_NB %>% 
						group_by(country, time, sex, classif1) %>% mutate(value = value / max(value) * 100) %>% 
						ungroup %>% 
						mutate(classif2 = paste0(classif2, '_DT')) %>%
						spread(classif2, value) %>% 
						mutate(time = as.numeric(time))
		
		
load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/POP.Rdata'))
		
POP_2POP_SEX_AGE_GEO_NB <- POP %>% filter(time %in% unique(DT_POP_SEX_AGE_GEO$time)) %>% select(-indicator) %>% left_join(DT_POP_SEX_AGE_GEO,  by = c("country", "time", "classif1", "sex"))		

rm(POP, DT_POP_SEX_AGE_GEO)
		
POP_2POP_SEX_AGE_GEO_NB <- POP_2POP_SEX_AGE_GEO_NB %>% 
					mutate(	GEO_COV_NAT = value, 
							GEO_COV_URB = value / 100 * GEO_COV_URB_DT, 
							GEO_COV_RUR = value / 100 * GEO_COV_RUR_DT) %>% 
					select(-GEO_COV_NAT_DT, -GEO_COV_URB_DT, -GEO_COV_RUR_DT, -value) %>%
					gather(classif2, value, -indicator, -country, -time, -sex, -classif1, -note_value , -collection , -source, na.rm = TRUE) 

					
					
if(save_file){								

POP_2POP_SEX_AGE_GEO_NB	%>%  select(-note_value) %>% 
			# filter( country %in% c('X86','X87', 'X45')) %>%
			mutate(value = as.numeric(value)) %>% 
			mutate(note_source = 'R1:2474') %>% 
			clean_csv %>% 
			data.table:::fwrite(paste0(dir_file, '2_POP_2POP_SEX_AGE_GEO_NB.csv'), na = "")	 


}									
					
rm(POP_2POP_SEX_AGE_GEO_NB)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 
					
}									
									
##################################################################
######## prepare POP_2POP_GEO_NB 
##################################################################

POP_2POP_GEO_NB <- function(){ 

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/test_country.Rdata'))

POP_2POP_GEO_NB <- bind_rows( 
			readxl:::read_excel(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/WUP2014-F18-Total_Population_Annual.xls'), sheet = 'ANNUAL DATA', skip = 16) %>% mutate(classif1 = 'GEO_COV_NAT'),
			readxl:::read_excel(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/WUP2014-F19-Urban_Population_Annual.xls'), sheet = 'ANNUAL DATA', skip = 16)	 %>% mutate(classif1 = 'GEO_COV_URB'),
			readxl:::read_excel(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/WUP2014-F20-Rural_Population_Annual.xls'), sheet = 'ANNUAL DATA', skip = 16)	 %>% mutate(classif1 = 'GEO_COV_RUR')
		
		)	%>% rename(country = `Country Code`) %>%
		
		select(country, classif1, `1990`:`2030`) %>% 
		gather(time, value, -country, -classif1,na.rm = TRUE) %>% 
		mutate(country = as.character(country)) %>% 
		left_join(select(ilo$code$cl_country, country = code_iso3n, code)%>% filter(!country %in% NA) %>% mutate_all(as.character) ,  by = "country") %>% 
		mutate(country = code) %>% 
		select(-code) %>% 
		left_join(test,  by = "country") %>% 
		filter(ref %in% 1) %>% select(-ref)  %>% 
		mutate(classif2 = NA, sex = NA, indicator = 'POP_2POP_GEO_NB') 
rm(test)
		
POP_2POP_GEO_NB <- POP_2POP_GEO_NB %>% bind_rows(add_aggregate(POP_2POP_GEO_NB))  %>%  
		arrange(country, time, sex, classif1, classif2)	 # resort


	
DT_POP_2POP_GEO_NB <- POP_2POP_GEO_NB %>% 
						group_by(country, time) %>% mutate(value = value / max(value) * 100) %>% 
						ungroup %>% 
						mutate(classif1 = paste0(classif1, '_DT')) %>%
						spread(classif1, value) %>% 
						mutate(time = as.numeric(time))
		
load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/POP.Rdata'))

POP_2POP_GEO_NB <- POP %>% 
				filter(time %in% unique(DT_POP_2POP_GEO_NB$time), classif1 %in% 'AGE_5YRBANDS_TOTAL', sex %in% 'SEX_T') %>% 
				select(-indicator, -classif1, -sex, -classif2) %>% 
				left_join(DT_POP_2POP_GEO_NB,  by = c("country", "time"))		
rm(DT_POP_2POP_GEO_NB)			
POP_2POP_GEO_NB <- POP_2POP_GEO_NB %>% 
					mutate(	GEO_COV_NAT = value, 
							GEO_COV_URB = value / 100 * GEO_COV_URB_DT, 
							GEO_COV_RUR = value / 100 * GEO_COV_RUR_DT) %>% 
					select(-GEO_COV_NAT_DT, -GEO_COV_URB_DT, -GEO_COV_RUR_DT, -value) %>%
					gather(classif1, value, -indicator, -country, -time, -sex, -classif2 , -note_value, -collection , -source, na.rm = TRUE) 

					
if(save_file){								

POP_2POP_GEO_NB	%>%  select(-sex, -note_value, -classif2) %>% 
		# filter( country %in% c('X86','X87', 'X45')) %>%
		mutate(note_source = 'R1:2474') %>% 
		clean_csv %>% 
		data.table:::fwrite(paste0(dir_file, '3_POP_2POP_GEO_NB.csv'), na = "")	

}									
rm(POP_2POP_GEO_NB, POP)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 

}	

##################################################################
######## prepare NEW_MASTER UR and co
##################################################################    

rebuild_master <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/MASTER.Rdata'))

MASTER <- MASTER %>% filter( time <= max(time_get), time >= min(time_get) , !classif1 %in% 'AGE_YTHADULT_Y15-64')


GET <- read_dta(file_get_master) %>% mutate(country = iso3code) %>% select(country, time = year, MFLF:FURA_P, MFURk:FURAk) %>% filter( time <= max(time_get), time >= min(time_get) )


REF <- MASTER %>% 	select(-classif2, -source, -collection, -EIP, -LFPR, -INAR) %>% 
			mutate(classif1 = classif1 %>% plyr:::mapvalues(
														from = c("AGE_YTHADULT_Y15-24",  "AGE_YTHADULT_YGE15", "AGE_YTHADULT_YGE25"),
														to = c("YTH",  "TOT", "ADU")), 
					sex = gsub('SEX_', '', sex, fixed = TRUE)) %>% 
			filter(!substr(country,1,1) %in% 'X' )
rm(MASTER)			

X <- left_join(
		REF %>% select(-EAP) %>% mutate(sex = paste0('WAP_', sex)) %>% unite(test, sex, classif1 , sep = '_', remove = TRUE )%>% spread(test, WAP),	
		REF %>% select(-WAP) %>% mutate(sex = paste0('EAP_', sex)) %>% unite(test, sex, classif1 , sep = '_', remove = TRUE )%>% spread(test, EAP),			
		by = c("country", "time")) %>% 
	left_join(GET %>% select(country, time, MFUR_P:FURAk), by = c("country", "time")) %>% 
	rename(
			UR_T_TOT = MFUR_P, 
			UR_M_TOT = MUR_P, 
			UR_F_TOT = FUR_P, 
			UR_T_YTH = YUR_P, 
			UR_M_YTH = MURY_P, 
			UR_F_YTH = FURY_P,
			UR_T_ADU = AUR_P, 
			UR_M_ADU = MURA_P, 
			UR_F_ADU = FURA_P,
			UREST_T_TOT = MFURk, 
			UREST_M_TOT = MURk, 
			UREST_F_TOT = FURk, 
			UREST_T_YTH = YURk, 
			UREST_M_YTH = MURYk, 
			UREST_F_YTH = FURYk,
			UREST_T_ADU = AURk, 
			UREST_M_ADU = MURAk, 
			UREST_F_ADU = FURAk
			) %>% 
		mutate(UNE_T_TOT = EAP_T_TOT * UR_T_TOT / 100) %>% 
		mutate(UNE_T_YTH = EAP_T_YTH * UR_T_YTH / 100) %>% 
		mutate(UNE_T_ADU = EAP_T_ADU * UR_T_ADU / 100) %>% 
		mutate(UNE_M_TOT = EAP_M_TOT * UR_M_TOT / 100) %>% 
		mutate(UNE_M_YTH = EAP_M_YTH * UR_M_YTH / 100) %>% 
		mutate(UNE_M_ADU = EAP_M_ADU * UR_M_ADU / 100) %>% 
		mutate(UNE_F_TOT = EAP_F_TOT * UR_F_TOT / 100) %>% 
		mutate(UNE_F_YTH = EAP_F_YTH * UR_F_YTH / 100) %>% 
		mutate(UNE_F_ADU = EAP_F_ADU * UR_F_ADU / 100) %>% 
		select(-contains('UR_')) %>% 
		gather(ind, value, -country, -time) %>% 
		separate(ind, c('indicator', 'sex', 'classif1'), sep = '_', remove = TRUE) %>% 
		mutate( indicator = indicator %>% 							# mapping indicator
							plyr::mapvalues(from = c("WAP",  "EAP",     "UNE", 'UREST'),
											to =   c("POP_2POP_SEX_AGE_NB",  "EAP_2EAP_SEX_AGE_NB",   "UNE_2UNE_SEX_AGE_NB", 'note_UR')),
				sex 	= 	sex %>% 							# mapping sex
							plyr::mapvalues(from = c('T','M','F'),
											to = c('SEX_T','SEX_M','SEX_F')), 
				classif1=	classif1 %>% 						# mapping classif1
							plyr::mapvalues(from = c("TOT", "YTH", "ADU"),
											to = c('AGE_YTHADULT_YGE15','AGE_YTHADULT_Y15-24','AGE_YTHADULT_YGE25')), 
				classif2 = as.character(NA)) %>% 
		select( country,  indicator, sex, classif1, classif2, time, value) 



Y <- bind_rows(	
			X, 
			X %>% filter(indicator %in% 'POP_2POP_SEX_AGE_NB') %>% add_aggregate(use_SDG = SDG_GRP),
			X %>% filter(indicator %in% 'EAP_2EAP_SEX_AGE_NB') %>% add_aggregate(use_SDG = SDG_GRP),
			X %>% filter(indicator %in% 'UNE_2UNE_SEX_AGE_NB') %>% add_aggregate(use_SDG = SDG_GRP)
			) %>% arrange(country, indicator, time, sex, classif1, classif2)  %>% 
		mutate(collection = 'ILOEST') %>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') 
								
								
rm(X)			
		

NEW_MASTER <- Y %>% spread(indicator, value) %>% 
			mutate(note_UR = ifelse(note_UR %in% 1, 'R', as.character(NA))) %>% 
			mutate(
				EAP_2WAP_SEX_AGE_RT = EAP_2EAP_SEX_AGE_NB / POP_2POP_SEX_AGE_NB * 100,
				UNE_2EAP_SEX_AGE_RT = UNE_2UNE_SEX_AGE_NB / EAP_2EAP_SEX_AGE_NB * 100, 
				EMP_2EMP_SEX_AGE_NB = EAP_2EAP_SEX_AGE_NB - UNE_2UNE_SEX_AGE_NB,
				EIP_2EIP_SEX_AGE_NB = POP_2POP_SEX_AGE_NB - EAP_2EAP_SEX_AGE_NB, 
				EIP_2WAP_SEX_AGE_RT = EIP_2EIP_SEX_AGE_NB / POP_2POP_SEX_AGE_NB * 100, 
				EMP_2WAP_SEX_AGE_RT = EMP_2EMP_SEX_AGE_NB / POP_2POP_SEX_AGE_NB * 100) %>% 
			select(-classif2) %>% arrange(country, time, sex, classif1) %>% 
			select(collection, country, source, sex, classif1, time, POP_2POP_SEX_AGE_NB, EAP_2EAP_SEX_AGE_NB, EMP_2EMP_SEX_AGE_NB, UNE_2UNE_SEX_AGE_NB, EIP_2EIP_SEX_AGE_NB, EAP_2WAP_SEX_AGE_RT,EMP_2WAP_SEX_AGE_RT, UNE_2EAP_SEX_AGE_RT, note_UR, EIP_2WAP_SEX_AGE_RT)
			
			
rm(Y)

save(NEW_MASTER, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

if(save_file){

NEW_MASTER	%>% select(collection:time, value = EMP_2EMP_SEX_AGE_NB) %>% mutate(indicator = 'EMP_2EMP_SEX_AGE_NB') %>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
	# filter( country %in% c('X86','X87', 'X45')) %>%
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '6_EMP_2EMP_SEX_AGE_NB.csv'), na = "")	


NEW_MASTER	%>% select(collection:time, value = EMP_2WAP_SEX_AGE_RT) %>% mutate(indicator = 'EMP_2WAP_SEX_AGE_RT') %>%  select(collection, country, source, indicator, sex, classif1, time, value) %>%
	# filter( country %in% c('X86','X87', 'X45')) %>%
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '15_EMP_2WAP_SEX_AGE_RT.csv'), na = "")	

NEW_MASTER	%>% select(collection:time, value = UNE_2UNE_SEX_AGE_NB) %>% mutate(indicator = 'UNE_2UNE_SEX_AGE_NB') %>%  select(collection, country, source, indicator, sex, classif1, time, value) %>%
	# filter( country %in% c('X86','X87', 'X45')) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '16_UNE_2UNE_SEX_AGE_NB.csv'), na = "")
				
NEW_MASTER	%>% select(collection:time, value = UNE_2EAP_SEX_AGE_RT, obs_status = note_UR) %>% mutate(indicator = 'UNE_2EAP_SEX_AGE_RT') %>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>%
	# filter( country %in% c('X86','X87', 'X45')) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '17_UNE_2EAP_SEX_AGE_RT.csv'), na = "")	
				
}
	
rm(NEW_MASTER)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 

}

##################################################################
######## prepare STE
##################################################################

EMP_2EMP_SEX_STE <- function(){ 

GET <- read_dta(file_get_master) %>% mutate(country = iso3code, year = as.numeric(year)) %>% select(country, time = year, WSWP_MF:UFWP_F,  WSW_MFk:UFW_Fk) %>% filter( time <= max(time_get), time >= min(time_get) )

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))


EMP_STE_ICSE93 <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				spread(sex, EMP_2EMP_SEX_AGE_NB)  %>% 
				left_join(GET, by = c("country", "time")) %>% 
				mutate(
						SEX_TSTE_ICSE93_1 = paste0(SEX_T * WSWP_MF/100, ' ', WSW_MFk), 
						SEX_TSTE_ICSE93_2 = paste0(SEX_T * SEWEP_MF/100, ' ', SEWE_MFk), 
						SEX_TSTE_ICSE93_3 = paste0(SEX_T * SEWOEP_MF/100, ' ', SEWOE_MFk), 
						SEX_TSTE_ICSE93_5 = paste0(SEX_T * UFWP_MF/100, ' ', UFW_MFk), 
						SEX_MSTE_ICSE93_1 = paste0(SEX_M * WSWP_M/100, ' ', WSW_Mk), 
						SEX_MSTE_ICSE93_2 = paste0(SEX_M * SEWEP_M/100, ' ', SEWE_Mk), 
						SEX_MSTE_ICSE93_3 = paste0(SEX_M * SEWOEP_M/100, ' ', SEWOE_Mk), 
						SEX_MSTE_ICSE93_5 = paste0(SEX_M * UFWP_M/100, ' ', UFW_Mk),
						SEX_FSTE_ICSE93_1 = paste0(SEX_F * WSWP_F/100, ' ', WSW_Fk), 
						SEX_FSTE_ICSE93_2 = paste0(SEX_F * SEWEP_F/100, ' ', SEWE_Fk), 
						SEX_FSTE_ICSE93_3 = paste0(SEX_F * SEWOEP_F/100, ' ', SEWOE_Fk), 
						SEX_FSTE_ICSE93_5 = paste0(SEX_F * UFWP_F/100, ' ', UFW_Fk), 
						SEX_TSTE_ICSE93_TOTAL = as.character(SEX_T) , 
						SEX_MSTE_ICSE93_TOTAL = as.character(SEX_M) , 
						SEX_FSTE_ICSE93_TOTAL = as.character(SEX_F) ) %>% 
				select(country, time, contains('STE_ICSE93')) %>% 
				gather(classif1, val, -country, -time) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>% 
				mutate(	sex = substr(classif1, 1,5), 
						classif1 = str_sub(classif1,6,-1), 
						value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA))
rm(GET)			



EMP_STE_aggregate <- 	EMP_STE_ICSE93 %>% 
		mutate(classif1=	classif1 %>% 						# mapping classif1
							plyr::mapvalues(from = c('STE_ICSE93_TOTAL', 'STE_ICSE93_1','STE_ICSE93_2','STE_ICSE93_3', 'STE_ICSE93_5'),
											to = c('STE_AGGREGATE_TOTAL', 'STE_AGGREGATE_EES','STE_AGGREGATE_SLF','STE_AGGREGATE_SLF', 'STE_AGGREGATE_SLF'))) %>% 
		group_by(country, time, sex, classif1) %>% 
		summarise(value = sum(value)) %>% 
		ungroup 
				
EMP_STE <- bind_rows(EMP_STE_ICSE93,EMP_STE_aggregate) %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_STE_NB') 

EMP_STE <- EMP_STE %>% bind_rows(add_aggregate(EMP_STE))

############### add ilo source XA:...


EMP_2EMP_SEX_STE_NB <- EMP_STE %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(EMP_STE_ICSE93, EMP_STE_aggregate, EMP_STE)


EMP_2EMP_SEX_STE_DT <- EMP_2EMP_SEX_STE_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_STE_DT') %>% select(-classif1_version, -sex_version, -classif2_version)# %>%

					

if(save_file){

EMP_2EMP_SEX_STE_NB	%>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '7_EMP_2EMP_SEX_STE_NB.csv'), na = "")	

EMP_2EMP_SEX_STE_DT	%>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '11_EMP_2EMP_SEX_STE_DT.csv'), na = "")	

	}
save(EMP_2EMP_SEX_STE_NB, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_STE_NB.Rdata'))
save(EMP_2EMP_SEX_STE_DT, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_STE_DT.Rdata'))
	
rm(EMP_2EMP_SEX_STE_NB,EMP_2EMP_SEX_STE_DT )	
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 
			
}

##################################################################
######## prepare ECO sector
##################################################################

EMP_2EMP_SEX_ECO <- function(){


load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))


# ECO DETAILS TEST

GET <- read_dta(file_get_eco_details) %>% mutate(year = as.numeric(year)) %>% select(ilo_code, sex = sexid, time = year, EMP1shP:EMP14shP, dummy1:dummy14) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
		left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code) %>% filter(!ilo_code %in% NA) , by = "ilo_code") 		%>% 
					select(-ilo_code) %>% mutate(sex = as.character(sex) %>% plyr:::mapvalues(from = c('1','2','3'), to = c('SEX_T','SEX_M','SEX_F')))




EMP_ECO_DETAILS <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				left_join(GET, by = c("country", "time", 'sex')) %>% 
				mutate(
						ECO_DETAILS_A = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP1shP)/100,' ', dummy1), 
						ECO_DETAILS_B = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP2shP)/100,' ', dummy2), 
						ECO_DETAILS_C = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP3shP)/100,' ', dummy3),
						ECO_DETAILS_DE = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP4shP)/100,' ', dummy4),
						ECO_DETAILS_F = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP5shP)/100,' ', dummy5),
						ECO_DETAILS_G = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP6shP)/100,' ', dummy6),
						ECO_DETAILS_I = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP7shP)/100,' ', dummy7),
						ECO_DETAILS_HJ = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP8shP)/100,' ', dummy8),
						ECO_DETAILS_K = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP9shP)/100,' ', dummy9),
						ECO_DETAILS_P = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP10shP)/100,' ', dummy10),
						ECO_DETAILS_Q = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP11shP)/100,' ', dummy11),
						ECO_DETAILS_O = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP12shP)/100,' ', dummy12),
						ECO_DETAILS_LMN = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP13shP)/100,' ', dummy13), 
						ECO_DETAILS_RSTU = 	paste0(EMP_2EMP_SEX_AGE_NB * (EMP14shP)/100,' ', dummy14), 
						ECO_DETAILS_TOTAL = paste0(EMP_2EMP_SEX_AGE_NB),
						 ) %>% 
				select(country, sex, time, contains('ECO_DETAILS')) %>% 
				gather(classif1, val, -country, -time, -sex) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>% 
				mutate(	value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA))
						
						
						
EMP_ECO_DETAILS <- EMP_ECO_DETAILS	 %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_ECO_NB') 

EMP_ECO_DETAILS <- EMP_ECO_DETAILS %>% bind_rows(add_aggregate(EMP_ECO_DETAILS))
					
						



EMP_2EMP_SEX_ECO_NB <- EMP_ECO_DETAILS %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(GET, EMP_ECO_DETAILS)			


EMP_2EMP_SEX_ECO_DT <- EMP_2EMP_SEX_ECO_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_ECO_DT') %>% select(-classif1_version, -sex_version, -classif2_version) #%>%
					
					

if(save_file){

EMP_2EMP_SEX_ECO_NB	%>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '8_EMP_2EMP_SEX_ECO_NB_DETAILS.csv'), na = "")	

EMP_2EMP_SEX_ECO_DT	%>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '12_EMP_2EMP_SEX_ECO_DT_DETAILS.csv'), na = "")	

	}
rm(EMP_2EMP_SEX_ECO_NB,EMP_2EMP_SEX_ECO_DT )				
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 

# ECO AGGREGATE TEST

GET <- read_dta(file_get_eco_aggregate) %>% select(ilo_code, sex = sexid, real, time = year, EMP1shP:EMP6shP, dummy1:dummy6) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
		left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code) %>% filter(!ilo_code %in% NA) , by = "ilo_code") 		%>% 
					select(-ilo_code) %>% mutate(sex = as.character(sex) %>% plyr:::mapvalues(from = c('1','2','3'), to = c('SEX_T','SEX_M','SEX_F')))


EMP_ECO_AGGREGATE <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				left_join(GET, by = c("country", "time", 'sex')) %>% 
				mutate(
						ECO_AGGREGATE_AGR = paste0(EMP_2EMP_SEX_AGE_NB * (EMP1shP)/100,' ', dummy1), 
						ECO_AGGREGATE_MAN = paste0(EMP_2EMP_SEX_AGE_NB * (EMP3shP)/100,' ', dummy2), 
						ECO_AGGREGATE_CON = paste0(EMP_2EMP_SEX_AGE_NB * (EMP2shP)/100,' ', dummy3),
						ECO_AGGREGATE_MEL = paste0(EMP_2EMP_SEX_AGE_NB * (EMP4shP)/100,' ', dummy4), 
						ECO_AGGREGATE_MKT = paste0(EMP_2EMP_SEX_AGE_NB * (EMP5shP)/100,' ', dummy5), 
						ECO_AGGREGATE_PUB = paste0(EMP_2EMP_SEX_AGE_NB * (EMP6shP)/100,' ', dummy6),
						ECO_AGGREGATE_TOTAL = paste0(EMP_2EMP_SEX_AGE_NB),
						
						 ) %>% 
				select(country, sex, time, contains('ECO_AGGREGATE')) %>% 
				gather(classif1, val, -country, -time, -sex) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>% 
				mutate(	value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA))
						
EMP_ECO_AGGREGATE <- EMP_ECO_AGGREGATE	 %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_ECO_NB') 

EMP_ECO_AGGREGATE <- EMP_ECO_AGGREGATE %>% bind_rows(add_aggregate(EMP_ECO_AGGREGATE))
					
						



EMP_2EMP_SEX_ECO_NB <- EMP_ECO_AGGREGATE %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(GET, EMP_ECO_AGGREGATE)			


EMP_2EMP_SEX_ECO_DT <- EMP_2EMP_SEX_ECO_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_ECO_DT') %>% select(-classif1_version, -sex_version, -classif2_version) #%>%
					
					

if(save_file){

EMP_2EMP_SEX_ECO_NB	%>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '8_EMP_2EMP_SEX_ECO_NB_AGGREGATE.csv'), na = "")	

EMP_2EMP_SEX_ECO_DT	%>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '12_EMP_2EMP_SEX_ECO_DT_AGGREGATE.csv'), na = "")	

	}
rm(EMP_2EMP_SEX_ECO_NB,EMP_2EMP_SEX_ECO_DT )					
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 


# ECO SECTOR TEST

GET <- read_dta(file_get_eco_aggregate) %>% select(ilo_code, sex = sexid, real, time = year, EMP1shP:EMP6shP, dummy1:dummy6) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
		left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code) %>% filter(!ilo_code %in% NA) , by = "ilo_code") 		%>% 
					select(-ilo_code) %>% mutate(sex = as.character(sex) %>% plyr:::mapvalues(from = c('1','2','3'), to = c('SEX_T','SEX_M','SEX_F')))


EMP_ECO_SECTOR <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				left_join(GET, by = c("country", "time", 'sex')) %>% 
				mutate(
						ECO_SECTOR_AGR = paste0(EMP_2EMP_SEX_AGE_NB * (EMP1shP)/100, ' ', dummy1), 
						ECO_SECTOR_IND = paste0(EMP_2EMP_SEX_AGE_NB * (EMP2shP + EMP3shP + EMP4shP + EMP4shP)/100), 
						ECO_SECTOR_SER = paste0(EMP_2EMP_SEX_AGE_NB * (EMP5shP + EMP6shP)/100), 
						ECO_SECTOR_TOTAL = paste0(EMP_2EMP_SEX_AGE_NB),
						
						 ) %>% 
				select(country, sex, time, contains('ECO_SECTOR')) %>% 
				gather(classif1, val, -country, -time, -sex) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>% 
				mutate(	value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA))
						
EMP_ECO_SECTOR <- EMP_ECO_SECTOR	 %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_ECO_NB') 

EMP_ECO_SECTOR <- EMP_ECO_SECTOR %>% bind_rows(add_aggregate(EMP_ECO_SECTOR))
					
						



EMP_2EMP_SEX_ECO_NB <- EMP_ECO_SECTOR %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(GET, EMP_ECO_SECTOR)			


EMP_2EMP_SEX_ECO_DT <- EMP_2EMP_SEX_ECO_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_ECO_DT') %>% select(-classif1_version, -sex_version, -classif2_version) #%>%
					
					

if(save_file){

EMP_2EMP_SEX_ECO_NB	%>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '8_EMP_2EMP_SEX_ECO_NB_SECTOR.csv'), na = "")	

EMP_2EMP_SEX_ECO_DT	%>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '12_EMP_2EMP_SEX_ECO_DT_SECTOR.csv'), na = "")	

	}
rm(EMP_2EMP_SEX_ECO_NB, EMP_2EMP_SEX_ECO_DT)					

rm(NEW_MASTER)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 



}

##################################################################
######## prepare OCU occupation
##################################################################

EMP_2EMP_SEX_OCU <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

GET <- read_dta(file_get_ocu_skill) %>%  				
					select(ilo_code, time = year, real, EMPoc1shP:EMPoc3shP, sexid, dummy1:dummy3) %>% 
					filter( time <= max(time_get), time >= min(time_get) ) %>% 
					left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
										select(ilo_code, country = code), by = "ilo_code") 		%>% 
									select(-ilo_code) %>% 
					mutate(	real = as.numeric(real), 
							sexid = as.character(as_factor(sexid)), 
							sex = sexid %>% plyr:::mapvalues(	from = c('Female', 'Male', 'Total'), 
															to   = c('SEX_F', 'SEX_M', 'SEX_T')))  %>% 
					select(-sexid)
					



EMP_OCU_SKILL <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				left_join(GET, by = c("country", "time", 'sex')) %>% 
				mutate(
						OCU_SKILL_L1 = paste0(EMP_2EMP_SEX_AGE_NB * EMPoc1shP/100, ' ', dummy1), 
						OCU_SKILL_L2 = paste0(EMP_2EMP_SEX_AGE_NB * EMPoc2shP/100, ' ', dummy2), 
						OCU_SKILL_L3 = paste0(EMP_2EMP_SEX_AGE_NB * EMPoc3shP/100, ' ', dummy3), 
						OCU_SKILL_TOTAL = as.character(EMP_2EMP_SEX_AGE_NB)) %>% 
				select(country, time, sex, contains('OCU_SKILL')) %>% 
				gather(classif1, val, -country, -time, -sex) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>%
				mutate(	value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA), 
						classif1 = ifelse(classif1 %in% 'OCU_SKILL_L3', 'OCU_SKILL_L3-4', classif1)) %>% 
				filter(!value %in% NA) 			
						
						
rm(NEW_MASTER)						
						
EMP_OCU_SKILL <- EMP_OCU_SKILL	 %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_OCU_NB') 

EMP_OCU_SKILL <- EMP_OCU_SKILL %>% bind_rows(add_aggregate(EMP_OCU_SKILL))
					
						



EMP_2EMP_SEX_OCU_NB <- EMP_OCU_SKILL %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(GET, EMP_OCU_SKILL)			


EMP_2EMP_SEX_OCU_DT <- EMP_2EMP_SEX_OCU_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_OCU_DT') %>% select(-classif1_version, -sex_version, -classif2_version) # %>%
					
					

if(save_file){

EMP_2EMP_SEX_OCU_NB	%>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 

				clean_csv %>% data.table:::fwrite(paste0(dir_file, '9_EMP_2EMP_SEX_OCU_NB.csv'), na = "")	

EMP_2EMP_SEX_OCU_DT	%>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>% 

				clean_csv %>% data.table:::fwrite(paste0(dir_file, '12_EMP_2EMP_SEX_OCU_DT.csv'), na = "")	

	}
save(EMP_2EMP_SEX_OCU_DT, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_OCU_DT.Rdata'))
save(EMP_2EMP_SEX_OCU_NB, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_OCU_NB.Rdata'))
		
	
rm(EMP_2EMP_SEX_OCU_DT, EMP_2EMP_SEX_OCU_NB)					

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 

}

##################################################################
######## prepare GDP
################################################################## 

GDP_NB <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

GET <- read_dta(file_get_gdp) %>% mutate(year = as.numeric(year)) %>% select(ilo_code, time = year, GDP_205U_NOC_NB = prdUSD, GDP_211P_NOC_NB = prd) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
			left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code), by = "ilo_code") 		%>% 
					select(-ilo_code) 

					
GDP <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15', sex %in% 'SEX_T') %>% 
				left_join(GET, by = c("country", "time")) %>% 
				rename(EMP = EMP_2EMP_SEX_AGE_NB ) %>% 
				mutate(classif1 = 'NOC_VALUE', 
			   sex = NA) %>% select(-source, -collection)
rm(NEW_MASTER)					
					
############### calculate regional aggregate

		
ilo:::init_ilo(-cl)

REF_AGG <- ilo$code$cl_country_by_group %>% 					# prepare mapping for country group 'region'
					filter(	substr(code,1,7) %in% c('ILO_GEO', 'ECO_G20', 'ECO_EU2', 'ECO_BRI', 'ECO_ASE', 'ECO_WMB')) %>% 
					select(region = code, country = code_country) %>% 
					mutate(	region = gsub('ECO_EU28', 'ECO_EU28_X82', region, fixed = TRUE), 
							region = gsub('ECO_G20', 'ECO_G20_X83', region, fixed = TRUE), 
							region = gsub('ECO_ASEAN', 'ECO_ASEAN_X84', region, fixed = TRUE),
							region = gsub('ECO_WMBRICS', 'ECO_WMBRICS_X87', region, fixed = TRUE),
							region = gsub('ECO_BRICS', 'ECO_BRICS_X85', region, fixed = TRUE), 
							region = str_sub(region, -3,-1) ) %>% 
					arrange(region, country)

AGG <- NULL														# set result dataset to empty
test <- unique(REF_AGG$region) %>% sort							# prepare loop on region
for (i in 1:length(test)){
ref <- REF_AGG %>% 
		filter(region %in% test[i]) %>% 
		select(country) %>% t %>% as.character					# store relevant country list
		
AGG <-  GDP %>% 
		filter(country %in% ref) %>% 							# filter country i
		group_by( time, sex, classif1) %>%# group 	
		summarise(	country = test[i], 							# rename country = region
					GDP_205U_NOC_NB = sum((GDP_205U_NOC_NB * EMP), na.rm = TRUE) / sum(EMP , na.rm = TRUE),
					GDP_211P_NOC_NB = sum((GDP_211P_NOC_NB * EMP), na.rm = TRUE) / sum(EMP , na.rm = TRUE)
					
					) %>% 		# sum
		ungroup %>% bind_rows(AGG) 								# ungroup
rm(ref)
}	
rm(test)
					
	GDP <- GDP %>% 
		bind_rows(AGG) %>% 										# 
		arrange(country, time, sex, classif1)					# resort
rm(AGG)

GDP <- GDP %>% select(-EMP) %>% gather(indicator, value, -time, -country, -sex,  -classif1)
	



GDP <- GDP %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

		

if(save_file){

GDP	%>% filter(indicator %in% 'GDP_211P_NOC_NB') %>% mutate(indicator = 'GDP_211P_NOC_NB') %>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
# filter( country %in% c('X86','X87', 'X45')) %>%
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '20_GDP_211P_NOC_NB.csv'), na = "")	

GDP	%>% filter(indicator %in% 'GDP_205U_NOC_NB') %>% mutate(indicator = 'GDP_205U_NOC_NB') %>% select(collection, country, source, indicator, sex, classif1, time, value) %>% 
# filter( country %in% c('X86','X87', 'X45')) %>%
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '21_GDP_205U_NOC_NB.csv'), na = "")	


	
}
save(GDP, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/GDP.Rdata'))

rm(GDP)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 


}

##################################################################
######## prepare income CLASS
################################################################## SDG

EMP_2EMP_SEX_AGE_CLA <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

GET <- read_dta(file_get_class)  %>% select(country = iso3code, time = year, MF_T_class1P:F_A_class14P, STEP1) %>%  filter( time <= max(time_get), time >= min(time_get) ) %>% 
			gather(ref, value, -country, -time, -STEP1) %>% 
			separate(ref, c('sex','classif1','classif2'), sep = '_')
			

	
	

EMP_2EMP_SEX_AGE_CLA_NB <- GET %>% filter(!classif2 %in% 'EMP') %>% 
				mutate(classif2 = classif2 %>% 						# mapping classif2
									plyr::mapvalues(from = c("class1P",  "class2P",  "class3P",  "class4P",  "class5P",  "class6P",  "class7P",  "class8P",  "class9P",  "class10P", "class11P", "class12P", "class13P", "class14P"),
											to = c(	'CLA_ECOCLA_USDLT2',
													'CLA_ECOCLA_USDGE2LT3', 'CLA_ECOCLA_USDGE2LT3', 
													'CLA_ECOCLA_USDGE3LT5', 'CLA_ECOCLA_USDGE3LT5', 
													'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5'))
								) %>% 
				group_by(country, time, sex, classif1, classif2) %>% summarise(value = sum(value, na.rm = TRUE), STEP1 = first(STEP1)) %>% ungroup  %>% 
				mutate(	sex 	= 	sex %>% 							# mapping sex
							plyr::mapvalues(from = c('MF','M','F'),
											to = c('SEX_T','SEX_M','SEX_F')), 
				classif1=	classif1 %>% 						# mapping classif1
							plyr::mapvalues(from = c('T','Y','A'),
											to = c('AGE_YTHADULT_YGE15','AGE_YTHADULT_Y15-24', 'AGE_YTHADULT_YGE25')) 
							
					) %>% spread(classif2, value)
					
					

		
EMP_2EMP_SEX_AGE_CLA_NB <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X') %>% 
				right_join(EMP_2EMP_SEX_AGE_CLA_NB	, by = c("country", "time", 'sex', 'classif1')) %>% 
				rename(CLA_ECOCLA_TOTAL = EMP_2EMP_SEX_AGE_NB ) %>% select(-source, -collection) %>% 
				mutate(
						CLA_ECOCLA_USDLT2 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDLT2/100,
						CLA_ECOCLA_USDGE2LT3 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE2LT3/100,
						CLA_ECOCLA_USDGE3LT5 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE3LT5/100,
						CLA_ECOCLA_USDGE5 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE5/100) %>% 
				gather(classif2, value, -country, -sex, -classif1, -time, -STEP1) %>%
				arrange(country, time, sex, classif1, classif2) %>% 
				rename(obs_status = STEP1)	%>% 
				mutate(obs_status = ifelse(obs_status %in% 1, 'R', obs_status))
					

###################### ok we have country level but only 138 country, now have to fill gaps for EMP in order to get 188 country
				

GAP <- NEW_MASTER %>% 
				select(collection:time, value = EMP_2EMP_SEX_AGE_NB, -source, -collection) %>% 
				filter( !substr(country,1,1) %in% 'X', !country %in% unique(EMP_2EMP_SEX_AGE_CLA_NB$country)) 


rm(NEW_MASTER)
				
GET_NB_COU <- EMP_2EMP_SEX_AGE_CLA_NB %>% 
			bind_rows( 	GAP %>% mutate(classif2 = 'CLA_ECOCLA_TOTAL'), 
						GAP %>% mutate(classif2 = 'CLA_ECOCLA_USDGE3'))

								

GET_NB_COU <- GET_NB_COU %>% mutate(classif2 = classif2 %>% 						# mapping classif2
							plyr::mapvalues(from = c('CLA_ECOCLA_USDLT2',
													 'CLA_ECOCLA_USDGE2LT3', 
													 'CLA_ECOCLA_USDGE3LT5', 
													 'CLA_ECOCLA_USDGE5'),
											to = c('CLA_ECOCLA_USDLT2',
													 'CLA_ECOCLA_USDGE2LT3', 
													 'CLA_ECOCLA_USDGE3', 
													 'CLA_ECOCLA_USDGE3')), 
							indicator = 'EMP_2EMP_SEX_AGE_CLA_NB')				
							

REF_AGG <- ilo$code$cl_country_by_group %>% 					# prepare mapping for country group 'region'
					filter(
						substr(code,1,9) %in% c('ILO_GEO_X', 'ILO_GEO_W')) %>% 
					select(region = code, country = code_country) %>% 
					mutate(region = str_sub(region, -3,-1))
	
	
AGG <- NULL														# set result dataset to empty
test <- unique(REF_AGG$region) %>% sort							# prepare loop on region
for (i in 1:length(test)){
ref <- REF_AGG %>% 
		filter(region %in% test[i]) %>% 
		select(country) %>% t %>% as.character					# store relevant country list
		
AGG <-  GET_NB_COU %>% 
		filter(country %in% ref) %>% 							# filter country i
		group_by(indicator, time, sex, classif1, classif2) %>%# group 	
		summarise(	country = test[i], 							# rename country = region
					value = sum(value, na.rm = TRUE)) %>% 		# sum
		ungroup %>% bind_rows(AGG) 								# ungroup
rm(ref)
}	
rm(test, GET_NB_COU, REF_AGG, GAP)

#################################### add benchmark country with STEP1


EMP_2EMP_SEX_AGE_CLA_NB <- EMP_2EMP_SEX_AGE_CLA_NB %>% filter(!country %in% 'BHR') %>%  bind_rows(AGG) %>% 										# 
		arrange(country, time, sex, classif1, classif2)					# resort
rm(AGG)










EMP_2EMP_SEX_AGE_CLA_NB <- EMP_2EMP_SEX_AGE_CLA_NB %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			 arrange(collection, country, source, indicator, time, sex, classif1, classif2) %>% mutate(indicator = 'EMP_2EMP_SEX_AGE_CLA_NB')



EMP_2EMP_SEX_AGE_CLA_DT <- EMP_2EMP_SEX_AGE_CLA_NB %>%   group_by(collection, country, source, time, indicator, sex, classif1) %>% 
					mutate(value = value / first(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_AGE_CLA_DT') 
	

if(save_file){

EMP_2EMP_SEX_AGE_CLA_NB  %>% select(collection, country, source, indicator, sex, classif1, classif2, time, value) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '10_EMP_2EMP_SEX_AGE_CLA_NB.csv'), na = "")	

EMP_2EMP_SEX_AGE_CLA_DT  %>% select(collection, country, source, indicator, sex, classif1, classif2, time, value, obs_status) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '14_EMP_2EMP_SEX_AGE_CLA_DT.csv'), na = "")	


			
			
}

save(EMP_2EMP_SEX_AGE_CLA_NB, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_AGE_CLA_NB.Rdata'))
save(EMP_2EMP_SEX_AGE_CLA_DT, file = paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/EMP_2EMP_SEX_AGE_CLA_DT.Rdata'))
		
	
rm(EMP_2EMP_SEX_AGE_CLA_NB, EMP_2EMP_SEX_AGE_CLA_DT)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))				
 


}

##################################################################
######## prepare lfep age median indicator
################################################################## 

EAP_2MDN_SEX_NB <- function(){

	GET <- read_dta(file_lfep_median) %>% select(-ref_area_label) %>% gather(sex, obs_value, -ref_area, -year) %>% 
					rename(time = year) %>% 
					mutate(sex = sex %>% plyr:::mapvalues(from = c('median_age_LF_MF', 'median_age_LF_M', 'median_age_LF_F'), to = c('SEX_T', 'SEX_M', 'SEX_F'))) %>% 
					left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, ref_area = label_en) %>%
						mutate(	ref_area = str_sub(ref_area,1,3), 
								source = as.character(source)), by = 'ref_area') %>% 
					mutate(collection = 'ILOEST', indicator = 'EAP_2MDN_SEX_NB') %>% 
					select(collection, ref_area, source, indicator, sex, time, obs_value) %>% 
					data.table:::fwrite(paste0(dir_file, '22_EAP_2MDN_SEX_NB.csv'), na = "")	
			
}


##################################################################
######## prepare OTHER RATIO FOR KI
################################################################## 

POP_2MLF_NOC_RT <- function(){

################# POP_2MLF_NOC_RT Male labour force as % of working-age population -- ILO modeled estimates
# 100 * [EAP_2EAP_SEX_AGE_NB ] SEX_M & AGE_YTHADULT_YGE15 / [POP_2POP_SEX_AGE_NB] SEX_T & AGE_YTHADULT_YGE15

nominator <- read_csv(paste0(dir_file, '/4d_EAP_2EAP_SEX_AGE_NB_YTHA.csv')) %>% filter(sex %in% 'SEX_M', classif1 %in% 'AGE_YTHADULT_YGE15')  %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% rename(EAP = obs_value)

denominator <- read_csv(paste0(dir_file, '/1d_POP_2POP_SEX_AGE_NB_YTHA.csv')) %>% filter(sex %in% 'SEX_T', classif1 %in% 'AGE_YTHADULT_YGE15') %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% select(-note_source) %>% rename(POP  = obs_value)


left_join(nominator, denominator) %>% mutate(obs_value = EAP/POP * 100) %>% select(-EAP, -POP) %>%
				mutate(classif1 = 'NOC_VALUE', indicator = 'POP_2MLF_NOC_RT', collection = 'ILOEST')  %>% data.table:::fwrite(paste0(dir_file, '15_POP_2MLF_NOC_RT.csv'), na = "")	

}


POP_2FLF_NOC_RT <- function(){

################# POP_2FLF_NOC_RT Female labour force as % of working-age population -- ILO modeled estimates
# 100 * [EAP_2EAP_SEX_AGE_NB ] SEX_F & AGE_YTHADULT_YGE15 / [POP_2POP_SEX_AGE_NB] SEX_T & AGE_YTHADULT_YGE15

nominator <- read_csv(paste0(dir_file, '/4d_EAP_2EAP_SEX_AGE_NB_YTHA.csv')) %>% filter(sex %in% 'SEX_F', classif1 %in% 'AGE_YTHADULT_YGE15')  %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% rename(EAP = obs_value)

denominator <- read_csv(paste0(dir_file, '/1d_POP_2POP_SEX_AGE_NB_YTHA.csv')) %>% filter(sex %in% 'SEX_T', classif1 %in% 'AGE_YTHADULT_YGE15') %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% select(-note_source) %>% rename(POP  = obs_value)


 left_join(nominator, denominator) %>% mutate(obs_value = EAP/POP * 100) %>% select(-EAP, -POP) %>%
				mutate(classif1 = 'NOC_VALUE', indicator = 'POP_2FLF_NOC_RT', collection = 'ILOEST') %>% 
				data.table:::fwrite(paste0(dir_file, '15_POP_2FLF_NOC_RT.csv'), na = "")	

}


POP_2LDR_NOC_RT <- function(){

################# POP_2LDR_NOC_RT Labour dependency ratio -- ILO modeled estimates
#([POP_2POP_SEX_AGE_NB SEX_T & AGE_5YBANDS_TOTAL - [EMP_2EMP_SEX_AGE_NB ] SEX_T & AGE_YTHADULT_YGE15) / ([EMP_2EMP_SEX_AGE_NB ] SEX_T & AGE_YTHADULT_YGE15)

nominator <- read_csv(paste0(dir_file, '/6_EMP_2EMP_SEX_AGE_NB.csv')) %>% filter(sex %in% 'SEX_T', classif1 %in% 'AGE_YTHADULT_YGE15')  %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% rename(EMP = obs_value)

denominator <- read_csv(paste0(dir_file, '/1a_POP_2POP_SEX_AGE_NB_5YRB.csv')) %>% filter(sex %in% 'SEX_T', classif1 %in% 'AGE_5YRBANDS_TOTAL') %>% mutate(indicator = NA, classif1 = NA, sex = NA) %>% select(-note_source) %>% rename(POP  = obs_value)


 left_join(nominator, denominator) %>% mutate(obs_value = (POP - EMP)/EMP) %>% select(-EMP, -POP) %>%
				mutate(classif1 = 'NOC_VALUE', indicator = 'POP_2LDR_NOC_RT', collection = 'ILOEST') %>% 
				data.table:::fwrite(paste0(dir_file, '15_POP_2LDR_NOC_RT.csv'), na = "")	
}

##################################################################
######## rebuild ILOEST final ready to load
################################################################## 

combine_ILOEST <- function(){

				
	ref <- list.files(dir_file)	%>% as_data_frame %>% 
			mutate(	ref  = value, 
					value =  value %>% str_sub( 3, -5), 
					value = ifelse(str_sub(value, 1,1) %in% '_', str_sub(value,2,-1), value),
					value = ifelse(str_sub(value, 1,2) %in% c('a_', 'b_', 'c_', 'd_'), str_sub(value,3,-1), value)
					) %>% 
		separate(value, c('cla','rep_var','sex', 'cl1','cl2','nb'), extra = 'drop', sep = '_') %>% 
		mutate(nb = ifelse(!nb %in% c('NB','DT','RT'), NA, nb)) %>% select(-rep_var) %>% 
		unite(new, cla, sex, cl1,cl2,nb) %>% 
		mutate(new = str_replace_all(new, '_NA', ''))
				

check = unique(ref$new)				
				
				
for (i in 1:length(check)){


	test <- ref %>% filter(new %in% check[i])
	
	X <- NULL
	count <- 1
	for (j in 1:nrow(test)){
		X <- bind_rows(X, read_csv(paste0(dir_file,test$ref[j])))
	
		if(nrow(X)> 320000 & !j %in% nrow(test)){
			collect <- unique(X$collection)
			X %>% data.table:::fwrite(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/',collect, '_',check[i], '_', count, '.csv' ), na = "")
			X <- NULL
			count = count + 1		
		} else{
		
			if(j %in% nrow(test)){
				collect <- unique(X$collection)
				X %>% data.table:::fwrite(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/',collect, '_',check[i], '_', count, '.csv' ), na = "") 
				X <- NULL
			}
	
		}
	
	}
	

}				
				
				
	
}

##################################################################
######## SDG processing
################################################################## 


SDG_0852_UNE_DEAP_SEX_AGE_RT <- function(){



load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/MASTER.Rdata'))

MASTER <- MASTER %>% filter( time <= max(time_get), time >= min(time_get) , !classif1 %in% 'AGE_YTHADULT_Y15-64')


GET <- read_dta(file_get_master) %>% mutate(country = iso3code) %>% select(country, time = year, MFLF:FURA_P, MFURk:FURAk) %>% filter( time <= max(time_get), time >= min(time_get) )


REF <- MASTER %>% 	select(-classif2, -source, -collection, -EIP, -LFPR, -INAR) %>% 
			mutate(classif1 = classif1 %>% plyr:::mapvalues(
														from = c("AGE_YTHADULT_Y15-24",  "AGE_YTHADULT_YGE15", "AGE_YTHADULT_YGE25"),
														to = c("YTH",  "TOT", "ADU")), 
					sex = gsub('SEX_', '', sex, fixed = TRUE)) %>% 
			filter(!substr(country,1,1) %in% 'X' )
rm(MASTER)			

X <- left_join(
		REF %>% select(-EAP) %>% mutate(sex = paste0('WAP_', sex)) %>% unite(test, sex, classif1 , sep = '_', remove = TRUE )%>% spread(test, WAP),	
		REF %>% select(-WAP) %>% mutate(sex = paste0('EAP_', sex)) %>% unite(test, sex, classif1 , sep = '_', remove = TRUE )%>% spread(test, EAP),			
		by = c("country", "time")) %>% 
	left_join(GET %>% select(country, time, MFUR_P:FURAk), by = c("country", "time")) %>% 
	rename(
			UR_T_TOT = MFUR_P, 
			UR_M_TOT = MUR_P, 
			UR_F_TOT = FUR_P, 
			UR_T_YTH = YUR_P, 
			UR_M_YTH = MURY_P, 
			UR_F_YTH = FURY_P,
			UR_T_ADU = AUR_P, 
			UR_M_ADU = MURA_P, 
			UR_F_ADU = FURA_P,
			UREST_T_TOT = MFURk, 
			UREST_M_TOT = MURk, 
			UREST_F_TOT = FURk, 
			UREST_T_YTH = YURk, 
			UREST_M_YTH = MURYk, 
			UREST_F_YTH = FURYk,
			UREST_T_ADU = AURk, 
			UREST_M_ADU = MURAk, 
			UREST_F_ADU = FURAk
			) %>% 
		mutate(UNE_T_TOT = EAP_T_TOT * UR_T_TOT / 100) %>% 
		mutate(UNE_T_YTH = EAP_T_YTH * UR_T_YTH / 100) %>% 
		mutate(UNE_T_ADU = EAP_T_ADU * UR_T_ADU / 100) %>% 
		mutate(UNE_M_TOT = EAP_M_TOT * UR_M_TOT / 100) %>% 
		mutate(UNE_M_YTH = EAP_M_YTH * UR_M_YTH / 100) %>% 
		mutate(UNE_M_ADU = EAP_M_ADU * UR_M_ADU / 100) %>% 
		mutate(UNE_F_TOT = EAP_F_TOT * UR_F_TOT / 100) %>% 
		mutate(UNE_F_YTH = EAP_F_YTH * UR_F_YTH / 100) %>% 
		mutate(UNE_F_ADU = EAP_F_ADU * UR_F_ADU / 100) %>% 
		select(-contains('UR_')) %>% 
		gather(ind, value, -country, -time) %>% 
		separate(ind, c('indicator', 'sex', 'classif1'), sep = '_', remove = TRUE) %>% 
		mutate( indicator = indicator %>% 							# mapping indicator
							plyr::mapvalues(from = c("WAP",  "EAP",     "UNE", 'UREST'),
											to =   c("POP_2POP_SEX_AGE_NB",  "EAP_2EAP_SEX_AGE_NB",   "UNE_2UNE_SEX_AGE_NB", 'note_UR')),
				sex 	= 	sex %>% 							# mapping sex
							plyr::mapvalues(from = c('T','M','F'),
											to = c('SEX_T','SEX_M','SEX_F')), 
				classif1=	classif1 %>% 						# mapping classif1
							plyr::mapvalues(from = c("TOT", "YTH", "ADU"),
											to = c('AGE_YTHADULT_YGE15','AGE_YTHADULT_Y15-24','AGE_YTHADULT_YGE25')), 
				classif2 = as.character(NA)) %>% 
		select( country,  indicator, sex, classif1, classif2, time, value) 



Y <- bind_rows(	
			X, 
			X %>% filter(indicator %in% 'EAP_2EAP_SEX_AGE_NB') %>% add_aggregate(use_SDG = TRUE),
			X %>% filter(indicator %in% 'UNE_2UNE_SEX_AGE_NB') %>% add_aggregate(use_SDG = TRUE)
			) %>% arrange(country, indicator, time, sex, classif1, classif2)  %>% 
		mutate(collection = 'ILOEST') %>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') 
								
								
rm(X)			
		

NEW_MASTER <- Y %>% spread(indicator, value) %>% 
			mutate(note_UR = ifelse(note_UR %in% 1, 'R', as.character(NA))) %>% 
			mutate(
				UNE_2EAP_SEX_AGE_RT = UNE_2UNE_SEX_AGE_NB / EAP_2EAP_SEX_AGE_NB * 100 
) %>% 
			select(-classif2) %>% arrange(country, time, sex, classif1) %>% 
			select(collection, country, source, sex, classif1, time,  UNE_2EAP_SEX_AGE_RT, note_UR)
			
			
rm(Y)


NEW_MASTER	%>% select(collection:time, value = UNE_2EAP_SEX_AGE_RT, obs_status = note_UR) %>% 
				mutate(indicator = 'UNE_2EAP_SEX_AGE_RT') %>% select(collection, country, source, indicator, sex, classif1, time, value, obs_status) %>%
	# filter( country %in% c('X86','X87', 'X45')) %>% 
				clean_csv %>% data.table:::fwrite(paste0(dir_file, '17_UNE_2EAP_SEX_AGE_RT.csv'), na = "")	
				
 
 

ref <- bind_rows( 		
						readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds')) %>%  
								select(-country, -UNCODE) %>% distinct(region, .keep_all = TRUE) %>% 
								mutate(UNCODE = str_sub(region,4,-1), country = UNCODE) %>% 
								select(-region) %>% 
								rename(label = region.label)
			)


res <- NEW_MASTER	%>% select(country, sex, classif1, time, value =  UNE_2EAP_SEX_AGE_RT) %>% 
				filter(str_sub(country,3,3) %in% '_') %>% 
				mutate(country = ifelse(str_sub(country,3,3) %in% '_', str_sub(country,4,-1), country)) %>% 
				group_by(country, sex, classif1, time) %>% 
				summarise(value = first(value)) %>% 
				ungroup %>% 
				filter(time >= SDG_TIME[1], time <= SDG_TIME[2]) %>% arrange(country, time, sex, classif1) %>% 
				mutate(
						`Reference area code` = country, 
						`Reference area name` = country, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = value, 
						`Nature of data` = 'M', 
						`Data sources` = 'Ilo estimates, available at http://www.ilo.org/ilostat . For the specific sources by country, refer to ilostat directly.') %>% 
				filter(!country %in% 'TWN')%>% 
				select(-country, -sex, -classif1, -time, -value) %>% 
				mutate(`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			)

			
	#################### add country level data		
			
require(Rilostat)

ref <- bind_rows( 		
						readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/COU_SDG.rds')) %>%  
								select(country, country.label, UNCODE) 
			)

X <- get_ilostat('SDG_0852_SEX_AGE_RT_A') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !classif1 %in% 'AGE_YTHADULT_Y15-64', 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`)



			
			
test <- bind_rows(res, X %>% mutate(`Reference area code` = as.character(`Reference area code`)))	

test %>% data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0852_UNE_DEAP_SEX_AGE_RT.csv'), na = "")




col_order <- test %>% slice(0)
							
rm( NEW_MASTER, res, X, test)						
						
		
X <- bind_rows(col_order %>% rename(Contigency = `Age group`), 
		get_ilostat('SDG_0131_SEX_SOC_RT_A')	 %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], 
				!ref_area %in% 'KOS') %>% 
		switch_ilo( source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Contigency` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Contigency` = `Contigency` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Contigency: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) ) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0131_SEX_SOC_RT.csv'), na = "")
		
		
		
X <- bind_rows(col_order %>% select(-Sex) %>% rename(Occupation = `Age group`), get_ilostat('SDG_0552_OCU_RT_A')	%>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2],  
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						 
						Occupation = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						
						`Occupation` = `Occupation` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label) ) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
			arrange(`Reference area code`, `Time Period (Year)`) %>% 
			data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0552_OCU_RT.csv'), na = "")


			
X <- bind_rows(col_order %>% select(-`Age group`), 
				get_ilostat('SDG_A831_SEX_RT_A')	 %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2],  
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label, -note_classif.label)	) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_A831_SEX_RT.csv'), na = "")





	
X <- bind_rows(col_order %>% select(-`Age group`), get_ilostat('SDG_B831_SEX_RT_A')	 %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2],  
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label, -note_classif.label)	) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_B831_SEX_RT.csv'), na = "")
			
			
		
X <-  bind_rows(col_order %>%  rename(Occupation = `Age group`), 
		get_ilostat('SDG_0851_SEX_OCU_NB_A')	%>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !str_detect(classif1, 'SKILL'),   
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						 Sex = sex,
						Occupation = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -classif1,-sex,  -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						
						`Occupation` = `Occupation` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% 
																			gsub('International Standard Classification of Occupations, 2008', '', ., fixed = TRUE) %>% 
																			gsub('International Standard Classification of Occupations, 1988 ', '', ., fixed = TRUE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 							
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label) 
		) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0851_SEX_OCU_NB.csv'), na = "")



		
X <- bind_rows(col_order %>% select(-`Age group`), 
				get_ilostat('SDG_0861_SEX_RT_A')	 %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2],  
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label, -note_classif.label)	) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0861_SEX_RT.csv'), na = "")








		

X <- bind_rows(col_order,
		get_ilostat('SDG_0871_SEX_AGE_NB_A') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !classif1 %in% 'AGE_YTHADULT_Y15-64', 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_indicator = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label)) %>% 
		mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0871_SEX_AGE_NB.csv'), na = "")

X <- bind_rows(col_order, 
		get_ilostat('SDG_0871_SEX_AGE_RT_A') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !classif1 %in% 'AGE_YTHADULT_Y15-64', 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_indicator = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label)) %>% 
		mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0871_SEX_AGE_RT.csv'), na = "")


	
	
	


X <- bind_rows(col_order  %>% rename(`Migrant status` = `Age group`), 
		get_ilostat('SDG_N881_SEX_MIG_RT_A') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !classif1 %in% 'AGE_YTHADULT_Y15-64', 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Migrant status` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Migrant status` = `Migrant status` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Migrant status: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label)) %>% 
		mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_N881_SEX_MIG_RT.csv'), na = "")


		

		


X <- bind_rows(col_order  %>% rename(`Migrant status` = `Age group`), 
		get_ilostat('SDG_F881_SEX_MIG_RT_A') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], !classif1 %in% 'AGE_YTHADULT_Y15-64', 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Migrant status` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Migrant status` = `Migrant status` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Migrant status: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label)) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_F881_SEX_MIG_RT.csv'), na = "")



		
################# SDG_1041_NOC_RT						[10.4.1] Labour income share as a percent of GDP
X <- bind_rows(col_order %>% select(-`Age group`, -Sex), 
				get_ilostat('SDG_1041_NOC_RT_A')	 %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2],  
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label, -note_classif.label) 	)%>% data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_1041_NOC_RT.csv'), na = "")


############ SDG_0852_SEX_DSB_RT					[8.5.2] Unemployment rate by disability status; from ilostat back office only countries


X <- bind_rows(col_order  %>% rename(`Disability status` = `Age group`), 
		get_ilo(collection = 'SDG', indicator = 'SDG_0852_SEX_DSB_RT') %>% 
		filter(!str_sub(ref_area,1,1) %in% 'X', time >= SDG_TIME[1], time <= SDG_TIME[2], 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Disability status` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = NA) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Disability status` = `Disability status` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Disability status: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label)) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0852_SEX_DSB_RT.csv'), na = "")

						
				

}


SDG_0821_GDP_211P_NOC_RT <- function(){



load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

GET <- read_dta(file_get_gdp) %>% mutate(year = as.numeric(year)) %>% select(ilo_code, time = year, GDP_205U_NOC_NB = prdUSD, GDP_211P_NOC_NB = prd) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
			left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code), by = "ilo_code") 		%>% 
					select(-ilo_code) 

					
GDP <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15', sex %in% 'SEX_T') %>% 
				left_join(GET, by = c("country", "time")) %>% 
				rename(EMP = EMP_2EMP_SEX_AGE_NB ) %>% 
				mutate(	classif1 = 'NOC_VALUE', 
						sex = NA) %>% 
				select(-source, -collection) %>% 
				filter(!str_sub(country,3,3) %in% '_') ###################### ADD for SDG
rm(NEW_MASTER)					
		
					
############### calculate regional aggregate

		
ilo:::init_ilo(-cl)

REF_AGG <- readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds'))
		
	
AGG <- NULL														# set result dataset to empty
test <- unique(REF_AGG$region) %>% sort							# prepare loop on region
for (i in 1:length(test)){
ref <- REF_AGG %>% 
		filter(region %in% test[i]) %>% 
		select(country) %>% t %>% as.character					# store relevant country list
		
AGG <-  GDP %>% 
		filter(country %in% ref) %>% 							# filter country i
		group_by( time, sex, classif1) %>%# group 	
		summarise(	country = test[i], 							# rename country = region
					GDP_205U_NOC_NB = sum((GDP_205U_NOC_NB * EMP), na.rm = TRUE) / sum(EMP , na.rm = TRUE),
					GDP_211P_NOC_NB = sum((GDP_211P_NOC_NB * EMP), na.rm = TRUE) / sum(EMP , na.rm = TRUE)
					
					) %>% 		# sum
		ungroup %>% bind_rows(AGG) 								# ungroup
rm(ref)
}	
rm(test)
					
	GDP <- GDP %>% 
		bind_rows(AGG) %>% 										# 
		arrange(country, time, sex, classif1)					# resort
rm(AGG)

GDP <- GDP %>% select(-EMP) %>% gather(indicator, value, -time, -country, -sex,  -classif1)
	



GDP <- GDP %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)


ref <- bind_rows( 	readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/COU_SDG.rds')) %>% rename(label = country.label) %>% mutate(UNCODE = as.character(UNCODE)),
				readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds')) %>%  select(-country, -UNCODE) %>% distinct(region, .keep_all = TRUE) %>% mutate(UNCODE = str_sub(region,4,-1), country = UNCODE) %>% select(-region) %>% rename(label = region.label)
			)


GDP <- GDP	%>% filter(indicator %in% 'GDP_205U_NOC_NB') %>% 
				select(country, time, value) %>% 
				mutate(country = ifelse(str_sub(country,3,3) %in% '_', str_sub(country,4,-1), country)) %>% 
				group_by(country, time) %>% 
				summarise(value = first(value)) %>% 
				ungroup %>% 
				filter(time >= SDG_TIME[1] - 1, time <= SDG_TIME[2]) %>% 
				arrange(country, time) 
				
GDP_t <-  	GDP %>% filter(time >= SDG_TIME[1] , time <= SDG_TIME[2]) 	# get 2000 + 
GDP_tminus <-  	GDP %>% filter(time >= SDG_TIME[1]-1 , time <= SDG_TIME[2] -1)	%>% 	# get 1999 + 
				mutate(time = time + 1) %>% rename(tminus1 = value) 	

TEST <- left_join(GDP_t, GDP_tminus, by = c("country", "time"))	%>% mutate(value = (value - tminus1) / tminus1 * 100) %>% select(-tminus1)			
	
TEST <- TEST %>% mutate(
						`Reference area code` = country, 
						`Reference area name` = country, 
						`Time Period (Year)` = as.character(time), 
						
						`Observation value` = value, 
						`Nature of data` = 'M', 
						`Data sources` = 'Ilo estimates, available at http://www.ilo.org/ilostat . For the specific sources by country, refer to ilostat directly.') %>% 
				filter(!country %in% 'TWN')%>% 
				select(-country, -time, -value) %>% 
				mutate(`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$label), 
						 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			)  %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% arrange(`Reference area code`, `Time Period (Year)`)
			
TEST	%>% data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0821_GDP_205U_NOC_RT.csv'), na = "")
	
rm(GDP, GDP_t,GDP_tminus, TEST)	

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))	

}				
				
	
SDG_0111_EMP_2EMP_SEX_AGE_CLA_DT <- function(){

load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

GET <- read_dta(file_get_class)  %>% select(country = iso3code, time = year, MF_T_class1P:F_A_class14P, STEP1) %>%  filter( time <= max(time_get), time >= min(time_get) ) %>% 
			gather(ref, value, -country, -time, -STEP1) %>% 
			separate(ref, c('sex','classif1','classif2'), sep = '_')
			

	
	

EMP_2EMP_SEX_AGE_CLA_NB <- GET %>% filter(!classif2 %in% 'EMP') %>% 
				mutate(classif2 = classif2 %>% 						# mapping classif2
									plyr::mapvalues(from = c("class1P",  "class2P",  "class3P",  "class4P",  "class5P",  "class6P",  "class7P",  "class8P",  "class9P",  "class10P", "class11P", "class12P", "class13P", "class14P"),
											to = c(	'CLA_ECOCLA_USDLT2',
													'CLA_ECOCLA_USDGE2LT3', 'CLA_ECOCLA_USDGE2LT3', 
													'CLA_ECOCLA_USDGE3LT5', 'CLA_ECOCLA_USDGE3LT5', 
													'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5', 'CLA_ECOCLA_USDGE5'))
								) %>% 
				group_by(country, time, sex, classif1, classif2) %>% summarise(value = sum(value, na.rm = TRUE), STEP1 = first(STEP1)) %>% ungroup  %>% 
				mutate(	sex 	= 	sex %>% 							# mapping sex
							plyr::mapvalues(from = c('MF','M','F'),
											to = c('SEX_T','SEX_M','SEX_F')), 
				classif1=	classif1 %>% 						# mapping classif1
							plyr::mapvalues(from = c('T','Y','A'),
											to = c('AGE_YTHADULT_YGE15','AGE_YTHADULT_Y15-24', 'AGE_YTHADULT_YGE25'))
					) %>% spread(classif2, value)
					
					

		
EMP_2EMP_SEX_AGE_CLA_NB <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X') %>% 
				right_join(EMP_2EMP_SEX_AGE_CLA_NB	, by = c("country", "time", 'sex', 'classif1')) %>% 
				rename(CLA_ECOCLA_TOTAL = EMP_2EMP_SEX_AGE_NB ) %>% select(-source, -collection) %>% 
				mutate(
						CLA_ECOCLA_USDLT2 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDLT2/100,
						CLA_ECOCLA_USDGE2LT3 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE2LT3/100,
						CLA_ECOCLA_USDGE3LT5 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE3LT5/100,
						CLA_ECOCLA_USDGE5 = CLA_ECOCLA_TOTAL * CLA_ECOCLA_USDGE5/100) %>% 
				gather(classif2, value, -country, -sex, -classif1, -time, -STEP1) %>%
				arrange(country, time, sex, classif1, classif2)	%>% 
				rename(obs_status = STEP1)	%>% 
				mutate(obs_status = ifelse(obs_status %in% 1, 'R', obs_status))
					

###################### ok we have country level but only 138 country, now have to fill gaps for EMP in order to get 188 country
				

GAP <- NEW_MASTER %>% 
				select(collection:time, value = EMP_2EMP_SEX_AGE_NB, -source, -collection) %>% 
				filter( !substr(country,1,1) %in% 'X', !country %in% unique(EMP_2EMP_SEX_AGE_CLA_NB$country)) 


rm(NEW_MASTER)
				
GET_NB_COU <- EMP_2EMP_SEX_AGE_CLA_NB %>% 
			bind_rows( 	GAP %>% mutate(classif2 = 'CLA_ECOCLA_TOTAL'), 
						GAP %>% mutate(classif2 = '	CLA_ECOCLA_USDGE3'))

								

GET_NB_COU <- GET_NB_COU %>% mutate(classif2 = classif2 %>% 						# mapping classif2
							plyr::mapvalues(from = c('CLA_ECOCLA_USDLT2',
													 'CLA_ECOCLA_USDGE2LT3', 
													 'CLA_ECOCLA_USDGE3LT5', 
													 'CLA_ECOCLA_USDGE5'),
											to = c('CLA_ECOCLA_USDLT2',
													 'CLA_ECOCLA_USDGE2LT3', 
													 'CLA_ECOCLA_USDGE3', 
													 'CLA_ECOCLA_USDGE3')), 
							indicator = 'EMP_2EMP_SEX_AGE_CLA_NB') %>% 
							filter(!str_sub(country,3,3) %in% '_') ######## add for SDG
							

REF_AGG <- readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds'))
	
AGG <- NULL														# set result dataset to empty
test <- unique(REF_AGG$region) %>% sort							# prepare loop on region
for (i in 1:length(test)){
ref <- REF_AGG %>% 
		filter(region %in% test[i]) %>% 
		select(country) %>% t %>% as.character					# store relevant country list
		
AGG <-  GET_NB_COU %>% 
		filter(country %in% ref) %>% 							# filter country i
		group_by(indicator, time, sex, classif1, classif2) %>%# group 	
		summarise(	country = test[i], 							# rename country = region
					value = sum(value, na.rm = TRUE)) %>% 		# sum
		ungroup %>% bind_rows(AGG) 								# ungroup
rm(ref)
}	
rm(test, GET_NB_COU, REF_AGG, GAP)

#################################### add benchmark country with STEP1


EMP_2EMP_SEX_AGE_CLA_NB <- EMP_2EMP_SEX_AGE_CLA_NB %>% filter(obs_status %in% 'R') %>% ####################### remove inputed value
								bind_rows(AGG) %>% 										# 
								arrange(country, time, sex, classif1, classif2)					# resort
rm(AGG)





EMP_2EMP_SEX_AGE_CLA_NB <- EMP_2EMP_SEX_AGE_CLA_NB %>%  
						mutate(collection = 'ILOEST')%>%
						left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
										filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
										select(source = code, country = label_en) %>%
										mutate(	country = str_sub(country,1,3)), 
										source = as.character(source), 
										by = 'country') %>% 
			 arrange(collection, country, source, indicator, time, sex, classif1, classif2) %>% mutate(indicator = 'EMP_2EMP_SEX_AGE_CLA_NB') 



EMP_2EMP_SEX_AGE_CLA_DT <- EMP_2EMP_SEX_AGE_CLA_NB %>%  filter(classif2 %in% c('CLA_ECOCLA_TOTAL', 'CLA_ECOCLA_USDLT2')) %>% 
					group_by(collection, country, source, time, indicator, sex, classif1) %>% 
					summarise(count = n(), value = last(value) / first(value) * 100, ) %>% ungroup %>% 
					mutate(indicator = 'EMP_2EMP_SEX_AGE_CLA_DT', 
							value = ifelse(count %in% 1, 0, value)) %>% select(-count)
	
ref <- bind_rows( 	readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/COU_SDG.rds')) %>% rename(label = country.label) %>% mutate(UNCODE = as.character(UNCODE)),
			readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds')) %>%  select(-country, -UNCODE) %>% distinct(region, .keep_all = TRUE) %>% mutate(UNCODE = str_sub(region,4,-1), country = UNCODE) %>% select(-region) %>% rename(label = region.label)
			)

ALL <- EMP_2EMP_SEX_AGE_CLA_DT %>% count(country, sex, classif1, time) %>% ungroup %>% mutate(classif2 = 'CLA_ECOCLA_USDLT2') %>% 
							mutate(country = ifelse(str_sub(country,3,3) %in% '_', str_sub(country,4,-1), country))  %>% 
							filter(time >= SDG_TIME[1], time <= SDG_TIME[2]) %>% select(-n)


NEW <- EMP_2EMP_SEX_AGE_CLA_DT	%>% select(country, sex, classif1, time, value ) %>% 
				mutate(country = ifelse(str_sub(country,3,3) %in% '_', str_sub(country,4,-1), country)) %>% group_by(country, sex, classif1, time) %>% summarise(value = first(value)) %>% ungroup %>% 
				filter(time >= SDG_TIME[1], time <= SDG_TIME[2]) %>% arrange(country, time, sex, classif1) 
				
DIST <- left_join(ALL,NEW , by = c("country", "sex", "classif1", "time")) %>% arrange(country, time, sex, classif1)	%>% filter(!value %in% NA)		
				
				
				
				
DIST <- 		DIST %>% 	select(-classif2 ) %>% 
					mutate(
						`Reference area code` = country, 
						`Reference area name` = country, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = value, 
						`Nature of data` = 'M', 
						`Data sources` = 'Ilo estimates, available at http://www.ilo.org/ilostat . For the specific sources by country, refer to ilostat directly.') %>% 
				filter(!country %in% 'TWN') %>% 
				select(-country, -sex, -classif1, -time, -value) %>% 
				mutate(`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(	from = as.character(ilo$code$cl_classif$code), 
																								to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2))
						
						
DIST %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>%		
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0111_EMP_2EMP_SEX_AGE_CLA.csv'), na = "")
						
rm(EMP_2EMP_SEX_AGE_CLA_DT, EMP_2EMP_SEX_AGE_CLA_NB,ref,ALL,NEW,DIST)						
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))


}
			

SDG_0922_EMP_ECO_MAN_RT <- function(){


load(paste0(ilo:::path$data,'REP_ILO/ILOEST/input/temp/NEW_MASTER.Rdata'))

ref <- bind_rows(
				readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/COU_SDG.rds')) %>%  
								select(country, country.label, UNCODE) , 
				readRDS(paste0(ilo:::path$query,'REP_ILO/SDG/help/GRP_SDG.rds')) %>%  
								select(-country, -UNCODE) %>% distinct(region, .keep_all = TRUE) %>% 
								mutate(UNCODE = as.numeric(str_sub(region,4,-1)), country = as.character(UNCODE)) %>% 
								select(-region) %>% 
								rename(country.label = region.label)
			)

GET <- read_dta(file_get_eco_aggregate) %>% select(ilo_code, sex = sexid, real, time = year, EMP1shP:EMP6shP, dummy1:dummy6) %>% filter( time <= max(time_get), time >= min(time_get) ) %>% 
		left_join(	ilo$code$cl_country %>% 					# add iso code 3 for country
					select(ilo_code, country = code) %>% filter(!ilo_code %in% NA) , by = "ilo_code") 		%>% 
					select(-ilo_code) %>% mutate(sex = as.character(sex) %>% plyr:::mapvalues(from = c('1','2','3'), to = c('SEX_T','SEX_M','SEX_F')))


EMP_ECO_AGGREGATE <- NEW_MASTER %>% 
				select(collection:time, EMP_2EMP_SEX_AGE_NB) %>% 
				filter( !substr(country,1,1) %in% 'X',  
						classif1 %in% 'AGE_YTHADULT_YGE15') %>% 
				left_join(GET, by = c("country", "time", 'sex')) %>% 
				mutate(
						ECO_AGGREGATE_AGR = paste0(EMP_2EMP_SEX_AGE_NB * (EMP1shP)/100,' ', dummy1), 
						ECO_AGGREGATE_MAN = paste0(EMP_2EMP_SEX_AGE_NB * (EMP3shP)/100,' ', dummy2), 
						ECO_AGGREGATE_CON = paste0(EMP_2EMP_SEX_AGE_NB * (EMP2shP)/100,' ', dummy3),
						ECO_AGGREGATE_MEL = paste0(EMP_2EMP_SEX_AGE_NB * (EMP4shP)/100,' ', dummy4), 
						ECO_AGGREGATE_MKT = paste0(EMP_2EMP_SEX_AGE_NB * (EMP5shP)/100,' ', dummy5), 
						ECO_AGGREGATE_PUB = paste0(EMP_2EMP_SEX_AGE_NB * (EMP6shP)/100,' ', dummy6),
						ECO_AGGREGATE_TOTAL = paste0(EMP_2EMP_SEX_AGE_NB),
						
						 ) %>% 
				select(country, sex, time, contains('ECO_AGGREGATE')) %>% 
				gather(classif1, val, -country, -time, -sex) %>% 
				separate(val, c('value', 'obs_status'), fill = 'right', sep = ' ', remove = TRUE) %>% 
				mutate(	value = as.numeric(value), 
						obs_status = ifelse(obs_status %in% '1', 'R', NA))
						
EMP_ECO_AGGREGATE <- EMP_ECO_AGGREGATE	 %>% mutate(classif2 = as.character(NA), indicator = 'EMP_2EMP_SEX_ECO_NB') 

EMP_ECO_AGGREGATE <- add_aggregate(EMP_ECO_AGGREGATE, use_SDG = TRUE) %>% 
				filter(sex %in% 'SEX_T', classif1 %in% c('ECO_AGGREGATE_MAN', 'ECO_AGGREGATE_TOTAL'))

					
						



EMP_2EMP_SEX_ECO_NB <- EMP_ECO_AGGREGATE %>%  mutate(collection = 'ILOEST')%>%
		left_join(	ilo$code$cl_survey %>% 						# prepare mapping of source
						filter(str_sub(code,1,2) %in% 'XA', label_en %>% str_detect('ILO Estimates and Projections')) %>% 
						select(source = code, country = label_en) %>%
						mutate(	country = str_sub(country,1,3)), 
								source = as.character(source), by = 'country') %>% 
			arrange(country, time, sex, classif1)

rm(GET, EMP_ECO_AGGREGATE)			


EMP_2EMP_SEX_ECO_NB <- EMP_2EMP_SEX_ECO_NB %>% switch_ilo(version) %>%   group_by(collection, country, source, time, indicator, sex, classif1_version) %>% 
					mutate(classif1 = classif1, value = value / max(value) * 100) %>% ungroup %>% 
					mutate(indicator = 'SDG_0922_NOC_RT') %>% select(-classif1_version, -sex_version, -classif2_version) %>%
					filter(classif1 %in%  'ECO_AGGREGATE_MAN') %>% 
					mutate(classif1 = 'NOC_VALUE', time = as.character(time))
					

X <- bind_rows(EMP_2EMP_SEX_ECO_NB %>% rename(obs_value = value, ref_area = country), get_ilostat('SDG_0922_NOC_RT_A'))  %>% 
		mutate(ref_area = ifelse(str_sub(ref_area,3,3) %in% '_', str_sub(ref_area,4,-1), ref_area)) %>% 
		filter(time >= SDG_TIME[1], time <= SDG_TIME[2], 
				!ref_area %in% 'KOS') %>% 
		mutate(obs_status = NA, note_classif = NA) %>% 
		switch_ilo(note_source, source) %>% 
		select(-indicator, -collection) %>% 
		mutate(
						`Reference area code` = ref_area, 
						`Reference area name` = ref_area, 
						`Time Period (Year)` = as.character(time), 
						Sex = sex, 
						`Age group` = classif1, 
						`Observation value` = obs_value, 
						`Nature of data` = 'R', 
						`Data sources` = ifelse(source.label %in% NA, 'Ilo estimates, available at http://www.ilo.org/ilostat . For the specific sources by country, refer to ilostat directly.', NA)) %>% 
		filter(!ref_area %in% 'TWN')%>% 
				select(-ref_area, -sex, -classif1, -time, -obs_value) %>% 
				mutate(	`Reference area code` = `Reference area code` 	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$UNCODE,  warn_missing = FALSE),
						`Reference area name` = `Reference area name`	%>% plyr:::mapvalues(from = ref$country, 
																					to = ref$country.label,  warn_missing = FALSE), 
						Sex = Sex										%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Sex: ', '', ., fixed = TRUE), 
						`Age group` = `Age group` 						%>% plyr:::mapvalues(from = as.character(ilo$code$cl_classif$code), 
																					to = as.character(ilo$code$cl_classif$label_en),  warn_missing = FALSE) %>% gsub('Youth and adults: ', '', ., fixed = TRUE), 
						`Observation value` = `Observation value` %>% as.numeric %>% round(.,2)
			) %>% select(-obs_status.label) %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		select_(.dots = c("`Reference area code`",  "`Reference area name`", "`Time Period (Year)`",'`Observation value`', "`Nature of data`", "`Data sources`", "source.label", "note_classif.label", "note_indicator.label", "note_source.label"))  %>% mutate(`Reference area code` = as.numeric(`Reference area code`)) %>% 
		arrange(`Reference area code`, `Time Period (Year)`) %>% 
		data.table:::fwrite(paste0(ilo:::path$query,'REP_ILO/SDG/output/', 'SDG_0922_NOC_RT.csv'), na = "")
		


}
		








			
			
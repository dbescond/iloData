


#' data from: http://data.imf.org
#' then sign in with user: bescond@ilo.org
#' pwd = Ilostat123+++
#' then go to Bulkdownload or application (Download IMF data section)
#' then click on 'Consumer Price Index (CPI)'
#' select Data format Flat

init_time <- Sys.time() 
cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }
require(Ariane,quietly =TRUE)
require(lubridate, quietly =TRUE)
require(RSelenium, quietly =TRUE)
require(readxl,quietly =TRUE)

setwd(paste0(ilo:::path$data, '/REP_IMF/CPI/'))

Sys.setenv(http_proxy="")
Sys.setenv(ftp_proxy="")
source(paste0(ilo:::path$data, 'REP_IMF/CPI/','do/REP_IMF.CPI_functions.r'))


download_data_CPI()



require(ilo)
init_ilo(-cl)
X <- read_csv('./input/IMF_CPI.csv') %>% select(-X9) 


colnames(X) <- c('IMF.label',  'IMF.code', 'indicator.label', 'indicatorIMF', 'time', 'obs_value', 'obs_status',  'comments')


# Map_Indicator <- X %>% count(indicator.label, indicatorIMF) %>% 
		# separate(indicatorIMF, c('clas', 'ind'), sep = '_', remove = FALSE, extra = 'merge') %>% 
		# mutate(ind = paste0('PCPI_', ind)) %>% 
		# mutate(indicator = ind %>% plyr:::mapvalues( from = c('PCPI_IX','PCPI_PC_PP_PT','PCPI_PC_CP_A_PT','PCPI_WT_PT'), 
												# to = c('CPI_NCPI_COI_IN','CPI_NCPD_COI_RT','CPI_NCYR_COI_RT','CPI_NWGT_COI_DT'), warn_missing = FALSE)) %>%
		# filter(str_sub(indicator, 1,4) %in% 'CPI_') %>%
		# mutate(clas = paste0(clas, '_'), 
					# classif1 = clas %>% plyr:::mapvalues( from = c('PCPIF_','PCPIFBT_','PCPIA_','PCPIH_', 'PCPIHO_', 'PCPIM_', 'PCPIT_', 'PCPIEC_', 'PCPIR_', 'PCPIED_', 'PCPIRE_', 'PCPIO_', 'PCPI_'), 
												# to = c('COI_COICOP_CP01','COI_COICOP_CP02','COI_COICOP_CP03', 'COI_COICOP_CP04', 'COI_COICOP_CP05', 'COI_COICOP_CP06', 'COI_COICOP_CP07', 'COI_COICOP_CP08',
														# 'COI_COICOP_CP09', 'COI_COICOP_CP10', 'COI_COICOP_CP11', 'COI_COICOP_CP12', 'COI_COICOP_CP01T12'
												# ), warn_missing = FALSE)) %>% 
		# filter(str_sub(classif1,1,10) %in% 'COI_COICOP') %>% save_ilo()


# Map_Country <- X %>% count(IMF.label, IMF.code)


# init_ilo()	
# REF <- ilo$code$cl_country %>% filter(!str_sub(code,1,1) %in% 'X') %>% mutate_all(as.character) %>% mutate(code_iso3n = as.numeric(code_iso3n), TEST = tolower(str_sub(label_en,1,6)))




# Map_Country <- Map_Country %>% mutate(TEST = tolower(str_sub(IMF.label,1,6))) %>%
# left_join(select(REF,TEST, label_en, code ))

# Map_Source <- Ariane:::CODE_ORA$T_SUR_SURVEY %>% filter(SUR_SOURCE_CODE %in% 'GA') %>% filter(SUR_SORT %in% '60')


Map_Country <- readxl:::read_excel("./input/Ref_Mapping_Country.xlsx")

Map_Indicator <- readxl:::read_excel("./input/Ref_Mapping_Indicator.xlsx")



Y <- X %>% 	left_join(select(Map_Country, ref_area = code, source, IMF.label, IMF.code), by = c("IMF.label", "IMF.code")) %>% 
			select(-IMF.label, -IMF.code, -indicator.label) %>% 
			left_join(select(Map_Indicator, -indicator.label, -n), by = c("indicatorIMF")) %>% filter(!indicator %in% NA) %>% select(-indicatorIMF, -clas, -ind) %>% mutate(obs_status = NA) %>% 
			arrange(ref_area, source, time, indicator, classif1)	%>% 
			select(ref_area, source, indicator, classif1, time, obs_value, obs_status, comments) %>% 
			filter(!obs_value %in% NA) %>% 
			# switch_ilo(ref_area, source, indicator, keep) %>% 
			mutate(note_source = 'R1:3139') # add IMF repo notes
	


# Y %>% count(indicator)

	
			
# ind1_A <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% '') %>% spread(classif1, obs_value)			
# ind1_Q <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% 'Q') %>% spread(classif1, obs_value)			
# ind1_M <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% 'M') %>% spread(classif1, obs_value)			
# save_ilo(ind1_A = 'A_CPI_NCPD_COI_RT',ind1_Q = 'Q_CPI_NCPD_COI_RT', ind1_M = 'M_CPI_NCPD_COI_RT')
			
# ind2_A <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% '') %>% spread(classif1, obs_value)			
# ind2_Q <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% 'Q') %>% spread(classif1, obs_value)			
# ind2_M <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% 'M') %>% spread(classif1, obs_value)			
# save_ilo(ind2_A = 'A_CPI_NCPI_COI_IN',ind2_Q = 'Q_CPI_NCPI_COI_IN', ind2_M = 'M_CPI_NCPI_COI_IN')
				
# ind3_A <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% '') %>% spread(classif1, obs_value)			
# ind3_Q <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% 'Q') %>% spread(classif1, obs_value)			
# ind3_M <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% 'M') %>% spread(classif1, obs_value)			
# save_ilo(ind3_A = 'A_CPI_NCYR_COI_RT',ind3_Q = 'Q_CPI_NCYR_COI_RT', ind3_M = 'M_CPI_NCYR_COI_RT')
		
##ind4_A <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% '') %>% spread(classif1, obs_value)			
# ind4_Q <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% 'Q') %>% spread(classif1, obs_value)			
# ind4_M <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% 'M') %>% spread(classif1, obs_value)			
# save_ilo(ind4_Q = 'Q_CPI_NWGT_COI_DT', ind4_M = 'M_CPI_NWGT_COI_DT')
					
			
			
# Y <- 	Y %>% mutate(sex = NA, classif2 = NA) %>% switch_ilo(classif1, keep) %>% select(-sex, -sex.label, -classif2, -classif2.label)		
			
# ind1_A <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% '') %>% spread(time, obs_value)			
# ind1_Q <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% 'Q') %>% spread(time, obs_value)			
# ind1_M <- Y %>% filter(indicator %in% 'CPI_NCPD_COI_RT' , str_sub(time,5,5) %in% 'M') %>% spread(time, obs_value)			
# save_ilo(ind1_A = 'A_CPI_NCPD_COI_RT',ind1_Q = 'Q_CPI_NCPD_COI_RT', ind1_M = 'M_CPI_NCPD_COI_RT')
			
# ind2_A <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% '') %>% spread(time, obs_value)			
# ind2_Q <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% 'Q') %>% spread(time, obs_value)			
# ind2_M <- Y %>% filter(indicator %in% 'CPI_NCPI_COI_IN' , str_sub(time,5,5) %in% 'M') %>% spread(time, obs_value)			
# save_ilo(ind2_A = 'A_CPI_NCPI_COI_IN',ind2_Q = 'Q_CPI_NCPI_COI_IN', ind2_M = 'M_CPI_NCPI_COI_IN')
				
# ind3_A <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% '') %>% spread(time, obs_value)			
# ind3_Q <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% 'Q') %>% spread(time, obs_value)			
# ind3_M <- Y %>% filter(indicator %in% 'CPI_NCYR_COI_RT' , str_sub(time,5,5) %in% 'M') %>% spread(time, obs_value)			
# save_ilo(ind3_A = 'A_CPI_NCYR_COI_RT',ind3_Q = 'Q_CPI_NCYR_COI_RT', ind3_M = 'M_CPI_NCYR_COI_RT')
		
##ind4_A <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% '') %>% spread(classif1, obs_value)			
# ind4_Q <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% 'Q') %>% spread(time, obs_value)			
# ind4_M <- Y %>% filter(indicator %in% 'CPI_NWGT_COI_DT' , str_sub(time,5,5) %in% 'M') %>% spread(time, obs_value)			
# save_ilo(ind4_Q = 'Q_CPI_NWGT_COI_DT', ind4_M = 'M_CPI_NWGT_COI_DT')
					
				
			
	
  ref_file <- unique(Y$ref_area)
  
  
  for (i in 1:length(ref_file)){
    saveRDS(Y %>% filter(ref_area %in% ref_file[i]), file = paste0("./output/",ref_file[i],".rds"))
  }
  
	ref <- cbind(PATH = paste0(getwd(),"/output/",ref_file,".rds"),ID = NA, Types  ="CL", REF = paste0(ref_file)) %>% as_data_frame
	data.table:::fwrite(ref,paste0("./FileToLoad.csv"))
  rm(X, Y, ref_file)


final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)		
		
		
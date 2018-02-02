
init_time <- Sys.time() 
Sys.time() 

# require(plyr)
require(stringr)
require(tidyverse)
require(iloMicro)

# Micro_file_ready()

setwd('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/input/TEST_DTA')
ref <- list.files() %>% as_data_frame %>% filter(stringr:::str_detect(value, 'EULFS') )%>% t %>% as.character



# test
# ilo_joball_tru 
# ilo_olf_dlma
# ilo_lfs

X <- NULL
for (i in 1:length(ref)){


load(ref[i])


test_name <- ts_model %>% names 
test_name <- test_name[str_detect(test_name, 'ilo_age_aggregate_A')]
if(length(test_name) > 0){
print(ref[i])
X <- bind_rows(X, attributes(ts_model[[test_name[1]]])$tsp  %>% as_data_frame %>% bind_cols(c('min', 'max', 'count')%>% as_data_frame %>% rename(test = value)) %>% mutate(file = ref[i]))

}

rm(ts_model)

}


X %>% spread(test, value) %>% 
			separate(file, c('country', 'source'), extra = 'drop') %>% 
			mutate(min = str_sub(min, 1, 4), max = str_sub(max, 1,4)) %>% 
			select(-count) %>% ilo:::save_ilo()



Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)





################ check EULFS data



require(tidyverse)
require(stringr)
require(ilo)
init_ilo()

##################################################################
##################################################################
################################################################## TEST EULFS
##################################################################
##################################################################

ref_mapping <- readxl::read_excel('J:/COMMON/STATISTICS/DPAU/QUERY/REP_UNESCO/UIS/input/UNESCO_NEET_tabulation-layout_2017.09.19.xlsx', 'mapping') %>% filter(str_detect(indicator, 'UN1_|UN2_|UN3_'))

ref_files <- read_csv('J:/COMMON/STATISTICS/DPAU/MICRO/_Admin/CMD/FileToLoad_toCheck.csv') %>% filter(str_detect(PATH, 'EULFS') )

################### yet clean
X <- as.list(ref_files$PATH) %>% 
			plyr:::ldply(function(x) {load(x); return(X)}) %>% 
			as.tbl %>% 
			filter(	indicator %in% 'POP_XWAP_SEX_AGE_NB', classif1 %in% 'AGE_AGGREGATE_TOTAL', sex %in% 'SEX_T') %>% distinct(ref_area, time, obs_value)
			
	
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
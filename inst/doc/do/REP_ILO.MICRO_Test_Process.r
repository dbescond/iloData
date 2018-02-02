
init_time <- Sys.time() 
Sys.time() 

require(plyr)
require(tidyverse)
require(haven)
require(stringr)
require(iloMicro)

# prepare dta test


X <- read_csv(paste0(ilo:::path$micro, '_Admin/CMD/FileToLoad_toCheck.csv')) %>% 
		select(PATH) %>% 
		mutate(ref = PATH %>% stringr::str_replace('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/output/', ''))%>% 
		tidyr::separate(ref, c('country', 'source'), sep = '/', extra = "drop") %>% select(country, source)


for (i in 1:nrow(X)){



a <- try(
Micro_check_dta_series(country = X$country[i], source = X$source[i], validate = TRUE, 	saveCSV = TRUE)
								
	, silent = TRUE)
if(str_sub(X, 1,5) %in% 'Error') print(paste0('Error with: ', X$country[i],' / ', source = X$source[i]))
	

}



#  Micro_check_dta_series(country = 'BEL', source = 'EULFS', validate = TRUE, saveCSV = TRUE)

# Micro_check_dta_series_view(country = 'BEL', source = 'EULFS')


Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
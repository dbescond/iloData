
init_time <- Sys.time() 
Sys.time() 

require(stringr)
require(tidyverse)
require(iloMicro)
require(ilo)

init_ilo()






# Micro_file_ready()

Micro_backup_cmd('H:/_Sys/packages/')

Micro_process(collection = 'STI', print_ind = FALSE, validate = TRUE, saveCSV = FALSE, consolidate = '23')


# copy for production:
X <- read_csv(paste0(ilo:::path$micro, '_Admin/CMD/FileToLoad.csv'), col_types = cols_only(
																				PATH = col_character(),
																				ID = col_character(),
																				Types = col_character(),
																				REF = col_character()
																))
X %>% data.table:::fwrite(file =paste0(ilo:::path$data, 'REP_ILO/MICRO/FileToLoad.csv'))		



Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
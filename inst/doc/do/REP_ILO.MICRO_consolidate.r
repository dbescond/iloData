
init_time <- Sys.time() 
Sys.time() 

require(iloMicro)

Micro_process_all(skip = 'EULFS', PUB = TRUE)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
# copy for production:
X <- read_csv(paste0(ilo:::path$micro, '_Admin/CMD/FileToLoad.csv'), col_types = cols_only(
																				PATH = col_character(),
																				ID = col_double(),
																				Types = col_character(),
																				REF = col_character()
																))
X %>% data.table:::fwrite(file =paste0(ilo:::path$data, 'REP_ILO/MICRO/FileToLoad.csv'))		

rm(X)

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

Micro_file_ready(DDI = TRUE)


Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}


rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)



init_time <- Sys.time() 
Sys.time() 

# require(plyr)
require(stringr)
#require(tidyverse)
require(iloMicro)

# Micro_file_ready()

Micro_backup_cmd('H:/_Sys/packages/')

workflow <- Micro_get_workflow() %>% filter(processing_status %in% c('Published'), CC_ilostat %in% 'Yes', !freq_code %in% NA, !source %in% 'EULFS') %>% distinct(ref_area, source)

for (i in 1:nrow(workflow)){

	print(paste0(workflow$ref_area[i],"_",workflow$source[i], "/ ", Sys.time() ))

	run <- paste0("Micro_process(collection = 'STI', print_ind = FALSE, validate = TRUE, saveCSV = FALSE, consolidate = '12' ,ref_area  = '",workflow$ref_area[i],"', source = '",workflow$source[i],"' )")

	TEST <- try(eval(parse(text = run)), silent = TRUE)

	if(stringr:::str_sub(TEST[1], 1, 5) %in% 'Error') {print('Error found')}

}


Micro_process_all()





# copy for production:
X <- read_csv(paste0(ilo:::path$micro, '_Admin/CMD/FileToLoad.csv'), col_types = cols_only(
																				PATH = col_character(),
																				ID = col_character(),
																				Types = col_character(),
																				REF = col_character()
																))
X %>% data.table:::fwrite(file =paste0(ilo:::path$data, 'REP_ILO/MICRO/FileToLoad.csv'))		



Micro_file_ready(DDI = TRUE)


Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)


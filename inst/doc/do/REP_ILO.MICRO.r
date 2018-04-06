
init_time <- Sys.time() 
Sys.time() 

# require(plyr)
require(stringr)
#require(tidyverse)
require(iloMicro)


workflow <- Micro_get_workflow() %>% filter(processing_status %in% c('Published'), CC_ilostat %in% 'Yes', !freq_code %in% NA, !source %in% 'EULFS') %>% distinct(ref_area, source)

workflow <- Ariane::splitCountryMicro(workflow, commandArgs(TRUE))

workflow$ref_area

for (i in 1:nrow(workflow)){

	print(paste0(workflow$ref_area[i],"_",workflow$source[i], "/ ", Sys.time() , '/ part 12'))

	run <- paste0("Micro_process(collection = 'STI', print_ind = FALSE, validate = TRUE, saveCSV = FALSE, consolidate = '12' ,ref_area  = '",workflow$ref_area[i],"', source = '",workflow$source[i],"' )")

	TEST <- try(eval(parse(text = run)), silent = TRUE)

	if(stringr:::str_sub(TEST[1], 1, 5) %in% 'Error') {print('Error found')}

}

Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)


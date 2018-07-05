
init_time <- Sys.time() 
Sys.time() 

# require(plyr)
require(stringr)
#require(tidyverse)
require(iloMicro)

  workflow <- Micro_get_workflow() %>% filter(processing_status %in% c('Published'), CC_ilostat %in% 'Yes', !freq_code %in% NA, !source %in% 'EULFS') %>% distinct(ref_area, source)
# workflow <- Micro_get_workflow() %>% filter(processing_status %in% c('Published'), CC_ilostat %in% 'Yes', !freq_code %in% NA) %>% distinct(ref_area, source)

  ref_wf <- readr:::read_csv(paste0(ilo:::path$data, 'REP_ILO/MICRO/FileToLoad.csv'), col_type = cols(
																									PATH = col_character(),
																									ID = col_integer(),
																									Types = col_character(),
																									REF = col_character(),
																									drop = col_character())) %>% 
			mutate(PATH  = gsub('J:/DPAU/DATA/REP_ILO/MICRO/output/', '', PATH, fixed = TRUE)) %>% 
			separate(PATH, c('ref_area', 'source'), sep = '/', extra = 'drop') %>% select(ref_area, source, ID)
  

  workflow <- workflow %>% left_join(ref_wf , by = c("ref_area", "source"))
  
if(!length(commandArgs(TRUE))==0){
	arguments <- commandArgs(TRUE)
	for(i in 1:length(arguments)){ 
		eval(parse(text=arguments[[i]]))
	}
	workflow <- workflow %>% filter(as.numeric(ID) %in% as.numeric(batch)) 
	rm(arguments)
	print(paste0("argument batch = ", batch, "then nb = ", nb))
}






workflow

for (i in 1:nrow(workflow)){

	print(paste0(workflow$ref_area[i],"_",workflow$source[i], "/ ", Sys.time() , '/ part 12'))

	run <- paste0("Micro_process(collection = 'STI', print_ind = FALSE, validate = TRUE, saveCSV = FALSE, PUB = TRUE, consolidate = '12' ,ref_area  = '",workflow$ref_area[i],"', source = '",workflow$source[i],"' )")

	TEST <- try(eval(parse(text = run)), silent = TRUE)

	if(stringr:::str_sub(TEST[1], 1, 5) %in% 'Error') {print('Error found')}

}

Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)


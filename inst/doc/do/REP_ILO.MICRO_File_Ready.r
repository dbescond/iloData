
init_time <- Sys.time() 
Sys.time() 

require(tidyverse)
require(haven)
require(stringr)
require(iloMicro)

Micro_file_ready()

Sys.time() 
final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
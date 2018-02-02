
init_time <- Sys.time() 

iloMicro:::Micro_process(collection = 'STI', print_ind = FALSE, validate = TRUE, consolidate = '123')




final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% hms() } - { init_time %>% str_sub(12,19) %>% hms()}
rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)
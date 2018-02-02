







require(tidyverse)
require(stringr)


################# test micro versus YI for deletion
{
readMicro <- read_csv("J:\\COMMON\\STATISTICS\\DPAU\\DATA\\REP_ILO\\MICRO\\FileToLoad.csv")$PATH

res <- NULL
for (i in 1:length(readMicro)){

	load(readMicro[i])

	res <- bind_rows(res,  X %>% filter(str_sub(time,5,5) %in% '') %>% distinct(ref_area,source, indicator, time))
	
	rm(X)

 
}


######## WARNINGS ARG I13:3888
res
require(ilo)
init_ilo()

para <- list()

para$min <- min(res$time)
para$max <- max(res$time)
para$source <- unique(res$source)
cou <- unique(res$ref_area)

YIold <- get_ilo(ref_area = cou, query = "filter(source %in% para$source,  as.numeric(time) >= para$min)", add = para,  info  ='COL') 


STIMicro  <- get_ilo(collection = 'STI', ref_area = cou, query = "filter(source %in% para$source, as.numeric(time) >= para$min)", add = para,  info  ='COL', note_source = 'R1:3513')

cou <- unique(YIold$ref_area)
X <- NULL
for (i in 1:length(cou)){

	TEST <- YIold %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) 

	ref <- STIMicro %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) %>% count(ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% ungroup %>% mutate(STI = 1)

TEST <-  TEST %>% left_join(ref %>% distinct(ref_area, source, indicator, time, STI)) %>% filter(STI %in% 1) 


if(nrow(TEST) > 0){
name <- paste0('G:/del_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')


 #PRINT <- TEST %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time)  
 #PRINT$obs_value <- format(PRINT$obs_value, scientific = FALSE)
 
  #PRINT %>% data.table::fwrite( file = name)

 name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/input/', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 if(length(name) > 1) {
 
	name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/input/', unique(TEST$ref_area), '_',  '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 
 
 }
 
 TEST %>% select(-STI, -contains('_version')) %>% filter(!str_detect(indicator , 'IFL_'))%>% data.table::fwrite( file = name)

 
 
 
X <- bind_rows(X, TEST %>% select(-STI, -contains('_version')) )


}
rm(TEST,ref)
	
	
}

X <- X %>% filter(!str_detect(indicator , 'IFL_'))


}


X %>% mutate(time = as.character(time)) %>% save_ilo(format = 'del')



################################ test eurostat version national source
{


####################################### 
require(tidyverse)
require(stringr)

require(ilo)
init_ilo()


eurostat <- ilo$code$cl_survey %>% filter(str_detect(label_en, '- LFS - EU Labour Force Survey')) %>% select(code) %>% t %>% as.character

# eurostat <- c('BA:536', 'BA:2257', 'BA:2258', 'BA:2253', 'BA:2259', 'BA:2249', 'BA:2242', 'BA:2487', 'BA:2244', 'BA:2518', 'BA:2486', 'BA:2260', 'BA:2247', 'BA:2240', 'BA:2251', 'BA:2237', 'BA:772', 'BA:2255', 'BA:2238', 'BA:2245', 'BA:2261', 'BA:2246', 'BA:2239', 'BA:2248', 'BA:2236', 'BA:2250', 'BA:2241', 'BA:2252', 'BA:2235', 'BA:2254', 'BA:2243', 'BA:2519', 'BA:2256')

X <- get_ilo(collection = 'YI', query = "filter(source %in% eurostat)", add = eurostat)

X <- X  %>% filter(source %in% eurostat)


res <- X  %>% distinct(ref_area, indicator, time)

rm(X)





para <- list()

para$min <- min(res$time)
para$max <- max(res$time)
para$source <- eurostat
cou <- unique(res$ref_area)

X <- NULL
for (i in 1:length(cou)){

TEST <- get_ilo(ref_area = cou[i], query = "filter(  as.numeric(time) >= para$min)", add = para,  info  ='COL', source = 'BA') %>% 
			filter(!source %in% eurostat) %>% 
			switch_ilo(version) 

ref <- get_ilo(collection = 'YI', ref_area = cou[i], query = "filter(source %in% para$source, as.numeric(time) >= para$min)", add = para) %>% 
			filter(source %in% eurostat) %>% 
			switch_ilo(version) %>% count(ref_area, indicator, sex_version, classif1_version, classif2_version, time) %>% ungroup %>% mutate(STI = 1)

TEST <-  TEST %>% left_join(ref %>% distinct(ref_area, indicator, time, STI), by = c("ref_area", "indicator", "time")) %>% filter(STI %in% 1)


if(nrow(TEST) > 0){

 name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/input/EUROSTAT_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 TEST %>% select(-STI, -contains('_version'))  %>% write_csv(  path = name, na = '')

X <- bind_rows(X, TEST %>% select(-STI, -contains('_version')) )
}
rm(TEST,ref)
	
	
}



}

X %>% mutate(time = as.character(time)) %>% save_ilo(format = 'del')






############################## test kilm data versus STI bulk
{


res <- get_ilo(query = "filter(stringr:::str_detect(note_source, 'R1:3901'))", info = 'COL') %>% distinct(ref_area,source, indicator, time)


######## WARNINGS ARG I13:3888
res
para$min <- min(res$time)
para$max <- max(res$time)
para$source <-  unique(res$source)
cou <- unique(res$ref_area)

YIold <- get_ilo(ref_area = cou, query = "filter(  as.numeric(time) >= para$min, stringr:::str_detect(note_source, 'R1:3901'))", add = para,  info  ='COL') 


STIMicro  <- get_ilo(collection = 'STI', ref_area = cou, query = "filter(source %in% para$source, as.numeric(time) >= para$min)", add = para) %>% 
				# filter(!str_detect(note_source, 'R1:2382')) %>% 
				filter(!str_detect(note_source, 'R1:3902')) 

cou <- unique(YIold$ref_area)
X <- NULL
for (i in 1:length(cou)){

	TEST <- YIold %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) 

	ref <- STIMicro %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) %>% count(ref_area, indicator, sex_version, classif1_version, classif2_version, time) %>% ungroup %>% mutate(STI = 1)

TEST <-  TEST %>% left_join(ref %>% distinct(ref_area, indicator, time, STI)) %>% filter(STI %in% 1)


if(nrow(TEST) > 0){
# name <- paste0('G:/del_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 # TEST %>% distinct(collection, ref_area, indicator, sex_version, classif1_version, classif2_version, time)  # %>% write_csv(  path = name, na = '')

 #name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO_MICRO/input/', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 TEST %>% select(-STI, -contains('_version')) # %>% write_csv(  path = name, na = '')

X <- bind_rows(X, TEST %>% select(-STI, -contains('_version')) )
}
rm(TEST,ref)
	
	
}









}




################################## compare YI to bulk STI
{


YIold <- get_ilo( info  ='COL') 


STIMicro  <- get_ilo(collection = 'STI', ref_area = cou, query = "filter(source %in% para$source, as.numeric(time) >= para$min)", add = para) %>% 
				# filter(!str_detect(note_source, 'R1:2382')) %>% 
				filter(!str_detect(note_source, 'R1:3902')) 

cou <- unique(YIold$ref_area)
X <- NULL
for (i in 1:length(cou)){

	TEST <- YIold %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) 

	ref <- STIMicro %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) %>% count(ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% ungroup %>% mutate(STI = 1)

TEST <-  TEST %>% left_join(ref %>% distinct(ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, STI)) %>% filter(STI %in% 1)


if(nrow(TEST) > 0){
# name <- paste0('G:/del_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 # TEST %>% distinct(collection, ref_area, indicator, sex_version, classif1_version, classif2_version, time)  # %>% write_csv(  path = name, na = '')

 #name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO_MICRO/input/', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 TEST %>% select(-STI, -contains('_version')) # %>% write_csv(  path = name, na = '')

X <- bind_rows(X, TEST %>% select(-STI, -contains('_version')) )
}
rm(TEST,ref)
	
	
}


}



################################ test oecd version national source
{

require(ilo)
init_ilo()


X <- get_ilo(collection = 'STI', query = "filter(stringr:::str_detect(note_source , 'R1:2382'))")


res <- X %>%  distinct(ref_area, source, indicator, time) %>% filter(!str_detect(indicator, '_STE_'))







para <- list()

para$min <- min(res$time)
para$max <- max(res$time)
para$source <- unique(res$source)
para$indicator <- unique(res$indicator)
para$ref_area <- unique(res$ref_area)
cou <- unique(res$ref_area)

YIold <- get_ilo( query = "filter(ref_area %in% para$ref_area,   as.numeric(time) >= para$min, source %in% para$source, indicator %in% para$indicator)", add = para,  info  ='COL') 


STIMicro <- get_ilo(collection = 'STI', ref_area = cou, query = "filter(source %in% para$source, as.numeric(time) >= para$min, indicator %in% para$indicator)", add = para) %>% 
				filter(str_detect(note_source, 'R1:2382')) # %>% # oecd
				# filter(!str_detect(note_source, 'R1:3902')) 

cou <- unique(YIold$ref_area)
X <- NULL
for (i in 1:length(cou)){

	TEST <- YIold %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) 

	ref <- STIMicro %>% filter(ref_area %in% cou[i]) %>% switch_ilo(version) %>% count(ref_area, indicator, sex_version, classif1_version, classif2_version, time) %>% ungroup %>% mutate(STI = 1)

TEST <-  TEST %>% left_join(ref %>% distinct(ref_area, indicator, time, STI)) %>% filter(STI %in% 1)


if(nrow(TEST) > 0){
# name <- paste0('G:/del_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 # TEST %>% distinct(collection, ref_area, indicator, sex_version, classif1_version, classif2_version, time)  # %>% write_csv(  path = name, na = '')

 name <- paste0('J:/COMMON/STATISTICS/DPAU/DATA/REP_ILO/MICRO/input/OECD_', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
 TEST %>% select(-STI, -contains('_version')) %>% write_csv(  path = name, na = '')

X <- bind_rows(X, TEST %>% select(-STI, -contains('_version')) )
}
rm(TEST,ref)
	
	
}


}



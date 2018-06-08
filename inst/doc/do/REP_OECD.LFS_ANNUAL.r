#' process data from OECD, fill gap on STI Annual after 1990, mainly for the GET model
#' 
#' The following link is the one used to get the actual values of the data:
#' 
#' 
#' To obtain this link follow the instructions:
#' 0) USE MOZILLA FIREFOX!
#'
#' 

#' @return csv files compare with ilostat database
#' @author ILO / gomisr@ilo.org / bescond@ilo.org
#' @keywords ILO, bulkdownload
#'
#' @examples
#' ## Not run:
#'	
#'     source(paste0("J:/COMMON/STATISTICS/DPAU/DATA/REP_OECD/LFS_ANNUAL/do/REP_OECD.LFS_ANNUAL.r") )                  
#'              
#'	
#' ## End(**Not run**)
#' @export
#' @rdname REP_OECD.ANNUAL_LFS


init_time <- Sys.time() 

download = TRUE
require(ilo)		
Sys.setenv(http_proxy="proxyos.ilo.org:8080")
Sys.setenv(htts_proxy="proxyos.ilo.org:8080")
Sys.setenv(ftp_proxy="proxyos.ilo.org:8080")

setwd(paste0(ilo:::path$data, '/REP_OECD/LFS_ANNUAL/'))
source(paste0(getwd(),"/do/REP_OECD.LFS_ANNUAL_functions.r"))


################################################### Download section

if(download){
	INPUT <- paste0(getwd(), '/input/')
	Mapping_File <- readxl:::read_excel("./ReadME_OECD_LFS_ANNUAL.xlsx", sheet="File")  %>% filter(!ID%in%NA) %>% as.data.frame
	# require(RSelenium)
	for(i in 1:length(Mapping_File$NAME)){

		REP_OECD_LFS_ANNUAL_download(Mapping_File %>% slice(i))
	}
}

###################################################

 
  REP_OECD.LFS_ANNUAL_input_ANNUAL_EMP_SEX_ECO_ISIC3()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_EMP_SEX_ECO_ISIC4()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_5YRBANDS()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_10YRBANDS()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_AGGREGATE()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_AGE_DUR()
  REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_EMP_STE()
  
  
  res <-	bind_rows( 	readRDS('./input/ANNUAL_UNE_AGE_DUR.rds'), 
						readRDS('./input/ANNUAL_AGE.rds'),
						readRDS('./input/ANNUAL_STE.rds'), 
						readRDS('./input/ANNUAL_EMP_SEX_ECO_ISIC3.rds'),
						readRDS('./input/ANNUAL_EMP_SEX_ECO_ISIC4.rds')) %>% 
			mutate( value = round(obs_value, 3))	%>% 
			select(ref_area, indicator, source, sex:obs_value, obs_status, note_classif, note_source)  %>% 
			switch_ilo(version) %>% 
			mutate(del = ifelse(time<1990 & !ref_area %in% 'KOR', 'del', 'keep')) %>%
			filter(del %in% 'keep')	%>% select(-del)			
  
  invisible(gc(reset = TRUE))
  invisible(gc(reset = TRUE))
  
  
  res <- res %>%     filter(!ref_area %in% c('IND','CHN', 'IDN', 'BRA')) %>% 
					 mutate(time = as.character(time)) %>%
					mutate(note_source = paste0('R1:3903_', note_source)) %>% 
					filter(!(ref_area %in% 'COL' & classif1_version %in% 'ECO_ISIC3' & obs_value %in% 0)) %>%
					filter(!(ref_area %in% 'CRI' & as.numeric(time) >2010)) %>%
					filter(!(ref_area %in% 'CHL' & as.numeric(time) >2009)) %>%
					filter(!(ref_area %in% 'USA' & as.numeric(time) >1993)) %>%
					filter(!(ref_area %in% 'USA' & !indicator %in% 'UNE_TUNE_SEX_AGE_DUR_NB')) %>%
					filter(!(ref_area %in% 'ZAF' & as.numeric(time) >2007)) %>%
					filter(!(ref_area %in% 'HRV'))
  
  invisible(gc(reset = TRUE))
  invisible(gc(reset = TRUE))
  
  ref_file <- unique(res$ref_area)
  require(ilo)
  init_ilo(-cl)
  ref_eulfs <- ilo$code$cl_survey %>% filter(str_detect(label_en, 'LFS - EU Labour Force Survey')) %>% select(code, label_en) %>% mutate(ref_area = str_sub(label_en, 1,3)) %>% .$ref_area
  close_ilo()
  
  
  
  
  ref_file_test <- ref_file
  for (i in 1:length(ref_file_test)){

	pass <- res %>% filter(ref_area %in% ref_file_test[i])
	######################################################################
	######################################################################
	######################################################################
	################################### compare with EUROSTAT query and reduce 
	###################################
	######################################################################

	if(ref_file_test[i] %in% ref_eulfs){
	
		load(paste0(ilo:::path$data, 'REP_EUROSTAT/LFS_BULK/output/REP_EUROSTAT_',ref_file_test[i] ,'.Rdata'))
		X <- X %>% switch_ilo(version) %>% distinct(indicator, sex_version, classif1_version, classif2_version, time) %>% mutate(del = 1) %>% rename(rep_var = indicator)
		
		pass <- pass %>% mutate(rep_var = paste0(str_sub(indicator, 1,9), str_sub(indicator, -2,-1))) %>% left_join(X, by = c('rep_var', "sex_version", "classif1_version", "classif2_version", "time")) %>% 
		filter(!del %in% 1) %>% select(-del, -rep_var)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	}
	
	if(nrow(pass) > 1){
		saveRDS(pass, file = paste0("./output/",ref_file_test[i],".rds"))	
	} else {
		ref_file <- ref_file[!ref_file %in% ref_file_test[i]]
	}
	rm(pass)
	print(ref_file_test[i])
}
  

  
	ref <- cbind(PATH = paste0(getwd(),"/output/",ref_file,".rds"),ID = NA, Types  ="CL", REF = paste0(ref_file)) %>% as_data_frame
	data.table:::fwrite(ref,paste0("./FileToLoad.csv"))
  rm(res, ref_file)


final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)

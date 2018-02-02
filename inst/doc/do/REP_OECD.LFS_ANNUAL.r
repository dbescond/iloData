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

setwd(paste0(ilo:::path$data, '/REP_OECD/LFS_ANNUAL/'))
source(paste0(getwd(),"/do/REP_OECD.LFS_ANNUAL_functions.r"))


################################################### Download section

if(download){
	Sys.setenv(http_proxy="")
	INPUT <- paste0(getwd(), '/input/')
	Mapping_File <- readxl:::read_excel("./ReadME_OECD_LFS_ANNUAL.xlsx", sheet="File")  %>% filter(!ID%in%NA) %>% as.data.frame
	require(RSelenium)
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
			mutate(	value = round(obs_value, 3))	%>% 
			select(ref_area, indicator, source, sex:obs_value, obs_status, note_classif, note_source)  %>% 
			switch_ilo(version) %>% 
			filter(as.numeric(time) >= 1990)				
  
  
  
  
  res <- res %>%     filter(!ref_area %in% c('IND','CHN', 'IDN')) %>% 
					mutate(note_source = paste0('R1:3903_', note_source)) %>% 
					filter(!(ref_area %in% 'COL' & classif1_version %in% 'ECO_ISIC3' & obs_value %in% 0)) %>%
					filter(!(ref_area %in% 'CRI' & as.numeric(time) >2010)) %>%
					filter(!(ref_area %in% 'HRV'))
  
  
  
  ref_file <- unique(res$ref_area)
  
  
  for (i in 1:length(ref_file)){
    saveRDS(res %>% filter(ref_area %in% ref_file[i]), file = paste0("./output/",ref_file[i],".rds"))
  }
  
	ref <- cbind(PATH = paste0(getwd(),"/output/",ref_file,".rds"),ID = NA, Types  ="CL", REF = paste0(ref_file)) %>% as_data_frame
	data.table:::fwrite(ref,paste0("./FileToLoad.csv"))
  rm(res, ref_file)


final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)

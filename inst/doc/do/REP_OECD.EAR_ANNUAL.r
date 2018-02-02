#' process data from OECD, fill gap on STI Annual after 1990, mainly for the GET model
#' 
#' The following link is the one used to get the actual values of the data:
#' 
#' 
#' To obtain this link follow the instructions:
#' 0) USE MOZILLA FIREFOX!
#'
#' THIS IS A VARIANT OF THE FILE ADAPTED FROM LFS_ANNUAL TO PROCES EAR_ANNUAL!!

#' @return csv files compare with ilostat database
#' @author ILO / gomisr@ilo.org / bescond@ilo.org
#' @keywords ILO, bulkdownload
#'
#' @examples
#' ## Not run:
#'	
#'     source(paste0("J:/COMMON/STATISTICS/DPAU/DATA/REP_OECD/EAR_ANNUAL/do/REP_OECD.EAR_ANNUAL.r") )                  
#'              
#'	
#' ## End(**Not run**)
#' @export
#' @rdname REP_OECD.ANNUAL_EAR


init_time <- Sys.time() 

download = FALSE
require(ilo)		

setwd(paste0(ilo:::path$data, '/REP_OECD/EAR_ANNUAL/'))
source(paste0(getwd(),"/do/REP_OECD.EAR_ANNUAL_functions.r"))


################################################### Download section

if(download){
	Sys.setenv(http_proxy="")
	INPUT <- paste0(getwd(), '/input/')
	Mapping_File <- readxl:::read_excel("./ReadME_OECD_EAR_ANNUAL.xlsx", sheet="File")  %>% filter(!ID%in%NA) %>% as.data.frame
	require(RSelenium)
	for(i in 1:length(Mapping_File$NAME)){

		REP_OECD_EAR_ANNUAL_download(Mapping_File %>% slice(i))
	}
}

###################################################

 
  REP_OECD.EAR_ANNUAL_input_ANNUAL_EAR_XTLP_SEX_RT ()




V <- as.vector(list.files("./input") )
V <- V [ V %>% str_detect("Output")]
load(file = paste0('input/empty','.Rdata'))

Y <- empty
for (v in V) {
  #Y <- gtools::smartbind(Y,read.csv( paste0(getwd(),"/input/",v) ) )
  Y <- bind_rows(Y,read_csv( paste0(getwd(),"/input/",v)) %>% mutate(time = as.character(time)) )
}

Y <- Y %>%
  mutate( freq_code="m" ) %>% mutate_all(funs(as.character)) %>% mutate(obs_value = as.numeric(obs_value))


	
res <- Y %>% 
			mutate(	obs_value = round(obs_value, 3))	%>% 
			#select(ref_area, indicator, source, sex:obs_value, obs_status, note_classif, note_source)  %>% 
			switch_ilo(version) %>% 
			filter(as.numeric(time) >= 1990)%>% 
      select(-contains('_version'))				

ref_file <- unique(res$ref_area)
  
  
for (i in ref_file ) {
    
    invisible(gc(reset = TRUE))
    X <- res %>% filter(ref_area %in% i  )
    save(X,file = paste("./output/REP_OECD_",i,".Rdata",sep=""))
    print(i)
}


ref <- cbind(PATH = paste0(getwd(),"/output/REP_OECD_",ref_file,".Rdata"),ID = NA, Types  ="OECD_ilostat", REF = paste0(ref_file)) %>% as_data_frame
data.table:::fwrite(ref,paste0("./FileToLoad.csv"))
rm(res, ref_file)


final_time <- Sys.time();{final_time  %>% str_sub(12,19) %>% lubridate:::hms() } - { init_time %>% str_sub(12,19) %>% lubridate:::hms()}

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)

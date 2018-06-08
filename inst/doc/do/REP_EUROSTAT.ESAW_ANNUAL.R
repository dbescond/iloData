
#' This Script processes INJ data
#' 
#'
#' 
#' @param work_directory inside the DATA repo
#' @param check for automation, return a warning file at the root, default is TRUE
#' @return csv files compare with ilostat database


ilo::init_ilo()
## parameters
work_directory = "REP_EUROSTAT/ESAW_ANNUAL"
## Initialize
setwd(paste0(ilo:::path$data, work_directory))
source(paste0(getwd(),"/do/REP_EUROSTAT.ESAW_ANNUAL_functions.r"))

#Run the functions
#Reference group does not  match EULFS (almost never)
#REP_EUROSTAT.EASW_ANNUAL.lfsa_egan.proINJ_WORK_SEX_MIG_NB()
#REP_EUROSTAT.EASW_ANNUAL.lfsq_egan2.proINJ_WORK_ECO_NB()

#Mixed Eurostat ad oc + bulk
REP_EUROSTAT.EASW_ANNUAL.lfsa_egan.INJ_WORK_SEX_MIG_NB()
REP_EUROSTAT.EASW_ANNUAL.lfsq_egan2.INJ_WORK_ECO_NB()

#Bulk Eurostat
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_02.INJ_FATL_ECO_NB()
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_01.INJ_NFTL_ECO_NB()
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_NFTL_INJ_ECO_NB() 
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_DAYS_ECO_NB()

#Ad Hoc Eurostat Request
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_DAYS_SEX_MIG_NB()
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_MIG_NB()
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_FATL_SEX_MIG_NB()
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_INJ_MIG_NB()

# Collect all the data in one set

V <- as.vector(list.files("./input") )
V <- V [ V %>% str_detect("Output")]
load(file = paste0('input/empty','.Rdata'))

Y <- empty
for (v in V) {
  #Y <- gtools::smartbind(Y,read.csv( paste0(getwd(),"/input/",v) ) )
  Y <- rbind(Y,read.csv( paste0(getwd(),"/input/",v) ) )
}

Y <- Y %>%
  mutate( freq_code="m" ) %>%
  as.tbl() %>% mutate_all(funs(as.character)) %>% mutate(obs_value = as.numeric(obs_value))

for (i in unique(Y$ref_area) ){
  invisible(gc(reset = TRUE))
  X <- Y %>% filter(ref_area %in% i  )
  save(X,file = paste("./output/REP_EUROSTAT_",i,".Rdata",sep=""))
  print(i)
}

LOAD <- cbind(PATH = paste0(getwd(), "/output/REP_EUROSTAT_",unique(Y$ref_area),".Rdata"),ID = NA, Types  ="EUROSTAT_ilostat", REF = unique(as.character(Y$ref_area)))
write.csv(LOAD,"./FileToLoad.csv",row.names = FALSE,na="")

################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************






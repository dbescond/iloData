#' This Script rprocesses Eurostat SILC data
#' 
#'
#' 
#' @param work_directory inside the DATA repo
#' @param check for automation, return a warning file at the root, default is TRUE
#' @return csv files compare with ilostat database


ilo::init_ilo()
## parameters
work_directory = "REP_EUROSTAT/SILC_ANNUAL"
require(ilo)
## Initialize
setwd(paste0(ilo:::path$data, work_directory))
source(paste0(getwd(),"/do/REP_EUROSTAT.SILC_ANNUAL_functions.r"))


REP_EUROSTAT.SES_ANNUAL.ilc_iw01.POV_DEMP_SEX_AGE_RT()


# Collect all the data in one set

V <- as.vector(list.files("./input") )
V <- V [ V %>% str_detect("Output")]
load(file = paste0('input/empty','.Rdata'))

Y <- empty
for (v in V) {

  Y <- rbind(Y,read.csv( paste0(getwd(),"/input/",v) ) )
}
Y <- Y %>%
  mutate( freq_code="m" ) %>%
  as.tbl()%>% mutate_all(funs(as.character)) %>% mutate(obs_value = as.numeric(obs_value))

for (i in unique(Y$ref_area) ){
  invisible(gc(reset = TRUE))
  X <- Y %>% filter(ref_area %in% i  )
  save(X,file = paste("./output/REP_EUROSTAT_",i,".Rdata",sep=""))
  print(i)
}

LOAD <- cbind(PATH = paste0(getwd(), "/output/REP_EUROSTAT_",unique(Y$ref_area),".Rdata"),ID = NA, Types  ="EUROSTAT_ilostat", REF = unique(Y$ref_area))
write.csv(LOAD,"./FileToLoad.csv",row.names = FALSE,na="")

################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************
################# ****************************************************************************************


#### Archive of functions, for the function script 


### ***************
# METADATA Now Done

# 1 - Poverty while at work, by sex and age 
REP_EUROSTAT.SES_ANNUAL.ilc_iw01.POV_DEMP_SEX_AGE_RT <- function (check = TRUE) {
  #POV_DEMP_SEX_AGE_RT ilc_iw01 Working poverty rate by sex and age
  rm(list=setdiff(ls(), c("ilo")))

  ############## Getting the data
  
  # Input <- eurostat:::get_eurostat('ilc_iw01', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  # saveRDS(Input, paste0('./input/Input.POV_DEMP_SEX_AGE_RT.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.POV_DEMP_SEX_AGE_RT.RDS')) 
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='POV_DEMP_SEX_AGE_RT') -> GrossTarget
  # saveRDS(GrossTarget, paste0('./input/POV_DEMP_SEX_AGE_RT.GrossTarget.RDS'))
  GrossTarget <- readRDS(paste0('./input/POV_DEMP_SEX_AGE_RT.GrossTarget.RDS'))
  ## end
  
  ################ Mappings
  
  ## Country Map
  Country.Map <- Input %>% 
    distinct(geo) %>%
    left_join( 
      ilo$code$cl_country %>% 
        rename(geo = code_iso2),
      by="geo"
    ) %>%
    select(geo, code) %>%
    mutate( 
      code = if_else( geo=="EL", "GRC", code ),
      code = if_else( geo=="UK", "GBR", code )
    ) %>%
    rename( ref_area = code )
  
  Source.Map <- GrossTarget %>%
    filter(time>2002) %>% 
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source)
  
  Potential.Sources <- ilo$code$cl_survey %>% 
    filter(str_detect( label_en, "Income and Living Conditions") ) %>%
    mutate( source = code,
            ref_area = str_extract(label_en,"^[:upper:]+")
    ) %>%
    distinct(ref_area, source) %>%
    select(ref_area, source) %>% 
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source)
  
  Source.Map <- rbind(Source.Map, Potential.Sources) %>%
    distinct(ref_area, source) %>% 
    switch_ilo(keep) %>%
    group_by(ref_area) %>% 
    mutate( n = n()) %>%
    ungroup() %>%
    filter( n == 1 | (n == 2 & str_detect(source.label, "EU") ) ) %>%
    select(ref_area,source) %>%
    # Note, certain countries did not have the survey
    #CHE, MKD, POL  need to add -> EU Statistics on Income and Living Conditions BB sort 15
    rbind(c("CHE","BB:13356")) %>%
    rbind(c("MKD","BB:13357")) %>%
    rbind(c("POL","BB:13355")) %>%
    distinct(ref_area, source)
  
  
  
  
  
  
  
  
  ## end
  
  
  ############## Modifying input, and saving output
  #based on the general Eurostat metadata
  Output <- Input %>%
    
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    select( -geo ) %>%
    mutate(
      sex = paste0("SEX_", sex),
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = "T20:180",
      note_source = "S3:5_S4:4137_S5:4138_T2:85",
      indicator = "POV_DEMP_SEX_AGE_RT",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", b= "B" , c = "C", p = "P",  u ="U", s = "E", i = NA_character_, d = NA_character_ ),
      obs_status = as.character(obs_status)
    ) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") ) %>%
    
    # filter by breakdown of interest, note that age bands will have to be noted as being non standard
    filter(wstatus=="EMP") %>%
    mutate(
      classif1 = age %>% recode( "Y15-24" = "AGE_YTHADULT_Y15-24", "Y_GE18" = "AGE_YTHADULT_YGE15"  )
    ) %>%
    filter ( !str_detect(classif1,"^Y")) %>%
    #note for the aggregate total
    mutate( note_classif = if_else(classif1 == "AGE_YTHADULT_YGE15", "C6:1056" , note_classif ) ) %>%
    mutate( note_classif = if_else(classif1 == "AGE_YTHADULT_Y15-24", "C6:1058" , note_classif ) ) %>%
    select( -age, - wstatus)
  
  
  
  
  
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time >2002 )
  
  Comparison <- Net.Target %>% mutate( time = as.character(time)) %>% left_join(Output, by=c("ref_area", "time", "sex", "classif1")) %>%
    mutate(diff = 100 * (as.double(obs_value.x)-as.double(obs_value.y) )/obs_value.x )
  
  
  
  write.csv(Output, file = './input/Output.POV_DEMP_SEX_AGE_RT.csv', row.names=FALSE, na="")
  
}


#' This Script rprocesses Eurostat SES data
#' 
#'
#' 
#' @param work_directory inside the DATA repo
#' @param check for automation, return a warning file at the root, default is TRUE
#' @return csv files compare with ilostat database


ilo::init_ilo()
## parameters
work_directory = "REP_EUROSTAT/SES_ANNUAL"
require(ilo)
## Initialize
setwd(paste0(ilo:::path$data, work_directory))
source(paste0(getwd(),"/do/REP_EUROSTAT.SES_ANNUAL_functions.r"))



REP_EUROSTAT.SES_ANNUAL.earn_mw_cur.EAR_INEE_NOC_NB()
REP_EUROSTAT.SES_ANNUAL.lc_lci_lev.LAC_XEES_ECO_NB()
REP_EUROSTAT.SES_ANNUAL.lc_an_costh.LAC_XEES_ECO_NB2()
#no gender gap for the moment (by economic activity)
#REP_EUROSTAT.SES_ANNUAL.earn_gr_gpgr2.EAR_GGAP_ECO_RT()

REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_14.EAR_HEES_SEX_OCU_NB()
REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_20.EAR_XEES_SEX_ECO_NB()
REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_21.EAR_XEES_SEX_OCU_NB()
#REP_EUROSTAT.SES_ANNUAL.earn_ses_pub1s.EAR_XTLP_SEX_RT()

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
#### There is a  need to establish the way it will work the upload to YI                      



ilo::init_ilo()
## parameters
work_directory = "REP_EUROSTAT/SES_ANNUAL"
## Initialize
setwd(paste0(ilo:::path$data, work_directory))



#### Archive of functions, for the function script 
#### There is a  need to establish the way it will work the upload to YI     


# 1 - Done
REP_EUROSTAT.SES_ANNUAL.earn_mw_cur.EAR_INEE_NOC_NB <- function (check = TRUE) {
  
  ############## Minimum wage EAR_INEE_NOC_NB
  
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  
  
  ## Get Input 
  #Input <- eurostat:::get_eurostat('earn_mw_cur', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.EAR_INEE_NOC_NB.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_INEE_NOC_NB.RDS'))
  
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='EAR_INEE_NOC_NB') -> GrossTarget
  # saveRDS(GrossTarget , file = './input/EAR_INEE_NOC_NB.GrossTarget.RDS')
  GrossTarget <- readRDS(paste0('./input/EAR_INEE_NOC_NB.GrossTarget.RDS'))
  
  
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
  
  
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  ## Source Map
  
  Source.Map <- GrossTarget %>%
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source) %>%
    filter( !(source %in% c("DA:670","FX:507")))
  
  
  
  
  ## end
  
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    select( -geo ) %>%
    mutate(
      sex = NA_character_,
      classif1 = "NOC_VALUE",
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "EAR_INEE_NOC_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E"),
      obs_status = as.character(obs_status)
    ) %>%
    
    ## filtering - Second Semester, remove Purchasing Power Standard
    filter( !is.na(obs_value) ) %>%
    filter( str_detect(time,"S2") ) %>%
    #formating to obtain year now that is unique
    mutate(time = str_sub(time,1,-3)) %>%
    
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    # Exception to add new Lithuanian currency (euro)
    mutate( note_indicator = if_else(ref_area=="LTU", "T30:377" , note_indicator) ) %>%
    
    
    # note_source, repository ILOSTAT and July as reference period
    #institutional sector is total in general, see the stored readme in the help folder
    mutate(note_source = "R1:2383_R1:3903_S3:18") %>%
    mutate( note_source = if_else(ref_area %in% c("BEL", "GRC"),"R1:2383_R1:3903_S3:18_S7:57" ,note_source  ) ) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source = "NEED SOURCE!!!") ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="MKD", "FX:13348" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="MNE", "FX:13349" ,source) )
  
  # Check against target
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time>1998)
  
  write.csv(Output, file = './input/Output.EAR_INEE_NOC_NB.csv', row.names=FALSE, na="")
}



# 2 - Done (Note the data are actually from LCS_ANNUAL, but due to the closeness to the rest of the extract and mapping the function is in the SES_ANNUAL project)
REP_EUROSTAT.SES_ANNUAL.lc_lci_lev.LAC_XEES_ECO_NB <- function (check = TRUE) {
  #LAC_XEES_ECO_NB lc_lci_lev Labour cost levels by NACE Rev. 2 activity (lc_lci_lev)
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  
  # Input <- eurostat:::get_eurostat('lc_lci_lev', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  # saveRDS(Input, paste0('./input/Input.LAC_XEES_ECO_NB.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.LAC_XEES_ECO_NB.RDS'))
  
  ## Other data
  ##### this should never be run again: get_ilo(indicator='LAC_XEES_ECO_NB') -> GrossTarget
  # saveRDS(GrossTarget , file = './input/LAC_XEES_ECO_NB.GrossTarget.RDS')
  GrossTarget <- readRDS(paste0('./input/LAC_XEES_ECO_NB.GrossTarget.RDS'))
  
  
  
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
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  ## ECO map
  Sector.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC4_[:alpha:]$")) %>%
    mutate(nace_r2 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r2)
  
  ## Source Map
  
  Source.Map <- GrossTarget %>%
    filter(time>1998) %>% 
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source) %>%
    filter( !(source %in% c("DA:721","DA:428")))
  
  
  
  
  
  ## end
  
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    select( -geo ) %>%
    mutate(
      sex = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "LAC_XEES_ECO_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P"),
      obs_status = as.character(obs_status)
    ) %>%
    
    ## filtering     - notice we only want total labour cost
    filter( lcstruct == "D" ) %>%
    select( -lcstruct ) %>%
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    # Exception to add new Lithuanian currency (euro)
    mutate( note_indicator = if_else(ref_area=="LTU", "T30:377" , note_indicator) ) %>%
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( Sector.Map , by = c("nace_r2") ) %>%
    mutate( classif1 = if_else(nace_r2=="B-S_X_O", "ECO_ISIC4_TOTAL",classif1)  ) %>%
    mutate( note_source ="R1:2383_R1:3903_S6:48_S8:2566" ) %>%
    filter( !is.na(classif1) ) %>%
    select( -nace_r2) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") )%>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="GRC", "DA:13351" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="LTU", "DA:13352" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="NOR", "DA:13350" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="SWE", "DA:13353" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="TUR", "DA:13354" ,source) )
  
  
  
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time>1998)
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "classif1"))
  #All manually checked, largest issues are:
  #revisions, non-standard units reference year or other in Target data
  
  ##RECOMENDATION, migrate to the sytem since 2012
  Output <- Output %>% filter(time>2011)
  write.csv(Output, file = './input/Output.LAC_XEES_ECO_NB.csv', row.names=FALSE, na="")
  
}



# 3 - Done (Note the data are actually from LCS_ANNUAL, but due to the closeness to the rest of the extract and mapping the function is in the SES_ANNUAL project)
REP_EUROSTAT.SES_ANNUAL.lc_an_costh.LAC_XEES_ECO_NB2 <- function (check = TRUE) {
  #LAC_XEES_ECO_NB lc_an_costh Hourly labour costs by NACE Rev. 1.1 activity
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  
  # Input <- eurostat:::get_eurostat('lc_an_costh', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  # saveRDS(Input, paste0('./input/Input.LAC_XEES_ECO_NB2.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.LAC_XEES_ECO_NB2.RDS'))
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='LAC_XEES_ECO_NB') -> GrossTarget
  GrossTarget <- readRDS(paste0('./input/LAC_XEES_ECO_NB.GrossTarget.RDS'))
  
  
  
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
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  ## ECO map
  Sector.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC3_[:alpha:]$")) %>%
    mutate(nace_r1 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r1)
  
  ## Source Map
  
  Source.Map <- GrossTarget %>%
    filter(time>1998) %>% 
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source) %>%
    filter( !(source %in% c("DA:721","DA:428")))
  
  ## end
  
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    select( -geo ) %>%
    mutate(
      sex = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "LAC_XEES_ECO_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P", u ="U", b="B"),
      obs_status = as.character(obs_status)
    ) %>%
    
    # obs_status, not indicated as usual, some metadata comments that have to be manually introduced
    mutate( obs_status = if_else(ref_area=="ESP" & time=="2001","B",obs_status), 
            obs_status = if_else(ref_area=="DNK" & time=="2000","B",obs_status)
    ) %>%
    
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    # Exception to add new Lithuanian currency (euro)
    mutate( note_indicator = if_else(ref_area=="LTU", "T30:377" , note_indicator) ) %>%
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( Sector.Map , by = c("nace_r1") ) %>%
    mutate( classif1 = if_else(nace_r1=="C-O", "ECO_ISIC3_TOTAL",classif1)  ) %>%
    mutate( note_source ="R1:2383_R1:3903_S8:3907_S8:1619" ) %>%
    filter( !is.na(classif1) ) %>%
    select( -nace_r1) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="GRC", "DA:13351" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="LTU", "DA:13352" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="NOR", "DA:13350" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="SWE", "DA:13353" ,source) ) %>%
    mutate( source =  if_else(source == "NEED SOURCE!!!" & ref_area=="TUR", "DA:13354" ,source) )
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time>1998)
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "classif1"))
  #All manually checked, largest issues are:
  #revisions, non-standard units reference year or other in Target data
  
  ##RECOMENDATION, migrate overwriting the few existing data points (Manually checked, only one country affected very small differences)
  
  write.csv(Output, file = './input/Output.LAC_XEES_ECO_NB2.csv', row.names=FALSE, na="")
  
}


# 4 - Done
REP_EUROSTAT.SES_ANNUAL.earn_gr_gpgr2.EAR_GGAP_ECO_RT <- function (check = TRUE) {
  #EAR_GGAP_ECO_RT Unadjusted gender wage gap by NACE Rev. 2 activity (earn_gr_gpgr2)
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  
  # Input <- eurostat:::get_eurostat('earn_gr_gpgr2', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  # saveRDS(Input, paste0('./input/Input.EAR_GGAP_ECO_RT.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_GGAP_ECO_RT.RDS')) 
  
  
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
  
  
  
  ## ECO map
  Sector.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC4_[:alpha:]$")) %>%
    mutate(nace_r2 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r2)
  
  ## Source Map
  library(readxl)
  Source.Map <- read_excel("J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/SES_ANNUAL/input/SES_Surveys.xlsx") %>%
    rbind(c("HRV","DA:562") )
  
  
  
  
  
  ## end
  
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    select( -geo ) %>%
    mutate(
      sex = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      indicator = "EAR_GGAP_ECO_RT",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P",  u ="U", s = "E", i = NA_character_, d = NA_character_ ),
      obs_status = as.character(obs_status)
    ) %>%
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( Sector.Map , by = c("nace_r2") ) %>%
    mutate( classif1 = if_else(nace_r2=="B-S_X_O", "ECO_ISIC4_TOTAL",classif1)  ) %>%
    mutate( note_source ="R1:2383_R1:3903_S6:48_S8:2566" ) %>%
    filter( !is.na(classif1) ) %>%
    select( -nace_r2) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") ) %>%
    
    select(-unit) %>%
    
    # Final element, processing the obs_status (d) "definition differs" For Romania nothing is specifed in the metadata, but given the temporal profile a 
    # break is introduced,  for Switzerland T34:2128 _NOTE_EXCLUDING_APPRENTICES the obs_status d means "Overtime pay is not included in average hourly earnings for 2012 and 2013.The following main groups are not included: apprentices.
    mutate(
      obs_status = if_else( ref_area == "ROU" & time=="2010", "B" , obs_status),
      
      note_source = if_else( ref_area =="CHE", paste0(note_source,"_S9:3937")  , note_source ),
      note_indicator = if_else( ref_area =="CHE" & time %in% c("2012","2013"), "_T34:2128" , note_indicator )
      
      
    ) %>%
    ### also other notes 
    #I need to add _NOTE_EXCLUDING_APPRENTICES _NOTE_EXCLUDING_PART_TIMERS
    # Poland I obs_value for 2007 2009 2011 2013 2015 (odd years) 
    # I12:422  Cyprus: Mean monthly earnings are used in the calculation of the GPG between the 4-yearly Structure of Earnings Survey (SES).
    # REMOVE _S6:48  Czech Republic: Enterprises with 1+ employees are covered by data. The gender pay gap by    age, economic control and working time are for NACE sections B to S.
    # I12:422, _NOTE_MINAGE_18 _NOTE_MAX_AGE64  SWEDEN From 2011, between the 4-yearly Structure of Earnings Survey (SES), data are based on monthly earnings instead of hourly earnings. The population is aged 18-64 and work at least 5% of full time, excluding overtime hours.
    # SLOVENIA T12:148 T34:397 I12:422 where data on part-time workers are excluded, irregular payments are included and earnings per paid hour are not available, only annual and  monthly data per employee
    # UK T34:397 (includes irregular payments such as bonuses)
    # UK for 2010 2011 is used instead, I set obs_status to code I
    # Additionally UK is non calendar year, it is financial year Apr-Mar S3:27
    # (2011-2015)  S8:63 Iceland excludes some categories in the total  (In particular excludes B, I, L, M, N, R, S)
  
  mutate( 
    obs_status = if_else( ref_area=="POL" & time %in% c("2007","2009","2011","2013","2015"), "I", obs_status ), 
    
    note_indicator = if_else( ref_area == "CYP","I12:422", note_indicator  ),
    
    note_source = if_else( ref_area == "CZE", str_replace(note_source,"_S6:48_","_"), note_source  ),
    
    note_indicator = if_else( ref_area == "SWE" & time=="2011","I12:422", note_indicator  ),
    note_source = if_else( ref_area == "SWE", paste0(note_source,"_T2:87_T3:94"), note_source ),
    
    note_indicator = if_else( ref_area == "SVN",  paste0(note_indicator,"_I12:422_T12:148_T34:397"), note_indicator ),
    
    obs_status = if_else( ref_area=="GBR" & time %in% c("2010"), "I", obs_status ), 
    note_indicator = if_else( ref_area == "GBR", paste0(note_indicator,"_T34:397"), note_indicator ),
    note_source = if_else( ref_area == "GBR", paste0(note_source,"_S3:3915"), note_source ),
    
    note_source = if_else( ref_area == "ISL", paste0(note_source,"_S8:63"), note_source  )
    
  ) %>%
    mutate( note_indicator = str_replace(note_indicator,"NA_", "") ) %>%
    
    # Finally notice that for the years where there is not an SES survey, the data are  estimates
    mutate( obs_status = if_else(time %in% c("2007", "2008","2009","2011", "2012" ,"2013","2015") & is.na(obs_status), "E", obs_status) ) %>% 
    #Drop backcasted data
    filter(time != "2007")
  
  write.csv(Output, file = './input/Output.EAR_GGAP_ECO_RT.csv', row.names=FALSE, na="")
  
}




#- Collective metadata for 5 to 8 (note it excludes item 4 as the indicator has it's own metadata detailed by country - which often contradicts the rest)
# - In contrast it includes 8 because precisely the metadata used for 5 to 7 is referenced in 8


# 5  - Done // Important -> Notice: For 2010 NAC (National Currency) is not available, this is the most frequent cause of missing rows in new data
#indicator number 665
#this should only fill
REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_14.EAR_HEES_SEX_OCU_NB <- function (check = TRUE) {
  #EAR_HEES_SEX_OCU_NB earn_ses_dd_14 Mean nominal hourly earnings of employees by sex and occupation
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  if(FALSE) {
    Input <- eurostat:::get_eurostat('earn_ses14_14', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
      lapply(as.character) %>%
      data.frame(stringsAsFactors = FALSE) %>% 
      filter( age == "TOTAL", indic_se == "HE")
    
    indicator.list <- c("earn_ses10_14","earn_ses06_14","earn_ses_agt14")
    for (indicator in indicator.list) {
      gc(reset=TRUE)
      if (indicator == "earn_ses10_14") {
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)  %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "HE")
        
      } else {
        
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "HE") %>% 
          rename(currency = unit)
      }
      
      print(indicator)
      Input <- gtools::smartbind(Input,RawInput, fill="NO!")
    }
    
  }
  
  # saveRDS(Input, paste0('./input/Input.EAR_HEES_SEX_OCU_NB.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_HEES_SEX_OCU_NB.RDS'))
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='EAR_HEES_SEX_OCU_NB') -> GrossTarget
  # saveRDS(GrossTarget, paste0('./input/EAR_HEES_SEX_OCU_NB.GrossTarget.RDS'))
  GrossTarget <- readRDS(paste0('./input/EAR_HEES_SEX_OCU_NB.GrossTarget.RDS'))
  
  
  
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
  
  #Country map all sizes 2010 
  Size2002.Map <- ilo$code$cl_country %>% 
    filter ( label_en %in% c("Cyprus", "Germany", "Estonia", "Finland", "Hungary", "Ireland", "Lithuania", "Latvia", "Netherlands", "Norway", "Poland", "Slovenia", "Slovak Republic", "United Kingdom") ) %>%
    rename( ref_area = code) %>%
    select(ref_area) %>%
    mutate(metanote.size = "DELETE Size Limitation", time="2002")
  #Country map non-missing M to O
  Sector2002.Map <- Country.Map %>%
    filter( geo %in% c("RO", "NL", "ES", "IE", "SI", "CZ", "SK", "BG", "HU", "UK", "LT", "PL")) %>%
    select(ref_area) %>%
    mutate(metanote.sector = "DELETE Economic Sector Limitation", time="2002")
  
  
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  
  ## OCU map
  isco88.Map <- ilo$code$cl_classif %>% 
    filter( str_detect(code, "ISCO88_[:alnum:]$")  ) %>%
    mutate(isco88 = paste0("ISCO" ,str_extract(code,"[:alnum:]$") )   ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, isco88) %>%
    mutate(  isco88 = if_else(isco88 == "ISCOX", "UNK", isco88) )
  
  isco08.Map <- ilo$code$cl_classif %>% 
    filter(  str_detect(code, "ISCO08_[:alnum:]$") ) %>%
    mutate(isco08 = paste0("OC", str_extract(code,"[:alnum:]$") ) ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, isco08)
  
  ## Source Map
  
  Source.Map <- GrossTarget %>%
    filter(time>1998) %>% 
    left_join(Country.Map, by="ref_area") %>%
    filter( !is.na(geo)) %>%
    select( -geo) %>%
    distinct(ref_area, source) %>%
    filter( !(source %in% c("BA:199"))) 
  
  ## end
  
  
  ############## Modifying input, and saving output
  
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    
    mutate(
      sex = paste0("SEX_", sex),
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "EAR_HEES_SEX_OCU_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P", u ="U", s = "E", i = NA_character_),
      obs_status = as.character(obs_status)
    ) %>%
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    
    # Data selection - Only 10 or more employees are kept for comparability's sake
    filter( sizeclas == "GE10" | time == "2002"  ) %>%
    select( -sizeclas ) %>%
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( isco08.Map , by = c("isco08") ) %>%
    left_join( isco88.Map , by = c("isco88") ) %>%
    mutate(classif1 = if_else ( is.na(classif1.x), classif1.y,classif1.x ) ) %>%
    mutate( classif1 = if_else( isco88 =="TOTAL", "OCU_ISCO88_TOTAL",classif1)  ) %>%
    mutate( classif1 = if_else( isco08 =="TOTAL", "OCU_ISCO08_TOTAL",classif1)  ) %>%
    mutate( note_source = if_else( time>2009, "R1:2383_R1:3903_S6:48_S8:2566","R1:2383_R1:3903_S6:48_S8:3907_S8:3211" ) ) %>%
    filter( !is.na(classif1) ) %>%
    select( -classif1.y, -classif1.x, -isco08, -isco88) %>%
    
    ## a bit more work on the notes for year 2002 (L to O was optional however some countries did include it, Also heterogeneity by size)
    
    left_join( Size2002.Map, by = c("time", "ref_area") ) %>%
    left_join( Sector2002.Map, by = c("time", "ref_area") ) %>%
    # All countries are initially set to 10 or more employees and missing L-O
    mutate( note_source = if_else( time=="2002","R1:2383_R1:3903_S6:48_S8:2603_S8:3907" ,note_source ) ) %>%
    #Here the exceptions for either rule are corrected
    mutate( note_source = if_else( time==2002 & !is.na(metanote.size), str_replace(note_source,"_S6:48_","_"), note_source ) ) %>%
    mutate( note_source = if_else( time==2002 & !is.na(metanote.sector),  str_replace(note_source,"_S8:2603_S8:3907",""), note_source ) ) %>%
    
    select( -geo, -metanote.size, -metanote.sector, -indic_se, -age ) %>%
    
    # source
    
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") ) %>%
    
    
    ####### *************************************************
    # doubt with respect to in kind, metadata seems quite inconsistent -> Decision: Do not include. Justification: For Countries not reporting it in 2002 and 2006 in 2010 is mentioned that it is included for the first time, whereas other countries clearly mention it in those years
    # 2002 - GER The data refer to 2001 (October) _S3:21_S3:27,  SWE excluding 15-17, ROU did not cover apprentices, CZE includes bonuses T34:397
    # 2006 and 2010- CZE annual average 
    # 2006 and onwards Spain excludes apprentices
    # 2010 excluding apprentices in Slovenia but unsure about before + they say its negligible -> Decision: I exclude.
    #All years, SWE September, NOR September-October, HUN May, DK  Full year and then annual average,  Rest of countries October excluding CZE 2006 and 2010
    
    mutate( 
      # October as a reference period
      note_source = if_else( !(ref_area %in% c("SWE","NOR", "HUN", "CZE","DNK")) , paste0(note_source,"_S3:21"), note_source),
      # Reference period idosyncracies
      note_source = if_else( ref_area %in% c("CZE") & time<2006 , paste0(note_source,"_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_S3:20"), note_source),
      note_source = if_else( ref_area %in% c("NOR") , paste0(note_source,"_S3:20_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("HUN") , paste0(note_source,"S3:16"), note_source),
      note_source = if_else( ref_area %in% c("DEU") & time<2006 , paste0(note_source,"_S3:27"), note_source),
      # Other source notes
      #note_source = if_else( ref_area %in% c("ESP") & time>2004 , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("ROU") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("CZE") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_T2:86"), note_source),
      # Indicator notes
      note_indicator = if_else( ref_area %in% c("CZE") , paste0(note_indicator,"_T34:397"), note_indicator)
    ) %>% 
    mutate( note_indicator = str_replace(note_indicator,"NA_", "") ) %>%
    mutate( note_source = str_replace(note_source,"NA_", "") ) %>%
    filter( !(ref_area %in% c("ESP","FIN","GBR", "ISL","CZE", "SVK", "PRT")) )
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time>1998, !str_detect(classif1,"SKILL") )
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "sex", "classif1")) %>%
    mutate(diff = 100 * (obs_value.x-as.double(obs_value.y) )/obs_value.x )
  
  
  write.csv(Output, file = './input/Output.EAR_HEES_SEX_OCU_NB.csv', row.names=FALSE, na="")
  
}



# 6 - Done// Important Notice: For 2010 NAC (National Currency) is not available, this is the most frequent cause of missing rows in new data
#indicator number 50
#this should only fill
REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_20.EAR_XEES_SEX_ECO_NB <- function (check = TRUE) {
  #EAR_XEES_SEX_ECO_NB earn_ses_dd_20 Mean nominal monthly earnings of employees by sex and economic activity
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  if(FALSE) {
    
    Input <- eurostat:::get_eurostat('earn_ses14_20', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
      lapply(as.character) %>%
      data.frame(stringsAsFactors = FALSE) %>% 
      filter( age == "TOTAL", indic_se == "ME")
    
    indicator.list <- c("earn_ses10_20","earn_ses06_20","earn_ses_agt20")
    for (indicator in indicator.list) {
      gc(reset=TRUE)
      
      if (indicator == "earn_ses10_20") {
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)  %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "ME")
        
      } else {
        
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "ME") %>% 
          rename(currency = unit)
      }
      
      print(indicator)
      Input <- gtools::smartbind(Input,RawInput, fill="NO!")
    }
    
  }
  
  # saveRDS(Input, paste0('./input/Input.EAR_XEES_SEX_ECO_NB.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_XEES_SEX_ECO_NB.RDS'))
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='EAR_XEES_SEX_ECO_NB') -> GrossTarget
  # saveRDS(GrossTarget, paste0('./input/EAR_XEES_SEX_ECO_NB.GrossTarget.RDS'))
  GrossTarget <- readRDS(paste0('./input/EAR_XEES_SEX_ECO_NB.GrossTarget.RDS'))
  
  
  
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
  
  #Country map all sizes 2010 
  Size2002.Map <- ilo$code$cl_country %>% 
    filter ( label_en %in% c("Cyprus", "Germany", "Estonia", "Finland", "Hungary", "Ireland", "Lithuania", "Latvia", "Netherlands", "Norway", "Poland", "Slovenia", "Slovak Republic", "United Kingdom") ) %>%
    rename( ref_area = code) %>%
    select(ref_area) %>%
    mutate(metanote.size = "DELETE S6:48", time="2002")
  #Country map non-missing M to O
  Sector2002.Map <- Country.Map %>%
    filter( geo %in% c("RO", "NL", "ES", "IE", "SI", "CZ", "SK", "BG", "HU", "UK", "LT", "PL")) %>%
    select(ref_area) %>%
    mutate(metanote.sector = "_S8:2603_S8:3907", time="2002")
  
  
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  
  ## ECO map
  isic4.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC4_[:alpha:]$")) %>%
    mutate(nace_r2 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r2)
  
  ## ECO map
  isic3.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC3_[:alpha:]$")) %>%
    mutate(nace_r1 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r1)
  
  ## Source Map
  
  library(readxl)
  Source.Map <- read_excel("J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/SES_ANNUAL/input/SES_Surveys.xlsx") %>%
    rbind(c("HRV","DA:562") )
  
  
  ## end
  
  
  ############## Modifying input, and saving output
  
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    
    mutate(
      sex = paste0("SEX_", sex),
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "EAR_XEES_SEX_ECO_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P", u ="U", s = "E", i = NA_character_),
      obs_status = as.character(obs_status)
    ) %>%
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    
    # Data selection - Only 10 or more employees are kept for comparability's sake
    filter( sizeclas == "GE10" | time == "2002"  ) %>%
    select( -sizeclas ) %>%
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( isic4.Map , by = c("nace_r2") ) %>%
    left_join( isic3.Map , by = c("nace_r1") ) %>%
    mutate(classif1 = if_else ( is.na(classif1.x), classif1.y,classif1.x ) ) %>%
    mutate( classif1 = if_else(nace_r2=="B-S_X_O", "ECO_ISIC4_TOTAL",classif1)  ) %>%
    mutate( classif1 = if_else(nace_r1=="C-O_X_L", "ECO_ISIC3_TOTAL",classif1)  ) %>%
    mutate( note_source = if_else( time>2009, "R1:2383_R1:3903_S6:48_S8:2566","R1:2383_R1:3903_S6:48_S8:3907_S8:3211" ) ) %>%
    filter( !is.na(classif1) ) %>%
    select( -classif1.y, -classif1.x, -nace_r1, -nace_r2) %>%
    
    ## a bit more work on the notes for year 2002 (L to O was optional however some countries did include it, Also heterogeneity by size)
    
    left_join( Size2002.Map, by = c("time", "ref_area") ) %>%
    left_join( Sector2002.Map, by = c("time", "ref_area") ) %>%
    # All countries are initially set to 10 or more employees and missing L-O
    mutate( note_source = if_else( time=="2002","R1:2383_R1:3903_S6:48_S8:2603_S8:3907" ,note_source ) ) %>%
    #Here the exceptions for either rule are corrected
    mutate( note_source = if_else( time==2002 & !is.na(metanote.size), str_replace(note_source,"_S6:48_","_"), note_source ) ) %>%
    mutate( note_source = if_else( time==2002 & !is.na(metanote.sector),  str_replace(note_source,"_S8:2603_S8:3907",""), note_source ) ) %>%
    
    select( -geo, -metanote.size, -metanote.sector, -indic_se, -age ) %>%
    
    # source
    
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") )%>%
    
    
    ####### *************************************************
    # doubt with respect to in kind, metadata seems quite inconsistent -> Decision: Do not include. Justification: For Countries not reporting it in 2002 and 2006 in 2010 is mentioned that it is included for the first time, whereas other countries clearly mention it in those years
    # 2002 - GER The data refer to 2001 (October) _S3:21_S3:27,  SWE excluding 15-17, ROU did not cover apprentices, CZE includes bonuses T34:397
    # 2006 and 2010- CZE annual average 
    # 2006 and onwards Spain excludes apprentices
    # 2010 excluding apprentices in Slovenia but unsure about before + they say its negligible -> Decision: I exclude.
    #All years, SWE September, NOR September-October, HUN May, DK  Full year and then annual average,  Rest of countries October excluding CZE 2006 and 2010
    
    mutate( 
      # October as a reference period
      note_source = if_else( !(ref_area %in% c("SWE","NOR", "HUN", "CZE","DNK")) , paste0(note_source,"_S3:21"), note_source),
      # Reference period idosyncracies
      note_source = if_else( ref_area %in% c("CZE") & time<2006 , paste0(note_source,"_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_S3:20"), note_source),
      note_source = if_else( ref_area %in% c("NOR") , paste0(note_source,"_S3:20_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("HUN") , paste0(note_source,"S3:16"), note_source),
      note_source = if_else( ref_area %in% c("DEU") & time<2006 , paste0(note_source,"_S3:27"), note_source),
      # Other source notes
      note_source = if_else( ref_area %in% c("ESP") & time>2004 , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("ROU") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("CZE") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_T2:86"), note_source),
      # Indicator notes
      note_indicator = if_else( ref_area %in% c("CZE") , paste0(note_indicator,"_T34:397"), note_indicator)
    ) %>% 
    mutate( note_indicator = str_replace(note_indicator,"NA_", "") ) %>%
    mutate( note_source = str_replace(note_source,"NA_", "") ) %>%
    filter( !(ref_area %in% c("ESP","FIN","GBR", "ISL","CZE", "SVK", "BEL", "CHE", "NLD", "PRT")) ) %>%
    #Filter older entries, it is incomplete compared to what we have
    filter( time >2009   )
  
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time %in% c("2002","2006","2010","2014"), ! (str_detect(classif1,"AGGREGATE") ) )
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "sex", "classif1")) %>%
    mutate(diff = 100 * (obs_value.x-as.double(obs_value.y) )/obs_value.x )
  
  
  
  write.csv(Output, file = './input/Output.EAR_XEES_SEX_ECO_NB.csv', row.names=FALSE, na="")
  
}



# 7 - Done // Important Notice: For 2010 NAC (National Currency) is not available, this is the most frequent cause of missing rows in new data
#indicator number 52
#this should only fill
REP_EUROSTAT.SES_ANNUAL.earn_ses_dd_21.EAR_XEES_SEX_OCU_NB <- function (check = TRUE) {
  #EAR_XEES_SEX_OCU_NB earn_ses_dd_21 Mean nominal monthly earnings of employees by sex and occupation
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  if(FALSE) {
    Input <- eurostat:::get_eurostat('earn_ses14_21', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
      lapply(as.character) %>%
      data.frame(stringsAsFactors = FALSE) %>% 
      filter( age == "TOTAL", indic_se == "ME")
    
    indicator.list <- c("earn_ses10_21","earn_ses06_21","earn_ses_agt21")
    for (indicator in indicator.list) {
      gc(reset=TRUE)
      if (indicator == "earn_ses10_21") {
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)  %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "ME")
        
      } else {
        
        RawInput <- eurostat:::get_eurostat( indicator , time_format = 'raw', keepFlags  = TRUE, cache  = FALSE) %>% 
          lapply(as.character) %>% 
          data.frame(stringsAsFactors = FALSE) %>% 
          filter( age == "TOTAL", indic_se == "ME") %>% 
          rename(currency = unit)
      }
      
      print(indicator)
      Input <- gtools::smartbind(Input,RawInput, fill="NO!")
    }
    
  }
  
  # saveRDS(Input, paste0('./input/Input.EAR_XEES_SEX_OCU_NB.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_XEES_SEX_OCU_NB.RDS'))
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='EAR_XEES_SEX_OCU_NB') -> GrossTarget
  # saveRDS(GrossTarget, paste0('./input/EAR_XEES_SEX_OCU_NB.GrossTarget.RDS'))
  GrossTarget <- readRDS(paste0('./input/EAR_XEES_SEX_OCU_NB.GrossTarget.RDS'))
  
  
  
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
  
  #Country map all sizes 2010 
  Size2002.Map <- ilo$code$cl_country %>% 
    filter ( label_en %in% c("Cyprus", "Germany", "Estonia", "Finland", "Hungary", "Ireland", "Lithuania", "Latvia", "Netherlands", "Norway", "Poland", "Slovenia", "Slovak Republic", "United Kingdom") ) %>%
    rename( ref_area = code) %>%
    select(ref_area) %>%
    mutate(metanote.size = "DELETE Size Limitation", time="2002")
  #Country map non-missing M to O
  Sector2002.Map <- Country.Map %>%
    filter( geo %in% c("RO", "NL", "ES", "IE", "SI", "CZ", "SK", "BG", "HU", "UK", "LT", "PL")) %>%
    select(ref_area) %>%
    mutate(metanote.sector = "DELETE Economic Sector Limitation", time="2002")
  
  
  
  ## Currency Map
  Currency.Map <- ilo$code$cl_note_currency %>% 
    rename( currency = CUR_CODE) %>%
    mutate( ref_area = substr(label_en, 17, 19)) %>%
    select( code, currency, ref_area, sort) %>%
    mutate( currency = if_else(currency == "EUR", "EUR","NAC") )
  
  
  ## OCU map
  isco88.Map <- ilo$code$cl_classif %>% 
    filter( str_detect(code, "ISCO88_[:alnum:]$")  ) %>%
    mutate(isco88 = paste0("ISCO" ,str_extract(code,"[:alnum:]$") )   ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, isco88) %>%
    mutate(  isco88 = if_else(isco88 == "ISCOX", "UNK", isco88) )
  
  isco08.Map <- ilo$code$cl_classif %>% 
    filter(  str_detect(code, "ISCO08_[:alnum:]$") ) %>%
    mutate(isco08 = paste0("OC", str_extract(code,"[:alnum:]$") ) ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, isco08)
  
  ## Source Map
  
  library(readxl)
  Source.Map <- read_excel("J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/SES_ANNUAL/input/SES_Surveys.xlsx") %>%
    rbind(c("HRV","DA:562") )
  
  ## end
  
  
  ############## Modifying input, and saving output
  
  Output <- Input %>%
    
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    filter(!is.na(ref_area)) %>%
    # several renaming, and empty variables
    rename( obs_value = values ) %>%
    rename( obs_status = flags ) %>%
    
    mutate(
      sex = paste0("SEX_", sex),
      classif2 = NA_character_,
      note_classif = NA_character_,
      indicator = "EAR_XEES_SEX_OCU_NB",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P", u ="U", s = "E", i = NA_character_),
      obs_status = as.character(obs_status)
    ) %>%
    
    # note_indicator Mapping currency and droping in case it does not match
    # selecting the one with the lowest sort
    
    #note_indicator
    filter( currency != "PPS") %>%
    left_join( Currency.Map , by = c("ref_area","currency") ) %>%
    filter( ! is.na( sort) ) %>%
    group_by(ref_area, time) %>%
    mutate( minsort = min(sort) ) %>%
    ungroup() %>%
    filter( (sort == minsort) ) %>%
    mutate( note_indicator = as.character(code) ) %>%
    select( -minsort, -currency, -sort, -code ) %>%
    
    # Data selection - Only 10 or more employees are kept for comparability's sake
    filter( sizeclas == "GE10" | time == "2002"  ) %>%
    select( -sizeclas ) %>%
    
    
    # classif1, the mapping is required, also note that sectors are excluded
    
    left_join( isco08.Map , by = c("isco08") ) %>%
    left_join( isco88.Map , by = c("isco88") ) %>%
    mutate(classif1 = if_else ( is.na(classif1.x), classif1.y,classif1.x ) ) %>%
    mutate( classif1 = if_else( isco88 =="TOTAL", "OCU_ISCO88_TOTAL",classif1)  ) %>%
    mutate( classif1 = if_else( isco08 =="TOTAL", "OCU_ISCO08_TOTAL",classif1)  ) %>%
    mutate( note_source = if_else( time>2009, "R1:2383_R1:3903_S6:48_S8:2566","R1:2383_R1:3903_S6:48_S8:3907_S8:3211" ) ) %>%
    filter( !is.na(classif1) ) %>%
    select( -classif1.y, -classif1.x, -isco08, -isco88) %>%
    
    ## a bit more work on the notes for year 2002 (L to O was optional however some countries did include it, Also heterogeneity by size)
    
    left_join( Size2002.Map, by = c("time", "ref_area") ) %>%
    left_join( Sector2002.Map, by = c("time", "ref_area") ) %>%
    # All countries are initially set to 10 or more employees and missing L-O
    mutate( note_source = if_else( time=="2002","R1:2383_R1:3903_S6:48_S8:2603_S8:3907" ,note_source ) ) %>%
    #Here the exceptions for either rule are corrected
    mutate( note_source = if_else( time==2002 & !is.na(metanote.size), str_replace(note_source,"_S6:48_","_"), note_source ) ) %>%
    mutate( note_source = if_else( time==2002 & !is.na(metanote.sector),  str_replace(note_source,"_S8:2603_S8:3907",""), note_source ) ) %>%
    
    select( -geo, -metanote.size, -metanote.sector, -indic_se, -age ) %>%
    
    # source
    
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") )%>%
    
    
    ####### *************************************************
    # doubt with respect to in kind, metadata seems quite inconsistent -> Decision: Do not include. Justification: For Countries not reporting it in 2002 and 2006 in 2010 is mentioned that it is included for the first time, whereas other countries clearly mention it in those years
    # 2002 - GER The data refer to 2001 (October) _S3:21_S3:27,  SWE excluding 15-17, ROU did not cover apprentices, CZE includes bonuses T34:397
    # 2006 and 2010- CZE annual average 
    # 2006 and onwards Spain excludes apprentices
    # 2010 excluding apprentices in Slovenia but unsure about before + they say its negligible -> Decision: I exclude.
    #All years, SWE September, NOR September-October, HUN May, DK  Full year and then annual average,  Rest of countries October excluding CZE 2006 and 2010
    
    mutate( 
      # October as a reference period
      note_source = if_else( !(ref_area %in% c("SWE","NOR", "HUN", "CZE","DNK")) , paste0(note_source,"_S3:21"), note_source),
      # Reference period idosyncracies
      note_source = if_else( ref_area %in% c("CZE") & time<2006 , paste0(note_source,"_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_S3:20"), note_source),
      note_source = if_else( ref_area %in% c("NOR") , paste0(note_source,"_S3:20_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("HUN") , paste0(note_source,"S3:16"), note_source),
      note_source = if_else( ref_area %in% c("DEU") & time<2006 , paste0(note_source,"_S3:27"), note_source),
      # Other source notes
      note_source = if_else( ref_area %in% c("ESP") & time>2004 , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("ROU") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("CZE") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_T2:86"), note_source),
      # Indicator notes
      note_indicator = if_else( ref_area %in% c("CZE") , paste0(note_indicator,"_T34:397"), note_indicator)
    ) %>% 
    mutate( note_indicator = str_replace(note_indicator,"NA_", "") ) %>%
    mutate( note_source = str_replace(note_source,"NA_", "") )%>%
    filter( !(ref_area %in% c("ESP","FIN","GBR", "ISL","CZE", "SVK", "BEL", "LTU")) )
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time %in% c("2002","2006","2010","2014"), !str_detect(classif1,"SKILL") )
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "sex", "classif1")) %>%
    mutate(diff = 100 * (obs_value.x-as.double(obs_value.y) )/obs_value.x )
  
  
  write.csv(Output, file = './input/Output.EAR_XEES_SEX_OCU_NB.csv', row.names=FALSE, na="")
  
}



## ************************************************
# 8 - Almost Done (some chechking required) # note that this one is muted, since OECD has better data
#indicator number 658
REP_EUROSTAT.SES_ANNUAL.earn_ses_pub1s.EAR_XTLP_SEX_RT <- function (check = TRUE) {
  #EAR_XTLP_SEX_RT earn_ses_pub1s Low pay rate by sex
  rm(list=setdiff(ls(), c("ilo")))
  
  ############## Getting the data
  
  # Input <- eurostat:::get_eurostat('earn_ses_pub1s', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  # saveRDS(Input, paste0('./input/Input.EAR_XTLP_SEX_RT.RDS'))
  
  ## Read Input
  Input <- readRDS(paste0('./input/Input.EAR_XTLP_SEX_RT.RDS')) 
  
  ## Other data
  ##### this should never be run again get_ilo(indicator='EAR_XTLP_SEX_RT') -> GrossTarget
  # saveRDS(GrossTarget, paste0('./input/EAR_XTLP_SEX_RT.GrossTarget.RDS'))
  GrossTarget <- readRDS(paste0('./input/EAR_XTLP_SEX_RT.GrossTarget.RDS'))
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
  
  
  
  ## ECO map
  Sector.Map <- ilo$code$cl_classif %>% 
    filter(str_detect(code,"ISIC4_[:alpha:]$")) %>%
    mutate(nace_r2 = str_extract(code,"[:alpha:]$") ) %>%
    mutate(classif1 = as.character(code)) %>%
    select(classif1, nace_r2)
  
  ## Source Map
  library(readxl)
  Source.Map <- read_excel("J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/SES_ANNUAL/input/SES_Surveys.xlsx") %>%
    rbind(c("HRV","DA:562") )
  
  
  
  
  
  ## end
  
  
  ############## Modifying input, and saving output
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
      classif1 = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      indicator = "EAR_XTLP_SEX_RT",
      collection = "STI"
    ) %>%
    
    # value changes
    mutate( 
      obs_status =  obs_status %>% recode( e = "E", c = "C", p = "P",  u ="U", s = "E", i = NA_character_, d = NA_character_ ),
      obs_status = as.character(obs_status)
    ) %>%
    
    mutate( note_source = if_else( time=="2006","R1:2383_R1:3903_S6:48_S8:2566","R1:2383_R1:3903_S6:48_S8:3907_S8:3211") ) %>%
    
    # source
    left_join( Source.Map , by = "ref_area") %>%
    replace_na( list(source= "NEED SOURCE!!!") ) %>%
    
    select(-unit, -sizeclas) %>%
    
    
    ####### *************************************************
    # doubt with respect to in kind, metadata seems quite inconsistent -> Decision: Do not include. Justification: For Countries not reporting it in 2002 and 2006 in 2010 is mentioned that it is included for the first time, whereas other countries clearly mention it in those years
    # 2002 - GER The data refer to 2001 (October) _S3:21_S3:27,  SWE excluding 15-17, ROU did not cover apprentices, CZE includes bonuses T34:397
    # 2006 and 2010- CZE annual average 
    # 2006 and onwards Spain excludes apprentices
    # 2010 excluding apprentices in Slovenia but unsure about before + they say its negligible -> Decision: I exclude.
    #All years, SWE September, NOR September-October, HUN May, DK  Full year and then annual average,  Rest of countries October excluding CZE 2006 and 2010
    
    mutate( 
      # October as a reference period
      note_source = if_else( !(ref_area %in% c("SWE","NOR", "HUN", "CZE","DNK")) , paste0(note_source,"_S3:21"), note_source),
      # Reference period idosyncracies
      note_source = if_else( ref_area %in% c("CZE") & time<2006 , paste0(note_source,"_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_S3:20"), note_source),
      note_source = if_else( ref_area %in% c("NOR") , paste0(note_source,"_S3:20_S3:21"), note_source),
      note_source = if_else( ref_area %in% c("HUN") , paste0(note_source,"S3:16"), note_source),
      note_source = if_else( ref_area %in% c("DEU") & time<2006 , paste0(note_source,"_S3:27"), note_source),
      # Other source notes
      note_source = if_else( ref_area %in% c("ESP") & time>2004 , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("ROU") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("CZE") , paste0(note_source,"_S9:3937"), note_source),
      note_source = if_else( ref_area %in% c("SWE") , paste0(note_source,"_T2:86"), note_source),
      # Indicator notes
      note_indicator = if_else( ref_area %in% c("CZE") , paste0(note_indicator,"_T34:397"), note_indicator)
    ) %>% 
    mutate( note_indicator = str_replace(note_indicator,"NA_", "") ) %>%
    mutate( note_source = str_replace(note_source,"NA_", "") )
  
  
  Net.Target <- GrossTarget %>% left_join(Country.Map, by="ref_area") %>% filter( !is.na(geo)) %>% select( -geo) %>% filter(time %in% c("2006","2010","2014") )
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "sex")) %>%
    mutate(diff = 100 * (obs_value.x-as.double(obs_value.y) )/obs_value.x )
  
  #Halted, OECD has better data
  #write.csv(Output, file = './input/Output.EAR_XTLP_SEX_RT.csv', row.names=FALSE)
  
}



### ***************If there are additional targets this project shoulde be moved to its appropiate folder SILC_ANNUAL
# METADATA Now Done

# 9 - Poverty while at work, by sex and age 
REP_EUROSTAT.SES_ANNUAL.ilc_iw01.POV_DEMP_SEX_AGE_RT <- function (check = TRUE) {
  #POV_DEMP_SEX_AGE_RT ilc_iw01 Working poverty rate by sex and age
  rm(list=setdiff(ls(), c("ilo")))
  ## parameters
  work_directory = "REP_EUROSTAT/SILC_ANNUAL"
  ## Initialize
  setwd(paste0(ilo:::path$data, work_directory))
  
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
  
  Comparison <- Net.Target %>% left_join(Output, by=c("ref_area", "time", "sex", "classif1")) %>%
    mutate(diff = 100 * (obs_value.x-as.double(obs_value.y) )/obs_value.x )

  
  
  write.csv(Output, file = './input/Output.POV_DEMP_SEX_AGE_RT.csv', row.names=FALSE, na="")
  
}



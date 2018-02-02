
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

#Run the functions
#Reference group does not alway match EULFS
#REP_EUROSTAT.EASW_ANNUAL.lfsa_egan.INJ_WORK_SEX_MIG_NB()
#REP_EUROSTAT.EASW_ANNUAL.lfsq_egan2.INJ_WORK_ECO_NB()


REP_EUROSTAT.EASW_ANNUAL.hsw_n2_02.INJ_FATL_ECO_NB()
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_01.INJ_NFTL_ECO_NB()
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_NFTL_INJ_ECO_NB() 
REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_DAYS_ECO_NB()

#Wait, only 2014 data and Eurostat should soon provide more years
#REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_DAYS_SEX_MIG_NB()
#REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_MIG_NB()
#REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_FATL_SEX_MIG_NB()
#REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_INJ_MIG_NB()

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




#### Archive of functions, for the function script 


#Reference group does not always match

REP_EUROSTAT.EASW_ANNUAL.lfsa_egan.INJ_WORK_SEX_MIG_NB <- function (check = TRUE) {
  
  ############## Reference group by nationality INJ_WORK_SEX_MIG_NB
  


rm(list=setdiff(ls(), c("ilo")))
require(ilo)
require(dplyr)

#Get Input
#Input <- eurostat:::get_eurostat('lfsa_egan', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
#saveRDS(Input, paste0('./input/Input.INJ_WORK_SEX_MIG_NB.RDS'))

#Read Input
Input <- readRDS(paste0('./input/Input.INJ_WORK_SEX_MIG_NB.RDS'))

Input[] <- lapply(Input, as.character)
Input$values <- as.numeric(Input$values) 

###### Maps
Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
#Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
library(readxl)
Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "RefGroupSource")

############## Modifying input, and saving output
Output <- Input %>%
  
  ### Mappings (implies data conversion and/or selection)
  ## Via Map
  #ref_area
  left_join(Country.Map , by="geo" ) %>%
  #obs_status
  left_join(Flags.Map , by="flags" ) %>%
  # migrant status
  left_join(Migrant.Map, by="citizen") %>%
  #sources
  left_join(Source.Map, by="ref_area") %>%
  
  ##Via Filter
  #filters for mapped variables:
  filter(!is.na(ref_area)) %>%
  filter(!is.na(MIG_STATUS)) %>%
  filter(!is.na(source)) %>%
  filter(time>2007) %>%
  
  # Filter to keep only total age
  filter( age=="Y_GE15" ) %>%
  
  ### Format Issues
  mutate(
    
    sex = paste0("SEX_",sex),
    classif2 = NA_character_,
    note_classif = NA_character_,
    note_indicator = NA_character_,
    collection = "YI",
    
    indicator = "INJ_WORK_SEX_MIG_NB",
    obs_value=values*1000,
    classif1 = MIG_STATUS,
    note_source = "R1:2383_R1:3903_S3:5"
    
  ) %>%
  select(-unit, -age, -citizen, -geo, -flags, -values, -MIG_STATUS)



}


REP_EUROSTAT.EASW_ANNUAL.lfsq_egan2.INJ_WORK_ECO_NB <- function (check = TRUE) {
  
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #Input <- eurostat:::get_eurostat('lfsa_egan2', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.INJ_WORK_ECO_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_WORK_ECO_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  Input$values <- as.numeric(Input$values) 
  
  ###### Maps
  Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "RefGroupSource")
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    #obs_status
    left_join(Flags.Map , by="flags" ) %>%
    # migrant status
    left_join(Sector.Map, by="nace_r2") %>%
    #sources
    left_join(Source.Map, by="ref_area") %>%
    
    ##Via Filter
    #filters for mapped variables:
    filter(!is.na(ref_area)) %>%
    filter(!is.na(ECO_ISIC4)) %>%
    filter(!is.na(source)) %>%
    filter(time>2007) %>%
    
    # Filter to keep only total age
    filter( age=="Y_GE15" ) %>%
    filter( sex=="T") %>%
    
    ### Format Issues
    mutate(
      
      sex = paste0("SEX_",sex),
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_WORK_ECO_NB",
      obs_value=values*1000,
      classif1 = ECO_ISIC4,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select(-unit, -age, -nace_r2, -geo, -flags, -values, -ECO_ISIC4)
  
}



REP_EUROSTAT.EASW_ANNUAL.hsw_n2_02.INJ_FATL_ECO_NB <- function (check = TRUE) {

  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #Input <- eurostat:::get_eurostat('hsw_n2_02', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.INJ_FATL_ECO_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_FATL_ECO_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  Input$values <- as.numeric(Input$values) 
  
  ###### Maps
  Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "FatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    #obs_status
    left_join(Flags.Map , by="flags" ) %>%
    # migrant status
    left_join(Sector.Map, by="nace_r2") %>%
    #sources
    left_join(Source.Map, by="ref_area") %>%
    
    ##Via Filter
    #filters for mapped variables:
    filter(!is.na(ref_area)) %>%
    filter(!is.na(ECO_ISIC4)) %>%
    filter(!is.na(source)) %>%
    filter(time>2007) %>%
    filter( unit == "NR") %>%
    
    #Odd Value
    filter( !(ref_area=="FRA"&time==2008) ) %>%
    
    ### Format Issues
    mutate(
      
      sex = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_FATL_ECO_NB",
      obs_value=values,
      classif1 = ECO_ISIC4,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select(-unit, -nace_r2, -geo, -flags, -values, -ECO_ISIC4)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_fatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_fatal), T13 , paste0(T13,"_",T14_fatal) ) ) 
  
  Output <- Output[  ,1:13]

  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_FATL_ECO_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_FATL_ECO_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_FATL_ECO_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_FATL_ECO_NB.csv', row.names=FALSE, na="")

}


REP_EUROSTAT.EASW_ANNUAL.hsw_n2_01.INJ_NFTL_ECO_NB <- function (check = TRUE) {
  
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #Input <- eurostat:::get_eurostat('hsw_n2_01', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.INJ_NFTL_ECO_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_NFTL_ECO_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  Input$values <- as.numeric(Input$values) 
  
  ###### Maps
  Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    #obs_status
    left_join(Flags.Map , by="flags" ) %>%
    # migrant status
    left_join(Sector.Map, by="nace_r2") %>%
    #sources
    left_join(Source.Map, by="ref_area") %>%
    
    ##Via Filter
    #filters for mapped variables:
    filter(!is.na(ref_area)) %>%
    filter(!is.na(ECO_ISIC4)) %>%
    filter(!is.na(source)) %>%
    filter(time>2007) %>%
    filter(sex=="T") %>%
    filter( unit == "NR") %>%
    
    ### Format Issues
    mutate(
      
      sex = NA_character_,
      classif2 = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_NFTL_ECO_NB",
      obs_value=values,
      classif1 = ECO_ISIC4,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select(-unit, -nace_r2, -geo, -flags, -values, -ECO_ISIC4)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), T13 , paste0(T13,"_",T14_nonfatal) ) ) 
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries   ExclusionGeneralNFTL.Map
  Output <- Output %>% 
    
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_NFTL_ECO_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_NFTL_ECO_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_NFTL_ECO_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_NFTL_ECO_NB.csv', row.names=FALSE, na="")
  
}


REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_NFTL_INJ_ECO_NB <- function (check = TRUE) {
  
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #Input <- eurostat:::get_eurostat('hsw_n2_04', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.INJ_NFTL_INJ_ECO_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_NFTL_INJ_ECO_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  Input$values <- as.numeric(Input$values) 
  
  ###### Maps
  Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  Severity.Map <- read.csv('./input/maps/severity.INJ_INCAPACITY.Map.csv', stringsAsFactors = FALSE)
  #Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "PermanentVsTemporary")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    #obs_status
    left_join(Flags.Map , by="flags" ) %>%
    # migrant status
    left_join(Sector.Map, by="nace_r2") %>%
    
    
    #sources
    left_join(Source.Map, by="ref_area") %>%
    
    
    #injury type
    left_join( Severity.Map, by="severity") %>%
    

    
    ##Via Filter
    #filters for mapped variables:
    filter(!is.na(ref_area)) %>%
    filter(!is.na(ECO_ISIC4)) %>%
    filter(!is.na(source)) %>%
    filter(time>2007) %>%
    filter( !is.na(INJ_INCAPACITY) ) %>%
    filter( INJ_INCAPACITY!="TODROP" ) %>%
    filter( unit == "NR") %>%
    
     
    #Adding Up
    group_by(ref_area, ECO_ISIC4, INJ_INCAPACITY, source, time) %>% 
    mutate( obs_value= sum(values) ) %>%
    ungroup() %>%
    select(-severity, -values) %>%
    distinct( ref_area, time, ECO_ISIC4, INJ_INCAPACITY, .keep_all=TRUE ) %>%
    spread(INJ_INCAPACITY, obs_value) %>%
    mutate( INJ_INCAPACITY_TOTAL = INJ_INCAPACITY_PRM+INJ_INCAPACITY_TMP) %>%
    gather( classif1, obs_value, INJ_INCAPACITY_PRM:INJ_INCAPACITY_TOTAL  ) %>%

    
    
    ### Format Issues
    mutate(
      
      sex = NA_character_,

      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_NFTL_INJ_ECO_NB",

      classif2 = ECO_ISIC4,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select(-unit, -nace_r2, -geo, -flags,  -ECO_ISIC4)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), paste0("I3:2920","_",T13,"_",T14_nonfatal) , paste0("I3:2920","_",T13,"_",T14_nonfatal) ) ) 
  
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries   ExclusionGeneralNFTL.Map
  Output <- Output %>% 
    merge (Exclusion.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion) %>%
  
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_NFTL_INJ_ECO_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_NFTL_INJ_ECO_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_NFTL_INJ_ECO_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1","classif2")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_NFTL_INJ_ECO_NB.csv', row.names=FALSE, na="")
}


REP_EUROSTAT.EASW_ANNUAL.hsw_n2_04.INJ_DAYS_ECO_NB <- function (check = TRUE) {
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #Input <- eurostat:::get_eurostat('hsw_n2_04', time_format = 'raw', keepFlags  = TRUE, cache  = FALSE)
  #saveRDS(Input, paste0('./input/Input.INJ_DAYS_ECO_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_DAYS_ECO_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  Input$values <- as.numeric(Input$values) 
  
  ###### Maps
  Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  Severity.Map <- read.csv('./input/maps/severity.DaysLost.Map.csv', stringsAsFactors = FALSE)
  #Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "DaysLost")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Country.Map , by="geo" ) %>%
    #obs_status
    left_join(Flags.Map , by="flags" ) %>%
    # migrant status
    left_join(Sector.Map, by="nace_r2") %>%
    
    
    #sources
    left_join(Source.Map, by="ref_area") %>%
    
    
    #injury type
    left_join( Severity.Map, by="severity") %>%
    
    
    
    ##Via Filter
    #filters for mapped variables:
    filter(!is.na(ref_area)) %>%
    filter(!is.na(ECO_ISIC4)) %>%
    filter(!is.na(source)) %>%
    filter(time>2007) %>%
    filter( !is.na(proDays) ) %>%
    filter( proDays!="TODROP" ) %>%
    filter( unit == "NR") %>%
    
    #Adding Up
    mutate( values = as.numeric(proDays) * values) %>%
    
    group_by(ref_area, ECO_ISIC4, source, time) %>%
    mutate( obs_value= sum(values) ) %>%
    ungroup() %>%
    select(-severity, -values, -proDays) %>%
    distinct( ref_area, time, ECO_ISIC4, .keep_all=TRUE ) %>%
    
    ### Format Issues
    mutate(
      
      sex = NA_character_,
      
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_DAYS_ECO_NB",
      
      classif1 = ECO_ISIC4,
      classif2 = NA_character_,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select(-unit, -nace_r2, -geo, -flags,  -ECO_ISIC4)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), T13 , paste0(T13,"_",T14_nonfatal) ) ) 
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries
  Output <- Output %>% 
    merge (Exclusion.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion) %>%
    
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_DAYS_ECO_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_DAYS_ECO_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_DAYS_ECO_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_DAYS_ECO_NB.csv', row.names=FALSE, na="")
  
}

# Only 2014, Wait for Eurostat's reply
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_DAYS_SEX_MIG_NB <- function (check = TRUE) {
  
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #library(readxl)
  #Input <-  read_excel("./input/DataTransmisionChangeOfFormat.xlsx", sheet = "INJ_DAYS_SEX_MIG_NB")
  #saveRDS(Input, paste0('./input/Input.INJ_DAYS_SEX_MIG_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_DAYS_SEX_MIG_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)

  
  ###### Maps
  #Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  #Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  #Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Severity.Map <- read.csv('./input/maps/severity.DaysLost.Map.csv', stringsAsFactors = FALSE)
  Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "DaysLost")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #Reshape
    gather( names, data, SEX_F_NAT:SEX_M_TOTAL) %>%
    mutate( sex = str_sub(names,1,5)) %>%
    mutate( citizen = str_sub(names,7,-1)) %>%

    ### Mappings (implies data conversion and/or selection)
    ## Via Map
  
    #sources
    left_join(Source.Map, by="ref_area") %>%
    left_join(Migrant.Map, by="citizen") %>%
    
    ##Via Filter
    #filters for mapped variables:

    
    #Adding Up
    mutate( obs_value= as.numeric(data) ) %>%
    
    ### Format Issues
    mutate(

      obs_status = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_DAYS_SEX_MIG_NB",
      
      classif1 = MIG_STATUS,
      classif2 = NA_character_,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select( -citizen, -ref_arealabel,  -MIG_STATUS, -data, -names)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), T13 , paste0(T13,"_",T14_nonfatal) ) ) 
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries (since it's the eurostat ad hoc transmision the exclusions are not included)
  
  # We need to compute the total across sex
  Output <- Output %>%
    spread(sex, obs_value) %>%
    mutate(SEX_T=SEX_F+SEX_M) %>%
    gather(sex, obs_value, SEX_F, SEX_M, SEX_T)
  
  #Exclude Unreliable Countries
  Output <- Output %>% 
    merge (Exclusion.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion) %>%
    
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_DAYS_SEX_MIG_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_DAYS_SEX_MIG_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_DAYS_SEX_MIG_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1","sex")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_DAYS_SEX_MIG_NB.csv', row.names=FALSE, na="")

}

# Only 2014, Wait for Eurostat's reply
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_MIG_NB <- function (check = TRUE) {
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #library(readxl)
  #Input <-  read_excel("./input/DataTransmisionChangeOfFormat.xlsx", sheet = "INJ_NFTL_SEX_MIG_NB")
  #saveRDS(Input, paste0('./input/Input.INJ_NFTL_SEX_MIG_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_NFTL_SEX_MIG_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  
  
  ###### Maps
  #Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  #Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  #Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Severity.Map <- read.csv('./input/maps/severity.DaysLost.Map.csv', stringsAsFactors = FALSE)
  Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  #Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "DaysLost")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #Reshape
    gather( names, data, SEX_F_NAT:SEX_M_TOTAL) %>%
    mutate( sex = str_sub(names,1,5)) %>%
    mutate( citizen = str_sub(names,7,-1)) %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    
    #sources
    left_join(Source.Map, by="ref_area") %>%
    left_join(Migrant.Map, by="citizen") %>%
    
    ##Via Filter
    #filters for mapped variables:
    
    
    #Adding Up
    mutate( Min= as.integer(str_extract(data,"^[:digit:]+") )) %>%
    mutate( Max= as.integer(str_extract(data,"[:digit:]+$") )) %>%
    mutate( obs_value= (Min+Max)/2 ) %>%
    
    ### Format Issues
    mutate(
      
      obs_status = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_NFTL_SEX_MIG_NB",
      
      classif1 = MIG_STATUS,
      classif2 = NA_character_,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select( -citizen, -ref_arealabel,  -MIG_STATUS, -data, -names, -Min, -Max)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), T13 , paste0(T13,"_",T14_nonfatal) ) ) 
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries (since it's the eurostat ad hoc transmision the exclusions are not included)
  #Exclude Unreliable Countries   ExclusionGeneralNFTL.Map
  Output <- Output %>% 
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  
  # We need to compute the total across sex
  Output <- Output %>%
    spread(sex, obs_value) %>%
    mutate(SEX_T=SEX_F+SEX_M) %>%
    gather(sex, obs_value, SEX_F, SEX_M, SEX_T)
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_NFTL_SEX_MIG_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_NFTL_SEX_MIG_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_NFTL_SEX_MIG_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1","sex")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_NFTL_SEX_MIG_NB.csv', row.names=FALSE, na="")
  
}

# Only 2014, Wait for Eurostat's reply
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_FATL_SEX_MIG_NB <- function (check = TRUE) {
  
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #library(readxl)
  #Input <-  read_excel("./input/DataTransmisionChangeOfFormat.xlsx", sheet = "INJ_FATL_SEX_MIG_NB")
  #saveRDS(Input, paste0('./input/Input.INJ_FATL_SEX_MIG_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_FATL_SEX_MIG_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  
  
  ###### Maps
  #Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  #Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  #Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Severity.Map <- read.csv('./input/maps/severity.DaysLost.Map.csv', stringsAsFactors = FALSE)
  Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "FatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  #Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "DaysLost")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #Reshape
    gather( names, data, SEX_F_NAT:SEX_T_TOTAL) %>%
    mutate( sex = str_sub(names,1,5)) %>%
    mutate( citizen = str_sub(names,7,-1)) %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    
    #sources
    left_join(Source.Map, by="ref_area") %>%
    left_join(Migrant.Map, by="citizen") %>%
    
    ##Via Filter
    #filters for mapped variables:
    
    #Odd Value
    filter( !(ref_area=="FRA"&time==2008) ) %>%
    
    #Adding Up
    mutate( data = if_else(data=="c","1-3",data)) %>%
    mutate( Min= as.integer(str_extract(data,"^[:digit:]+") )) %>%
    mutate( Max= as.integer(str_extract(data,"[:digit:]+$") )) %>%
    mutate( obs_value= (Min+Max)/2 ) %>%

    
    ### Format Issues
    mutate(
      
      obs_status = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_FATL_SEX_MIG_NB",
      
      classif1 = MIG_STATUS,
      classif2 = NA_character_,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select( -citizen, -ref_arealabel,  -MIG_STATUS, -data, -names, -Min, -Max)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_fatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_fatal), T13 , paste0(T13,"_",T14_fatal) ) ) 
  
  Output <- Output[  ,1:13]
  
  
  
  #Exclude Unreliable Countries (since it's the eurostat ad hoc transmision the exclusions are not included)
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_FATL_SEX_MIG_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_FATL_SEX_MIG_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_FATL_SEX_MIG_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1","sex")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_FATL_SEX_MIG_NB.csv', row.names=FALSE, na="")

}

# Only 2014, Wait for Eurostat's reply
REP_EUROSTAT.EASW_ANNUAL.AdHocTransmission.INJ_NFTL_SEX_INJ_MIG_NB <- function (check = TRUE) {
  
  rm(list=setdiff(ls(), c("ilo")))
  require(ilo)
  require(dplyr)
  
  #Get Input
  #library(readxl)
  #Input <-  read_excel("./input/DataTransmisionChangeOfFormat.xlsx", sheet = "INJ_NFTL_SEX_INJ_MIG_NB")
  #saveRDS(Input, paste0('./input/Input.INJ_NFTL_SEX_INJ_MIG_NB.RDS'))
  
  #Read Input
  Input <- readRDS(paste0('./input/Input.INJ_NFTL_SEX_INJ_MIG_NB.RDS'))
  
  Input[] <- lapply(Input, as.character)
  
  
  ###### Maps
  #Country.Map <- read.csv('./input/maps/geo.ref_area.Map.csv', stringsAsFactors = FALSE)
  #Flags.Map <- read.csv('./input/maps/flags.obs_status.Map.csv', stringsAsFactors = FALSE)
  #Sector.Map <- read.csv('./input/maps/nace_r2.ECO_ISIC4.Map.csv', stringsAsFactors = FALSE)
  #Severity.Map <- read.csv('./input/maps/severity.DaysLost.Map.csv', stringsAsFactors = FALSE)
  Migrant.Map <- read.csv('./input/maps/citizen.MIG_STATUS.Map.csv', stringsAsFactors = FALSE)
  library(readxl)
  Source.Map  <- read_excel("./input/maps/MapSource.xlsx", sheet = "NonFatalSource")
  Metadata.Map  <- read_excel("./input/maps/MappingMetadata.xlsx", sheet = "Clean")
  Exclusion.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "PermanentVsTemporary")
  ExclusionGeneralNFTL.Map <- read_excel("./input/maps/Exclusions.xlsx", sheet = "NFTL")
  ############## Modifying input, and saving output
  Output <- Input %>%
    
    #Reshape
    gather( names, data, PERM_SEX_F_NAT:TEMP_SEX_M_TOTAL) %>%
    mutate( INJ_INCAPACITY = paste0( "INJ_INCAPACITY_",str_sub(names,1,4) ),
            INJ_INCAPACITY = str_replace(INJ_INCAPACITY,"E","")
            ) %>%
    mutate( sex = str_sub(names,6,10)) %>%
    mutate( citizen = str_sub(names,12,-1)) %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    
    #sources
    left_join(Source.Map, by="ref_area") %>%
    left_join(Migrant.Map, by="citizen") %>%
    
    ##Via Filter
    #filters for mapped variables:
    
    
    #Adding Up
    mutate( data = if_else(data=="c","1-3",data)) %>%
    mutate( Min= as.integer(str_extract(data,"^[:digit:]+") )) %>%
    mutate( Max= as.integer(str_extract(data,"[:digit:]+$") )) %>%
    mutate( obs_value= (Min+Max)/2 ) %>%
    
    ### Format Issues
    mutate(
      
      obs_status = NA_character_,
      note_classif = NA_character_,
      note_indicator = NA_character_,
      collection = "YI",
      
      indicator = "INJ_NFTL_SEX_INJ_MIG_NB",
      
      classif1 = INJ_INCAPACITY,
      classif2 =  MIG_STATUS,
      note_source = "R1:2383_R1:3903_S3:5"
      
    ) %>%
    select( -citizen, -ref_arealabel,  -MIG_STATUS, -data, -names, -Min, -Max, -INJ_INCAPACITY)
  
  ## Adding necessary Metadata
  
  Output <- Output %>%
    
    ### Mappings (implies data conversion and/or selection)
    ## Via Map
    #ref_area
    left_join(Metadata.Map , by="ref_area" ) %>%
    
    mutate( note_source = if_else( is.na(S7), note_source , paste0(note_source,"_",S7)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2014) | time>=2014, note_source , paste0(note_source,"_",S7_less2014)  ) ) %>%
    mutate( note_source = if_else( is.na(S7_less2013) | time>=2013, note_source , paste0(note_source,"_",S7_less2013)  ) ) %>%
    
    mutate( note_source = if_else( is.na(S8), note_source , paste0(note_source,"_",S8)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2008) | time!=2008, note_source , paste0(note_source,"_",S8_2008)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2009) | time!=2009, note_source , paste0(note_source,"_",S8_2009)  ) ) %>%
    mutate( note_source = if_else( is.na(S8_2010_2012) | (time<2010|time>2012  ), note_source , paste0(note_source,"_",S8_2010_2012)  ) ) %>%
    
    mutate( note_source = paste0(note_source,"_",S9_nonfatal) ) %>%
    
    mutate( note_indicator = if_else(is.na(T14_nonfatal), paste0("I3:2920","_",T13,"_",T14_nonfatal) , paste0("I3:2920","_",T13,"_",T14_nonfatal) ) ) 
  
  
  Output <- Output[  ,1:13]
  
  
  #Exclude Unreliable Countries (since it's the eurostat ad hoc transmision the exclusions are not included)
  
  
  # We need to compute the total across sex
  Output <- Output %>%
    spread(sex, obs_value) %>%
    mutate(SEX_T=SEX_F+SEX_M) %>%
    gather(sex, obs_value, SEX_F, SEX_M, SEX_T)
  
  # We need to compute the total across classif1 (INJ_INCAPACITY)
  Output <- Output %>%
    spread(classif1, obs_value) %>%
    mutate(INJ_INCAPACITY_TOTAL=INJ_INCAPACITY_PRM+INJ_INCAPACITY_TMP) %>%
    gather(classif1, obs_value, INJ_INCAPACITY_TOTAL, INJ_INCAPACITY_PRM, INJ_INCAPACITY_TMP)
  
  #Exclude Unreliable Countries
  Output <- Output %>% 
    merge (Exclusion.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion) %>%
    
    merge (ExclusionGeneralNFTL.Map, all=TRUE) %>% 
    filter( is.na(exclusion) ) %>%
    select(- exclusion)
  
  
  #Compare with existing data
  #Target <- get_ilo(indicator='INJ_NFTL_SEX_INJ_MIG_NB')
  #saveRDS(Target, paste0('./input/Target.INJ_NFTL_SEX_INJ_MIG_NB.RDS'))
  
  #Read Target
  Target <- readRDS(paste0('./input/Target.INJ_NFTL_SEX_INJ_MIG_NB.RDS'))
  
  Comparison <- merge(Output %>% mutate(new=1), Target,all.x = TRUE, all.y = TRUE,by= c("ref_area","time","source", "classif1","sex","classif2")) %>% filter(new==1)
  
  write.csv(Output, file = './input/Output.INJ_NFTL_SEX_INJ_MIG_NB.csv', row.names=FALSE, na="")
}



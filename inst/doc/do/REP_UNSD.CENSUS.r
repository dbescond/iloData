#' This Script rprocesses UNSD census data
#' 
#'
#' 
#' @param work_directory inside the DATA repo, default is 'REP_UNSD/CENSUS'
#' @param check for automation, return a warning file at the root, default is TRUE
 #' @return csv files compare with ilostat database
#'
#' @examples
#' ## Not run:
#'	
#' ## End(**Not run**)
#' @export
#' @rdname Micro_process
#' 
#' 




## **************************************************************************************
# PREDEFINED HELPER FUNCTIONS ***********
# ***************************************************************************************

ASSIGN_ISO_CODE <- function (X) {
  
  # IMPORTANT: FRANCE INCLUDES OVERSEAS DEPARTMENTS
  
  ISO3 <- ilo$code$cl_country %>% select(code, label_en) %>% rename(Country = label_en)
  X <- X %>% left_join(ISO3, by="Country")
  
  X <- X %>% 
    mutate(code = if_else(is.na(code), Country, code ))
  X <- X %>%
    mutate(complete_code =
    recode(X$code,
           "Bolivia (Plurinational State of)" = "BOL" ,
           "Cabo Verde" = "CPV" ,
           "China, Hong Kong SAR" = "HKG" ,
           "China, Macao SAR" = "MAC" ,
           "Czechia" = "CZE" ,
           "DPR of Korea" = "PRK" ,
           "Former Sudan" = "SDN" ,
           "France including Overseas Departments" = "FRA" ,
           "Guernsey (Channel Islands)" = "GGY",
           "Iran (Islamic Republic of)" = "IRN" ,
           "Jersey (Channel Islands)" = "JEY" ,
           "Lao PDR" = "LAO" ,
           "Micronesia (Federated States of)" = "FSM" ,
           "Pitcairn" = "TODROP" ,
           "Rep. of Korea" = "KOR" ,
           "Rep. of South Sudan" = "SSD" ,
           "Reunion" = "REU" ,
           "Russia" = "RUS" ,
           "Saint Helena ex. dep." = "SHN" ,
           "Sint Maarten (Dutch part)" = "SXM" ,
           "State of Palestine" = "PSE" ,
           "United Kingdom of Great Britain and Northern Ireland" = "GBR" ,
           "United Republic of Tanzania" = "TZA" ,
           "United States of America" = "USA" ,
           "US Virgin Islands" = "VIR" ,
           "Venezuela (Bolivarian Republic of)" = "VEN"  
           
           )
    )
}

ASSIGN_SURVEY_CODE <- function (X) {
  surveys <- ilo$code$cl_survey
  censuses <- surveys[str_detect(surveys$code,"AA:") , c("code", "label_en")] %>%
    mutate(label_en = substr(label_en, 1, 3)) %>% 
    rename( ref_area = label_en) %>%
    rename( source = code) %>%
    # this needs to be checked, as of 26/04/2017 there is no duplicates
    distinct(ref_area, .keep_all=TRUE)
  
  X <- X %>% left_join(censuses, by="ref_area")
}



## **************************************************************************************
# MAIN***** IMPORTANT, THE FUNCTION IS STRICT WITH RESPECT TO AGE CATEGORIES, DATA IS ONLY KEPT IF IT CAN BE UNAMBIGUOSLY MAPPED TO AGE_5YRBANDS
# ***************************************************************************************

REP_UNSD.CENSUS <- function (check = TRUE) {

  ## parameters
  work_directory = "REP_UNSD/CENSUS"
  

  ## Initialize
  setwd(paste0(ilo:::path$data, work_directory))
  require(ilo)
  
  
  init_ilo()
  
  # Note the indicators collected are set to ILOEST collection (should be changed to adequate one)
  
####################################################################################################################################****************      
##################################################################################################**************************************************   
##################################################  ************************************************************************************************
##############  INDICATOR 1 ************************************************************************************************************************
#***************************************************************************************************************************************************
  ## Source indicator B07 Population by age, sex and urban/rural residence - UNSD
  ## Target indicator POP_2POP_SEX_AGE_GEO_NB - ILOSTAT
  rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
  # taget classification AGE_5YRBANDS
  age_groups <- c("AGE_5YRBANDS_Y00-04", "AGE_5YRBANDS_Y05-09", "AGE_5YRBANDS_Y10-14", "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65", "AGE_5YRBANDS_TOTAL")
  
  ### Import
  X <- readxl:::read_excel(paste0('./input/', 'B07.xlsx'))
  colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
  colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
  X$Data <- as.integer(X$Data)
  
  ### Processing 
  Xp <- X %>% 
    
    ## Avoiding duplicates
   
    # Choosing the ones with the maximum disagregation
    group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
    mutate(age_data_richness=n()) %>%
    ungroup() %>%
    group_by(Country, Ref.Year, Area, Sex) %>%
    mutate(max_age_data_richness=max(age_data_richness)) %>%
    filter(age_data_richness==max_age_data_richness) %>%
    ungroup() %>%

    # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
    #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
    within( {sort = ave(Data,  FUN = seq_along)} ) %>%
    group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
    mutate(UNSD_sort=min(sort)) %>%
    ungroup() %>%
    group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
    mutate(minUNSD_sort=min(UNSD_sort)) %>%
    ungroup() %>%
    filter(minUNSD_sort==UNSD_sort) %>%
    
    # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
    
    # detect duplicates, (with precaution that having the exact same number of data categories might not necessarily mean that the age categories coincide - (it does in practice))
    # this is just a checking step
    #group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
    #mutate(pro_max_number_duplicates=n()) %>%
    #ungroup() %>%
    #group_by(Country, Ref.Year, Area, Sex) %>%
    #mutate(max_number_duplicates=max(pro_max_number_duplicates)) %>%
    #ungroup() %>%
  
  
    ## Match Format
    mutate(collection = "ILOEST") %>%
    ASSIGN_ISO_CODE() %>%
    rename( ref_area = complete_code ) %>%
    filter( ref_area!="TODROP") %>%
    ASSIGN_SURVEY_CODE() %>%
    mutate(indicator = "POP_2POP_SEX_AGE_GEO_NB" ) %>%
    mutate(sex =
             Sex %>%
             recode( "Both Sexes" = "SEX_T",
                     "Male" = "SEX_M",
                     "Female" = "SEX_F")
             ) %>%
    mutate(classif1 =
             Var.1 %>%
             recode( "Age: 0 - 4"="AGE_5YRBANDS_Y00-04",
                     "Age: 5 - 9"="AGE_5YRBANDS_Y05-09",
                     "Age: 10 - 14"="AGE_5YRBANDS_Y10-14",
                     "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                     "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                     "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                     "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                     "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                     "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                     "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                     "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                     "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                     "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                     "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                     "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                     "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                     "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                     "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                     "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                     "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                     "Age: 100 +"="AGE_5YRBANDS_YGE65",
                     "Age: Total"="AGE_5YRBANDS_TOTAL")
    ) %>%
    


    #Final formating issues
    mutate(classif2 =
             Area %>%
             recode( "Total" = "GEO_COV_NAT",
                     "Urban" = "GEO_COV_URB",
                     "Rural" = "GEO_COV_RUR")
    ) %>%
    mutate(time = Ref.Year,
           obs_value=Data,
           obs_status="",
           note_classif="",
           note_indicator="",
           note_source="R1:2474") 

  ## Obtaining the actual AGE_5YRBANDS_YGE65 (When the format does not fit perfectly the 5 year constant bands until 100+)
  # Note: Only explained in detail this once! 
  # ************************************************************************************ (Not an explicit function)

  # The number of categories in the the band GE65 is obtained, also data on the bands included in the GE65 is kept, 
  # notice that in the GE65 there should be 8 intervals (according to the recode above) if not, an adjustment is needed
  Xp <- Xp %>%      
    # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
    mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
        group_by(ref_area, time, sex, classif1, classif2) %>%
        mutate(Count=n()) %>%
        ungroup() %>%
        group_by(ref_area, time, sex,classif2) %>%
        mutate(maxCount=max(Count)) %>%
        ungroup() %>%
        select(-Count)
  
  # detecting the categories which encompass subsequent bands (for instance 75+)
  Xp$flag <- str_detect(Xp$classif1,"[//+]" )
  # the age of the cuttof is also obtained, (for instance for 75+ the number 75 is stored )
  Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
    # this discards countries that start at 60+ orless, (as we want the band 60-65 to be present)
    mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
    group_by(ref_area, time, sex,classif2) %>%
    # we ensure now that we select the lowest cuttof value available (for instance 75+ instead of 85+)
    mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
    ungroup() %>%
    # this variable (pro_classif1) is obtained to add when appropiate the band AGE_5YRBANDS_YGE65 to one of the cuttof bands (75+ for instance)
    mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
    # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category if the result is "Problem" we should not include the band
    # (for instance if 75+ cuttof is taken, we then would not want to include 85-90), notice that Age: 100 + has to be discarded as well (the variable OLDEST_INCLUDED_IN_OLD is missing for those cases)
    mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") )%>%
    # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
    mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
    group_by(ref_area,sex, classif2, time) %>%
    mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 

# The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
  Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
    mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
    mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
  
  Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"

  # A copy is maintained because now information will start to be discarded
  Xcopy <- Xp
  
   # This simply obtains the totals of the group AGE_5YRBANDS_YGE65
   Xp <- Xp %>% 
          group_by(ref_area, time, sex, classif1, classif2) %>%
          mutate(Test = sum(obs_value)) %>%
          ungroup() %>%
          mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
          mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
          filter(classif1 %in% age_groups) %>%
          distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)

    #This removes cases when ANY of the target age categories is not defined (for instance 15-19 might not be defined if the country has another split)
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==15) 
    # note, countries lost "BTN" "FSM" "MCO" "NCL" "PER" "QAT" "SMR" "TGO" "VNM"
  # **************************************************************************************************************
  
    #**************
    ## Numerical Checks 
    
    #totals should add up
    SampleTotals <- Xp %>% filter(classif1!="AGE_5YRBANDS_TOTAL") %>%
    group_by( ref_area,  sex, classif2, time) %>%
    summarise(obs_value=sum(obs_value)) %>%
    ungroup() 
    
    #Notice that we can have a discrepancy due to either the missing category, or to mistakes in source data (see excel file in help document) 
    Check <- left_join(Xp, SampleTotals, by = c("ref_area","sex","classif2", "time")) %>%
      mutate(discrepancy = if_else( classif1=="AGE_5YRBANDS_TOTAL", obs_value.x - obs_value.y , as.integer(0) ) ) %>%
      filter(classif1=="AGE_5YRBANDS_TOTAL")
    # Match the unknown category with the discrepancy, and remove if it fits
    Check <- Check %>% 
      left_join( Xcopy %>% filter(classif1=="Age: Unknown") %>% 
                   select(ref_area, sex, classif2, time, obs_value) %>% 
                   rename(Unkown=obs_value) , by = c("ref_area","sex","classif2", "time")
                 ) %>%
      filter(discrepancy != Unkown | is.na(Unkown)) %>%
      filter( discrepancy != 0 )
      
    # notice the only countries left with an issue are (26/04/2017 - checked) are posted below
    # I have not checked all countries in the list but many of them present numerical errors at the source (including the one with the largest deviations TURKEY)
    #Algeria	Antigua and Barbuda	Aruba	Canada	Cayman Islands	Côte d'Ivoire	France including Overseas Departments	Honduras	Israel	Jamaica	Kenya	New Zealand	Sint Maarten (Dutch part)	South Africa	Spain	Suriname	Thailand	Trinidad and Tobago	Turkey
    # ******************
    
    
    
    # ************************************* ONLY RUN AFTER CHECKING THAT THE TOTALS ARE CORRECT, as this substitutes the original data (with data generated during the Checks) *************************
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) %>%
      left_join(SampleTotals, by = c("ref_area","sex","classif2", "time")) %>%
      mutate( obs_value.x = if_else(classif1 == "AGE_5YRBANDS_TOTAL", obs_value.y, obs_value.x )) %>% 
      select(-obs_value.y) %>%
      rename(obs_value = obs_value.x)
    
    saveRDS(Xfinal,paste0('./output/', 'POP_2POP_SEX_AGE_GEO_NB___FROM___B07.RDS'))
    # readRDS(paste0('./output/', 'POP_2POP_SEX_AGE_GEO_NB___FROM___B07.RDS'))
    
####################################################################################################################################****************      
##################################################################################################**************************************************     
##################################################  ************************************************************************************************
##############  INDICATOR 2 ************************************************************************************************************************
#***************************************************************************************************************************************************   
    ## Source indicator B15 Population 15 years of age and over, by educational attainment, age and sex - UNSD
    ## Target indicator POP_XWAP_SEX_AGE_EDU_NB - ILOSTAT
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
    
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B15.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
     ## First format steps
    Xp <- X %>% 
      
      ## Only data that corresponds to ISCED (97)
      filter(str_detect(X$Var.4,"ISCED")) %>%
      ## Avoiding duplicates
        # must be checked, but on 26/04/2017 there are none

      ## Format
      mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "POP_XWAP_SEX_AGE_EDU_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                       "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                       "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                       "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                       "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                       "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                       "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                       "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                       "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                       "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                       "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                       "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                       "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                       "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                       "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                       "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                       "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                       "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                       "Age: 100 +"="AGE_5YRBANDS_YGE65")
             # notice that in this case there is very few totals, therefore it is removed from the categories
      ) %>%
      
      #Final formating issues, notice that no collapsing of unknown and other categories is required as they do not coexist in any given country-year obsevation
      mutate(classif2 =
               Var.4 %>%
               recode( "Educational attainment: Pre-primary education (Level 0) [ISCED 1997]"="EDU_ISCED97_0",
                       "Educational attainment: Primary education (Level 1) [ISCED 1997]"="EDU_ISCED97_1",
                       "Educational attainment: Lower secondary education (Level 2) [ISCED 1997]"="EDU_ISCED97_2",
                       "Educational attainment: Upper secondary education (Level 3) [ISCED 1997]"="EDU_ISCED97_3",
                       "Educational attainment: Post-secondary education (Level 4) [ISCED 1997]"="EDU_ISCED97_4",
                       "Educational attainment: First stage of tertiary education (Level 5) [ISCED 1997]"="EDU_ISCED97_5",
                       "Educational attainment: Second stage of tertiary education (Level 6) [ISCED 1997]"="EDU_ISCED97_6",
                       "Educational attainment: Unknown [ISCED 1997]"="EDU_ISCED97_UNK",
                       "Educational attainment: Other [ISCED 1997]"="EDU_ISCED97_UNK",
                       "Educational attainment: Total [ISCED 1997]"="EDU_ISCED97_TOTAL",
                       "Educational attainment: No education [ISCED 1997]"="EDU_ISCED97_X")
      ) %>%
      
      
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
      ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )

    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 
    
    # The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"

    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)
    
    
    
    
    
    #removing cases when ALL the age categories are not defined
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==11) 
    # note countries lost ARM	IRN	KGZ	LSO	MYT	MEX	PLW	CHE	THA	TKL	GBR	VNM
    # **************************************************************************************************************
    
    ###Check
    
    #Checking the totals across education, notice complete mismatch in original data (for instance brazil the totals have nothing to do with the sum of the components in all likelyhood due to missing categories)
    SampleTotals <- Xp  %>% filter(classif2!="EDU_ISCED97_TOTAL") %>%
      group_by( ref_area,  sex, classif1, time) %>%
      mutate(obs_value=sum(obs_value)) %>%
      ungroup() 
    Check <- left_join(Xp, SampleTotals, by = c("ref_area","sex","classif1", "time")) %>%
      mutate(discrepancy = if_else( classif2.x=="EDU_ISCED97_TOTAL", obs_value.x - obs_value.y , as.integer(0) ) ) %>%
      filter(classif2.x=="EDU_ISCED97_TOTAL")
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'POP_XWAP_SEX_AGE_EDU_NB___FROM___B15.RDS'))
    #readRDS(paste0('./output/', 'POP_XWAP_SEX_AGE_EDU_NB___FROM___B15.RDS'))
    
####################################################################################################################################****************      
##################################################################################################**************************************************   
##################################################  ************************************************************************************************
##############  INDICATOR 3 ************************************************************************************************************************
#***************************************************************************************************************************************************  
    ## Source indicator B17 Population by activity status, age, sex and urban/rural residence - UNSD
    ## Target indicator POP_2WAP_SEX_AGE_GEO_LMS_NB - ILOSTAT
    #note, indicator does not exist in ILOSTAT
    # Target classification AGE_5YRBANDS
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
    
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B17.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>% 
      ## Avoiding duplicates, only 3 countries with record duplicates 27/04/2017

      #Choosing UNSD sort
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex, Var.1, Var.4) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # detection, must be checked (that the procedure works)
      #group_by(Country, Ref.Year, Area, Sex, Var.1, Var.4) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      #group_by(Country, Ref.Year, Area, Sex) %>%
      #mutate(max_number_duplicates=max(pro_max_number_duplicates)) %>%
      
      ## Format
      mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "POP_2WAP_SEX_AGE_GEO_LMS_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                 "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                 "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                 "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                 "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                 "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                 "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                 "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                 "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                 "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                 "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                 "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                 "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                 "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                 "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                 "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                 "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                 "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                 "Age: 100 +"="AGE_5YRBANDS_YGE65")
             # notice that in this case there is very few totals, therefore it is removed from the categories
      ) %>%
      
      #Final formating issues

      mutate(classif2 =
               Area %>%
               recode( "Total" = "GEO_COV_NAT",
                       "Urban" = "GEO_COV_URB",
                       "Rural" = "GEO_COV_RUR") 
             )%>%
      
      #notice that only Employed, Unemployed and Total are gathered, Inactive will be obtained by substraction (Inactive=Total-Employed-Unemployed)
      # this follows the same convention that in micro processing, however would be interesting to cross check with the total population obtained above
      mutate(classif3 =
               Var.4 %>%
               recode( "Activity status: Employed"="LMS_STATUS_EMP",
                       "Activity status: Unemployed"="LMS_STATUS_UNE",
                       "Activity status: Total population"="LMS_STATUS_TOTAL")
      ) %>%
      # here all other categories are dropped, notice that this incldue
      filter(classif3 %in% c("LMS_STATUS_EMP","LMS_STATUS_UNE","LMS_STATUS_TOTAL")) %>%
      
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing
      # notice that the function is more general, in the present case all countries present 2 categories
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2, classif3) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2, classif3) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )
    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2, classif3) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, classif3, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 

    
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"
    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2, classif3) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, classif3, time, .keep_all=TRUE)
    
    
    
    
    
    #removing cases when ALL the age categories are not defined
    Xp <- Xp %>% left_join( Xp %>% group_by( ref_area,sex, classif2, classif3, time) %>% count(), by = c("ref_area","sex","classif2","classif3", "time"), copy = FALSE) %>%
                  filter(n == 11) %>%
                  select(-n)
    
    Xp <- Xp %>%  left_join( Xp %>% group_by( ref_area,sex, classif1, classif2, time) %>% count(), by = c("ref_area","sex","classif1","classif2", "time"), copy = FALSE) %>%
                  filter(n == 3)
    # **************************************************************************************************************

    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, classif3, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    # The number of inactives have to be computed
    Xfinal <- Xfinal %>% spread(classif3,obs_value) %>%
                mutate(LMS_STATUS_EIP= LMS_STATUS_TOTAL - LMS_STATUS_EMP- LMS_STATUS_UNE) %>%
                gather(classif3, obs_value, LMS_STATUS_EMP:LMS_STATUS_EIP ) %>%
                filter(!is.na(obs_value))
    
    ### Check, compare to the previous totals and the inactive category in this round of raw data (this second one will not match perfectly precisely beacuse of the odd categories)
    
    #To develop
    
    
    ###
    saveRDS(Xfinal,paste0('./output/', 'POP_2WAP_SEX_AGE_GEO_LMS_NB___FROM___B17.RDS'))
    #readRDS(paste0('./output/', 'POP_2WAP_SEX_AGE_GEO_LMS_NB___FROM___B17.RDS'))
    
####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 4 ************************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B42 Employed population by status in employment, age and sex - UNSD
    ## Target indicator EMP_2EMP_SEX_AGE_STE_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
    
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B42.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
 
      # Choosing the ones with the maximum disagregation
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(age_data_richness=n()) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(max_age_data_richness=max(age_data_richness)) %>%
      filter(age_data_richness==max_age_data_richness) %>%
      ungroup() %>%
      
      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.1, Var.2) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%

    
    ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_AGE_STE_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                       "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                       "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                       "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                       "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                       "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                       "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                       "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                       "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                       "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                       "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                       "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                       "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                       "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                       "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                       "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                       "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                       "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                       "Age: 100 +"="AGE_5YRBANDS_YGE65")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.2 %>%
               recode( "Status in employment: Contributing family members"="STE_ICSE93_5",
                       "Status in employment: Employee"="STE_ICSE93_1",
                       "Status in employment: Employer"="STE_ICSE93_2",
                       "Status in employment: Member of producers' cooperative"="STE_ICSE93_4",
                       "Status in employment: Other"="STE_ICSE93_6",
                       "Status in employment: Own account worker"="STE_ICSE93_3",
                       "Status in employment: Total"="STE_ICSE93_TOTAL",
                       "Status in employment: Unknown"="STE_ICSE93_6")
      ) %>%
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 

    
    ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )
    
    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 
    
    # The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"
    
    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)
    
    
    
    
    
    #removing cases when ALL the age categories are not defined
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==11) 
    # note countries lost ARM	IRN	KGZ	LSO	MYT	MEX	PLW	CHE	THA	TKL	GBR	VNM
    # **************************************************************************************************************
    
    ###Check, notice that there are large numerical errors in the input data
    # the countries with the largest errors are:
    # JPN, AZE, DOM, TUR - I have checked these 4 manually and indeed there are numerical errrors (for Turkey the errors are present in the breakdown of age not the global 15+)
 
    SampleTotals <- Xp  %>% filter(classif2!="STE_ICSE93_TOTAL") %>%
      group_by( ref_area,  sex, classif1, time) %>%
      mutate(obs_value=sum(obs_value)) %>%
      ungroup() %>%
      distinct(ref_area,  sex, classif1, time, obs_value)
    Check <- left_join(Xp, SampleTotals, by = c("ref_area","sex","classif1", "time")) %>%
      mutate(discrepancy = if_else( classif2=="STE_ICSE93_TOTAL", obs_value.x - obs_value.y , as.integer(0) ) ) %>%
      filter(classif2=="STE_ICSE93_TOTAL")
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EMP_2EMP_SEX_AGE_STE_NB___FROM___B42.RDS'))
    #readRDS(paste0('./output/', 'EMP_2EMP_SEX_AGE_STE_NB___FROM___B42.RDS'))
    
####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 5 ************************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B43 Employed population by industry, age and sex - UNSD
    ## Target indicator EMP_2EMP_SEX_AGE_ECO_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
    ISIC3 <- c("ECO_ISIC3_A", "ECO_ISIC3_B", "ECO_ISIC3_C", "ECO_ISIC3_D", "ECO_ISIC3_E", "ECO_ISIC3_F", "ECO_ISIC3_G", "ECO_ISIC3_H", "ECO_ISIC3_I", "ECO_ISIC3_J", "ECO_ISIC3_K", "ECO_ISIC3_L", "ECO_ISIC3_M", "ECO_ISIC3_N", "ECO_ISIC3_O", "ECO_ISIC3_P", "ECO_ISIC3_Q", "ECO_ISIC3_TOTAL", "ECO_ISIC3_X")
    ISIC4 <- c("ECO_ISIC4_A", "ECO_ISIC4_B", "ECO_ISIC4_C", "ECO_ISIC4_D", "ECO_ISIC4_E", "ECO_ISIC4_F", "ECO_ISIC4_G", "ECO_ISIC4_H", "ECO_ISIC4_I", "ECO_ISIC4_J", "ECO_ISIC4_K", "ECO_ISIC4_L", "ECO_ISIC4_M", "ECO_ISIC4_N", "ECO_ISIC4_O", "ECO_ISIC4_P", "ECO_ISIC4_Q", "ECO_ISIC4_R", "ECO_ISIC4_S", "ECO_ISIC4_T", "ECO_ISIC4_TOTAL", "ECO_ISIC4_U", "ECO_ISIC4_X")
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B43.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
      
      # Choosing the ones with the maximum disagregation
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(age_data_richness=n()) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(max_age_data_richness=max(age_data_richness)) %>%
      filter(age_data_richness==max_age_data_richness) %>%
      ungroup() %>%
      
      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.1, Var.3) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      
      
      ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_AGE_ECO_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                 "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                 "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                 "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                 "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                 "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                 "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                 "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                 "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                 "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                 "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                 "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                 "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                 "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                 "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                 "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                 "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                 "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                 "Age: 100 +"="AGE_5YRBANDS_YGE65")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.3 %>%
               recode( #ISIC 3-3.1
                       "Industry: Agriculture, hunting and forestry (ISIC Rev.3/3.1: Section A)"="ECO_ISIC3_A",
                       "Industry: Fishing (ISIC Rev.3/3.1: Section B)"="ECO_ISIC3_B",
                       "Industry: Mining and quarrying (ISIC Rev.3/3.1: Section C)"="ECO_ISIC3_C",
                       "Industry: Manufacturing (ISIC Rev.3/3.1: Section D)"="ECO_ISIC3_D",
                       "Industry: Electricity, gas and water supply (ISIC Rev.3/3.1: Section E)"="ECO_ISIC3_E",
                       "Industry: Construction (ISIC Rev.3/3.1: Section F)"="ECO_ISIC3_F",
                       "Industry: Wholesale and retail trade; repair of motor vehicles, motorcycles and personal and household goods (ISIC Rev.3/3.1: Section G)"="ECO_ISIC3_G",
                       "Industry: Hotels and restaurants (ISIC Rev.3/3.1: Section H)"="ECO_ISIC3_H",
                       "Industry: Transport, storage and communications (ISIC Rev.3/3.1: Section I)"="ECO_ISIC3_I",
                       "Industry: Financial intermediation (ISIC Rev.3/3.1:  Section J)"="ECO_ISIC3_J",
                       "Industry: Real estate, renting and business activities (ISIC Rev.3/3.1: Category K)"="ECO_ISIC3_K",
                       "Industry: Public administration and defense; compulsory social security (ISIC-Rev.3/3.1: Section L)"="ECO_ISIC3_L",
                       "Industry: Education (ISIC Rev.3/3.1: Section M)"="ECO_ISIC3_M",
                       "Industry: Health and social work (ISIC Rev.3/3.1: Section N)"="ECO_ISIC3_N",
                       "Industry: Other community, social and personal service activities (ISIC Rev.3/3.1: Section O)"="ECO_ISIC3_O",
                       "Industry: Activities of private households as employers and undifferentiated production activities of private households (ISIC Rev.3/3.1: Category P)"="ECO_ISIC3_P",
                       "Industry: Extraterritorial organizations and bodies (ISIC Rev.3/3.1: Section Q)"="ECO_ISIC3_Q",
                       "Industry: Total (for ISIC Rev.3/3.1)"="ECO_ISIC3_TOTAL",
                       "Industry: Unknown (ISIC Rev.3)"="ECO_ISIC3_X",
                       # ISIC 4
                       "Industry: Agriculture, forestry and fishing (ISIC Rev.4: Section A)"="ECO_ISIC4_A",
                       "Industry: Mining and quarrying (ISIC Rev.4: Section B)"="ECO_ISIC4_B",
                       "Industry: Manufacturing (ISIC Rev.4: Section C)"="ECO_ISIC4_C",
                       "Industry: Electricity, gas, steam and air conditioning supply (ISIC Rev.4: Section D)"="ECO_ISIC4_D",
                       "Industry: Water supply; sewerage, waste management and remediation activities (ISIC Rev.4: Section E)"="ECO_ISIC4_E",
                       "Industry: Construction (ISIC Rev.4: Section F)"="ECO_ISIC4_F",
                       "Industry: Wholesale and retail trade; repair of motor vehicles and motorcycles (ISIC Rev.4: Section G)"="ECO_ISIC4_G",
                       "Industry: Transportation and storage (ISIC Rev.4: Section H)"="ECO_ISIC4_H",
                       "Industry: Accommodation and food service activities (ISIC Rev.4: Section I)"="ECO_ISIC4_I",
                       "Industry: Information and communication (ISIC Rev.4: Section J)"="ECO_ISIC4_J",
                       "Industry: Financial and insurance activities (ISIC Rev.4: Section K)"="ECO_ISIC4_K",
                       "Industry: Real estate activities (ISIC Rev.4: Section L)"="ECO_ISIC4_L",
                       "Industry: Professional, scientific and technical activities (ISIC Rev.4: Section M)"="ECO_ISIC4_M",
                       "Industry: Administrative and support service activities (ISIC Rev.4: Section N)"="ECO_ISIC4_N",
                       "Industry: Public administration and defence; compulsory social security (ISIC Rev.4: Section O)"="ECO_ISIC4_O",
                       "Industry: Education (ISIC Rev.4: Section P)"="ECO_ISIC4_P",
                       "Industry: Human health and social work activities (ISIC Rev.4: Section Q)"="ECO_ISIC4_Q",
                       "Industry: Arts, entertainment and recreation (ISIC Rev.4: Section R)"="ECO_ISIC4_R",
                       "Industry: Other service activities (ISIC Rev.4: Section S)"="ECO_ISIC4_S",
                       "Industry: Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use (ISIC Rev.4: Section T)"="ECO_ISIC4_T",
                       "Industry: Total (for ISIC Rev.4)"="ECO_ISIC4_TOTAL",
                       "Industry: Activities of extraterritorial organizations and bodies (ISIC Rev.4: Section U)"="ECO_ISIC4_U",
                       "Industry: Unknown (ISIC Rev.4)"="ECO_ISIC4_X"
                       )
      ) %>% # only leaving ISIC 3/3.1 and 4 categories, others (ISIC 1 or 2 and others) are removed
      filter(classif2 %in% ISIC3 | classif2 %in% ISIC4 ) %>%
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    
    ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )
    
    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 
    
    # The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"
    
    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)
    

    #removing cases when ALL the age categories are not defined
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==11) 
    # note countries lost ARM	IRN	KGZ	LSO	MYT	MEX	PLW	CHE	THA	TKL	GBR	VNM
    # **************************************************************************************************************
    
    ###Check
    
    # To develop
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EMP_2EMP_SEX_AGE_ECO_NB___FROM___B43.RDS'))
    #readRDS(paste0('./output/', 'EMP_2EMP_SEX_AGE_ECO_NB___FROM___B43.RDS'))
    
    
####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 6 ************************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B44 Employed population by occupation, age and sex - UNSD
    ## Target indicator EMP_2EMP_SEX_AGE_OCU_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
    ISCO88 <- c("OCU_ISCO88_0", "OCU_ISCO88_1", "OCU_ISCO88_2", "OCU_ISCO88_3", "OCU_ISCO88_4", "OCU_ISCO88_5", "OCU_ISCO88_6", "OCU_ISCO88_7", "OCU_ISCO88_8", "OCU_ISCO88_9", "OCU_ISCO88_TOTAL", "OCU_ISCO88_X")
    ISCO08 <- c("OCU_ISCO08_0", "OCU_ISCO08_1", "OCU_ISCO08_2", "OCU_ISCO08_3", "OCU_ISCO08_4", "OCU_ISCO08_5", "OCU_ISCO08_6", "OCU_ISCO08_7", "OCU_ISCO08_8", "OCU_ISCO08_9", "OCU_ISCO08_TOTAL", "OCU_ISCO08_X")
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B44.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
      
      # Choosing the ones with the maximum disagregation
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(age_data_richness=n()) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(max_age_data_richness=max(age_data_richness)) %>%
      filter(age_data_richness==max_age_data_richness) %>%
      ungroup() %>%
      
      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.1, Var.3) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      
      
      ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_AGE_OCU_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                 "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                 "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                 "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                 "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                 "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                 "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                 "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                 "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                 "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                 "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                 "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                 "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                 "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                 "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                 "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                 "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                 "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                 "Age: 100 +"="AGE_5YRBANDS_YGE65")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.3 %>%
               recode( #ISCO 88
                 "Occupation: Armed forces (ISCO 88 - 0)"="OCU_ISCO88_0",
                 "Occupation: Legislators, senior officials and managers (ISCO 88 - 1)"="OCU_ISCO88_1",
                 "Occupation: Professionals (ISCO 88 - 2)"="OCU_ISCO88_2",
                 "Occupation: Technicians and associate professionals (ISCO 88 - 3)"="OCU_ISCO88_3",
                 "Occupation: Clerks (ISCO 88 - 4)"="OCU_ISCO88_4",
                 "Occupation: Service workers and shop and market sales workers (ISCO 88 - 5)"="OCU_ISCO88_5",
                 "Occupation: Skilled agricultural and fishery workers (ISCO 88 - 6)"="OCU_ISCO88_6",
                 "Occupation: Craft and related trades workers (ISCO 88 - 7)"="OCU_ISCO88_7",
                 "Occupation: Plant and machine operators and assemblers (ISCO 88 - 8)"="OCU_ISCO88_8",
                 "Occupation: Elementary occupations (ISCO 88 - 9)"="OCU_ISCO88_9",
                 "Occupation: Total (ISCO 88)"="OCU_ISCO88_TOTAL",
                 "Occupation: Unknown (ISCO88)"="OCU_ISCO88_X",
                 # ISCO 08
                 "Occupation: Armed forces occupations (ISCO 08 - 0)"="OCU_ISCO08_0",
                 "Occupation: Managers (ISCO 08 - 1)"="OCU_ISCO08_1",
                 "Occupation: Professionals (ISCO 08 - 2)"="OCU_ISCO08_2",
                 "Occupation: Technicians and associate professionals (ISCO 08 - 3)"="OCU_ISCO08_3",
                 "Occupation: Clerical support workers (ISCO 08 - 4)"="OCU_ISCO08_4",
                 "Occupation: Service and sales workers (ISCO 08 - 5)"="OCU_ISCO08_5",
                 "Occupation: Skilled agricultural, forestry and fishery workers (ISCO 08 - 6)"="OCU_ISCO08_6",
                 "Occupation: Craft and related trades workers (ISCO 08 - 7)"="OCU_ISCO08_7",
                 "Occupation: Plant and machine operators, and assemblers (ISCO 08 - 8)"="OCU_ISCO08_8",
                 "Occupation: Elementary occupations (ISCO 08 - 9)"="OCU_ISCO08_9",
                 "Occupation: Total (ISCO 08)"="OCU_ISCO08_TOTAL",
                 "Occupation: Unknown (ISCO 08)"="OCU_ISCO08_X"
               )
      ) %>% # only leaving ISIC 3/3.1 and 4 categories, others (ISIC 1 or 2 and others) are removed
      filter(classif2 %in% ISCO88 | classif2 %in% ISCO08 ) %>%
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    
    ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )
    
    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 
    
    # The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"
    
    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)
    
    
    #removing cases when ALL the age categories are not defined
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==11) 
    # note countries lost ARM	IRN	KGZ	LSO	MYT	MEX	PLW	CHE	THA	TKL	GBR	VNM
    # **************************************************************************************************************
    
    ###Check
    
    # To develop
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EMP_2EMP_SEX_AGE_OCU_NB___FROM___B44.RDS'))
    #readRDS(paste0('./output/', 'EMP_2EMP_SEX_AGE_OCU_NB___FROM___B44.RDS'))
    
    
####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 7 ************************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B45 Employed population by status in employment, industry and sex - UNSD
    ## Target indicator EMP_2EMP_SEX_STE_ECO_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    ISIC3 <- c("ECO_ISIC3_A", "ECO_ISIC3_B", "ECO_ISIC3_C", "ECO_ISIC3_D", "ECO_ISIC3_E", "ECO_ISIC3_F", "ECO_ISIC3_G", "ECO_ISIC3_H", "ECO_ISIC3_I", "ECO_ISIC3_J", "ECO_ISIC3_K", "ECO_ISIC3_L", "ECO_ISIC3_M", "ECO_ISIC3_N", "ECO_ISIC3_O", "ECO_ISIC3_P", "ECO_ISIC3_Q", "ECO_ISIC3_TOTAL", "ECO_ISIC3_X")
    ISIC4 <- c("ECO_ISIC4_A", "ECO_ISIC4_B", "ECO_ISIC4_C", "ECO_ISIC4_D", "ECO_ISIC4_E", "ECO_ISIC4_F", "ECO_ISIC4_G", "ECO_ISIC4_H", "ECO_ISIC4_I", "ECO_ISIC4_J", "ECO_ISIC4_K", "ECO_ISIC4_L", "ECO_ISIC4_M", "ECO_ISIC4_N", "ECO_ISIC4_O", "ECO_ISIC4_P", "ECO_ISIC4_Q", "ECO_ISIC4_R", "ECO_ISIC4_S", "ECO_ISIC4_T", "ECO_ISIC4_TOTAL", "ECO_ISIC4_U", "ECO_ISIC4_X")
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B45.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
      

      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.2, Var.3) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      
      
      ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_STE_ECO_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.2 %>%
               recode( "Status in employment: Contributing family members"="STE_ICSE93_5",
                       "Status in employment: Employee"="STE_ICSE93_1",
                       "Status in employment: Employer"="STE_ICSE93_2",
                       "Status in employment: Member of producers' cooperative"="STE_ICSE93_4",
                       "Status in employment: Other"="STE_ICSE93_6",
                       "Status in employment: Own account worker"="STE_ICSE93_3",
                       "Status in employment: Total"="STE_ICSE93_TOTAL",
                       "Status in employment: Unknown"="STE_ICSE93_6")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.3 %>%
               recode( #ISIC 3-3.1
                 "Industry: Agriculture, hunting and forestry (ISIC Rev.3/3.1: Section A)"="ECO_ISIC3_A",
                 "Industry: Fishing (ISIC Rev.3/3.1: Section B)"="ECO_ISIC3_B",
                 "Industry: Mining and quarrying (ISIC Rev.3/3.1: Section C)"="ECO_ISIC3_C",
                 "Industry: Manufacturing (ISIC Rev.3/3.1: Section D)"="ECO_ISIC3_D",
                 "Industry: Electricity, gas and water supply (ISIC Rev.3/3.1: Section E)"="ECO_ISIC3_E",
                 "Industry: Construction (ISIC Rev.3/3.1: Section F)"="ECO_ISIC3_F",
                 "Industry: Wholesale and retail trade; repair of motor vehicles, motorcycles and personal and household goods (ISIC Rev.3/3.1: Section G)"="ECO_ISIC3_G",
                 "Industry: Hotels and restaurants (ISIC Rev.3/3.1: Section H)"="ECO_ISIC3_H",
                 "Industry: Transport, storage and communications (ISIC Rev.3/3.1: Section I)"="ECO_ISIC3_I",
                 "Industry: Financial intermediation (ISIC Rev.3/3.1:  Section J)"="ECO_ISIC3_J",
                 "Industry: Real estate, renting and business activities (ISIC Rev.3/3.1: Category K)"="ECO_ISIC3_K",
                 "Industry: Public administration and defense; compulsory social security (ISIC-Rev.3/3.1: Section L)"="ECO_ISIC3_L",
                 "Industry: Education (ISIC Rev.3/3.1: Section M)"="ECO_ISIC3_M",
                 "Industry: Health and social work (ISIC Rev.3/3.1: Section N)"="ECO_ISIC3_N",
                 "Industry: Other community, social and personal service activities (ISIC Rev.3/3.1: Section O)"="ECO_ISIC3_O",
                 "Industry: Activities of private households as employers and undifferentiated production activities of private households (ISIC Rev.3/3.1: Category P)"="ECO_ISIC3_P",
                 "Industry: Extraterritorial organizations and bodies (ISIC Rev.3/3.1: Section Q)"="ECO_ISIC3_Q",
                 "Industry: Total (for ISIC Rev.3/3.1)"="ECO_ISIC3_TOTAL",
                 "Industry: Unknown (ISIC Rev.3)"="ECO_ISIC3_X",
                 # ISIC 4
                 "Industry: Agriculture, forestry and fishing (ISIC Rev.4: Section A)"="ECO_ISIC4_A",
                 "Industry: Mining and quarrying (ISIC Rev.4: Section B)"="ECO_ISIC4_B",
                 "Industry: Manufacturing (ISIC Rev.4: Section C)"="ECO_ISIC4_C",
                 "Industry: Electricity, gas, steam and air conditioning supply (ISIC Rev.4: Section D)"="ECO_ISIC4_D",
                 "Industry: Water supply; sewerage, waste management and remediation activities (ISIC Rev.4: Section E)"="ECO_ISIC4_E",
                 "Industry: Construction (ISIC Rev.4: Section F)"="ECO_ISIC4_F",
                 "Industry: Wholesale and retail trade; repair of motor vehicles and motorcycles (ISIC Rev.4: Section G)"="ECO_ISIC4_G",
                 "Industry: Transportation and storage (ISIC Rev.4: Section H)"="ECO_ISIC4_H",
                 "Industry: Accommodation and food service activities (ISIC Rev.4: Section I)"="ECO_ISIC4_I",
                 "Industry: Information and communication (ISIC Rev.4: Section J)"="ECO_ISIC4_J",
                 "Industry: Financial and insurance activities (ISIC Rev.4: Section K)"="ECO_ISIC4_K",
                 "Industry: Real estate activities (ISIC Rev.4: Section L)"="ECO_ISIC4_L",
                 "Industry: Professional, scientific and technical activities (ISIC Rev.4: Section M)"="ECO_ISIC4_M",
                 "Industry: Administrative and support service activities (ISIC Rev.4: Section N)"="ECO_ISIC4_N",
                 "Industry: Public administration and defence; compulsory social security (ISIC Rev.4: Section O)"="ECO_ISIC4_O",
                 "Industry: Education (ISIC Rev.4: Section P)"="ECO_ISIC4_P",
                 "Industry: Human health and social work activities (ISIC Rev.4: Section Q)"="ECO_ISIC4_Q",
                 "Industry: Arts, entertainment and recreation (ISIC Rev.4: Section R)"="ECO_ISIC4_R",
                 "Industry: Other service activities (ISIC Rev.4: Section S)"="ECO_ISIC4_S",
                 "Industry: Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use (ISIC Rev.4: Section T)"="ECO_ISIC4_T",
                 "Industry: Total (for ISIC Rev.4)"="ECO_ISIC4_TOTAL",
                 "Industry: Activities of extraterritorial organizations and bodies (ISIC Rev.4: Section U)"="ECO_ISIC4_U",
                 "Industry: Unknown (ISIC Rev.4)"="ECO_ISIC4_X"
               )
      ) %>% # only leaving ISIC 3/3.1 and 4 categories, others (ISIC 1 or 2 and others) are removed
      filter(classif2 %in% ISIC3 | classif2 %in% ISIC4 ) %>%
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    
    ###Check
    
    # To develop
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EMP_2EMP_SEX_STE_ECO_NB___FROM___B45.RDS'))
    #readRDS(paste0('./output/', 'EMP_2EMP_SEX_STE_ECO_NB___FROM___B45.RDS'))    

####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 8 ************************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B46 Employed population by status in employment, occupation and sex - UNSD
    ## Target indicator EMP_2EMP_SEX_STE_OCU_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    ISCO88 <- c("OCU_ISCO88_0", "OCU_ISCO88_1", "OCU_ISCO88_2", "OCU_ISCO88_3", "OCU_ISCO88_4", "OCU_ISCO88_5", "OCU_ISCO88_6", "OCU_ISCO88_7", "OCU_ISCO88_8", "OCU_ISCO88_9", "OCU_ISCO88_TOTAL", "OCU_ISCO88_X")
    ISCO08 <- c("OCU_ISCO08_0", "OCU_ISCO08_1", "OCU_ISCO08_2", "OCU_ISCO08_3", "OCU_ISCO08_4", "OCU_ISCO08_5", "OCU_ISCO08_6", "OCU_ISCO08_7", "OCU_ISCO08_8", "OCU_ISCO08_9", "OCU_ISCO08_TOTAL", "OCU_ISCO08_X")
    ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B46.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
      
      
      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.2, Var.3) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      
      
      ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_STE_ECO_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.2 %>%
               recode( "Status in employment: Contributing family members"="STE_ICSE93_5",
                       "Status in employment: Employee"="STE_ICSE93_1",
                       "Status in employment: Employer"="STE_ICSE93_2",
                       "Status in employment: Member of producers' cooperative"="STE_ICSE93_4",
                       "Status in employment: Other"="STE_ICSE93_6",
                       "Status in employment: Own account worker"="STE_ICSE93_3",
                       "Status in employment: Total"="STE_ICSE93_TOTAL",
                       "Status in employment: Unknown"="STE_ICSE93_6")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.3 %>%
               recode( #ISCO 88
                 "Occupation: Armed forces (ISCO 88 - 0)"="OCU_ISCO88_0",
                 "Occupation: Legislators, senior officials and managers (ISCO 88 - 1)"="OCU_ISCO88_1",
                 "Occupation: Professionals (ISCO 88 - 2)"="OCU_ISCO88_2",
                 "Occupation: Technicians and associate professionals (ISCO 88 - 3)"="OCU_ISCO88_3",
                 "Occupation: Clerks (ISCO 88 - 4)"="OCU_ISCO88_4",
                 "Occupation: Service workers and shop and market sales workers (ISCO 88 - 5)"="OCU_ISCO88_5",
                 "Occupation: Skilled agricultural and fishery workers (ISCO 88 - 6)"="OCU_ISCO88_6",
                 "Occupation: Craft and related trades workers (ISCO 88 - 7)"="OCU_ISCO88_7",
                 "Occupation: Plant and machine operators and assemblers (ISCO 88 - 8)"="OCU_ISCO88_8",
                 "Occupation: Elementary occupations (ISCO 88 - 9)"="OCU_ISCO88_9",
                 "Occupation: Total (ISCO 88)"="OCU_ISCO88_TOTAL",
                 "Occupation: Unknown (ISCO88)"="OCU_ISCO88_X",
                 # ISCO 08
                 "Occupation: Armed forces occupations (ISCO 08 - 0)"="OCU_ISCO08_0",
                 "Occupation: Managers (ISCO 08 - 1)"="OCU_ISCO08_1",
                 "Occupation: Professionals (ISCO 08 - 2)"="OCU_ISCO08_2",
                 "Occupation: Technicians and associate professionals (ISCO 08 - 3)"="OCU_ISCO08_3",
                 "Occupation: Clerical support workers (ISCO 08 - 4)"="OCU_ISCO08_4",
                 "Occupation: Service and sales workers (ISCO 08 - 5)"="OCU_ISCO08_5",
                 "Occupation: Skilled agricultural, forestry and fishery workers (ISCO 08 - 6)"="OCU_ISCO08_6",
                 "Occupation: Craft and related trades workers (ISCO 08 - 7)"="OCU_ISCO08_7",
                 "Occupation: Plant and machine operators, and assemblers (ISCO 08 - 8)"="OCU_ISCO08_8",
                 "Occupation: Elementary occupations (ISCO 08 - 9)"="OCU_ISCO08_9",
                 "Occupation: Total (ISCO 08)"="OCU_ISCO08_TOTAL",
                 "Occupation: Unknown (ISCO 08)"="OCU_ISCO08_X"
               )
      ) %>% # only leaving ISIC 3/3.1 and 4 categories, others (ISIC 1 or 2 and others) are removed
      filter(classif2 %in% ISCO88 | classif2 %in% ISCO08 ) %>%
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    
    ###Check
    
    # To develop
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EMP_2EMP_SEX_STE_OCU_NB___FROM___B46.RDS'))
    #readRDS(paste0('./output/', 'EMP_2EMP_SEX_STE_OCU_NB___FROM___B46.RDS'))
    
####################################################################################################################################****************      
##################################################################################################**************************************************    
##################################################  ************************************************************************************************
##############  INDICATOR 4.5(!!!!) ****************************************************************************************************************
#***************************************************************************************************************************************************
    ## Source indicator B24 Population not economically active by functional category, age, sex and urban/rural residence - UNSD
    ## Target indicator EIP_2EIP_SEX_AGE_FUN_NB - ILOSTAT
    # note, indicator does not exist in ILOSTAT, also classification does not exist (I suppouse it needs to be created)
    
    
    # Target classification AGE_5YRBANDS
    
    rm(list=setdiff(ls(), c("ilo","ASSIGN_SURVEY_CODE","ASSIGN_ISO_CODE")))
    age_groups <- c( "AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65")
     ### Import
    X <- readxl:::read_excel(paste0('./input/', 'B24.xlsx'))
    colnames(X) <- colnames(X) %>% str_replace_all(' ', '')  
    colnames(X) <- colnames(X) %>% str_replace_all("'", "")  
    X$Data <- as.integer(X$Data)
    
    
    ### Processing 
    
    ## First format steps
    Xp <- X %>%
      
      ## Avoiding duplicates
      
      # Choosing the ones with the maximum disagregation
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(age_data_richness=n()) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex) %>%
      mutate(max_age_data_richness=max(age_data_richness)) %>%
      filter(age_data_richness==max_age_data_richness) %>%
      ungroup() %>%
      
      # Choosing the ones with the UNSD sort, BUT making sure we do not mix several combinations of the uniquely identifying combinations
      #i.e.Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote
      within( {sort = ave(Data,  FUN = seq_along)} ) %>%
      group_by(Country, Series, Ref.Year, Area, Sex, RecordType,  Reliability, Cov.Note, SourceYear, Fnote) %>%
      mutate(UNSD_sort=min(sort)) %>%
      ungroup() %>%
      group_by(Country, Ref.Year, Area, Sex, Var.1) %>%
      mutate(minUNSD_sort=min(UNSD_sort)) %>%
      ungroup() %>%
      filter(minUNSD_sort==UNSD_sort) %>%
      
      # Important, for some reason some notes are not in the bulk download file and instead appear as a different series (See Egypt 2006 for instance)
      
      # detect duplicates
      # this is just a checking step
      #group_by(Country, Ref.Year, Area, Sex, Var.1, Var.3) %>%
      #mutate(pro_max_number_duplicates=n()) %>%
      #ungroup() %>%
      
      
      ## Match Format
    mutate(collection = "ILOEST") %>%
      ASSIGN_ISO_CODE() %>%
      rename( ref_area = complete_code ) %>%
      filter( ref_area!="TODROP") %>%
      ASSIGN_SURVEY_CODE() %>%
      mutate(indicator = "EMP_2EMP_SEX_AGE_ECO_NB" ) %>%
      mutate(sex =
               Sex %>%
               recode( "Both Sexes" = "SEX_T",
                       "Male" = "SEX_M",
                       "Female" = "SEX_F")
      ) %>%
      mutate(classif1 =
               Var.1 %>%
               recode( 
                 "Age: 15 - 19"="AGE_5YRBANDS_Y15-19",
                 "Age: 20 - 24"="AGE_5YRBANDS_Y20-24",
                 "Age: 25 - 29"="AGE_5YRBANDS_Y25-29",
                 "Age: 30 - 34"="AGE_5YRBANDS_Y30-34",
                 "Age: 35 - 39"="AGE_5YRBANDS_Y35-39",
                 "Age: 40 - 44"="AGE_5YRBANDS_Y40-44",
                 "Age: 45 - 49"="AGE_5YRBANDS_Y45-49",
                 "Age: 50 - 54"="AGE_5YRBANDS_Y50-54",
                 "Age: 55 - 59"="AGE_5YRBANDS_Y55-59",
                 "Age: 60 - 64"="AGE_5YRBANDS_Y60-64",
                 "Age: 65 - 69"="AGE_5YRBANDS_YGE65",
                 "Age: 70 - 74"="AGE_5YRBANDS_YGE65",
                 "Age: 75 - 79"="AGE_5YRBANDS_YGE65",
                 "Age: 80 - 84"="AGE_5YRBANDS_YGE65",
                 "Age: 85 - 89"="AGE_5YRBANDS_YGE65",
                 "Age: 90 - 94"="AGE_5YRBANDS_YGE65",
                 "Age: 95 - 99"="AGE_5YRBANDS_YGE65",
                 "Age: 100 +"="AGE_5YRBANDS_YGE65")
      ) %>%
      
      
      
      #Final formating issues
      mutate(classif2 =
               Var.2
               
      ) %>% 
      mutate(time = Ref.Year,
             obs_value=Data,
             obs_status="",
             note_classif="",
             note_indicator="",
             note_source="R1:2474") 
    
    
    ## Obtaining the actual AGE_5YRBANDS_YGE65
    # ************************************************************************************ (Not an explicit function --> refer to the same code above for more comments!)
    Xp <- Xp %>% 
      # Useful for later, largest age covered by the broad category, note that 100 + is set to missing 
      mutate ( OLDEST_INCLUDED_IN_OLD = if_else(classif1=="AGE_5YRBANDS_YGE65",as.integer(str_sub(Var.1, -2, -1)),as.integer(0) )) %>%
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Count=n()) %>%
      ungroup() %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(maxCount=max(Count)) %>%
      ungroup() %>%
      select(-Count)
    
    Xp$flag <- str_detect(Xp$classif1,"[//+]" )
    
    
    Xp <- Xp %>% mutate( groupage = if_else(flag==TRUE ,as.integer(substr(classif1, 6, 7)),NA_integer_ )) %>%
      # note that this discard countries that start at 60+
      mutate( groupage = if_else(groupage >=65 ,groupage,NA_integer_ )) %>%
      group_by(ref_area, time, sex,classif2) %>%
      mutate(mingroupage=min(groupage, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(pro_classif1 = if_else(groupage==mingroupage & maxCount<8,"AGE_5YRBANDS_YGE65","Other")) %>%
      
      # now we have to ensure that there are no classifications above the + sign, included in the AGE_5YRBANDS_YGE65 category
      mutate( attention = if_else((mingroupage <= OLDEST_INCLUDED_IN_OLD & maxCount < 8) |(Var.1 == "Age: 100 +" & maxCount < 8) , "Problem", "NoProblem") ) %>%
      # it is also necessary to check that for the incomplete cases there is at least one valid superior cuttof (i.e to ensure that we do not let a group of GE65 with only people 65-72 for instance)
      mutate( number_proclassif = if_else( pro_classif1=="AGE_5YRBANDS_YGE65", 1,0 ) ) %>%
      group_by(ref_area,sex, classif2, time) %>%
      mutate( number_proclassif=sum(number_proclassif, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate( attention2 = if_else(classif1=="AGE_5YRBANDS_YGE65" & maxCount < 8 & (number_proclassif==0), "Problem", "NoProblem")  ) 
    
    # The appropiate cuttof bands (such as 75+) are included in GE65 in classif1, whilst marking for removal all the ones above  
    Xp <- replace_na(Xp, list(pro_classif1 ="Other" , attention="NoProblem", attention2="Problem" ) ) %>%
      mutate(classif1 = if_else( attention=="Problem", "ToBeDiscarded" , classif1 )    ) %>%
      mutate(classif1 = if_else( attention2=="Problem", "ToBeDiscarded" , classif1 )    ) 
    
    Xp[ Xp$pro_classif1=="AGE_5YRBANDS_YGE65", "classif1"] <- "AGE_5YRBANDS_YGE65"
    
    
    Xcopy <- Xp
    # obtaining the totals of the group AGE_5YRBANDS_YGE65
    Xp <- Xp %>% 
      group_by(ref_area, time, sex, classif1, classif2) %>%
      mutate(Test = sum(obs_value)) %>%
      ungroup() %>%
      mutate(Test = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, as.integer(-100))) %>%
      mutate( obs_value = if_else(classif1 == "AGE_5YRBANDS_YGE65", Test, obs_value) ) %>%
      filter(classif1 %in% age_groups) %>%
      distinct(ref_area, sex, classif1, classif2, time, .keep_all=TRUE)
    
    
    #removing cases when ALL the age categories are not defined
    IncompleteCategories <- Xp %>% group_by( ref_area,sex, classif2, time) %>% count()
    Xp <- left_join(Xp, IncompleteCategories, by = c("ref_area","sex","classif2", "time"), copy = FALSE) %>%
      filter(n==11) 
    # note countries lost ARM	IRN	KGZ	LSO	MYT	MEX	PLW	CHE	THA	TKL	GBR	VNM
    # **************************************************************************************************************
    
    ###Check
    
    # To develop
    
    
    ### FINAL STEPS
    # remove auxiliar variables and select the totals computed, not the source ones
    Xfinal <- Xp %>% select(collection, ref_area, source, indicator, sex, classif1, classif2, time, obs_value, obs_status, note_classif, note_indicator, note_source) 
    
    saveRDS(Xfinal,paste0('./output/', 'EIP_2EIP_SEX_AGE_FUN_NB___FROM___B24.RDS'))
    #readRDS(paste0('./output/', 'EIP_2EIP_SEX_AGE_FUN_NB___FROM___B24.RDS'))
    
    
}




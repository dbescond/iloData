
REP_OECD.LFS_ANNUAL_input_ANNUAL_EMP_SEX_ECO_ISIC3 <- function()  { # Employment and Employees by Eco Activity, ISIC3

	### STEP 1 Loading local data obtained from link described above

	input_path	<- paste0(getwd(),'/input/ANNUAL_EMP_SEX_ECO_ISIC3.csv')
	
	X 	<- read_delim(input_path, delim = ',')
	colnames(X)[1] <- 'ref_area'
	
	X <- X %>% select(	ref_area,	SUBJECT,	Subject,	sex = Sex,	time = Time,	obs_value = Value,	obs_status = `Flag Codes`)   %>% filter(as.numeric(time) <2009 )	
  
	
	### STEP 2 Extract and Separate Information of the indicators
  
	
	Xp <- X %>% mutate( ISIC= str_extract(X$Subject, "\\([^()]+\\)") , 
						ISIC=str_sub(ISIC, 2, -2), 
						ISIC=str_replace(ISIC, "ISIC rev.3, A-X", "TOTAL"), 
						indicator= if_else(str_detect(SUBJECT,"YA99"),"EMP_TEMP_SEX_ECO_NB",""), 
						indicator2= if_else(str_detect(SUBJECT,"YW99"),"EES_TEES_SEX_ECO_NB",""), 
						indicator=if_else(indicator!="",indicator,indicator2)) %>%
				select(	-indicator2, -Subject,-SUBJECT) 
  
	
  ### STEP 3 Target dataset consistent with ILOSTAT format (changing observations names)
  

  # Observations
  Xpp <- Xp %>% mutate(	classif1=paste0("ECO_ISIC3_",ISIC), 
						sex= sex %>% recode("All persons"="SEX_T",
						                    "Males"="SEX_M",
						                    "Females"="SEX_F"))%>% 
				select(-ISIC)
  

  Xpp	<-	REP_OECD.LFS_ANNUAL_mapping_source(Xpp)
  
  
  ### STEP 4 - Working with wide format - consistency check + aggregates
  
  
  ##Seeing which categories are missing
  Y	<-	Xpp %>% spread(classif1, obs_value) %>% select(ECO_ISIC3_TOTAL, everything())
  Y <- Y %>% mutate(TOT=rowSums(.[13:(length(Y))],na.rm=TRUE))
  z_name <- colnames(Y[,14:(length(Y))-1])
  
  #this collects each missing category per observation and concatenates it
 Y <- Y %>% mutate(Miss="")
  for(var in z_name) { 
    print(var)
    Y$Miss[is.na(Y[,var])] <- paste0(Y$Miss[is.na(Y[,var])],str_sub(var,-1,-1),"_")
  }
  
  # this command obtains descriptive data, it turns out not many missing
  #Y %>% group_by(Miss) %>% count() %>% save_ilo()
  # Only X category is allowed to be missing, the rest will be flagged (flag==1->I will discard them)
  Y <- Y %>% mutate(flag=if_else(Miss==""|Miss=="X_",0,1 )) %>% filter(flag==0)
  
  ## Cheching the totals, not more than a 3 thousand deviation, also Total existant
  Y <- Y %>% mutate(flag2=abs(Y$TOT-Y$ECO_ISIC3_TOTAL)>3) %>% filter(flag2==FALSE) 
  
  
  ## aggregates
  #sector aggregate
  Y <- Y %>% mutate(ECO_AGGREGATE_AGR=ECO_ISIC3_A+ECO_ISIC3_B,
               ECO_AGGREGATE_CON=ECO_ISIC3_F,
               ECO_AGGREGATE_MAN=ECO_ISIC3_D,
               ECO_AGGREGATE_MEL=ECO_ISIC3_C+ECO_ISIC3_E,
               ECO_AGGREGATE_MKT=ECO_ISIC3_G+ECO_ISIC3_H+ECO_ISIC3_I+ECO_ISIC3_J+ECO_ISIC3_K,
               ECO_AGGREGATE_PUB=ECO_ISIC3_L+ECO_ISIC3_M+ECO_ISIC3_N+ECO_ISIC3_O+ECO_ISIC3_P+ECO_ISIC3_Q,
               ECO_AGGREGATE_X=ECO_ISIC3_X,
               ECO_AGGREGATE_TOTAL=ECO_ISIC3_TOTAL) %>%
  #broad sector
        mutate(ECO_SECTOR_AGR=ECO_AGGREGATE_AGR,
               ECO_SECTOR_IND=ECO_AGGREGATE_CON+ECO_AGGREGATE_MAN+ECO_AGGREGATE_MEL,
               ECO_SECTOR_SER=ECO_AGGREGATE_MKT+ECO_AGGREGATE_PUB,
               ECO_SECTOR_X=ECO_AGGREGATE_X,
               ECO_SECTOR_TOTAL=ECO_AGGREGATE_TOTAL)%>%
	# Remove manually computed total
	select(-TOT)
  
  
  ### STEP 5 Reshape
  Xpp <- Y %>% 	select(-flag, -flag2, -Miss) %>% 
				gather(classif1, obs_value,  -ref_area, -source, -note_source, -sex, -sex_version, -time, -obs_status, -indicator,-classif1_version,  -classif2, -classif2_version) %>% 
				filter(!obs_value %in% c(NA, ''), !(classif1 %in% c('ECO_AGGREGATE_X', 'ECO_SECTOR_X', 'ECO_ISIC3_X') & obs_value %in% 0))
  
  
  ### STEP 6 Exporting
Xpp$time <- as.character(Xpp$time)
saveRDS(Xpp,file = './input/ANNUAL_EMP_SEX_ECO_ISIC3.rds' )

#ensure only one object    

}

REP_OECD.LFS_ANNUAL_input_ANNUAL_EMP_SEX_ECO_ISIC4 <- function() { # Employment and Employees by Eco Activity, ISIC4
  
  
  ### STEP 1 Loading local data obtained from link described above
  

  input_path<- paste0(getwd(),'/input/ANNUAL_EMP_SEX_ECO_ISIC4.csv')
  
  X<- read_delim(input_path, delim = ',')
  colnames(X)[1] <- 'ref_area'
  

  X <- X %>% select(	ref_area,	SUBJECT,	Subject,	sex = Sex,	time = Time,	obs_value = Value,	obs_status = `Flag Codes`) %>% filter(as.numeric(time) >2008 )	
  
  
  ### STEP 2 Extract and Separate Information of the indicators
  
  Xp <- X %>% mutate( ISIC= str_extract(X$Subject, "\\([^()]+\\)") ,
					  ISIC=str_replace(ISIC, "\\(C \\)", "\\(C\\)"),  
					  ISIC=str_replace(ISIC, "\\( R\\)", "\\(R\\)"),  
                      ISIC=str_sub(ISIC, 2, -2), 
                      ISIC=str_replace(ISIC, "ISIC rev.4, A-U", "TOTAL"), 
                      indicator= if_else(str_detect(SUBJECT,"YA99"),"EMP_TEMP_SEX_ECO_NB",""), 
                      indicator2= if_else(str_detect(SUBJECT,"YW99"),"EES_TEES_SEX_ECO_NB",""), 
                      indicator=if_else(indicator!="",indicator,indicator2)) %>%
        select(	-indicator2, -Subject,-SUBJECT) 
  
  
  
  
  ### STEP 3 Target dataset consistent with ILOSTAT format () observations names)
  
  
  # Observations
  Xpp <- Xp %>% mutate(	classif1=paste0("ECO_ISIC4_",ISIC), 
                        sex= sex %>% recode("All persons"="SEX_T","Males"="SEX_M","Females"="SEX_F"))%>% 
    select(-ISIC)
  

  Xpp<-REP_OECD.LFS_ANNUAL_mapping_source(Xpp)
  
  ### STEP 4 - Working with wide format - consistency check + aggregates
  
  
  ##Seeing which categories are missing
  Y<-Xpp %>% spread(classif1, obs_value) %>% select(ECO_ISIC4_TOTAL, everything())
  Y %>% mutate(TOT=rowSums(.[13:(length(Y))],na.rm=TRUE))->Y
  z_name <- colnames(Y[,14:(length(Y))-1])
  
  #this collects each missing category per observation and concatenates it
  Y <- Y %>% mutate(Miss="")
  for(var in z_name) { 
    print(var)
    Y$Miss[is.na(Y[,var])] <- paste0(Y$Miss[is.na(Y[,var])],str_sub(var,-1,-1),"_")
  }
  
  # this command obtains descriptive data, it turns out a bit more of missing (I discard all but maybe it should be changed)
  #Y %>% group_by(Miss) %>% count() %>% save_ilo()
  # Only X category is allowed to be missing, the rest will be flagged 
  Y <- Y %>% mutate(flag=if_else(Miss=="",0,1 )) %>% filter(flag==0)
  
  ## Cheching the totals, not more than a 3 thousand deviation, also Total existant
  Y <- Y %>% mutate(flag2=abs(Y$TOT-Y$ECO_ISIC4_TOTAL)>3) %>% filter(flag2==FALSE) 
  
  
  ## aggregates
  #sector aggregate
  Y <- Y %>% mutate(ECO_AGGREGATE_AGR=ECO_ISIC4_A,
                    ECO_AGGREGATE_CON=ECO_ISIC4_F,
                    ECO_AGGREGATE_MAN=ECO_ISIC4_C,
                    ECO_AGGREGATE_MEL=ECO_ISIC4_B+ECO_ISIC4_D+ECO_ISIC4_E,
                    ECO_AGGREGATE_MKT=ECO_ISIC4_G+ECO_ISIC4_H+ECO_ISIC4_I+ECO_ISIC4_J+ECO_ISIC4_K+ECO_ISIC4_L+ECO_ISIC4_M+ECO_ISIC4_N,
                    ECO_AGGREGATE_PUB=ECO_ISIC4_O+ECO_ISIC4_P+ECO_ISIC4_Q+ECO_ISIC4_R+ECO_ISIC4_S+ECO_ISIC4_T+ECO_ISIC4_U,
			
                    ECO_AGGREGATE_TOTAL=ECO_ISIC4_TOTAL) %>%
    #broad sector
    mutate(ECO_SECTOR_AGR=ECO_AGGREGATE_AGR,
           ECO_SECTOR_IND=ECO_AGGREGATE_CON+ECO_AGGREGATE_MAN+ECO_AGGREGATE_MEL,
           ECO_SECTOR_SER=ECO_AGGREGATE_MKT+ECO_AGGREGATE_PUB,

           ECO_SECTOR_TOTAL=ECO_AGGREGATE_TOTAL) %>%
	# Remove manually computed total
	select(-TOT)
  
  
  ### STEP 5 Reshape
  Xpp <- Y %>% 	select(-flag, -flag2, -Miss) %>% 
				gather(classif1, obs_value,  -ref_area, -source, -note_source, -sex, -sex_version, -time, -obs_status, -indicator,-classif1_version,  -classif2, -classif2_version) %>% 
				filter(!obs_value %in% c(NA, ''), !(classif1 %in% c('ECO_AGGREGATE_X', 'ECO_SECTOR_X', 'ECO_ISIC3_X') & obs_value %in% 0))
  
  ### STEP 6 Exporting
Xpp$time <- as.character(Xpp$time) 
saveRDS(Xpp,file = './input/ANNUAL_EMP_SEX_ECO_ISIC4.rds' )

#ensure only one object  
}

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_5YRBANDS <- function(){ # open oecd by 5 yrbands data
  

  n_row_ref <- readr::read_csv("./input/ANNUAL_AGE_5YRBANDS.csv", col_names = FALSE, n_max = 1) %>% ncol
  X <- readr::read_csv("./input/ANNUAL_AGE_5YRBANDS.csv", col_types = paste0(rep('c', n_row_ref), collapse =''), guess_max = 1)
  colnames(X)[1] <- 'ref_area' #fix bad UTF8 encoding issue for first character/column
  
  rm(n_row_ref)
  
  X <- X %>%  
    select(ref_area,sex = SEX, classif1 = Age, indicator = Series, time = TIME, obs_value =  Value, Flags) %>% 
    filter(!ref_area %in% 'FTFR') %>% 
    mutate(obs_value = as.numeric(obs_value))
  
  # test if age classification is complete and total is accurate
  Y <- X %>% 	
    mutate(obs_value = as.numeric(obs_value)) %>% spread(classif1, obs_value)  %>% mutate(TOT = rowSums(.[6:16], na.rm = TRUE)) 
  
  # remove inaccurate data 
  Y <- Y %>% 	
    mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% 
    filter(test %in% FALSE, Unknown %in% NA, !ref_area %in% c('GBR', 'RUS')) %>% 
    distinct(ref_area, sex, indicator, time) %>% mutate(delete = TRUE) 
  X <- X %>% 	
    left_join(Y, by = c("ref_area", "sex", "indicator", "time")) %>% 
    filter(!delete %in% TRUE) %>% 
    select(-Flags, - delete)
  rm(Y)
  
  
  # create the Total AGE
  Total <- X %>% 
    filter(!classif1 %in% c("Total", "Unknown")) %>% 
    group_by(ref_area, sex, indicator, time) %>% 
    summarise(classif1 = 'Total', NEW = sum(as.numeric(obs_value))) %>% 
    ungroup
  
  # test
  # X %>% filter(!classif1 %in% c("Total", "Unknown")) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
  #			spread(classif1, obs_value)  %>% mutate(TOT = rowSums(.[5:15], na.rm = TRUE)) %>% left_join(Total) %>% fix
  
  AGE_5 <- X %>%
    mutate(obs_value = as.numeric(obs_value)) %>% filter(!classif1 %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, obs_value = NEW)) %>% 
    mutate(classif1 = classif1 %>% 
             plyr::mapvalues(	
               from = 	c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+", "Total"), 
               to = 	c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24", "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34", "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44", "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54", "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64", "AGE_5YRBANDS_YGE65", "AGE_5YRBANDS_TOTAL")
             )	) %>% 
				filter(!obs_value %in% c(NA, ''))
  

  saveRDS(AGE_5,file = './input/ANNUAL_AGE_5YRBANDS.rds' )
	rm(Total, X, AGE_5)	
  invisible(gc(reset = TRUE))											
}									

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_10YRBANDS <- function(){ # open oecd by 10 yrbands data

  readr::read_csv("./input/ANNUAL_AGE_10YRBANDS.csv", col_types = 'c_c__c_c__c_______d_c') -> X
  # test if age classification is complete and total accurante
  colnames(X)[1] <- 'COUNTRY'
  
  X %>% filter(!COUNTRY %in% 'FTFR')-> X
  
  X %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[6:11], na.rm = TRUE)) -> Y
  
  # remove inaccurate data 
  Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA, !COUNTRY %in% c('POL', 'RUS')) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
  X %>% left_join(Y, by = c("COUNTRY", "SEX", "Series", "TIME")) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
  rm(Y)
  
  
  # create the Total AGE
  X %>% filter(!Age %in% c("Total", "Unknown")) %>% group_by(COUNTRY, SEX, Series, TIME) %>% summarise(Age = 'Total', NEW = sum(as.numeric(Value))) %>% ungroup -> Total
  
  # test
  # X %>% filter(!Age %in% c("Total", "Unknown")) %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[5:9], na.rm = TRUE)) %>% left_join(Total) %>% fix
  
  AGE_10 <- X %>% mutate(Value = as.numeric(Value)) %>% filter(!Age %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, Value = NEW)) 
  
  AGE_10 <- AGE_10  %>% 
    rename(ref_area = COUNTRY, sex = SEX, indicator = Series, time = TIME, obs_value = Value, classif1 = Age) %>% 
    mutate(classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+", "Total"), 
                                                    to = 	c("AGE_10YRBANDS_Y15-24", "AGE_10YRBANDS_Y25-34", "AGE_10YRBANDS_Y35-44", "AGE_10YRBANDS_Y45-54", "AGE_10YRBANDS_Y55-64", "AGE_10YRBANDS_YGE65", "AGE_10YRBANDS_TOTAL")))%>% 
				filter(!obs_value %in% c(NA, ''))
  

  
  saveRDS(AGE_10,file = './input/ANNUAL_AGE_10YRBANDS.rds' )
  #	X <- readRDS('./input/ANNUAL_AGE_10YRBANDS.rds')
  
  
  rm(Total, X, AGE_10)
  invisible(gc(reset = TRUE))
  
}

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE_AGGREGATE <- function(){ # open oecd by AGGREGATE and YTHADULT AGE data

  readr::read_csv("./input/ANNUAL_AGE_AGGREGATE.csv", col_types = 'c_c__c_c__c_______d_c') -> X
  colnames(X)[1] <- 'COUNTRY'
  
  X %>% filter(!COUNTRY %in% 'FTFR')-> X
  
  
  # test if age classification is complete and total accurante
  X %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[6:9], na.rm = TRUE)) -> Y
  
  # remove inaccurate data #####no deletion
  # Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
  # X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
  X %>% select(-Flags)-> X
  # rm(Y)
  
  
  # create the Total AGE
  X %>% filter(!Age %in% c("Total", "Unknown")) %>% group_by(COUNTRY, SEX, Series, TIME) %>% summarise(Age = 'Total', NEW = sum(as.numeric(Value))) %>% ungroup -> Total
  
  # test
  # X %>% filter(!Age %in% c("Total", "Unknown")) %>% mutate(Value = as.numeric(Value)) %>% spread(Age, Value)  %>% mutate(TOT = rowSums(.[5:9], na.rm = TRUE)) %>% left_join(Total) %>% fix
  
  AGE_AGGREGATE <- X %>% mutate(Value = as.numeric(Value)) %>% filter(!Age %in% c("Total", "Unknown")) %>% bind_rows(rename(Total, Value = NEW)) 
  AGE_AGGREGATE <- AGE_AGGREGATE  %>% 
    rename(ref_area = COUNTRY, sex = SEX, indicator = Series, time = TIME, obs_value = Value, classif1 = Age) %>% 
    mutate(classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 54", "55 to 64", "65+", "Total"), 
                                                    to = 	c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_YGE65", "AGE_AGGREGATE_TOTAL")))
  
  AGE_YTHADULT <- AGE_AGGREGATE %>% mutate(classif1 = classif1 %>% plyr::mapvalues(
    from= c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_YGE65", "AGE_AGGREGATE_TOTAL"), 
    to = c("AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE15")
  )) %>% 
    group_by(ref_area, indicator, sex, classif1, time) %>% 
    summarise(	obs_value = sum(obs_value, na.rm = TRUE)) %>% 
    ungroup %>% 
				filter(!obs_value %in% c(NA, ''))
  
  

  
  saveRDS(bind_rows(AGE_AGGREGATE, AGE_YTHADULT),file = './input/ANNUAL_AGE_AGGREGATE.rds' )
  #	X <- readRDS('./input/ANNUAL_AGE_AGGREGATE.rds')
  
  
  rm(Total, X, AGE_YTHADULT, AGE_AGGREGATE)
  invisible(gc(reset = TRUE))

  
}												

REP_OECD.LFS_ANNUAL_input_ANNUAL_AGE <- function(){ # back AGE with result combine and add notes create emp_rt, eap_rt, une_rt
 
  rm (list=ls())
  
  X <- bind_rows( readRDS('./input/ANNUAL_AGE_5YRBANDS.rds'), 
                  readRDS('./input/ANNUAL_AGE_10YRBANDS.rds'),  
                  readRDS('./input/ANNUAL_AGE_AGGREGATE.rds')) %>% 
    mutate(	sex = 	sex %>% recode(	"MW" = "SEX_T", 
                                   "MEN" = "SEX_M", 
                                   "WOMEN" = "SEX_F"), 
            indicator = indicator %>% plyr::mapvalues(	from = 	c("Employment", "Labour Force", "Population", "Unemployment"), 
                                                       to = c("EMP_TEMP_SEX_AGE_NB", "EAP_TEAP_SEX_AGE_NB", "POP_XWAP_SEX_AGE_NB", "UNE_TUNE_SEX_AGE_NB")	)) 

  
  
  # test total all equal 
  # X %>% filter(classif1 %in% c('AGE_5YRBANDS_TOTAL', 'AGE_10YRBANDS_TOTAL', 'AGE_AGGREGATE_TOTAL')) %>%
  # spread(classif1, obs_value)															
  
  # add source 
  
  ReadMeSource <- readxl::read_excel(paste0("./ReadME_OECD_LFS_ANNUAL.xlsx"), sheet="MappingSource") %>%
    mutate(ID = substr(ID,1,3)) %>% rename(ref_area = ID, source = REF)
  
  
  X <- X %>% left_join(ReadMeSource)													
  
  
  
  
  
  ########## backup formaer mapping from ODR done by YZ in 2013															
  # require(ilo)
  # init_ilo()
  # Y <- get_ilo(collection = 'ODR')													
  # Y <- Y %>% filter(indicator %in% c("EMP_TEMP_SEX_AGE_NB", "EAP_TEAP_SEX_AGE_NB", "POP_XWAP_SEX_AGE_NB", "UNE_TUNE_SEX_AGE_NB"))			
  # Y <- Y %>% group_by(ref_area, indicator, time) %>% summarise(note_source = first(note_source))	%>% ungroup 
  
  # Y %>% readr::write_csv(paste0("./input\\backupnote.csv"))
  
  
  Y <- readr::read_csv(paste0("./help/backupnote.csv"))
  # cleaning unuseful notes
  Y <- Y %>% mutate(
    note_source = gsub('R1:2383_','',note_source,fixed = TRUE),
    note_source = gsub('S3:5_','',note_source,fixed = TRUE),
    note_source = gsub('S3:9_','',note_source,fixed = TRUE),
    note_source = gsub('S3:14_','',note_source,fixed = TRUE),
    note_source = gsub('S3:1490_','',note_source,fixed = TRUE),
    note_source = gsub('S3:19_','',note_source,fixed = TRUE),
    note_source = gsub('S3:20_','',note_source,fixed = TRUE),
    note_source = gsub('S3:16_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2882_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2883_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2885_','',note_source,fixed = TRUE),
    note_source = gsub('S4:29_','',note_source,fixed = TRUE),
    note_source = gsub('S4:34_','',note_source,fixed = TRUE),
    note_source = gsub('S5:37_','',note_source,fixed = TRUE),
    note_source = gsub('S5:38_','',note_source,fixed = TRUE),
    note_source = gsub('S5:39_','',note_source,fixed = TRUE),
    note_source = gsub('S5:40_','',note_source,fixed = TRUE),
    note_source = gsub('S5:41_','',note_source,fixed = TRUE),
    note_source = gsub('S5:352_','',note_source,fixed = TRUE),
    note_source = gsub('S5:356_','',note_source,fixed = TRUE)
  )
  
  
  Y <- Y %>% distinct(ref_area, indicator, .keep_all = TRUE) %>% mutate(time = as.character(time))	
  LAST_NOTE <- Y %>% arrange(time) %>% group_by(ref_area, indicator) %>% summarise(time = first(time), note_source = first(note_source)) %>% ungroup
  
  X <- left_join(X, Y, by = c("ref_area", "indicator", "time")) %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:2382_T2:84_T3:89', note_source), 
                                                                          note_source = ifelse(ref_area %in% 'RUS','R1:2382_T2:84_T3:102',note_source ), 
                                                                          note_source = ifelse(ref_area %in% 'JPN' & time %in% '2011','R1:2382_S4:34_T2:84_T3:102',note_source )
  ) 
  
  
  ANNUAL_AGE <- X %>% select(ref_area, source, sex:note_source) %>% mutate(classif2 = NA) %>% ilo:::switch_ilo(version)
  
  
  #calculate indicator rate
  
  UNE_DEAP_SEX_AGE_RT  <- 	bind_rows(
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"UNE_TUNE_SEX_AGE_NB"), 
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"EAP_TEAP_SEX_AGE_NB")  %>% select(-contains('notes'))
  ) %>% spread (indicator, obs_value) %>% 
    mutate(obs_value = round(UNE_TUNE_SEX_AGE_NB / EAP_TEAP_SEX_AGE_NB * 100,3)) %>% 
    filter(!obs_value %in% NA) %>%
    select(-UNE_TUNE_SEX_AGE_NB, -EAP_TEAP_SEX_AGE_NB) %>% 
    mutate(indicator = 'UNE_DEAP_SEX_AGE_RT')
  
  
  
  EAP_DWAP_SEX_AGE_RT  <- 	bind_rows(
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"EAP_TEAP_SEX_AGE_NB"), 
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"POP_XWAP_SEX_AGE_NB")  %>% select(-contains('notes'))
  ) %>% spread (indicator, obs_value) %>% 
    mutate(obs_value = round(EAP_TEAP_SEX_AGE_NB / POP_XWAP_SEX_AGE_NB * 100,3)) %>% 
    filter(!obs_value %in% NA) %>%
    select(-EAP_TEAP_SEX_AGE_NB, -POP_XWAP_SEX_AGE_NB) %>% 
    mutate(indicator = 'EAP_DWAP_SEX_AGE_RT')
  
  
  
  EMP_DWAP_SEX_AGE_RT  <- 	bind_rows(
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"EMP_TEMP_SEX_AGE_NB"), 
    ANNUAL_AGE %>% 	
      filter(	indicator%in%"POP_XWAP_SEX_AGE_NB")  %>% select(-contains('notes'))
  ) %>% spread (indicator, obs_value) %>% 
    mutate(obs_value = round(EMP_TEMP_SEX_AGE_NB / POP_XWAP_SEX_AGE_NB * 100,3)) %>% 
    filter(!obs_value %in% NA) %>%
    select(-EMP_TEMP_SEX_AGE_NB, -POP_XWAP_SEX_AGE_NB) %>% 
    mutate(indicator = 'EMP_DWAP_SEX_AGE_RT') 
  
  
  saveRDS(
			bind_rows(ANNUAL_AGE,EAP_DWAP_SEX_AGE_RT, UNE_DEAP_SEX_AGE_RT, EMP_DWAP_SEX_AGE_RT ) %>% 
				select(-contains('_version')) %>% 
				filter(!obs_value %in% c(NA, ''))
				,file = './input/ANNUAL_AGE.rds' )
  #	X <- readRDS('./input/ANNUAL_AGE.rds')
  
  
  rm(EAP_DWAP_SEX_AGE_RT, UNE_DEAP_SEX_AGE_RT, EMP_DWAP_SEX_AGE_RT, X, Y,   ANNUAL_AGE, LAST_NOTE, ReadMeSource)
  invisible(gc(reset = TRUE))			
}				

REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_AGE_DUR <- function(){ # processing of "UNE_TUNE_SEX_AGE_DUR_NB
  # open oecd by unemployment by age and duration
 
  X <- readr::read_csv("./input/ANNUAL_UNE_AGE_DUR.csv") 
  
  colnames(X)[1] <- 'COUNTRY'
  X <- X %>% select(COUNTRY, SEX, Age, Duration,  TIME,  Value)
  
  
  X %>% filter(!COUNTRY %in% c('FTFR'))-> X
  
  ########### no erro found 
  # test if age classification is complete and total accurante
  # X %>% mutate(Value = as.numeric(Value)) %>% spread(Duration, Value)  %>% mutate(TOT = rowSums(.[c(6:10,13)], na.rm = TRUE)) -> Y
  
  # # remove inaccurate data 
  # Y %>% mutate(test = round(as.numeric(Total),1) == round(as.numeric(TOT),1)) %>% filter(test %in% FALSE, Unknown %in% NA, !COUNTRY %in% c('GBR', 'RUS')) %>% distinct(COUNTRY, SEX, Series, TIME) %>% mutate(delete = TRUE) -> Y
  # X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select(-Flags, - delete)-> X
  # rm(Y)
  
  
  
  
  
  
  X <- X  %>% 		filter(!Duration %in% 'Total Declared') %>%
    rename(ref_area = COUNTRY, sex = SEX, time = TIME, obs_value = Value, classif1 = Age, classif2 = Duration) %>% 
    mutate(	indicator = 'UNE_TUNE_SEX_AGE_NB',
            classif1 = classif1 %>% plyr::mapvalues(	from = 	c("15 to 24", "25 to 54", "55+", "Total"), 
                                                     to = 	c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_TOTAL")), 
            sex = 	sex %>% plyr::mapvalues(	from = 	c("MW", "MEN", "WOMEN"), 
                                            to = c("SEX_T", "SEX_M", "SEX_F")	))
  
  
  DUR_AGGREGATE <- X %>% mutate(classif2 = classif2 %>% plyr::mapvalues(
    from= c("< 1 month", "> 1 month and < 3 months", "> 3 month and < 6 months", "> 6 month and < 1 year", "1 year and over", "Total", "Unknown"), 
    to = c("DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MLT6", "DUR_AGGREGATE_MGE6LT12", "DUR_AGGREGATE_MGE12", "DUR_AGGREGATE_TOTAL", "DUR_AGGREGATE_X")
  ))	%>%
    group_by(ref_area, indicator, sex, classif1, classif2, time) %>% 
    summarise(	obs_value = sum(obs_value, na.rm = TRUE)) %>% 
    ungroup
  
  
  DUR_DETAILS <- X %>% mutate(classif2 = classif2 %>% plyr::mapvalues(
    from= c("< 1 month", "> 1 month and < 3 months", "> 3 month and < 6 months", "> 6 month and < 1 year", "1 year and over", "Total", "Unknown"), 
    to = c("DUR_DETAILS_MLT1", "DUR_DETAILS_MGE1LT3", "DUR_DETAILS_MGE3LT6", "DUR_DETAILS_MGE6LT12", "DUR_DETAILS_MGE12LT24", "DUR_DETAILS_TOTAL", "DUR_DETAILS_X")
  ))										
  
  DUR_DETAILS <- DUR_DETAILS %>% mutate(note_classif = ifelse(classif2 %in% 'DUR_DETAILS_MGE12LT24', 'C7:3716',NA ))
  
  DUR_AGE_AGGREGATE <- bind_rows(DUR_DETAILS, DUR_AGGREGATE)
  
  
  DUR_AGE_YTHADULT <- DUR_AGE_AGGREGATE %>% 
    mutate(classif1 = classif1 %>% plyr::mapvalues(
      from= c("AGE_AGGREGATE_Y15-24", "AGE_AGGREGATE_Y25-54", "AGE_AGGREGATE_Y55-64", "AGE_AGGREGATE_TOTAL"), 
      to = c("AGE_YTHADULT_Y15-24", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE25", "AGE_YTHADULT_YGE15")
    )) %>% 
    group_by(ref_area, indicator, sex, classif1, classif2, time) %>% 
    summarise(	obs_value = sum(obs_value, na.rm = TRUE), 
               note_classif = first(note_classif)) %>% 
    ungroup 
  
  DUR_AGE_AGGREGATE <- DUR_AGE_AGGREGATE %>% 
    mutate(note_classif = ifelse(classif1 %in% 'AGE_AGGREGATE_Y55-64', paste0(note_classif, '_C6:2343'), note_classif), 
           note_classif = gsub('NA_', '', note_classif, fixed = TRUE))
  
  
  ANNUAL_UNE_AGE_DUR <- bind_rows(DUR_AGE_YTHADULT, DUR_AGE_AGGREGATE) %>% mutate(time = as.character(time))
  
  ############ add note and source 
  
  ANNUAL_UNE_AGE_DUR <- left_join(	ANNUAL_UNE_AGE_DUR, 
                                   readRDS('./input/ANNUAL_AGE.rds') %>% filter(indicator %in% 'UNE_TUNE_SEX_AGE_NB') %>% distinct(ref_area, indicator, time, source, note_source), by = c('ref_area', 'indicator', 'time')) %>% 
    mutate(indicator = 'UNE_TUNE_SEX_AGE_DUR_NB') %>% 
				filter(!obs_value %in% c(NA, ''))
  
  
  saveRDS(ANNUAL_UNE_AGE_DUR,file = './input/ANNUAL_UNE_AGE_DUR.rds' )
  #	X <- readRDS('./input/ANNUAL_AGE.rds')
  
  
  rm(DUR_AGE_YTHADULT, DUR_AGE_AGGREGATE, DUR_DETAILS, DUR_AGGREGATE, X, ANNUAL_UNE_AGE_DUR)	
  invisible(gc(reset = TRUE))		
}

REP_OECD.LFS_ANNUAL_input_ANNUAL_ANNUAL_EMP_STE <- function(){	# processing of EMP_TEMP_SEX_STE_NB
  
  readr::read_csv("./input/ANNUAL_EMP_STE.csv", col_types = 'c__c_c__c_______d_c') -> X
  colnames(X)[1] <- 'ref_area'
  
  
  
  X <- X %>% 	rename( sex = Sex, classif1 = Subject, time = TIME, obs_value = Value, obs_status = Flags) %>% 
    mutate(
      classif1 = classif1 %>% 
        plyr::mapvalues(	from = c("Civil Employment all status, all activities", "Employees in all activities", "Employers and persons working on own account all activities",  "Unpaid family workers all activities"), 
                         to = c('STE_ICSE93_TOTAL','STE_ICSE93_1','STE_ICSE93_3','STE_ICSE93_5')),
      sex = sex %>% 
        plyr::mapvalues(	from = c("Males", 'Females', 'All persons'), 
                         to = c('SEX_M','SEX_F','SEX_T'))
      
      
    )
  
  
  
  
  
  # test if classification is complete and total accurate
  # X %>% select(-obs_status) %>% spread(classif1, obs_value)  %>% mutate(TOT = rowSums(.[4:6], na.rm = TRUE)) %>% filter(!TOT == 0, as.numeric(time) > 1959) %>% mutate(STE_ICSE93_6 = STE_ICSE93_TOTAL - TOT) -> Y
  
  
  
  
  #Y %>% mutate(test = (round(as.numeric(STE_ICSE93_TOTAL),0) - round(as.numeric(TOT),0) ) > 1) %>% as.tbl -> Y
  
  # remove inaccurate data 
  #Y %>% left_join(TEST) %>% filter(!obs_value %in% NA)  %>% mutate(test2 = abs((round(as.numeric(STE_ICSE93_TOTAL),0) - round(as.numeric(obs_value),0)) ) > 3) %>% filter(test2 %in% TRUE) %>% distinct(ref_area, sex, time) %>% mutate(delete = TRUE) -> Y
  #X %>% left_join(Y) %>% filter(!delete %in% TRUE) %>% select( -delete)-> X
  X <- X %>% mutate(obs_value = as.numeric(as.character(obs_value))) %>% filter(!classif1 %in% c(0, '0'))
  
  
  ######### delete obvious error the reclaculate them
  
  X <- X %>%   filter(
    !(ref_area %in% 'TUR' & time %in% c('1960','1961', '1965')),
    !(ref_area %in% 'USA' & time %in% c('1955','1956', '1957', '1958', '1959', '1960', '1961', '1962') & sex %in% c('SEX_F','SEX_M')),					
    !(ref_area %in% 'NZL' & time %in% c('1957','1958', '1959', '1960') & classif1 %in% 'STE_ICSE93_1') 
  )
  
  
  
  
  X <- X %>% arrange(desc(sex)) %>% group_by(ref_area, classif1, time) %>% mutate(test = paste0(sex, collapse = ' ')) %>% ungroup 
  
  unique(X$test)
  
  ADD1 <- X %>% filter(test %in% 'SEX_T SEX_F') %>% group_by(ref_area, classif1, time) %>% summarise(sex = 'SEX_M', obs_value = first(obs_value) - last(obs_value), obs_status = first(obs_status)) %>% ungroup
  ADD2 <- X %>% filter(test %in% 'SEX_M SEX_F') %>% group_by(ref_area, classif1, time) %>% summarise(sex = 'SEX_T', obs_value = first(obs_value) + last(obs_value), obs_status = first(obs_status)) %>% ungroup
  
  X <- bind_rows(X, ADD1, ADD2) %>% arrange(ref_area, time)
  
  
  
  X %>% arrange(desc(classif1)) %>% group_by(ref_area, sex, time) %>% mutate(test = paste0(classif1, collapse = ' ')) %>% ungroup -> X
  
  unique(X$test)
  
  ADD1 <- X %>% filter(test %in% 'STE_ICSE93_3 STE_ICSE93_1') %>% group_by(ref_area, sex, time) %>% summarise(classif1 = 'STE_ICSE93_TOTAL', obs_value = first(obs_value) + last(obs_value), obs_status = first(obs_status)) %>% ungroup
  ADD2 <- X %>% filter(test %in% 'STE_ICSE93_TOTAL STE_ICSE93_1') %>% group_by(ref_area, sex, time) %>% summarise(classif1 = 'STE_ICSE93_3', obs_value = first(obs_value) - last(obs_value), obs_status = first(obs_status)) %>% ungroup
  
  
  
  
  
  X <- bind_rows(X, ADD1) %>% arrange(ref_area, time)
  
  
  X %>% arrange(desc(classif1), desc(sex)) %>% mutate(REF = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% group_by(ref_area, time) %>% mutate(test = paste0(REF, collapse = ' ')) %>% ungroup %>% select(-REF) -> X
  
  X %>% filter(!test %in% c(	'T_TOTAL M_TOTAL F_TOTAL',
                             'T_TOTAL',
                             'T_1')) %>% select(-test)-> X
  
  
  
  STE_AGGREGATEPLUS <- X %>% 
    mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3", 'STE_ICSE93_5'), 
                                                    to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF", 'STE_AGGREGATE_SLF'))) %>% 
    group_by(ref_area,sex,time, classif1) %>% 
    summarise(obs_value = sum(obs_value, na.rm = TRUE), obs_status = first(obs_status)) %>% ungroup
  
  
  X %>% arrange(desc(classif1), desc(sex)) %>% mutate(REF = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% group_by(ref_area, time) %>% mutate(test = paste0(REF, collapse = ' ')) %>% ungroup %>% select(-REF) -> X
  
  X %>% filter(!test %in% c(	'T_TOTAL M_TOTAL F_TOTAL T_1 M_1 F_1',
                             'T_TOTAL M_TOTAL F_TOTAL T_1',
                             'T_TOTAL T_1', 
                             'T_TOTAL M_TOTAL F_TOTAL T_3 T_1', 
                             'T_TOTAL T_3 T_1', 
                             'T_TOTAL M_TOTAL F_TOTAL T_3 M_3 F_3 T_1 M_1 F_1', 
                             'T_TOTAL M_TOTAL F_TOTAL T_3 T_1 M_1 F_1')) %>% select(-test)-> X
  
  
  
  
  
  STE <- bind_rows(X,STE_AGGREGATEPLUS )
  
  
  # STE %>%  mutate(test = paste0(substr(sex,5,5), '_', stringr::str_sub(classif1,12,-1))) %>% select(ref_area:obs_value, test) %>% select(-sex, -classif1) %>% spread(test , obs_value) %>% save_ilo()
  
  
  STE <- STE %>% arrange(ref_area, time, obs_status) %>% group_by(ref_area, time) %>% mutate(obs_status = first(obs_status)) %>% ungroup() %>%arrange(ref_area, time, desc(sex), classif1)
  
  
  
  # X %>% select(-obs_status) %>% spread(classif1, obs_value) -> Y
  
  REF <- readRDS('./input/ANNUAL_AGE.rds')  %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB') %>% distinct(ref_area, source,note_source) %>% group_by(ref_area, source ) %>% summarise(note_source = last(note_source)) %>% ungroup 
  
  STE <- STE %>% left_join( REF, by = "ref_area")
  
  STE <- STE %>% mutate(indicator = 'EMP_TEMP_SEX_STE_NB') %>%
  mutate(	
    obs_status = ifelse(obs_status %in% 'Break', 'B', NA), 
    note_source = ifelse(obs_status %in% 'B' & note_source %in% NA, 'I11:264', note_source), 
    note_source = ifelse(obs_status %in% 'B' & !note_source %in% NA, paste0(note_source, '_I11:264'), note_source)
  )	%>% 
				filter(!obs_value %in% c(NA, ''))
  
  
  saveRDS(STE,file = './input/ANNUAL_STE.rds' )
  #	X <- readRDS('./input/ANNUAL_STE.rds')
  
  
  
  # STE %>% select(ref_area:obs_value) %>% mutate(test  = paste0(substr(sex, 5,5), '_', stringr::str_sub(classif1,12,-1))) %>% select(-sex, -classif1) %>% spread(test, obs_value)
  
  
  # test with emp by age and add missing on age
  
  # Y %>% left_join(
  # ANNUAL_AGE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB', classif1 %in% 'AGE_AGGREGATE_TOTAL') %>% select(ref_area, sex, time, obs_value) %>% rename(AGE = obs_value)
  # ) %>% 	filter(AGE %in% NA) %>% 
  # select(ref_area, sex,  time, STE_ICSE93_TOTAL) %>% 
  # rename(obs_value = STE_ICSE93_TOTAL) %>% 
  # mutate(classif1 = 'AGE_AGGREGATE_TOTAL', indicator = 'EMP_TEMP_SEX_AGE_NB') %>% 
  # left_join(LAST_NOTE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB') %>% distinct(ref_area, note_source)) %>% 
  # left_join(ANNUAL_AGE %>% distinct(ref_area, source)) -> NEW
  
  # ANNUAL_AGE <- bind_rows(ANNUAL_AGE, NEW) %>% filter(!note_source %in% NA) %>% filter(!obs_value %in% NA)
  # rm(NEW)
  
  
  
  
  
  # TEST <- Y %>% filter((STE_ICSE93_3 %in% NA & STE_ICSE93_5 %in% NA) | (STE_ICSE93_3 %in% 0 & STE_ICSE93_5 %in% 0) | STE_ICSE93_3 %in% c(0, NA) | STE_ICSE93_TOTAL %in% NA) %>% select(ref_area, sex, time) %>% mutate(delete = TRUE)
  
  # X <- X %>% left_join(TEST) %>% filter(!delete %in% TRUE) %>% select( -delete) %>% filter(!obs_value %in% 0)
  
  # rm(TEST)
  
  # X  %>% select(-obs_status) %>% spread(classif1, obs_value) -> Y
  
  # TEST <- Y %>% filter(STE_ICSE93_5 %in% NA) %>% distinct(ref_area,   sex,  time) %>% mutate(keep = TRUE)
  
  # STE_AGGREGATE <- X %>% left_join(TEST) %>% filter(keep %in% TRUE) %>% select(-keep) %>% 
  # mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3"), 
  # to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF")))
  
  
  # STE_ISCE93 <- X %>% left_join(TEST) %>% filter(!keep %in% TRUE) %>% select(-keep)
  
  # STE_AGGREGATEPLUS <- STE_ISCE93 %>% 
  # mutate(classif1  = classif1 %>% plyr::mapvalues(from = c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_3", 'STE_ICSE93_5'), 
  # to = c("STE_AGGREGATE_TOTAL", "STE_AGGREGATE_EES", "STE_AGGREGATE_SLF", 'STE_AGGREGATE_SLF'))) %>% 
  # group_by(ref_area,sex,time, classif1) %>% 
  # summarise(obs_value = sum(obs_value), obs_status = first(obs_status)) %>% ungroup
  
  # STE_ISCE93 <- STE_ISCE93 %>% mutate(note_classif = ifelse(classif1 %in% 'STE_ICSE93_3', 'C2:975', NA))			
  
  
  # STE <- bind_rows(STE_ISCE93, STE_AGGREGATEPLUS, STE_AGGREGATE) %>% left_join(
  # ANNUAL_AGE %>% filter(indicator %in% 'EMP_TEMP_SEX_AGE_NB', classif1 %in% 'AGE_AGGREGATE_TOTAL') %>% distinct(ref_area, time, source, note_source) 
  # ) %>% filter(!note_source %in% NA) %>% 
  # mutate(indicator = 'EMP_TEMP_SEX_STE_NB')
  
  
  
  
  
  rm( STE_AGGREGATEPLUS)
  invisible(gc(reset = TRUE))
  
}	

REP_OECD.LFS_ANNUAL_mapping_source <- function (DataWithoutSource) {  # Utility to get source and note_source- with cleaning (Specific to OECD data)
  
  
  ReadMeSource <- readxl::read_excel(paste0("./ReadME_OECD_LFS_ANNUAL.xlsx"), sheet="MappingSource") %>%
    mutate(ID = substr(ID,1,3)) %>% rename(ref_area = ID, source = REF)
  
  
  X <- DataWithoutSource %>% left_join(ReadMeSource)													
  
  
  Y <- readr::read_csv(paste0("./help/backupnote.csv"))
  
  # cleaning unuseful notes
  Y <- Y %>% mutate(
    note_source = gsub('R1:2383_','',note_source,fixed = TRUE),
    note_source = gsub('S3:5_','',note_source,fixed = TRUE),
    note_source = gsub('S3:9_','',note_source,fixed = TRUE),
    note_source = gsub('S3:14_','',note_source,fixed = TRUE),
    note_source = gsub('S3:1490_','',note_source,fixed = TRUE),
    note_source = gsub('S3:19_','',note_source,fixed = TRUE),
    note_source = gsub('S3:20_','',note_source,fixed = TRUE),
    note_source = gsub('S3:16_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2882_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2883_','',note_source,fixed = TRUE),
    note_source = gsub('S3:2885_','',note_source,fixed = TRUE),
    note_source = gsub('S4:29_','',note_source,fixed = TRUE),
    note_source = gsub('S4:34_','',note_source,fixed = TRUE),
    note_source = gsub('S5:37_','',note_source,fixed = TRUE),
    note_source = gsub('S5:38_','',note_source,fixed = TRUE),
    note_source = gsub('S5:39_','',note_source,fixed = TRUE),
    note_source = gsub('S5:40_','',note_source,fixed = TRUE),
    note_source = gsub('S5:41_','',note_source,fixed = TRUE),
    note_source = gsub('S5:352_','',note_source,fixed = TRUE),
    note_source = gsub('S5:356_','',note_source,fixed = TRUE)
  )
  
  
  Y <- Y %>% distinct(ref_area, indicator, .keep_all = TRUE) 
  LAST_NOTE <- Y %>% arrange(time) %>% group_by(ref_area, indicator) %>% summarise(time = first(time), note_source = first(note_source)) %>% ungroup
  
  X <- left_join(X, Y, by = c("ref_area", "indicator", "time")) %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:2382_T2:84_T3:89', note_source), 
                                                                           note_source = ifelse(ref_area %in% 'RUS','R1:2382_T2:84_T3:102',note_source ), 
                                                                           note_source = ifelse(ref_area %in% 'JPN' & time %in% '2011','R1:2382_S4:34_T2:84_T3:102',note_source )
  ) 
  
  
  DataWithSource <- X %>% select(ref_area, source, sex:note_source) %>% mutate(classif2 = NA) %>% ilo:::switch_ilo(version)
  DataWithSource}
  
REP_OECD_LFS_ANNUAL_download <- function(REF){

# REF <- Mapping_File %>% slice(1)

cleanTemp <- list.files('C:\\temp\\') %>% as_data_frame %>% filter(value %>% str_detect('\\.'))
if(nrow(cleanTemp) > 0) {for (i in 1:nrow(cleanTemp)){unlink(paste0('C:\\temp\\', cleanTemp$value[i]))} }; rm(cleanTemp)

shell('java -jar  C:/R/library/RSelenium/bin/selenium-server-standalone.jar', wait   = FALSE)
	Sys.sleep(2)
# startServer(dir = 'C://R//library//RSelenium//bin/', args = NULL, log = FALSE)
fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\temp"
                                ,  browser.download.folderList = 2L
                                , browser.download.manager.showWhenStarting = FALSE
                                , browser.helperApps.neverAsk.saveToDisk = "application/text/csv"))
#RSelenium::startServer()
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()   

remDr$navigate(REF$URL) # go to query webpage

	Sys.sleep(10)	

remDr$executeScript("redirectToSurvey('csv');") # open csv download pop up

remDr$switchToFrame('DialogFrame')


	Sys.sleep(10)	

remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_rbCustomLayout')$clickElement()
	Sys.sleep(3)	
#remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_cbLabel')$clickElement() ########## keep label
# 	Sys.sleep(3)	
remDr$findElement('id', 'divExportToCSV2')$findChildElement('id' , '_ctl12_btnExportCSV')$clickElement()
	Sys.sleep(100)	
	
	


try(remDr$close(), silent = TRUE)
remDr$closeServer()

test <- list.files("C:\\temp\\")
test <- test[substr(test, nchar(test)-3, nchar(test)) %in% '.csv']
file.rename(paste0("C:\\temp\\",test),paste0(INPUT, REF$NAME,'.csv'))
invisible(gc(reset = TRUE))


}
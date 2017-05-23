


setwd("D:/_MIG_EUROSTAT")

# source(paste("I:/COMMON/A0 Short term indicators/Processing/R_Functions/0_My_function.R", sep=""))
		
require(plyr)
require(dplyr)
require(readr)
require(tidyr)
require(stringr)
		
		
		
eurostat_combine <- function(ref_file,HEADER_CHGE,PATCH_CODE){

test 		<- 	HEADER_CHGE$FILE%in%ref_file

new_colnames <- HEADER_CHGE %>% 
				filter(FILE%in%ref_file) %>%
				select(contains("NEW")) %>% 
				t %>% 
				as.data.frame %>%
				filter(!as.character(V1)%in%NA) %>%
				mutate(V1 = as.character(V1)) %>%
				t %>% 
				as.vector

X <- as.tbl(as.data.frame(read.csv(paste0("./CSV/",ref_file),header = TRUE,sep=",",stringsAsFactors=FALSE)))

colnames(X) <- 	new_colnames

X 			<- 	X %>% 
				select(-contains("DELETE")) %>%
				mutate(FILE = ref_file)
		
if(length(grep("Indicator_Code",HEADER_CHGE[test,]))==0L){ X <- X %>% mutate(Indicator_Code = NA)}	
if(!HEADER_CHGE[test,"ADD_INDICATOR"]%in%NA){ X <- X %>% mutate(Indicator_Code = HEADER_CHGE[test,"ADD_INDICATOR"] %>% as.character)}
if(!HEADER_CHGE[test,"Notes_Topic_Code"]%in%NA){X <- X %>% mutate(Notes_Topic_Code = HEADER_CHGE[test,"Notes_Topic_Code"]  %>% as.character)}
if(!HEADER_CHGE[test,"Notes_Source_Code"]%in%NA){X <- X %>% mutate(Notes_Source_Code = HEADER_CHGE[test,"Notes_Source_Code"]  %>% as.character)}
if(length(grep("Classif2_Code",HEADER_CHGE[test,]))==0L){ X <- X %>% mutate(Classif2_Code = NA)}
if(length(grep("Classif1_Code",HEADER_CHGE[test,]))==0L){ X <- X %>% mutate(Classif1_Code = NA)}
if(!length(grep("INFO",HEADER_CHGE[test,]))==0L){ X <- X %>% filter(!INFO %in% c("Not worked","Not applicable"))}
		
# exception	
X 	<- 	X %>%
				# mutate(	Indicator_Code = ifelse(FILE%in%"ILO_POP_141.csv" & Indicator_Code%in%"EIP_TEIP_SEX_AGE_NB","EIP_TEIP_SEX_NB",Indicator_Code)) %>%
				filter(	!(FILE%in%"ILO_POP_141.csv" & Indicator_Code%in%"EIP_TEIP_SEX_NB" & !Classif1_Code%in%"AGE_5YRBANDS_TOTAL") ,
						#!(FILE%in%"ILO_POP_142.csv" & Indicator_Code%in%"EIP_TEIP_SEX_AGE_EDU_NB"),
						!(FILE%in%"ILO_POP_142_97.csv" & Classif2_Code%in%c("LX","MX","HX")) #,
						# !(FILE%in%"ILO_POP_143.csv" & Indicator_Code%in%"EIP_TEIP_SEX_GEO_NB")
						) %>%
				mutate(	
					#Classif1_Code = ifelse(FILE%in%"ILO_POP_141.csv" & Indicator_Code%in%"EIP_TEIP_SEX_NB",NA,Classif1_Code),
					Classif2_Code = ifelse(FILE%in%"ILO_EMP_109_NaceRev1.csv" & Classif2_Code%in%c(999,"999"),"OCU_ISCO88_X",Classif2_Code),
				Classif1_Code = ifelse(FILE%in%"ILO_EMP_100_Isco88.csv" & Classif1_Code%in%c(999,"999"),"OCU_ISCO88_X",Classif1_Code),
				Indicator_Code = gsub("SEX_ECO1_","SEX_ECO_",Indicator_Code),
				Indicator_Code = gsub("SEX_OCU1_","SEX_OCU_",Indicator_Code),
				Indicator_Code = ifelse(Indicator_Code%in%c("HOW_XEES_SEX_OCO_NB"),"HOW_XEES_SEX_OCU_NB",Indicator_Code)) %>%
				filter(	!Indicator_Code%in%"ILOSTAT_NRESP",
						!(Indicator_Code%in%c("EES_TEES_ECO_OCU_NB","EMP_TEMP_ECO_OCU_NB") & Sex_Code%in%c("SEX_F","SEX_M"))) %>%
				mutate(Notes_Indicator_Code = ifelse(BREAK%in%"b","I11:269",NA ), 
				Value_Status_Code = ifelse(BREAK%in%"b", "B", Value_Status_Code),
				Sex_Code = ifelse(Indicator_Code%in%c("EES_TEES_ECO_OCU_NB","EMP_TEMP_ECO_OCU_NB") & Sex_Code%in%c("SEX_T"),NA,Sex_Code)) %>%  
				select(-contains("BREAK"),-contains("INFO")) %>%  
				mutate(Classif1_Code = as.character(Classif1_Code)) %>%
				left_join(select(as.tbl(PATCH_CODE) ,Classif1_Code = OLD, NEW_CL1 = NEW), by="Classif1_Code")	%>% 
				mutate(Classif1_Code = NEW_CL1) %>% select(-NEW_CL1) %>%
				mutate(Classif2_Code = as.character(Classif2_Code)) %>%
				left_join(select(as.tbl(PATCH_CODE) ,Classif2_Code = OLD, NEW_CL2 = NEW), by = "Classif2_Code")%>% mutate(Classif2_Code = NEW_CL2) %>% select(-NEW_CL2) %>%
				separate(Sex_Code,"Sex_Version_Code", sep="_", extra = "drop", remove = FALSE) %>%
				separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
				unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>%
				separate(Classif2_Code,c("CODE_CLACL2","CODE_VSCL2"), sep="_", extra = "drop", remove = FALSE) %>%
				unite(	Classif2_Version_Code,CODE_CLACL2,CODE_VSCL2, sep = "_", remove = TRUE) %>%
				mutate(	Classif1_Version_Code = gsub("NA_NA",NA,Classif1_Version_Code),
						Classif2_Version_Code = gsub("NA_NA",NA,Classif2_Version_Code),
						Value_Status_Code = ifelse(Value_Status_Code%in%"",NA,Value_Status_Code))	
						
			print(paste0(ref_file, ' : ', nrow(X)))		
return(X)
}

clean_une_prev_emp <- function(REF, X, new){

# REF <- filter(X,	FILE%in%"ILO_UNE_34.csv",	Classif1_Code%in%"CAT_UNE_PRE",	Time<2008);	X 	<- filter(X,	FILE%in%"ILO_UNE_35_NacePRev1.csv",	Classif1_Version_Code %in% "ECO_ISIC3");							new <- c(FILE = "ILO_UNE_35_NacePRev1.csv",	Indicator_Code 	= "UNE_TUNE_SEX_ECO_NB", Classif1_Version_Code = "ECO_ISIC3")


new <- as.data.frame(t(new), stringsAsFactors  = FALSE)


		# ref coming from another table will replace old TOTAL value
		# new 
REF_TOT <- 	REF %>% 
		mutate(
				FILE 			= as.character(new$FILE),
				Indicator_Code 	= as.character(new$Indicator_Code),
				Classif1_Code 	= paste0(as.character(new$Classif1_Version_Code),"_TOTAL"),
				Classif1_Version_Code = as.character(new$Classif1_Version_Code)) %>%
		unite_("ID",c("Country_Code","Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code","Time","FILE"), sep="_", remove = FALSE) %>%
		unite_("ID_QT",c("Country_Code","Indicator_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","FILE"), sep="_", remove = FALSE)

		
X 	<- 	X %>% 
		unite_("ID",c("Country_Code","Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code","Time","FILE"), sep="_", remove = FALSE) %>%
		unite_("ID_QT",c("Country_Code","Indicator_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","FILE"), sep="_", remove = FALSE)
		
		
DEL <- 	X %>% 
		filter(	Classif1_Code %in% paste0(as.character(new$Classif1_Version_Code),"_TOTAL"),
				!ID %in% unique(REF_TOT$ID))

		
REF_TOT <- REF_TOT %>% 
		filter(ID_QT%in%X$ID_QT)
		
X 	<- 	X %>% 
		filter(	!ID %in% REF_TOT$ID,
				!ID_QT %in% DEL$ID_QT) %>% 
				select(	-ID,-ID_QT) 
		
REF_SUM <- X %>% 
		filter(!Classif1_Code %in% paste0(as.character(new$Classif1_Version_Code),"_X"),
			   !Classif1_Code %in% paste0(as.character(new$Classif1_Version_Code),"_TOTAL")) %>%
		group_by(Country_Code,Indicator_Code,Sex_Code,Classif1_Version_Code,Time,FILE) %>%
		summarise(	Value = sum(Value, na.rm = TRUE),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Sex_Version_Code = first(Sex_Version_Code),
					Classif2_Code = first(Classif2_Code),
					Classif2_Version_Code = first(Classif2_Version_Code)) %>%
		ungroup() %>%
		mutate(Classif1_Code = paste0(as.character(Classif1_Version_Code),"_TOTAL")) %>%
		unite_("ID",c("Country_Code","Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code","Time","FILE"), sep="_", remove = FALSE)
		

REF_X <- REF_SUM %>% 
		mutate(Value_SUM = Value) %>% 
		select(-Value,-Country_Code, -Indicator_Code, -Sex_Code, -Classif1_Version_Code,-Classif2_Version_Code, -Time, -FILE, -Notes_Indicator_Code, -Sex_Version_Code, -Classif1_Code,-Classif2_Code) %>% 
		left_join(REF_TOT, by="ID") %>%
		mutate(Value = Value - Value_SUM,
			   Classif1_Code = paste0(as.character(new$Classif1_Version_Code),"_X"))
		
		
		
X 	<- X %>% 
		filter(!Classif1_Code %in% paste0(as.character(new$Classif1_Version_Code),"_X"),
			   !Classif1_Code %in% paste0(as.character(new$Classif1_Version_Code),"_TOTAL")) %>%
		bind_rows(REF_TOT,REF_X) %>%
		select(	-ID,-ID_QT,-Value_SUM)
		
		
return(X)

}

CAL_how_aggregate <- function(X) {


Y_CL1_LEVEL1 <- X %>% 
		filter(substr(Indicator_Code,1,3)%in%"HOW"	) %>%
		filter(Classif1_Code%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by ="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		mutate(REF = as.numeric(DENOMINATOR) * as.numeric(Value)) %>%
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(REF)), 
				DENOMINATOR = sum(as.numeric(DENOMINATOR)), 
				Notes_Topic_Code = first(Notes_Topic_Code),    
				Notes_Source_Code = first(Notes_Source_Code),
				Notes_Indicator_Code = first(Notes_Indicator_Code))%>%
		mutate(Value = Value / DENOMINATOR)%>%
		ungroup()
							

Y_CL1_LEVEL2 <- Y_CL1_LEVEL1 %>% 
		filter(Classif1_Code%in%CALC$L1) %>%
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		mutate(REF = as.numeric(DENOMINATOR) * as.numeric(Value)) %>%
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(REF)), 
					DENOMINATOR = sum(as.numeric(DENOMINATOR)), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code))%>%
		mutate(Value = Value / DENOMINATOR)%>%
		ungroup()
					
X <- bind_rows(Y_CL1_LEVEL1,Y_CL1_LEVEL2) %>% 
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) 
return(X)

}
	
CAL_nb_aggregate_l1 <- function(X) {


Y_CL1_LEVEL1 <- X %>% 
		filter( !substr(Indicator_Code,1,3)%in%"HOW", 
				!substr(Indicator_Code,(nchar(Indicator_Code)-1),nchar(Indicator_Code))%in%"RT") %>%
		filter(Classif1_Code%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(Value), na.rm = TRUE),
				Notes_Topic_Code = first(Notes_Topic_Code),    
				Notes_Source_Code = first(Notes_Source_Code),
				Notes_Indicator_Code = first(Notes_Indicator_Code),
				Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate(Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)
		
							

Y_CL1_LEVEL2 <- Y_CL1_LEVEL1 %>% 
		filter(Classif1_Code%in%CALC$L1) %>%
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(Value), na.rm = TRUE), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate(Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)


X <- bind_rows(Y_CL1_LEVEL1,Y_CL1_LEVEL2)		
		
########## exeption AGE YTHADULT	


Y_CL1_LEVEL3_1 <- X %>% # create AGE_YTHADULT_YGE15 and AGE_YTHADULT_Y15-24
		filter(Classif1_Code %in% c("AGE_AGGREGATE_TOTAL","AGE_AGGREGATE_Y15-24")) %>%
		select(-Classif1_Version_Code) %>%
		mutate(Classif1_Code = ifelse(Classif1_Code %in% "AGE_AGGREGATE_TOTAL","AGE_YTHADULT_YGE15","AGE_YTHADULT_Y15-24")) %>% 
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) 



				
					
Y_CL1_LEVEL3_2 <- X %>% # create AGE_YTHADULT_Y15-64
		filter(Classif1_Code %in% c("AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y25-54","AGE_AGGREGATE_Y55-64")) %>%
		mutate(Classif1_Code = "AGE_YTHADULT_Y15-64") %>%        
		select(-Classif1_Version_Code) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(Value), na.rm = TRUE), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate(Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)
				
				
				
Y_CL1_LEVEL3_3 <- X %>% # create AGE_YTHADULT_YGE25
		filter(Classif1_Code %in% c("AGE_AGGREGATE_Y25-54","AGE_AGGREGATE_Y55-64","AGE_AGGREGATE_YGE65")) %>%
		mutate(Classif1_Code = "AGE_YTHADULT_YGE25") %>%
		select(-Classif1_Version_Code) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(Value), na.rm = TRUE), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup()%>%
		mutate(Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)	
					
				
X <- X %>% bind_rows(Y_CL1_LEVEL3_1,Y_CL1_LEVEL3_2, Y_CL1_LEVEL3_3)%>% 
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
		filter(!(Value%in%NA & Value_Status_Code%in%NA))
					
					
				



				
					

return(X)

}
	
CAL_nb_aggregate_l2 <- function(X) {


Y_CL2_LEVEL1 <- X %>% 
		filter(!substr(Indicator_Code,1,3)%in%"HOW", 
				!substr(Indicator_Code,(nchar(Indicator_Code)-1),nchar(Indicator_Code))%in%"RT"	) %>%
		filter(Classif2_Code%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,Classif2_Code = L1, REF = L2), by="Classif2_Code") %>%
		select(-Classif2_Code,-Classif2_Version_Code) %>%
		rename(Classif2_Code = REF) %>%
		separate(Classif2_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif2_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	Value = sum(as.numeric(Value), na.rm = TRUE), 
				Notes_Topic_Code = first(Notes_Topic_Code),    
				Notes_Source_Code = first(Notes_Source_Code),
				Notes_Indicator_Code = first(Notes_Indicator_Code),
				Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate(Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)	%>% 
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
		filter(!(Value%in%NA & Value_Status_Code%in%NA))
	

				
					

return(Y_CL2_LEVEL1)

}

CAL_rt_aggregate_l1 <- function(X) {


Y_CL1_LEVEL1 <- X %>% 
		filter( substr(Indicator_Code,(nchar(Indicator_Code)-1),nchar(Indicator_Code))%in%"RT") %>%
		filter(Classif1_Code%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	NOMINATOR = sum(as.numeric(NOMINATOR), na.rm = TRUE), 
					DENOMINATOR = sum(as.numeric(DENOMINATOR), na.rm = TRUE),
				Notes_Topic_Code = first(Notes_Topic_Code),    
				Notes_Source_Code = first(Notes_Source_Code),
				Notes_Indicator_Code = first(Notes_Indicator_Code),
				Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate( Value = NOMINATOR / DENOMINATOR * 100,
				Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)
		
							

Y_CL1_LEVEL2 <- Y_CL1_LEVEL1 %>% 
		filter(Classif1_Code%in%CALC$L1) %>%
		left_join(select(CALC,Classif1_Code = L1, REF = L2), by="Classif1_Code") %>%
		select(-Classif1_Code,-Classif1_Version_Code) %>%
		rename(Classif1_Code = REF) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	NOMINATOR = sum(as.numeric(NOMINATOR), na.rm = TRUE), 
					DENOMINATOR = sum(as.numeric(DENOMINATOR), na.rm = TRUE),
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate( Value = NOMINATOR / DENOMINATOR * 100,
				Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)


X <- bind_rows(Y_CL1_LEVEL1,Y_CL1_LEVEL2)		
		
########## exeption AGE YTHADULT	


Y_CL1_LEVEL3_1 <- X %>% # create AGE_YTHADULT_YGE15 and AGE_YTHADULT_Y15-24
		filter(Classif1_Code %in% c("AGE_AGGREGATE_TOTAL","AGE_AGGREGATE_Y15-24")) %>%
		select(-Classif1_Version_Code) %>%
		mutate(Classif1_Code = ifelse(Classif1_Code %in% "AGE_AGGREGATE_TOTAL","AGE_YTHADULT_YGE15","AGE_YTHADULT_Y15-24")) %>% 
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) 



				
					
Y_CL1_LEVEL3_2 <- X %>% # create AGE_YTHADULT_Y15-64
		filter(Classif1_Code %in% c("AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y25-54","AGE_AGGREGATE_Y55-64")) %>%
		mutate(Classif1_Code = "AGE_YTHADULT_Y15-64") %>%        
		select(-Classif1_Version_Code) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	NOMINATOR = sum(as.numeric(NOMINATOR), na.rm = TRUE), 
					DENOMINATOR = sum(as.numeric(DENOMINATOR), na.rm = TRUE), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup() %>%
		mutate( Value = NOMINATOR / DENOMINATOR * 100,
				Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)
				
				
				
Y_CL1_LEVEL3_3 <- X %>% # create AGE_YTHADULT_YGE25
		filter(Classif1_Code %in% c("AGE_AGGREGATE_Y25-54","AGE_AGGREGATE_Y55-64","AGE_AGGREGATE_YGE65")) %>%
		mutate(Classif1_Code = "AGE_YTHADULT_YGE25") %>%
		select(-Classif1_Version_Code) %>%
		separate(Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		group_by(Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Time) %>% 
		summarise(	NOMINATOR = sum(as.numeric(NOMINATOR), na.rm = TRUE), 
					DENOMINATOR = sum(as.numeric(DENOMINATOR), na.rm = TRUE), 
					Notes_Topic_Code = first(Notes_Topic_Code),    
					Notes_Source_Code = first(Notes_Source_Code),
					Notes_Indicator_Code = first(Notes_Indicator_Code),
					Value_Status_Code = first(Value_Status_Code),
				test = n())%>%
		ungroup()%>%
		mutate( Value = NOMINATOR / DENOMINATOR * 100,
				Value_Status_Code = ifelse(test>1,NA,Value_Status_Code)) %>%
		select(-test)	
					
				
X <- X %>% bind_rows(Y_CL1_LEVEL3_1,Y_CL1_LEVEL3_2, Y_CL1_LEVEL3_3)%>% 
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
		filter(!(Value%in%NA & Value_Status_Code%in%NA))
					
					
				

		

return(X)

}

CAL_rt_new <- function(X) {


key_ind <- c("Country_Code", "Survey_Code", "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Sex_Code", "Classif1_Code", "Classif2_Code", "Time")

X <- X %>% 
		filter( substr(Indicator_Code,(nchar(Indicator_Code)-1),nchar(Indicator_Code))%in%"NB") %>%
		select(-ID_QT)

		
# EAP_DWAP_SEX_GEO_RT <- X %>% 	
				# filter(	Indicator_Code%in%"EAP_TEAP_SEX_GEO_NB") %>% 						# nominator
				# select(	-DENOMINATOR) %>% 
				# mutate(	NOMINATOR = Value) %>%
				# unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				# left_join(	X %>% 
								# filter(Indicator_Code%in%"POP_XWAP_SEX_GEO_NB") %>%			# denominator
								# unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								# select(ID, DENOMINATOR = Value), by="ID") %>%
				# mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						# Indicator_Code = "EAP_DWAP_SEX_GEO_RT") %>% 						# results
				# select(-ID)	
		
# EMP_DWAP_SEX_GEO_RT <- X %>% 	
				# filter(	Indicator_Code%in%"EMP_TEMP_SEX_GEO_NB") %>% 						# nominator
				# select(	-DENOMINATOR) %>% 
				# mutate(	NOMINATOR = Value) %>%
				# unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				# left_join(	X %>% 
								# filter(Indicator_Code%in%"POP_XWAP_SEX_GEO_NB") %>%			# denominator
								# unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								# select(ID, DENOMINATOR = Value), by="ID") %>%
				# mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						# Indicator_Code = "EMP_DWAP_SEX_GEO_RT") %>% 						# results
				# select(-ID)	

UNE_DEAP_SEX_GEO_RT <- X %>% 	
				filter(	Indicator_Code%in%"UNE_TUNE_SEX_GEO_NB") %>% 						# nominator
				select(	-DENOMINATOR) %>% 
				mutate(	NOMINATOR = Value) %>%
				unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				left_join(	X %>% 
								filter(Indicator_Code%in%"EAP_TEAP_SEX_GEO_NB") %>%			# denominator
								unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								select(ID, DENOMINATOR = Value), by="ID") %>%
				mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						Indicator_Code = "UNE_DEAP_SEX_GEO_RT") %>% 						# results
				select(-ID)
		
		
EAP_DWAP_SEX_EDU_RT <- X %>% 	
				filter(	Indicator_Code%in%"EAP_TEAP_SEX_AGE_EDU_NB") %>% 						# nominator
				filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
				select(-Classif1_Code,- Classif1_Version_Code) %>%
				rename(	Classif1_Code = Classif2_Code , 
						Classif1_Version_Code = Classif2_Version_Code) %>%
				mutate( Classif2_Code = as.character(NA),
						Classif2_Version_Code = as.character(NA)) %>%
				select(	-DENOMINATOR) %>% 
				mutate(	NOMINATOR = Value,
						Indicator_Code = "EAP_TEAP_SEX_EDU_NB") %>%
				unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				left_join(	X %>% 
								filter(Indicator_Code%in%"POP_XWAP_SEX_AGE_EDU_NB") %>%			# denominator
								filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
								select(-Classif1_Code,-Classif1_Version_Code) %>%
								rename(	Classif1_Code  = Classif2_Code, 
										Classif1_Version_Code = Classif2_Version_Code) %>%
								mutate( Classif2_Code = as.character(NA),
										Classif2_Version_Code = as.character(NA),
										Indicator_Code = "POP_XWAP_SEX_EDU_NB") %>%
								unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								select(ID, DENOMINATOR = Value), by="ID") %>%
				mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						Indicator_Code = "EAP_DWAP_SEX_EDU_RT") %>% 						# results
				select(-ID)		
		

EMP_DWAP_SEX_EDU_RT <- X %>% 	
				filter(	Indicator_Code%in%"EMP_TEMP_SEX_AGE_EDU_NB") %>% 						# nominator
				filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
				select(-Classif1_Code,- Classif1_Version_Code) %>%
				rename(	Classif1_Code = Classif2_Code , 
						Classif1_Version_Code = Classif2_Version_Code) %>%
				mutate( Classif2_Code = as.character(NA),
						Classif2_Version_Code = as.character(NA)) %>%
				select(	-DENOMINATOR) %>% 
				mutate(	NOMINATOR = Value,
						Indicator_Code = "EMP_TEMP_SEX_EDU_NB") %>%
				unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				left_join(	X %>% 
								filter(Indicator_Code%in%"POP_XWAP_SEX_AGE_EDU_NB") %>%			# denominator
								filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
								select(-Classif1_Code,-Classif1_Version_Code) %>%
								rename(	Classif1_Code  = Classif2_Code, 
										Classif1_Version_Code = Classif2_Version_Code) %>%
								mutate( Classif2_Code = as.character(NA),
										Classif2_Version_Code = as.character(NA),
										Indicator_Code = "POP_XWAP_SEX_EDU_NB") %>%
								unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								select(ID, DENOMINATOR = Value), by="ID") %>%
				mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						Indicator_Code = "EMP_DWAP_SEX_EDU_RT") %>% 						# results
				select(-ID)		
		

		
UNE_DEAP_SEX_EDU_RT  <- X %>% 	
				filter(	Indicator_Code%in%"UNE_TUNE_SEX_AGE_EDU_NB") %>% 						# nominator
				filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
				select(-Classif1_Code,- Classif1_Version_Code) %>%
				rename(	Classif1_Code = Classif2_Code , 
						Classif1_Version_Code = Classif2_Version_Code) %>%
				mutate( Classif2_Code = as.character(NA),
						Classif2_Version_Code = as.character(NA)) %>%
				select(	-DENOMINATOR) %>% 
				mutate(	NOMINATOR = Value,
						Indicator_Code = "UNE_TUNE_SEX_EDU_NB") %>%
				unite_(	"ID",key_ind, sep="/", remove = FALSE) %>%
				left_join(	X %>% 
								filter(Indicator_Code%in%"EAP_TEAP_SEX_AGE_EDU_NB") %>%			# denominator
								filter(Classif1_Code %in% c("AGE_10YRBANDS_TOTAL")) %>%
								select(-Classif1_Code,-Classif1_Version_Code) %>%
								rename(	Classif1_Code  = Classif2_Code, 
										Classif1_Version_Code = Classif2_Version_Code) %>%
								mutate( Classif2_Code = as.character(NA),
										Classif2_Version_Code = as.character(NA),
										Indicator_Code = "EAP_TEAP_SEX_EDU_NB") %>%
								unite_("ID",key_ind, sep="/", remove = FALSE) %>%
								select(ID, DENOMINATOR = Value), by="ID") %>%
				mutate(	Value = NOMINATOR / DENOMINATOR * 100,
						Indicator_Code = "UNE_DEAP_SEX_EDU_RT") %>% 						# results
				select(-ID)	


		
		
		
# to ADD		
# TRU_DEMP_SEX_AGE_RT  # 24 / 11 *100 = 25      TRU_TTRU_SEX_AGE_NB / EMP_TEMP_SEX_AGE_NB
# TRU_DEMP_SEX_ECO_RT  # 26 / 95 *100 = 27	  TRU_TTRU_SEX_ECO_NB
# EIP_NEET_SEX_RT  	 # [40] / [141] AGE_AGGREGATE_Y15-24 * 100          EIP_NEET_SEX_NB  / 
# EIP_DWAP_SEX_AGE_RT  # 100 - 6
# EIP_WDIS_SEX_AGE_RT  # (28 + 39) / (5+39) *100 = 112      EIP_WDIS_SEX_AGE_NB



# "UNE_DEAP_SEX_EDU_RT"       # TO CHECK
		


		
				
X <- bind_rows(	EAP_DWAP_SEX_GEO_RT,
				EMP_DWAP_SEX_GEO_RT,
				UNE_DEAP_SEX_GEO_RT,
				EAP_DWAP_SEX_EDU_RT,
				EMP_DWAP_SEX_EDU_RT, 
				UNE_DEAP_SEX_EDU_RT) %>% 
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
		filter(!(Value%in%NA & Value_Status_Code%in%NA))
					
					
				

		

return(X)

}

CAL_dt_new <- function(X) {


X <- X %>% 
		filter( substr(Indicator_Code,(nchar(Indicator_Code)-1),nchar(Indicator_Code))%in%"NB", 
				!substr(Indicator_Code,1,3)%in%"HOW",
				!substr(Indicator_Code,(nchar(Indicator_Code)-3),nchar(Indicator_Code))%in%"2_NB") %>%
		group_by(Country_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Time) %>%
		mutate(	Value = Value / first(Value) *100
		) %>%
		ungroup() %>%
		mutate(	Indicator_Code = gsub("_NB","_DT",Indicator_Code))%>% 
		select(-ID_QT) %>%
		unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
		filter(!(Value%in%NA & Value_Status_Code%in%NA))
		

return(X)

}

# Mapping_File 	<- read.xlsx("ReadME.xlsx", sheet="File")
Mapping_File 	<- readxl:::read_excel("ReadME.xlsx", sheet="File")
# load file names
ref_file <-  list.files("./CSV")
# test filenames
ref_file 	<- ref_file[ref_file%in%Mapping_File$PATH]
if(length(ref_file)<length(Mapping_File$PATH)) stop("incorrect number of file")
rm(Mapping_File)

###### reshape  142
X <- readr::read_csv("D:\\_MIG_EUROSTAT\\CSV\\ILO_POP_142.csv", col_types = 'cdccccccdccc')
X %>% filter(YEAR > 2013) %>% select(-HAT97LEV) %>% distinct(COUNTRY, YEAR, QUARTER, AGE, SEX, HAT11LEV, ILOSTAT, VALUE, FLAG, FLAG_BREAK, COUNTRY_ORDER, .keep_all = TRUE)%>% readr::write_csv("D:\\_MIG_EUROSTAT\\CSV\\ILO_POP_142_11.csv") 
X %>% filter(YEAR < 2014) %>% select(-HAT11LEV) %>% distinct(COUNTRY, YEAR, QUARTER, AGE, SEX, HAT97LEV, ILOSTAT, VALUE, FLAG, FLAG_BREAK, COUNTRY_ORDER, .keep_all = TRUE) %>% readr::write_csv("D:\\_MIG_EUROSTAT\\CSV\\ILO_POP_142_97.csv") 


YI_IND <- c('EAP_TEAP_SEX_NB',
			'EAP_TEAP_SEX_EDU_NB',
			#'EAP_DWAP_SEX_RT',
			'EAP_TEAP_SEX_AGE_EDU_NB',
			'EAP_TEAP_SEX_AGE_NB',
			'EAP_DWAP_SEX_AGE_RT',
			#'EAP_DWAP_SEX_EDU_RT',
			'EAP_TEAP_SEX_AGE_GEO_NB',
			#'EAP_DWAP_SEX_AGE_GEO_RT',
			'EES_TEES_ECO_OCU_NB',
			'EES_TEES_SEX_ECO_NB',
			'EES_TEES_SEX_OCU_NB',
			'EES_TEES_SEX_OCU2_NB', ###
			'EES_TEES_SEX_ECO2_NB', ###
			'EES_TEES_SEX_HOW_NB', ###
			#'EES_XFNA_NOC_RT',
			'EES_TEES_SEX_INS_NB',
			#'EIP_DWAP_SEX_AGE_RT',
			'EIP_WDIS_SEX_AGE_NB',
			'EIP_WDIS_SEX_AGE_RT',
			#'EIP_TEIP_SEX_AGE_GEO_NB',
			'EIP_NEET_SEX_NB',
			'EIP_NEET_SEX_RT',###
			'EMP_TEMP_SEX_NB',
			'EMP_TEMP_SEX_EDU_NB',
			#'EMP_XYTH_SEX_NB',
			#'EMP_DWAP_SEX_EDU_RT',
			#'EMP_TEMP_SEX_AGE_GR',
			'EMP_TEMP_ECO_OCU_NB',
			'EMP_TEMP_SEX_AGE_EDU_NB',
			'EMP_TEMP_SEX_ECO_NB',
			'EMP_TEMP_SEX_AGE_NB',
			'EMP_TEMP_SEX_OCU_NB',
			'EMP_DWAP_SEX_AGE_RT',
			#'EMP_XFEM_ECO_RT',
			'EMP_TEMP_SEX_OCU2_NB',###
			#'EMP_XFEM_OCU_RT',
			'EMP_TEMP_SEX_STE_NB',
			'EMP_TEMP_SEX_ECO2_NB',###
			'EMP_TEMP_SEX_HOW_NB',###
			'EMP_TEMP_SEX_AGE_GEO_NB',
			#'EMP_DWAP_SEX_AGE_GEO_RT',
			#'EMP_TVUL_SEX_NB',
			#'EMP_XOCW_SEX_RT',
			#'EMP_PTER_SEX_RT',
			'EMP_TEMP_SEX_INS_NB',
			#'EMP_XFMG_NOC_RT',
			'HOW_XEES_SEX_ECO_NB',
			'HOW_TEMP_SEX_ECO_NB',
			'HOW_TEMP_SEX_OCU2_NB',	
			'HOW_XEES_SEX_OCU2_NB',###
			'HOW_TEMP_SEX_OCU_NB',
			'HOW_XEES_SEX_OCU_NB',
			'HOW_XEES_SEX_ECO2_NB',###
			'HOW_TEMP_SEX_ECO2_NB',###
			#'POP_XWAP_SEX_NB',
			#'POP_XWAP_SEX_EDU_NB',
			'POP_XWAP_SEX_AGE_EDU_NB',
			'POP_XWAP_SEX_AGE_NB',
			'POP_XWAP_SEX_AGE_GEO_NB',
			'TRU_TTRU_SEX_NB',
			'TRU_TTRU_SEX_AGE_NB',
			#'TRU_DEMP_SEX_AGE_RT',
			'TRU_TTRU_SEX_ECO_NB',
			#'TRU_DEMP_SEX_ECO_RT',
			#'UNE_TUNE_SEX_NB',
			#'UNE_TUNE_SEX_EDU_NB',
			#'UNE_TUNE_SEX_DUR_NB',
			#'UNE_XYTH_SEX_NB',
			#'UNE_XYTH_SEX_RT',
			#'UNE_DEAP_SEX_RT',
			'UNE_TUNE_SEX_AGE_EDU_NB',
			'UNE_TUNE_SEX_AGE_NB',
			'UNE_TUNE_SEX_AGE_DUR_NB',
			'UNE_TUNE_SEX_ECO_NB',
			'UNE_DEAP_SEX_AGE_RT',
			'UNE_TUNE_SEX_OCU_NB',
			#'UNE_DEAP_SEX_EDU_RT',
			'UNE_TUNE_SEX_CAT_NB', ###
			#'UNE_XFEM_EDU_RT',
			#'UNE_DEAP_SEX_AGE_GEO_RT',
			'UNE_TUNE_SEX_AGE_GEO_NB' 
			)


PATCH_CODE 		<- readxl:::read_excel("ReadME.xlsx", sheet="PATCH_CODE") %>% rbind(c(OLD = as.character(NA), NEW = as.character(NA)))
HEADER_CHGE 	<- readxl:::read_excel("ReadME.xlsx", sheet="HEADER_CHGE")        
Mapping_Source 	<- readxl:::read_excel("ReadME.xlsx", sheet="MappingSource") 
CALC 			<- readxl:::read_excel("ReadME.xlsx", sheet="CALCULATION")        

# define ID_QT    
ID_QT <- c("Country_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")
ID    <- c("Country_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Code","Classif1_Code","Classif2_Code")


#X <- ref_file %>% ldply(function(x) eurostat_combine(x,HEADER_CHGE,PATCH_CODE)) %>% as.tbl


X <- NULL
for (i in seq_along(ref_file)){

X <- X %>% bind_rows( eurostat_combine(ref_file[i],HEADER_CHGE,PATCH_CODE))

}



########### Exception

Z <- X %>% filter(!(Country_Code %in% "CYP" & Indicator_Code%in% c("POP_XWAP_SEX_AGE_NB","EAP_DWAP_SEX_AGE_RT", "EMP_DWAP_SEX_AGE_RT") & Time %in% "1999"))


rm(ref_file, HEADER_CHGE,PATCH_CODE)


ref_file <- unique(X$Country_Code)

for (i in 1:length(ref_file)){

saveRDS(X[X$Country_Code%in%ref_file[i], ], file = paste0("./tmp/", ref_file[i], ".rds"))
} 
rm(X)

CODE_ORA <- Ariane:::CODE_ORA

attach(CODE_ORA)


for (i in 1:length(ref_file)){

X <- readRDS(paste0("./tmp/", ref_file[i], ".rds"))


# apply break to the Qtable # 
			
X <- bind_rows(	X %>% filter(	!(FILE%in%"ILO_UNE_35_NacePRev1.csv" & Classif1_Version_Code %in% "ECO_ISIC3"),
								!(FILE%in%"ILO_UNE_35_NacePRev2.csv" & Classif1_Version_Code %in% "ECO_ISIC4"),
								!(FILE%in%"ILO_UNE_36_Isco88.csv" 	& Classif1_Version_Code %in% "OCU_ISCO88"),
								!(FILE%in%"ILO_UNE_36_Isco08.csv" 	& Classif1_Version_Code %in% "OCU_ISCO08")),
				clean_une_prev_emp(REF = filter(X,	FILE%in%"ILO_UNE_34.csv",
												Classif1_Code%in%"CAT_UNE_PRE",
												Time<2008),
									X 	= filter(X,	FILE%in%"ILO_UNE_35_NacePRev1.csv",
												Classif1_Version_Code %in% "ECO_ISIC3"),
									new = c(FILE = "ILO_UNE_35_NacePRev1.csv",
											Indicator_Code 	= "UNE_TUNE_SEX_ECO_NB",
											Classif1_Version_Code = "ECO_ISIC3")),   
				clean_une_prev_emp(REF = filter(X,	FILE%in%"ILO_UNE_34.csv",
												Classif1_Code%in%"CAT_UNE_PRE",
												Time>2007),
									X 	= filter(X,	FILE%in%"ILO_UNE_35_NacePRev2.csv",
												Classif1_Version_Code %in% "ECO_ISIC4"),
									new = c(FILE = "ILO_UNE_35_NacePRev2.csv",
											Indicator_Code 	= "UNE_TUNE_SEX_ECO_NB",
											Classif1_Version_Code = "ECO_ISIC4")),
				clean_une_prev_emp(REF = filter(X,	FILE%in%"ILO_UNE_34.csv",
												Classif1_Code%in%"CAT_UNE_PRE",
												Time<2011),
									X 	= filter(X,	FILE%in%"ILO_UNE_36_Isco88.csv",
												Classif1_Version_Code %in% "OCU_ISCO88"),
									new = c(FILE = "ILO_UNE_36_Isco88.csv",
											Indicator_Code 	= "UNE_TUNE_SEX_OCU_NB",
											Classif1_Version_Code = "OCU_ISCO88")),											
				clean_une_prev_emp(REF = filter(X,	FILE%in%"ILO_UNE_34.csv",
												Classif1_Code%in%"CAT_UNE_PRE",
												Time>2010),
									X 	= filter(X,	FILE%in%"ILO_UNE_36_Isco08.csv",
												Classif1_Version_Code %in% "OCU_ISCO08"),
									new = c(FILE = "ILO_UNE_36_Isco08.csv",
											Indicator_Code 	= "UNE_TUNE_SEX_OCU_NB",
											Classif1_Version_Code = "OCU_ISCO08"))								
											) %>%
	group_by(Country_Code,Indicator_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Sex_Code,Classif1_Code,Classif2_Code,Time) %>%
	summarise(	Value 	= sum(Value),
				NOMINATOR = sum(NOMINATOR),
				DENOMINATOR = sum(DENOMINATOR),
				Value_Status_Code = first(Value_Status_Code),
				Notes_Topic_Code = first(Notes_Topic_Code),
				Notes_Source_Code = first(Notes_Source_Code),
				Notes_Indicator_Code = first(Notes_Indicator_Code),
				FILE = first(FILE))	%>%
	ungroup()	%>%										
	# add notes 		
	mutate(	Notes_Topic_Code = ifelse(substr(Indicator_Code,1,3)%in%"EES",paste0(Notes_Topic_Code,"_T11:141_T12:145"),Notes_Topic_Code),
			Notes_Topic_Code = ifelse(substr(Indicator_Code,1,3)%in%"UNE",paste0(Notes_Topic_Code,"_T5:114"),Notes_Topic_Code),
			Notes_Topic_Code = gsub('NA_', '', Notes_Topic_Code)
			) %>%
	# mapping of survey code
	left_join(Mapping_Source, by="Country_Code") %>%
	unite_("ID_QT",ID_QT, sep="_", remove = FALSE) %>%
	select(-FILE)
 
 
 
  # delete Qtable with data  97 	98 = no responses
test <- X %>% filter(substr(Indicator_Code,1,3)%in%"HOW" & substr(Value,1,2)%in%c("97","98")) %>% select(ID_QT) %>%
					  distinct(ID_QT) 

if(!empty(test)){X 	<- X %>% filter(!ID_QT%in%test$ID_QT)} ; rm(test)


 
 ############ calculation
 
X <- bind_rows(X,	CAL_how_aggregate(X)) ; dim(X)
X <- bind_rows(X,	CAL_nb_aggregate_l1(X)) ; dim(X)
X <- bind_rows(X,	CAL_nb_aggregate_l2(X)) ; dim(X)
X <- bind_rows(X,	CAL_rt_aggregate_l1(X)) ; dim(X)

# X <- bind_rows(X,	CAL_rt_new(X)) ; dim(X)
	

	
X <- X %>%  # sort
		left_join(select(T_CLV_CLASSIF_VERSION, Classif1_Version_Code = CLV_CODE, CLASSIF1_VERSION_SORT = CLV_SORT),by ="Classif1_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif2_Version_Code = CLV_CODE, CLASSIF2_VERSION_SORT = CLV_SORT),by ="Classif2_Version_Code") %>%
		left_join(select(T_CLA_CLASSIF, Sex_Code = CLA_CODE, SEX_SORT = CLA_SORT),by ="Sex_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif1_Code = CLA_CODE, CLASSIF1_SORT = CLA_SORT),by ="Classif1_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif2_Code = CLA_CODE, CLASSIF2_SORT = CLA_SORT),by ="Classif2_Code") %>%
		arrange(Country_Code,Survey_Code, Indicator_Code,CLASSIF1_VERSION_SORT,CLASSIF2_VERSION_SORT, Time, SEX_SORT, CLASSIF1_SORT, CLASSIF2_SORT) %>%
		select(-CLASSIF1_VERSION_SORT, -CLASSIF2_VERSION_SORT, -SEX_SORT, -CLASSIF1_SORT, -CLASSIF2_SORT)
		 	
	
# X <- bind_rows(X,	CAL_dt_new(X)) ; dim(X)
	
X <- X %>% filter(Indicator_Code%in%YI_IND) ; dim(X)
# Y <- X %>% filter(!Indicator_Code%in%YI_IND) ; dim(X)

############ various test

# X <-  X %>%
	# unite_("ID",ID, sep="_", remove = FALSE) %>%
	# arrange(ID) %>%
	# select(-ID)


DEL <- c(		"GEO_COV_NAT | GEO_COV_X","GEO_COV_X | GEO_COV_NAT",
					"ECO_AGGREGATE_TOTAL | ECO_AGGREGATE_X","ECO_AGGREGATE_X | ECO_AGGREGATE_TOTAL",
					"ECO_ISIC2_TOTAL | ECO_ISIC2_0","ECO_ISIC2_0 | ECO_ISIC2_TOTAL",
					"ECO_ISIC3_TOTAL | ECO_ISIC3_X","ECO_ISIC3_X | ECO_ISIC3_TOTAL",
					"ECO_ISIC4_TOTAL | ECO_ISIC4_X","ECO_ISIC4_X | ECO_ISIC4_TOTAL",
					"ECO_SECTOR_TOTAL | ECO_SECTOR_X","ECO_SECTOR_X | ECO_SECTOR_TOTAL",
					"HOW_BANDS_TOTAL | HOW_BANDS_X", "HOW_BANDS_X | HOW_BANDS_TOTAL",
					"OCU_AGGREGATE_TOTAL | OCU_AGGREGATE_X","OCU_AGGREGATE_X | OCU_AGGREGATE_TOTAL",
					"OCU_ISCO88_TOTAL | OCU_ISCO88_X","OCU_ISCO88_X | OCU_ISCO88_TOTAL",
					"OCU_ISCO08_TOTAL | OCU_ISCO08_X","OCU_ISCO08_X | OCU_ISCO08_TOTALOCU_ISCO08_X",
					"STE_ICSE93_TOTAL | STE_ICSE93_6", "STE_ICSE93_6 | STE_ICSE93_TOTAL",
					"STE_AGGREGATE_TOTAL | STE_AGGREGATE_X","STE_AGGREGATE_X | STE_AGGREGATE_TOTAL",
					"EDU_AGGREGATE_TOTAL | EDU_AGGREGATE_X","EDU_AGGREGATE_X | EDU_AGGREGATE_TOTAL",
					"EDU_ISCED97_TOTAL | EDU_ISCED97_UNK","EDU_ISCED97_UNK | EDU_ISCED97_TOTAL",
					"EDU_ISCED11_TOTAL | EDU_ISCED11_9","EDU_ISCED11_X | EDU_ISCED11_TOTAL",
					"AGE_YTHADULT_YGE15 | AGE_YTHADULT_Y15-24","AGE_YTHADULT_Y15-24 | AGE_YTHADULT_YGE15",
					"DUR_AGGREGATE_TOTAL | DUR_AGGREGATE_X","DUR_AGGREGATE_X | DUR_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL | DUR_DETAILS_X","DUR_DETAILS_X | DUR_DETAILS_TOTAL",
					# "AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y15-24 | AGE_AGGREGATE_TOTAL",
					"AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y55-64","AGE_AGGREGATE_Y55-64 | AGE_AGGREGATE_TOTAL",
					"AGE_10YRBANDS_TOTAL | AGE_10YRBANDS_Y15-24","AGE_10YRBANDS_Y15-24 | AGE_10YRBANDS_TOTAL",
					"CAT_UNE_TOTAL | CAT_UNE_UNK", "CAT_UNE_UNK | CAT_UNE_TOTAL", 
					"AGE_10YRBANDS_TOTAL",
					"AGE_5YRBANDS_TOTAL",
					"JOB_TIME_TOTAL",
					"AGE_YTHADULT_Y15-64",
					# "AGE_AGGREGATE_TOTAL",
					'AGE_YTHADULT_YGE15', 
					"EDU_ISCED97_TOTAL",
					'EDU_ISCED11_TOTAL',
					"EDU_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL",
					"DUR_AGGREGATE_TOTAL", 					
					"ECO_ISIC4_TOTAL", 
					"ECO_ISIC3_TOTAL", 
					"ECO_SECTOR_TOTAL",
					"HOW_BANDS_TOTAL", 
					"ECO_ISIC2_TOTAL", 
					"STE_ICSE93_TOTAL", 
					"STE_AGGREGATE_TOTAL",
					"GEO_COV_NAT", 
					"CAT_UNE_TOTAL"
			)

X <- X %>% filter(!Classif1_Version_Code %in% 'AGE_YTHADULT')
					
 
 # start by cleaning at cl2 levels (no empty qtable)
test <- X %>%	
	group_by(Country_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Time) %>% 
	summarise(ID_QT = first(ID_QT),TEST =  paste(unique(Classif2_Code), collapse= " | ")) %>%
	ungroup() %>%
	filter(TEST%in%DEL) %>%
	select(ID_QT)
if(!empty(test)){X <- X %>% filter(!ID_QT%in%test$ID_QT) }; rm(test)

 # then by cleaning at cl1 levels (no empty qtable)
test <- X %>%	
	group_by(Country_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Time) %>% 
	summarise(ID_QT = first(ID_QT),TEST =  paste(unique(Classif1_Code), collapse= " | ")) %>%
	ungroup() %>%
	filter(TEST%in%DEL) %>%
	select(ID_QT)
if(!empty(test)){X <- X %>% filter(!ID_QT%in%test$ID_QT) } ; rm(test)

 # clean notes at the Qtable levels (same notes apply to the entire qtable) # note indicator
test <- X %>%	
			unite_("ID",c("ID_QT","Notes_Indicator_Code"), sep="_", remove = FALSE) %>% 
			select(-Sex_Code,-Classif1_Code,-Classif2_Code) %>%
		distinct(ID, .keep_all = TRUE)%>%
		group_by(Country_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Time) %>% 
		summarise(ID_QT = unique(ID_QT), TEST =  n(), note = Notes_Indicator_Code[dplyr::first(order(dplyr::desc(nchar(Notes_Indicator_Code))))]) %>%
		ungroup() %>%
		filter(TEST %in% 2)

if(!empty(test)){ X <- X %>% mutate(Notes_Indicator_Code = ifelse(ID_QT%in%test$ID_QT,test$note,Notes_Indicator_Code))	} ; rm(test)
	

 # clean notes at the Qtable levels (same notes apply to the entire qtable) # note source
test <- X %>%	
			unite_("ID",c("ID_QT","Notes_Source_Code"), sep="_", remove = FALSE) %>% 
			select(-Sex_Code,-Classif1_Code,-Classif2_Code) %>%
		distinct(ID, .keep_all = TRUE)%>%
		group_by(Country_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Time) %>% 
		summarise(ID_QT = unique(ID_QT), TEST =  n(), note = Notes_Source_Code[dplyr::first(order(dplyr::desc(nchar(Notes_Source_Code))))]) %>%
		ungroup() %>%
		filter(TEST %in% 2)

if(!empty(test)){ X <- X %>% mutate(Notes_Source_Code = ifelse(ID_QT%in%test$ID_QT,test$note,Notes_Source_Code))	} ; rm(test)
	 
 
 
 
dim(X)

 
 
 
 
 
 ############ good format
X <- X %>% rename(Survey_Id = Survey_Code) %>%
		mutate(		Collection_Code = "YI",	
					Lang = "EN",
					Freq_Code = "A"
					) 
	
X <- Ariane::Clean_col_format(X)



nrow(X %>% filter(!Notes_Indicator_Code %in%NA))

############ clean note at Qtable levels




X <- X %>% 	mutate(		Notes_Indicator_Code = ifelse(!Notes_Topic_Code%in%NA,paste(Notes_Topic_Code,Notes_Indicator_Code,sep="_"),Notes_Indicator_Code),
					Notes_Indicator_Code =  gsub("_NA","",Notes_Indicator_Code)) %>%
					select(-Notes_Topic_Code)

		
key_QTA     <- c("Country_Code","Indicator_Code","Survey_Id","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")



X <- X %>% left_join(select(CODE_ORA$T_CLA_CLASSIF, Classif1_Code = CLA_CODE,IS_CLA1_TOTAL = CLA_IS_TOTAL),by ="Classif1_Code") %>%
		left_join(select(CODE_ORA$T_CLA_CLASSIF, Classif2_Code = CLA_CODE,IS_CLA2_TOTAL = CLA_IS_TOTAL),by ="Classif2_Code") %>%
		left_join(select(CODE_ORA$T_CLA_CLASSIF, Sex_Code = CLA_CODE,IS_SEX_TOTAL = CLA_IS_TOTAL),by ="Sex_Code")%>%
		unite_("TEST", c("IS_SEX_TOTAL","IS_CLA1_TOTAL","IS_CLA2_TOTAL"), sep="/", remove = TRUE) %>%
		unite_("KEY",key_QTA, sep ="/", remove = FALSE ) %>%
		unite_("ID", c("KEY","Notes_Indicator_Code"), sep ="|", remove = FALSE)
		
		

if(!length(unique(X$KEY))==length(unique(X$ID))){

NEW <- X %>% 
		filter(!TEST%in%c("YNANA","YYNA","YYY")) %>%
		select(-TEST) %>%
		group_by(ID) %>%
		summarise (ref = "1") %>%
		ungroup() %>%
		mutate(	ID = as.character(ID))%>% 
		separate_("ID",c("KEY","NOTE"),sep="|", remove = TRUE, extra = "merge") %>%
		mutate(Nchar = nchar(NOTE)) %>%
		arrange(KEY, desc(Nchar)) %>%
		group_by(KEY) %>%
		summarise(n = n(), Nchar = first(Nchar), NOTE  = first(NOTE) ) %>%
		ungroup() %>% select(KEY,NOTE)
X <- X %>% 
		left_join(select(NEW, KEY , NEW_QTABLE_NOTE = NOTE)	, by ="KEY") %>%
		mutate(	Notes_Indicator_Code = ifelse(!NEW_QTABLE_NOTE%in%NA,NEW_QTABLE_NOTE,Notes_Indicator_Code))
rm(NEW)
}
X <- X %>% select(-TEST,-KEY,-ID)







nrow(X %>% filter(!Notes_Indicator_Code %in%NA))
		



 ############ load existing collected value, compare and prepare upload


key_ALL     <- c("Country_Code","Collection_Code","Indicator_Code","Survey_Id","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Time","Sex_Code","Classif1_Code","Classif2_Code","Classif3_Code","Classif4_Code","Classif5_Code")
ALL_COL     <- c("Country_Code","Collection_Code","Indicator_Code","Survey_Id","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Time","Sex_Code","Classif1_Code","Classif2_Code","Classif3_Code","Classif4_Code","Classif5_Code","Value","Value_Status_Code","Currency_Code","Notes_Source_Code","Notes_Indicator_Code", "Notes_Classif_Code")
key_QTA     <- c("Country_Code","Indicator_Code","Survey_Id","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")



Title <- "ALL"

EUROSTAT <- X
rm(X)


indi_ref <- unique(EUROSTAT$Indicator_Code) 

EUROSTAT <- EUROSTAT %>% mutate(
								Notes_Source_Code = Ariane:::My_Resort_NotesJ(Notes_Source_Code, SEP = '_'),
								Notes_Indicator_Code = Ariane:::My_Resort_NotesJ(Notes_Indicator_Code, SEP = '_')
						) %>% filter(!(Value %in% NA & Value_Status_Code %in% NA))


require(Ariane)
# init_ <- getwd()
# setwd('H:/')
# CODE_ORA <- Ariane:::CODE_ORA
# QUERY <- paste0("	mutate_each(funs(as.character), -Value) %>%
			# filter(	Collection_Code %in% c('YI'), 
					# Load_Mode %in% 'COL',
					# Freq_Code %in% 'A',
					# Indicator_Code %in% c(",paste0("'", indi_ref, "'", collapse = ', '),"),
					# Survey_Id %in% c('536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256')
			# )") %>% gsub('\n', ' ', .)
# ORA  <-	Ariane:::collectOracle(QUERY, Country = unique(EUROSTAT$Country_Code), Internal = TRUE) 
# unique(ORA$Qta_Last_Check_Date)
# ORA <- ORA %>%  select(Country_Code:Notes_Classif_Code)
# setwd(init_)


					
########## TEST ############################# 
#############################################
# EUROSTAT <- EUROSTAT %>% filter(Indicator_Code %in% unique(ORA$Indicator_Code)) %>% 
					# mutate(Value = round(as.numeric(Value),1), 
					# Notes_Source_Code = as.character(NA), 
					# Notes_Indicator_Code =as.character(NA), 
					# Notes_Classif_Code = as.character(NA))
# ORA <- ORA %>%  mutate(Collection_Code = 'YI', 
					# Value = round(as.numeric(Value),1), 
					# Notes_Source_Code = as.character(NA), 
					# Notes_Indicator_Code = as.character(NA), 
					# Notes_Classif_Code = as.character(NA))

########## !TEST ############################# 
#############################################




############# test if data exist on ORACLE and EUROSTAT : then compare

EUROSTAT <- EUROSTAT %>% mutate(Value = round(as.numeric(Value),4), Source_Code = 'BA') %>% mutate_if(is.factor, funs(as.character))
EUROSTAT <- eval(parse(text= paste0("  EUROSTAT %>% arrange(",paste0(key_ALL, collapse=","),")"))) # sort by key_ALL
EUROSTAT <- Ariane:::cleanDf(EUROSTAT)																				## keep EUROSTAT data
EUROSTAT <- eval(parse(text= paste0("  EUROSTAT %>% mutate(ID = 	paste(",paste0(c(ALL_COL), collapse=","),",sep='/'))"))) 
EUROSTAT <- EUROSTAT %>%
			mutate(	Notes_Source_Code = Notes_Source_Code %>% My_Resort_Notes_Type(SEP = '_'),
					Notes_Source_Code = gsub('|', '', Notes_Source_Code, fixed = TRUE),
					Notes_Indicator_Code = Notes_Indicator_Code %>% My_Resort_Notes_Type(SEP = '_'), 
					Notes_Indicator_Code = gsub('|', '', Notes_Indicator_Code, fixed = TRUE))

# ORA <- ORA %>% mutate(Value = round(as.numeric(Value),4)) %>% mutate_if(is.factor, funs(as.character))
# ORA <- eval(parse(text= paste0("  ORA %>% arrange(",paste0(key_ALL, collapse=","),")"))) # sort by key_ALL
# ORA <- Ariane:::cleanDf(ORA)
# ORA <- eval(parse(text= paste0("  ORA %>% mutate(ID = 	paste(",paste0(c(ALL_COL), collapse=","),",sep='/'))"))) 						


########## TEST ############################# 
#############################################
# TEST <- left_join(EUROSTAT, rename(ORA, TEST = Value)) %>% rename(	collection = Collection_Code, 
					# ref_area = Country_Code, 
					# indicator = Indicator_Code, 
					# sex = Sex_Code, 
					# classif1 = Classif1_Code, 
					# classif2 = Classif2_Code, 
					# time = Time, 
					# obs_value = Value, 
					# obs_status = Value_Status_Code, 
					# note_classif  = Notes_Classif_Code, 
					# note_source = Notes_Source_Code, 
					# note_indicator = Notes_Indicator_Code
					# ) %>% 
					# unite(source, Source_Code, Survey_Id, sep = ':', remove = TRUE) %>% 
					# mutate( note_indicator = ifelse(!Currency_Code %in% NA, paste0('T30:',Currency_Code, '_',note_indicator), note_indicator), 
							# note_indicator = ifelse(stringr::str_sub(note_indicator, -1,-1)%in% '_', stringr::str_sub(note_indicator, 1,-2), note_indicator), 
							# obs_value = as.numeric(obs_value)) %>% 
					# select(-Currency_Code, -Freq_Code) %>% select(-dplyr::contains('_Version_')) %>% select(-dplyr::contains('_Code'))%>% 
					# mutate(REF = abs(as.numeric(obs_value) - as.numeric(TEST) )) %>% select(-ID, -contains('note_'))
########## TEST ############################# 
#############################################
					

# ON_EUROSTAT_NOT_ON_ORA <- EUROSTAT %>% filter(!ID %in% ORA$ID) ; dim(ON_EUROSTAT_NOT_ON_ORA)
# ON_EUROSTAT_NOT_ON_ORA <- eval(parse(text= paste0("  ON_EUROSTAT_NOT_ON_ORA %>% mutate(ID = 	paste(",paste0(c(key_QTA), collapse=","),",sep='/'))"))) 
# EUROSTAT <- eval(parse(text= paste0("  EUROSTAT %>% mutate(ID = 	paste(",paste0(c(key_QTA), collapse=","),",sep='/'))"))) 
# ON_EUROSTAT_NOT_ON_ORA <- EUROSTAT %>% filter(ID %in% ON_EUROSTAT_NOT_ON_ORA$ID); print(dim(ON_EUROSTAT_NOT_ON_ORA))
# X <- ON_EUROSTAT_NOT_ON_ORA %>% select(-ID); rm(ON_EUROSTAT_NOT_ON_ORA)



 
X <- EUROSTAT  %>% select(-ID)

X <- Ariane:::cleanDf(X)


########## TEST ############################# 
#############################################
# TEST1 <- X %>% filter(Country_Code %in% 'AUT', Collection_Code %in% 'YI', Indicator_Code %in% 'EAP_DWAP_SEX_AGE_RT', Time %in% '2015', Classif1_Version_Code %in% 'AGE_10YRBANDS')
# TEST2 <- readRDS(file = "H:\\COMMON\\A0 Short term indicators\\Processing\\ILO_Data\\ON_ORACLE\\AUT.rds") %>% filter(Country_Code %in% 'AUT', Collection_Code %in% 'YI', Indicator_Code %in% 'EAP_DWAP_SEX_AGE_RT', Time %in% '2015', Classif1_Version_Code %in% 'AGE_10YRBANDS')

###X <- Ariane:::Clean_col_format(X, col_names = KEY_ORACLE)
########## TEST ############################# 
#############################################

X <- X %>% filter(	!(Classif1_Version_Code%in%NA & !Classif2_Version_Code%in%NA), 
					!(Country_Code%in% "CYP" & Time %in%"1999"))       ### delete exception error

X <- X %>% rename(	collection = Collection_Code, 
					ref_area = Country_Code, 
					indicator = Indicator_Code, 
					sex = Sex_Code, 
					classif1 = Classif1_Code, 
					classif2 = Classif2_Code, 
					time = Time, 
					obs_value = Value, 
					obs_status = Value_Status_Code, 
					note_classif  = Notes_Classif_Code, 
					note_source = Notes_Source_Code, 
					note_indicator = Notes_Indicator_Code
					) %>% 
					unite(source, Source_Code, Survey_Id, sep = ':', remove = TRUE) %>% 
					mutate( note_indicator = ifelse(!Currency_Code %in% NA, paste0('T30:',Currency_Code, '_',note_indicator), note_indicator), 
							note_indicator = ifelse(stringr::str_sub(note_indicator, -1,-1)%in% '_', stringr::str_sub(note_indicator, 1,-2), note_indicator), 
							obs_value = as.numeric(obs_value)) %>% 
					select(-Currency_Code, -Freq_Code) %>% select(-dplyr::contains('_Version_')) %>% select(-dplyr::contains('_Code'))

					
					

# readr:::write_csv(X,paste0("./ON_ORACLE_To_Upload_By_Group/","REV","_","EUROSTAT","_",ref_file[i],"_",Sys.Date(),".csv"), na='')					

saveRDS(X,file = paste0("./Output/","QUERY","_","EUROSTAT","_",ref_file[i],".rds"), compress = 'xz')

print(paste0(i, " : ", ref_file[i], " : ", nrow(X)))

rm(X)




rm( EUROSTAT)


}


###################
################### STOP here for STI
###################
###################
###################

#Load_Mode %in% 'COL',

###################################### DELETION

require(Ariane)
CODE_ORA <- Ariane:::CODE_ORA
QUERY <- paste0("	mutate_each(funs(as.character), -Value) %>%
			filter(	Collection_Code %in% c('YI'),
					
					Survey_Id %in% c('536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256')
			)") %>% gsub('\n', ' ', .)

			
ref 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")

KEY_ORACLE <- as_data_frame(t(ref))
colnames(KEY_ORACLE) <- KEY_ORACLE[1,]
	

init_ <- getwd()
for (i in 1:length(ref_file)){

X <- Ariane:::collectOracle(QUERY, Country = ref_file[i], Internal = TRUE)
save(X, file  = paste("./baskup_YI/",ref_file[i],".rdata",sep=""))
rm(X)


setwd('H:/')	
ORA  <-	KEY_ORACLE %>% 
			slice(-1) %>% 
			bind_rows(	Ariane:::collectOracle(QUERY, Country = ref_file[i], Internal = TRUE) %>%  
							select(Country_Code:Classif2_Version_Code, Time) %>% 
							mutate_all(funs(as.character)) %>% 
							rename( Survey_Code  = Survey_Id
										) %>% select(-Source_Code)) %>% 
			mutate(Lang  = 'EN') %>% 
			distinct_(.dots = ref, .keep_all = TRUE)

setwd(init_)
write.csv(ORA,paste("./ON_ORACLE_To_Upload_By_Group/","DEL","_","EUROSTAT","_",ref_file[i],"_",Sys.Date(),".csv",sep=""),row.names = FALSE,na='')
			
}		

	




rm(list=ls(all=TRUE)) 
q(save = "no", status = 0, runLast = FALSE)










require(ilo)
init_ilo()
 OLD <- get_ilo(collection = 'YI', info, query = "filter(indicator %in% 'POP_XWAP_SEX_GEO_NB')") %>% filter(substr(info,5,7) %in% 'COL', !source %in% EUROSTAT)
 
 NEW <- get_ilo(collection = 'YI', info, query = "filter(indicator %in% 'EAP_TEAP_SEX_AGE_EDU_NB')") %>% filter(substr(info,5,7) %in% 'COL', !source %in% EUROSTAT)

 OLD <- OLD %>% mutate(ID = paste0(geo, source,  time))
 
 NEW <- NEW %>% mutate(ID = paste0(geo, source,  time))
 
 
 OLD <- OLD %>% filter(!ID %in% NEW$ID) %>% select(-ID) 
 


 EUROSTAT <- paste0('BA:', c('536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256'))

 NEW <- get_ilo(collection = 'YI', info, query = "filter(indicator %in% 'EAP_TEAP_SEX_AGE_EDU_NB')") %>% filter(!source %in% EUROSTAT)

 
TEST5 <- NEW %>% filter(substr(classif1,1,12) %in% 'AGE_5YRBANDS') %>%  mutate(ID = paste0(country, source,  time)) %>% filter(substr(info,5,7) %in% 'COL')


TEST10 <- NEW %>% filter(substr(classif1,1,13) %in% 'AGE_10YRBANDS') %>%  mutate(ID = paste0(geo, source,  time))

TEST5 %>% filter(ID %in% unique(TEST10$ID))
REF <- TEST10 %>% filter(ID %in% unique(TEST5$ID))%>% filter(substr(info,5,7) %in% 'CAL')




TESTYI <- c(
'EES_TEES_ECO_OCU_NB', 
'EES_TEES_SEX_ECO2_NB', 
'EES_TEES_SEX_HOW_NB',
'EES_TEES_SEX_OCU_NB',
'EES_TEES_SEX_OCU2_NB',
'EIP_NEET_SEX_NB', 
'EMP_TEMP_ECO_OCU_NB',
'EMP_TEMP_SEX_ECO2_NB', 
'EMP_TEMP_SEX_HOW_NB',   
'EMP_TEMP_SEX_OCU2_NB',    
'EMP_TEMP_SEX_STE_NB',
'HOW_TEMP_SEX_ECO2_NB',
'HOW_TEMP_SEX_OCU_NB',     
'HOW_TEMP_SEX_OCU2_NB',    
'HOW_XEES_SEX_ECO2_NB',
'HOW_XEES_SEX_OCU_NB',
'HOW_XEES_SEX_OCU2_NB',    
'POP_XWAP_SEX_AGE_EDU_NB',
'TRU_TTRU_SEX_ECO_NB', '
UNE_TUNE_SEX_AGE_DUR_NB', 
'UNE_TUNE_SEX_CAT_NB',
'UNE_TUNE_SEX_ECO_NB',
'UNE_TUNE_SEX_OCU_NB'
)

 EUROSTAT <- paste0('BA:', c('536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256'))

 
 
 
 
 
 
 
 
 
 

setwd("D:/_MIG_EUROSTAT")

# source(paste("I:/COMMON/A0 Short term indicators/Processing/R_Functions/0_My_function.R", sep=""))
		
require(plyr)
require(dplyr)
require(readr)
require(tidyr)
require(stringr)
	
  
CALC 			<- readxl:::read_excel("ReadME.xlsx", sheet="CALCULATION")        

 
 
Y_CL2_LEVEL1 <- X %>% mutate_if(is.factor, funs(as.character)) %>%
		filter(!substr(indicator,1,3)%in%"HOW", 
				!substr(indicator,(nchar(indicator)-1),nchar(indicator))%in%"RT"	) %>%
		filter(classif2%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,classif2 = L1, REF = L2), by="classif2") %>%
		select(-classif2) %>%
		rename(classif2 = REF) %>%
		group_by(country, indicator, source, sex, classif1, classif2, time) %>% 
		summarise(	value = sum(as.numeric(value), na.rm = TRUE), 
				note_source = first(note_source),
				note_indicator = first(note_indicator),
				note_classif = first(note_classif),
				note_value = first(note_value),
				test = n())%>%
		ungroup() %>%
		mutate(note_value = ifelse(test>1,NA,note_value)) %>%
		select(-test)

 
 
 
 
 
Y_CL1_LEVEL1 <- X %>% mutate_if(is.factor, funs(as.character)) %>%
		filter( !substr(indicator,1,3)%in%"HOW", 
				!substr(indicator,(nchar(indicator)-1),nchar(indicator))%in%"RT") %>%
		filter(classif1%in%CALC$L1) %>% # eliminate non mapped data (ie. SEX_ECO2)
		left_join(select(CALC,classif1 = L1, REF = L2), by="classif1") %>%
		select(-classif1) %>%
		rename(classif1 = REF) %>%
		group_by(collection, country, indicator, source, sex, classif1, classif2, time) %>% 
		summarise(	value = sum(as.numeric(value), na.rm = TRUE),
				note_value = first(note_value),
				note_classif = first(note_classif),
				note_indicator = first(note_indicator),
				note_source = first(note_source),    
				test = n())%>%
		ungroup() %>%
		mutate(note_value = ifelse(test>1,NA,note_value)) %>%
		select(-test)
		
 
save_ilo(Y_CL1_LEVEL1, format = 'CSV') 
 
 
 
 
 
 
 
 
 
 
 
 
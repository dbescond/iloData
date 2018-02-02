#############################################################################################
#
	# Workspace empty
# load the repository name	# call user-defined function 


setwd(paste0(ilo:::path$data, '/REP_ILO/STI_QUARTERLY_CPI/'))


col1 <- readxl:::read_excel("./input/CPI_Monthly_Change_STI.xlsx", sheet  ="CPI_MCPI_COI_RT") %>% ncol
col2 <- readxl:::read_excel("./input/CPI_Monthly_Change_STI.xlsx", sheet  ="CPI_ACPI_COI_RT") %>% ncol



X <- bind_rows(	readxl:::read_excel("./Input/CPI_Monthly_Change_STI.xlsx", sheet  ="CPI_MCPI_COI_RT", col_type = rep('text', col1)) %>% as.tbl %>% mutate_all(funs(as.character) ),
				readxl:::read_excel("./Input/CPI_Monthly_Change_STI.xlsx", sheet  ="CPI_ACPI_COI_RT", col_type = rep('text', col2))%>% as.tbl %>% mutate_all(funs(as.character))
				)  %>% select(Year, M01, M02, M03, M04, M05, M06, M07, M08, M09, M10, M11, M12, Y, contains('_Code'))		
		
		
		
CODE <- Ariane:::CODE_ORA

X <- X %>% left_join(select(CODE$T_NTE_NOTE, Notes_Geo_Code = NTE_ID, Notes_Geo_type = NTE_TYPE_CODE)%>% mutate_all(funs(as.character)), by = 'Notes_Geo_Code')
X <- X %>% left_join(select(CODE$T_NTE_NOTE, Notes_Pop_Code = NTE_ID, Notes_Pop_type = NTE_TYPE_CODE)%>% mutate_all(funs(as.character)), by = 'Notes_Pop_Code')
X <- X %>% left_join(select(CODE$T_NTE_NOTE, Notes_Group_Code = NTE_ID, Notes_Grp_type = NTE_TYPE_CODE)%>% mutate_all(funs(as.character)), by = 'Notes_Group_Code')
X <- X %>% left_join(select(CODE$T_NTE_NOTE, Notes_Coverage_Code = NTE_ID, Notes_Cov_type = NTE_TYPE_CODE)%>% mutate_all(funs(as.character)), by = 'Notes_Coverage_Code')
	
X <- X %>% mutate(	Notes_Classif_Code = as.character(NA), 
					Notes_Classif_Code = ifelse(Notes_Group_Code %in% NA, Notes_Classif_Code, paste0(Notes_Classif_Code, '_', Notes_Grp_type, ':', Notes_Group_Code)),
					Notes_Classif_Code = Notes_Classif_Code %>% stringr::str_replace_all('NA_', ''), 
					Notes_Indicator_Code = as.character(NA),
					Notes_Indicator_Code = ifelse(Notes_Coverage_Code %in% NA, Notes_Indicator_Code, paste0(Notes_Indicator_Code, '_', Notes_Cov_type, ':', Notes_Coverage_Code)),
					Notes_Indicator_Code = Notes_Indicator_Code %>% stringr::str_replace_all('NA_', ''), 
					Notes_Indicator_Code = ifelse(Notes_Pop_Code %in% NA, Notes_Indicator_Code, paste0(Notes_Indicator_Code, '_', Notes_Pop_type, ':', Notes_Pop_Code)),
					Notes_Indicator_Code = Notes_Indicator_Code %>% stringr::str_replace_all('NA_', ''), 
					Notes_Indicator_Code = ifelse(Notes_Geo_Code %in% NA, Notes_Indicator_Code, paste0(Notes_Indicator_Code, '_', Notes_Geo_type, ':', Notes_Geo_Code)),
					Notes_Indicator_Code = Notes_Indicator_Code %>% stringr::str_replace_all('NA_', '')
					
					
				) %>% select(-Notes_Coverage_Code, -Notes_Cov_type, -Notes_Group_Code, -Notes_Grp_type, -Notes_Pop_Code, -Notes_Pop_type, -Notes_Geo_Code, -Notes_Geo_type) %>%
			mutate(
					Notes_Source_Code = as.character(NA),
					Classif2_Code = as.character(NA),
					Sex_Code = 	as.character(NA), 
					Value_Status_Code = as.character(NA), 
					Currency_Code = as.character(NA)
			)

	

	
	
require(tidyr)	
	
CPI <- X %>%	unite_("ID", c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code", "Year", "Value_Status_Code", "Freq_Code", "Notes_Classif_Code", "Notes_Indicator_Code", "Notes_Source_Code", "Currency_Code"), sep="/", remove = TRUE) %>% 
		gather(Time, Value, -ID) %>% mutate(Time = as.character(Time)) %>%
		separate(ID, c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code", "Year", "Value_Status_Code", "Freq_Code", "Notes_Classif_Code", "Notes_Indicator_Code", "Notes_Source_Code", "Currency_Code"), sep="/", remove = TRUE) %>% 
		mutate_all(funs(plyr::mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) %>%
		filter(!Value%in%c(NA,"","NA") | !Value_Status_Code%in%c("",NA,"NA")) %>%
		mutate(	Time = ifelse(!Time%in%"Y",paste0(Year,Time),Year),
				Value = as.numeric(Value)) %>%		
		select(-Year) %>% 
		filter(!Value%in%c(NA,"","NA") | !Value_Status_Code%in%c("",NA,"NA")) %>% 
		mutate(	Add_Repository = as.character(NA), 	Add_Status = as.character(NA))

CPI <- CPI %>% mutate(Classif1_Code = ifelse(str_sub(Time, 5, 5) %in% 'M', gsub('COI_COMPONENT_', 'COI_COMPO_', Classif1_Code), Classif1_Code)) %>%	
			rename(	ref_area = Country_Code, 
					source = Source_Code, 
					indicator = Indicator_Code, 
					sex = Sex_Code, 
					classif1 = Classif1_Code, 
					classif2 = Classif2_Code, 
					obs_status = Value_Status_Code, 
					freq_code = Freq_Code, 
					note_classif = Notes_Classif_Code, 
					note_indicator = Notes_Indicator_Code, 
					note_source = Notes_Source_Code, 
					time = Time, 
					obs_value = Value) %>% 
			select(-Add_Repository, -Add_Status, -Currency_Code) %>% 
			mutate(	note_source = 'R1:3902', 
					collection = 'STI')   %>% # add note Manual Data entry
			select_(.dots = c("collection", "ref_area", "indicator", "source", "sex", "classif1", "classif2", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source", "freq_code")) %>% 
			filter(!(indicator %in% 'CPI_MCPI_RT' & str_sub(time, 5,5) %in% ''))
		
		
		
rm(X)		
invisible(gc(reset = TRUE))	
	
		

REF <- levels(as.factor(CPI$ref_area))
 
# split and save by ref_area
for (i in 1:length(REF)){
X <- CPI[CPI$ref_area%in%REF[i],]
save(X,file = paste("./output/REP_CPI_",REF[i],".Rdata",sep=""))
print(REF[i])
}


REF <- cbind(PATH = paste(getwd(), "/output/REP_CPI_",REF,".Rdata",sep=""),ID = NA, Types  ="NSO_ilostat", REF = NA)
write.csv(REF,"FileToLoad.csv",row.names = FALSE,na="")













		
rm(list=ls(all=TRUE)) 
q(save = "no", status = 0, runLast = FALSE)
#############################################################################################
#
	# Workspace empty

Ref_Country <- ReadMeSource %>% separate(ID,c("Country","Source"))  %>% select(Country) ; Ref_Country <- as.vector(c(Ref_Country$Country))
Ref_Survey <- ReadMeSource %>% separate(REF,c("Source","Survey"))  %>% select(Survey) ; Ref_Survey <- as.vector(c(Ref_Survey$Survey))


parameter <- list()

parameter$ref_area <- Ref_Country
parameter$source <- ReadMeSource$REF
parameter$classif1 <- c('AGE_AGGREGATE_Y15-24','AGE_AGGREGATE_Y25-54','AGE_YTHADULT_Y15-64','AGE_AGGREGATE_Y55-64','ECO_AGGREGATE_AGR','ECO_SECTOR_IND','ECO_AGGREGATE_MAN','ECO_AGGREGATE_CON','ECO_SECTOR_SER')
parameter$indicator <- c('POP_XWAP_SEX_AGE_NB','EAP_TEAP_SEX_AGE_NB','EAP_DWAP_SEX_AGE_RT','EMP_TEMP_SEX_AGE_NB','EMP_TEMP_SEX_ECO_NB','EMP_DWAP_SEX_AGE_RT','EES_TEES_SEX_ECO_NB','UNE_TUNE_SEX_AGE_NB','UNE_DEAP_SEX_AGE_RT','EIP_TEIP_SEX_AGE_NB','EIP_DWAP_SEX_AGE_RT')


require(ilo)
init_ilo()
X <- get_ilo(collection = 'STI', freq  ='Q', query = "filter(ref_area %in% parameter$ref_area, source %in% parameter$source, indicator %in% parameter$indicator, classif1 %in% parameter$classif1)", add = parameter, time = '2013Q')
close_ilo()										
			
colname <- colnames(X)

ref_freq <- "([I][1][2])[-:.]([0-9]+)" 

X <- 	X %>% mutate(OLD_FREQ = str_extract(note_indicator, ref_freq)) %>%
				left_join(select(Ariane:::CODE_ORA$T_FRQ_FREQUENCY, OLD_FREQ = NEW_CODE_ORACLE, Freq_Code = FRQ_CODE), by="OLD_FREQ") %>% 
				mutate(note_indicator = str_replace(note_indicator, OLD_FREQ, ""), 
							note_indicator = ifelse(substr(note_indicator,1,1)%in%"_", substr(note_indicator,2,nchar(note_indicator)), note_indicator ),
							note_indicator = ifelse(substr(note_indicator,nchar(note_indicator),nchar(note_indicator))%in%"_", substr(note_indicator,1,nchar(note_indicator)-1), note_indicator )
						) %>% 
				select(-OLD_FREQ) %>% 
				as.tbl %>% 
				select_( .dots = c(colname, 'Freq_Code')) %>%
				mutate(note_indicator = ifelse(note_indicator %in% "", NA, note_indicator))




MY_MATRIX <- X %>% 	arrange(desc(time)) %>%
			group_by(ref_area, indicator, source, sex, classif1, classif2) %>%
			summarise(	Freq_Code = first(Freq_Code), 
						note_classif = first(note_classif),
						note_indicator = first(note_indicator),
						note_source = first(note_source)) %>%
			ungroup() %>%
			unite(ID,ref_area,source,indicator,sex,classif1, classif2,sep = "/", remove = TRUE) %>%
			mutate(ID = gsub("/NA", "/XXX_XXX_XXX",ID, fixed =TRUE)) %>%
			rename(	NOTES_FREQUENCY_CODE = Freq_Code, 
					NOTES_CLASSIF_CODE = note_classif,
					NOTES_INDICATOR_CODE = note_indicator,
					NOTES_SOURCE_CODE = note_source) %>%
			select(ID,NOTES_FREQUENCY_CODE,NOTES_CLASSIF_CODE, NOTES_INDICATOR_CODE, NOTES_SOURCE_CODE)

MY_MATRIX <- MY_MATRIX %>% mutate(	NOTES_SOURCE_CODE = gsub('R1:2382_','',NOTES_SOURCE_CODE, fixed = TRUE), 
									NOTES_SOURCE_CODE = gsub('R1:3513_','',NOTES_SOURCE_CODE, fixed = TRUE))
			
			
save(MY_MATRIX, file = "./input/META_OECD_BA_SA.Rdata")
			
rm(MY_MATRIX, parameter, X, colname, Ref_Country, ref_freq, Ref_Survey)			

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
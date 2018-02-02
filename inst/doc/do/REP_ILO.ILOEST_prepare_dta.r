
require(ilo)
init_ilo()

region <- Rilostat:::get_ilostat_toc(segment = 'ref_area') %>% 
		distinct(ref_area,  wb_income_group.label, ilo_region.label, ilo_subregion_broad.label, ilo_subregion_detailled.label) %>% 
		rename(`World Bank Income Group` = wb_income_group.label, 
		ilo_subregion_detailed.label  = ilo_subregion_detailled.label)


setwd('H:/')


######################### Table 1 by age
{
X <- get_ilo(collection = 'ILOEST', indicator = '_SEX_AGE_NB|_SEX_AGE_RT', classif1 = 'YTHADULT', timefrom = '1991', timeto = '2022') %>% filter(!classif1 %in% 'AGE_YTHADULT_Y15-64', !str_detect(indicator, 'EIP_'))


Y <- X %>% switch_ilo(keep) %>% 
		select(ref_area, ref_area.label, indicator.label, sex.label, classif1.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and age -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by sex and age -- UN estimates and projections, July 2017', '', indicator.label), 
				indicator.label = gsub(' by sex and age -- ILO modelled estimates, July 2017', '', indicator.label), 
				indicator.label = gsub(' by sex and age -- ILO modelled estimates, July 2015', '', indicator.label), 
				classif1.label = gsub('Youth and adults: ', '', classif1.label), 
				sex.label = gsub('Sex: ', '', sex.label), 
			) %>% 
		mutate(TEST_SEX = sex.label %>% plyr:::mapvalues(from = c("Female","Male", "Total"), to = c(3, 2, 1))) %>% 
		mutate(TEST_CLA = classif1.label %>% plyr:::mapvalues(from = c("15-24", "15+", "25+"), to = c(2, 1, 3))) %>% 
		spread(indicator.label, obs_value) %>% 
		arrange(ref_area.label,time, TEST_SEX, TEST_CLA ) %>% 
		select(`ref_area`, `ref_area.label`, `sex.label`, `classif1.label`, `time`, `Population`, `Labour force`, `Employment`, `Unemployment`, `Labour force participation rate`, `Employment-to-population ratio`, `Unemployment rate`) 
		



COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group` , `Reference area` = `ref_area.label`, Sex = `sex.label`, Age = `classif1.label`, Year = `time`, 
			`Population (thousands)` = `Population`, 
				`Labour force (thousands)` = `Labour force`, 
				`Employment (thousands)` = `Employment`, 
				`Unemployment (thousands)` = `Unemployment`, 
				`Labour force participation rate (%)` = `Labour force participation rate`, 
				`Employment-to-population ratio (%)` = `Employment-to-population ratio`, 
				`Unemployment rate (%)`  = `Unemployment rate`) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`, Sex = `sex.label`, Age = `classif1.label`, Year = `time`, 
				`Population (thousands)` = `Population`, 
				`Labour force (thousands)` = `Labour force`, 
				`Employment (thousands)` = `Employment`, 
				`Unemployment (thousands)` = `Unemployment`, 
				`Labour force participation rate (%)` = `Labour force participation rate`, 
				`Employment-to-population ratio (%)` = `Employment-to-population ratio`, 
				`Unemployment rate (%)`  = `Unemployment rate`) 
					
		
 ### save_ilo(COUNTRY, AGGREGATE)

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_LMS') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}

 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						age = age_group, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>% haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_LMS_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_LMS') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}

AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:UR) %>% 
		rename(age = age_group, time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE		%>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_LMS_aggregate.dta'))




print(paste0('TEM_LMS : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))

rm(COUNTRY, AGGREGATE)				
}

		
######################### Table 2 by employment status		
{		
X <- get_ilo(collection = 'ILOEST', indicator = '_SEX_STE_NB|_SEX_STE_DT', classif1 = 'STE_ICSE93', timefrom = '1991', timeto = '2022') 

Y <- X %>% switch_ilo(keep) %>% select(ref_area, ref_area.label, indicator.label, sex.label, classif1.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and status in employment -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by status in employment (by sex) -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				classif1.label = str_sub(classif1.label,71,-1), 
				sex.label = gsub('Sex: ', '', sex.label), 
			) %>% 
		mutate(TEST_SEX = sex.label %>% plyr:::mapvalues(from = c("Female","Male", "Total"), to = c(3, 2, 1))) %>% 
		mutate(classif1.label = ifelse(str_detect(indicator.label, 'distribution'), paste0(classif1.label, ' (%)'), paste0(classif1.label, " (thousands)"))) %>% 
		filter(!classif1.label %in% 'Total (%)') %>% 
		select(-indicator.label) %>%
		spread(classif1.label, obs_value) %>% 
		arrange(ref_area.label,time, TEST_SEX ) %>% 
		select(`ref_area`,`ref_area.label`, `sex.label`,  `time`, `Total (thousands)`,                      
`Employees (%)`,                          
`Employers (%)`,                          
`Own-account workers (%)`,                
`Contributing family workers (%)`, 
`Employees (thousands)`,                  
`Employers (thousands)`,                  
`Own-account workers (thousands)`,        
`Contributing family workers (thousands)`) 
 	
                                            
		
	COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group`, `Reference area` = `ref_area.label`, Sex = `sex.label`,Year = `time`, 
			`Total employment (thousands)` = `Total (thousands)`,                      
`Employees (thousands)`,                  
`Employers (thousands)`,                  
`Own-account workers (thousands)`,        
`Contributing family workers (thousands)`, 
`Employees (%)`,                          
`Employers (%)`,                          
`Own-account workers (%)`,                
`Contributing family workers (%)`		) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
			
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`, Sex = `sex.label`, Year = `time`,
`Total employment (thousands)` = `Total (thousands)`,                          
`Employees (thousands)`,                  
`Employers (thousands)`,                  
`Own-account workers (thousands)`,        
`Contributing family workers (thousands)`, 
`Employees (%)`,                          
`Employers (%)`,                          
`Own-account workers (%)`,                
`Contributing family workers (%)`)		
		
		
		
 ### save_ilo(COUNTRY, AGGREGATE)
	

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_STE') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}


 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>% haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_STE_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_STE') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}

AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:UFWP) %>% 
		rename( time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>%	haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_STE_aggregate.dta'))
	
print(paste0('TEM_STE : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)	
}		
		
	
######################### Table 3 by economic activity
{
X <- get_ilo(collection = 'ILOEST', indicator = '_SEX_ECO_NB|_SEX_ECO_DT', classif1 = 'DETAILS', timefrom = '1991', timeto = '2022') 

Y <- X %>% switch_ilo(keep) %>% select(ref_area, ref_area.label, indicator.label, sex.label, classif1.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and economic activity -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by economic activity (by sex) -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				classif1.label = gsub('Detailed categories of economic activity: ', '', classif1.label), 
				sex.label = gsub('Sex: ', '', sex.label), 
			) %>% 
		mutate(TEST_SEX = sex.label %>% plyr:::mapvalues(from = c("Female","Male", "Total"), to = c(3, 2, 1))) %>% 
		mutate(classif1.label = ifelse(str_detect(indicator.label, 'distribution'), paste0(classif1.label, ' (%)'), paste0(classif1.label, " (thousands)"))) %>% 
		filter(!classif1.label %in% 'Total (%)') %>% 
		select(-indicator.label) %>%
		spread(classif1.label, obs_value) %>% 
		arrange(ref_area.label,time, TEST_SEX ) %>% 
		select(`ref_area`,`ref_area.label`, `sex.label`,  `time`, `Total (thousands)`,     
`Agriculture; forestry and fishing ~ISIC rev.4 A (thousands)`,                                   
`Mining and quarrying ~ISIC rev.4 B (thousands)`,
`Manufacturing ~ISIC rev.4 C (thousands)`,                                                       
`Utilities ~ISIC rev.4 D; E (thousands)`,                                                         
`Construction ~ISIC rev.4 F (thousands)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (thousands)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (thousands)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (thousands)`,                            
`Financial and insurance activities ~ISIC rev.4 K (thousands)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (thousands)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (thousands)`,       
`Education ~ISIC rev.4 P (thousands)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (thousands)`,                             
`Other services ~ISIC rev.4 R; S; T; U (thousands)`, 
`Agriculture; forestry and fishing ~ISIC rev.4 A (%)`,                                   
`Mining and quarrying ~ISIC rev.4 B (%)`,
`Manufacturing ~ISIC rev.4 C (%)`,                                                       
`Utilities ~ISIC rev.4 D; E (%)`,                                                         
`Construction ~ISIC rev.4 F (%)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (%)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (%)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (%)`,                            
`Financial and insurance activities ~ISIC rev.4 K (%)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (%)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (%)`,       
`Education ~ISIC rev.4 P (%)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (%)`,                             
`Other services ~ISIC rev.4 R; S; T; U (%)`) 
 	
                                            

		
	COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group` , `Reference area` = `ref_area.label`, Sex = `sex.label`,Year = `time`, 
			`Total employment (thousands)` = `Total (thousands)`,             
`Agriculture; forestry and fishing ~ISIC rev.4 A (thousands)`,                                   
`Mining and quarrying ~ISIC rev.4 B (thousands)`,
`Manufacturing ~ISIC rev.4 C (thousands)`,                                                       
`Utilities ~ISIC rev.4 D; E (thousands)`,                                                         
`Construction ~ISIC rev.4 F (thousands)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (thousands)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (thousands)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (thousands)`,                            
`Financial and insurance activities ~ISIC rev.4 K (thousands)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (thousands)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (thousands)`,       
`Education ~ISIC rev.4 P (thousands)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (thousands)`,                             
`Other services ~ISIC rev.4 R; S; T; U (thousands)`, 
`Agriculture; forestry and fishing ~ISIC rev.4 A (%)`,                                   
`Mining and quarrying ~ISIC rev.4 B (%)`,
`Manufacturing ~ISIC rev.4 C (%)`,                                                       
`Utilities ~ISIC rev.4 D; E (%)`,                                                         
`Construction ~ISIC rev.4 F (%)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (%)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (%)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (%)`,                            
`Financial and insurance activities ~ISIC rev.4 K (%)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (%)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (%)`,       
`Education ~ISIC rev.4 P (%)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (%)`,                             
`Other services ~ISIC rev.4 R; S; T; U (%)`
			) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
			
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`, Sex = `sex.label`, Year = `time`, 
				`Total employment (thousands)` = `Total (thousands)`,         
`Agriculture; forestry and fishing ~ISIC rev.4 A (thousands)`,                                   
`Mining and quarrying ~ISIC rev.4 B (thousands)`,
`Manufacturing ~ISIC rev.4 C (thousands)`,                                                       
`Utilities ~ISIC rev.4 D; E (thousands)`,                                                         
`Construction ~ISIC rev.4 F (thousands)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (thousands)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (thousands)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (thousands)`,                            
`Financial and insurance activities ~ISIC rev.4 K (thousands)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (thousands)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (thousands)`,       
`Education ~ISIC rev.4 P (thousands)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (thousands)`,                             
`Other services ~ISIC rev.4 R; S; T; U (thousands)`, 
`Agriculture; forestry and fishing ~ISIC rev.4 A (%)`,                                   
`Mining and quarrying ~ISIC rev.4 B (%)`,
`Manufacturing ~ISIC rev.4 C (%)`,                                                       
`Utilities ~ISIC rev.4 D; E (%)`,                                                         
`Construction ~ISIC rev.4 F (%)`,                                                        
`Wholesale and retail trade; repair of motor vehicles and motorcycles ~ISIC rev.4 G (%)`, 
`Transport; storage and communication ~ISIC rev.4 H; J (%)`,                             
`Accommodation and food service activities ~ISIC rev.4 I (%)`,                            
`Financial and insurance activities ~ISIC rev.4 K (%)`,                                  
`Real estate; business and administrative activities  ~ISIC rev.4 L; M; N (%)`,           
`Public administration and defence; compulsory social security ~ISIC rev.4 O (%)`,       
`Education ~ISIC rev.4 P (%)`,                                                            
`Human health and social work activities ~ISIC rev.4 Q (%)`,                             
`Other services ~ISIC rev.4 R; S; T; U (%)`) 
					
			
			
 ### save_ilo(COUNTRY, AGGREGATE)
	

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_ECO') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}


 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>%
						haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_ECO_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_ECO') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}


AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:EMP14shP) %>% 
		rename( time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>%	haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_ECO_aggregate.dta'))

print(paste0('TEM_ECO : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)	

}

	
######################### Table 4 by occupation (skill)		
{		
X <- get_ilo(collection = 'ILOEST', indicator = '_SEX_OCU_NB|_SEX_OCU_DT', classif1 = 'SKILL', timefrom = '1991', timeto = '2022') 

Y <- X %>% switch_ilo(keep) %>% select(ref_area, ref_area.label, indicator.label, sex.label, classif1.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and occupation -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by occupation (by sex) -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				classif1.label = gsub('Skill level: ', '', classif1.label), 
				sex.label = gsub('Sex: ', '', sex.label), 
			) %>% 
		mutate(TEST_SEX = sex.label %>% plyr:::mapvalues(from = c("Female","Male", "Total"), to = c(3, 2, 1))) %>% 
		mutate(classif1.label = ifelse(str_detect(indicator.label, 'distribution'), paste0(classif1.label, ' (%)'), paste0(classif1.label, " (thousands)"))) %>% 
		filter(!classif1.label %in% 'Total (%)') %>% 
		select(-indicator.label) %>%
		spread(classif1.label, obs_value) %>% 
		arrange(ref_area.label,time, TEST_SEX ) %>% 
		select(`ref_area`,`ref_area.label`, `sex.label`,  `time`, 
`Total (thousands)`,                        
`Skill levels 3 and 4 (high) (%)`,        
`Skill level 2 (medium) (%)`,             
`Skill level 1 (low) (%)`, 
`Skill levels 3 and 4 (high) (thousands)`,
`Skill level 2 (medium) (thousands)`,     
`Skill level 1 (low) (thousands)` ) 
 	
                                            
		
	COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group`, `Reference area` = `ref_area.label`, Sex = `sex.label`,Year = `time`, 
`Total employment (thousands)` = `Total (thousands)`,                                
`Skill levels 3 and 4 (high) (thousands)`,
`Skill level 2 (medium) (thousands)`,     
`Skill level 1 (low) (thousands)` , 
`Skill levels 3 and 4 (high) (%)`,        
`Skill level 2 (medium) (%)`,             
`Skill level 1 (low) (%)`		) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
			
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`, Sex = `sex.label`, Year = `time`,
`Total employment (thousands)` = `Total (thousands)`,                                  
`Skill levels 3 and 4 (high) (thousands)`,
`Skill level 2 (medium) (thousands)`,     
`Skill level 1 (low) (thousands)`, 
`Skill levels 3 and 4 (high) (%)`,        
`Skill level 2 (medium) (%)`,             
`Skill level 1 (low) (%)` )		
		
		
		
 ### save_ilo(COUNTRY, AGGREGATE)
	

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_SKILL') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}


 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>%
						haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_SKILL_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_SKILL') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}

AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:EMPoc3P) %>% 
		rename( time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>%	haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_SKILL_aggregate.dta'))
	

print(paste0('TEM_SKILL : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)	
	
}		
		
			
######################### Table 5 by class of worker
{
X <- get_ilo(collection = 'ILOEST', indicator = 'EMP_2EMP_SEX_AGE_CLA', classif1 = 'YTHADULT', timefrom = '1991', timeto = '2022') %>% filter(!classif1 %in% 'AGE_YTHADULT_Y15-64')


Y <- X %>% switch_ilo(keep) %>% 
		select(ref_area, ref_area.label, indicator.label, sex.label, classif1.label, classif2.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex, age and economic class -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by economic class (by sex and age) -- ILO modelled estimates, Nov. 2017', '', indicator.label, fixed = TRUE),  
				classif1.label = gsub('Youth and adults: ', '', classif1.label), 
				classif2.label = gsub('Economic class: ', '', classif2.label), 
				sex.label = gsub('Sex: ', '', sex.label), 
			) %>% 
		mutate(TEST_SEX = sex.label %>% plyr:::mapvalues(from = c("Female","Male", "Total"), to = c(3, 2, 1))) %>% 
		mutate(TEST_CLA = classif1.label %>% plyr:::mapvalues(from = c("15-24", "15+", "25+"), to = c(2, 1, 3))) %>% 
		mutate(classif2.label = ifelse(str_detect(indicator.label, 'distribution'), paste0(classif2.label, ' (%)'), paste0(classif2.label, " (thousands)"))) %>% 
		filter(!classif2.label %in% 'Total (%)') %>% 
		select(-indicator.label) %>%
		spread(classif2.label, obs_value) %>% 
		arrange(ref_area.label,time, TEST_SEX, TEST_CLA ) %>% 
		select(`ref_area`, `ref_area.label`, `sex.label`, `classif1.label`, `time`,  
`Total (thousands)`,                                      
`Extremely poor (<US$1.90, PPP) (%)`,                     
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (%)`,        
`Near poor (>=US$3.10 & <US$5, PPP) (%)`,     
`>=US$3.10, PPP (%)`,            
`>=US$5, PPP (%)`,                                        
`Extremely poor (<US$1.90, PPP) (thousands)`,             
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (thousands)`,
`Near poor (>=US$3.10 & <US$5, PPP) (thousands)`,         
`>=US$3.10, PPP (thousands)`,                             
`>=US$5, PPP (thousands)`       )

COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group` , `Reference area` = `ref_area.label`, Sex = `sex.label`, Age = `classif1.label`, Year = `time`, 
`Total employment (thousands)` = `Total (thousands)`,                                      
`Extremely poor (<US$1.90, PPP) (thousands)`,             
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (thousands)`,
`Near poor (>=US$3.10 & <US$5, PPP) (thousands)`,                                      
`>=US$5, PPP (thousands)`, 
`Extremely poor (<US$1.90, PPP) (%)`,                     
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (%)`,        
`Near poor (>=US$3.10 & <US$5, PPP) (%)`,             
`>=US$5, PPP (%)` ) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`, Sex = `sex.label`, Age = `classif1.label`, Year = `time`, 
`Total employment (thousands)` = `Total (thousands)`,                                                                                
`Extremely poor (<US$1.90, PPP) (thousands)`,             
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (thousands)`,       
`>=US$3.10, PPP (thousands)`, 
`Extremely poor (<US$1.90, PPP) (%)`,                     
`Moderately poor (>=US$1.90 & <US$3.10, PPP) (%)`,          
`>=US$3.10, PPP (%)`) 
					
		
 ### save_ilo(COUNTRY, AGGREGATE)

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_CLA') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}
 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						age = age_group,
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>%
						haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_CLA_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_CLA') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}
AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:class414P) %>% 
		rename( time  =year, age = age_group) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_CLA_aggregate.dta'))





print(paste0('TEM_CLA : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)			
}

######################### Table 6 productivity
			
{		
X <- get_ilo(collection = 'ILOEST', indicator = 'GDP_2', timefrom = '1991', timeto = '2022')
X2 <- get_ilo(collection = 'ILOEST', indicator = 'EMP_2EMP_SEX_AGE_NB', classif1 = 'AGE_YTHADULT_YGE15', sex = 'T', timefrom = '1991', timeto = '2022')
		
Y <- bind_rows(X, X2)	%>% switch_ilo(keep) %>% 
		select(ref_area, ref_area.label, indicator.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and age -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' -- ILO modelled estimates, Nov. 2017', '', indicator.label) 
			) %>% 
		spread(indicator.label, obs_value) %>% 
		arrange(ref_area.label,time ) %>% 
		select(`ref_area`, `ref_area.label`, `time`, Employment, `Output per worker (GDP constant 2010 US $)`, `Output per worker (GDP constant 2011 international $ in PPP)`) 

	


COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group` , `Reference area` = `ref_area.label`, Year = `time`, 
			`Total employment (thousands)` = Employment, `Output per worker (GDP constant 2010 US $)`, `Output per worker (GDP constant 2011 international $ in PPP)`) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`,  Year = `time`, 
				`Total employment (thousands)` = Employment, `Output per worker (GDP constant 2010 US $)`, `Output per worker (GDP constant 2011 international $ in PPP)`) 

	
 ### save_ilo(COUNTRY, AGGREGATE)

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_PRODY') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}
 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>%
						haven:::write_dta( paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_PRODY_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_PRODY') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}
AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:GDPPP) %>% 
		rename( time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_PRODY_aggregate.dta'))






print(paste0('TEM_PRODY : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)

				
}				
		
		
######################### Table 7 labour dependancy ratio
			
{	




	
X <- get_ilo(collection = 'ILOEST', indicator = 'POP_2LDR_NOC_RT', timefrom = '1991', timeto = '2022')
X1 <- get_ilo(collection = 'ILOEST', indicator = 'EMP_2EMP_SEX_AGE_NB', classif1 = 'AGE_YTHADULT_YGE15', sex = 'T', timefrom = '1991', timeto = '2022')
X2 <- get_ilo(collection = 'ILOEST', indicator = 'POP_2POP_SEX_AGE_NB', classif1 = 'AGE_5YRBANDS_TOTAL', sex = 'T', timefrom = '1991', timeto = '2022')
		
Y <- bind_rows(X, X1, X2)	%>% switch_ilo(keep) %>% 
		select(ref_area, ref_area.label, indicator.label, time, obs_value) %>% 
		mutate(
				indicator.label = gsub(' by sex and age -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' -- ILO modelled estimates, Nov. 2017', '', indicator.label), 
				indicator.label = gsub(' by sex and age -- UN estimates and projections, July 2017', '', indicator.label), 
				indicator.label = gsub(' -- ILO modelled estimates, November 2017', '', indicator.label) 
			) %>% 
		spread(indicator.label, obs_value) %>% 
		arrange(ref_area.label,time ) %>% 
		select(`ref_area`, `ref_area.label`, `time`, `Total employment (thousands)` = Employment, `Total not in employment (thousands)` = Population, `Labour dependency ratio`) %>% 
		mutate(`Total not in employment (thousands)` = `Total not in employment (thousands)` - `Total employment (thousands)`)

	


COUNTRY <- left_join(
				Y %>% filter(!str_sub(ref_area ,1,1) %in% 'X'), 
				region, by = 'ref_area') %>% 
			select(`ISO 3 Code` = `ref_area`, Region = ilo_region.label, `Sub-region broad` = ilo_subregion_broad.label, `Sub-region detailed` = ilo_subregion_detailed.label, `World Bank Income Group` , `Reference area` = `ref_area.label`, Year = `time`, 
			`Total employment (thousands)`, `Total not in employment (thousands)`, `Labour dependency ratio`) %>%
			mutate(`World Bank Income Group` = gsub('World: ', '', `World Bank Income Group`))
			
AGGREGATE <- Y %>% 
				filter(str_sub(ref_area ,1,1) %in% 'X') %>%
				arrange(ref_area) %>%
				select(`Reference area` = `ref_area.label`,  Year = `time`, 
				`Total employment (thousands)`, `Total not in employment (thousands)`, `Labour dependency ratio`) 

	
 ### save_ilo(COUNTRY, AGGREGATE)

refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_LDR') %>% filter(value %in% colnames(COUNTRY))

for (i in 1:nrow(refcol)){

attributes(COUNTRY[[i]])$label <- refcol$value[i]
colnames(COUNTRY)[i] <- refcol$code[i]

}

 COUNTRY %>% rename(	ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed,
						wb_income_grouplabel = income_group) %>%
haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_LDR_ref_area.dta'))



refcol <- readxl:::read_excel( paste0(ilo:::path$data,'REP_ILO/ILOEST/help/ref_label.xlsx'), 'TEM_LDR') %>% filter(value %in% colnames(AGGREGATE))

for (i in 1:nrow(refcol)){

attributes(AGGREGATE[[i]])$label <- refcol$value[i]
colnames(AGGREGATE)[i] <- refcol$code[i]

}

AGGREGATE <- AGGREGATE %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:LDR) %>% 
		rename( time  =year) 

attributes(AGGREGATE$ref_area)$label <- 'ISO 3 Code'
		
AGGREGATE %>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/TEM_LDR_aggregate.dta'))






print(paste0('TEM_LDR : ', nrow(COUNTRY), ' / ', nrow(AGGREGATE)))
		
rm(COUNTRY, AGGREGATE)


				
}				
		
				
		
		
		
{ ####### reshape LFEP

require(haven)

X <- haven:::read_dta("C:\\Users\\bescond\\Desktop\\TEST\\LFEP_ref_area.dta") 



Y <-  X %>% rename(ref_area = 'iso3code', 
						time = year, 
						ilo_regionlabel = region, 
						ilo_subregion_broadlabel = subregionbroad, 
						ilo_subregion_detailed = subregiondetailed, 
						age = age_group) %>% 
			mutate(
				ilo_regionlabel = as_factor(ilo_regionlabel) %>% as.character,
				ilo_subregion_broadlabel = as_factor(ilo_subregion_broadlabel) %>% as.character,
				ilo_subregion_detailed = as_factor(ilo_subregion_detailed) %>% as.character
			)

attributes(Y$ref_area)$label <- 'ISO 3 Code'
attributes(Y$ilo_regionlabel)$label <- 'Region'
attributes(Y$ilo_subregion_broadlabel)$label <- 'Sub-region broad'
attributes(Y$ilo_subregion_detailed)$label <- 'Sub-region detailed'

Y 	%>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/LFEP_ref_area.dta'))





X <- haven:::read_dta("C:\\Users\\bescond\\Desktop\\TEST\\LFEP_aggregate.dta") 



X <- X %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:PR_imp_F) %>% 
		rename( time  =year, age = age_group) 

attributes(X$ref_area)$label <- 'ISO 3 Code'
		

X 	%>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/LFEP_aggregate.dta'))






require(haven)

X <- haven:::read_dta("C:\\Users\\bescond\\Desktop\\TEST\\LFEP_MedianAge_LF.dta") 


X <- X %>% left_join(ilo$code$cl_country %>% select(ref_arealabel = label_en, ref_area = code) , by = "ref_arealabel") %>% select(ref_area, ref_arealabel:median_age_LF_F) %>% 
		rename( time  =year) 

attributes(X$ref_area)$label <- 'ISO 3 Code'
		

X 	%>% haven:::write_dta(paste0(ilo:::path$data,'REP_ILO/ILOEST/output/web_files/LFEP_MedianAge_LF.dta'))












}
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

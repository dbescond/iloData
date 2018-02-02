
# with R first install.packages
################ install.packages('Rilostat')
################ install.packages('tidyverse')


require(tidyverse)
require(Rilostat)




ref_indicator = c(	'EAP_TEAP_SEX_AGE_NB', 		# Labour Force by Sex and Age
					'EAP_TEAP_SEX_AGE_GEO_NB',	# Labour Force by Sex Age and Rural/Urban Areas
					'EAP_TEAP_SEX_AGE_EDU_NB',	# Labour Force by Sex, Age and Education
					'EMP_DWAP_SEX_AGE_RT'		# Employment to Population Ratio by Sex and Age
				)

X <- get_ilostat(id = 'CAN_A', segment = 'ref_area', filters = list(indicator = ref_indicator, timefrom = '2000', source = 'BA')) 




X %>% filter(indicator %in% 'EMP_DWAP_SEX_AGE_RT') %>% select(source:obs_value) %>% spread(time, obs_value)



 get_ilo(collection = 'STI', indicator = 'EMP_DWAP_SEX_AGE_GEO_RT', timefrom = '2008', ref_area = 'CAN', classif1 = 'AGGREGATE') %>% select(source:obs_value) %>% spread(time, obs_value) %>% save_ilo()

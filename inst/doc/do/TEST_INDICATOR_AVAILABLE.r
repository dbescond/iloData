
requrie(ilo)

init_ilo()

require(Rilostat)

country <- get_ilostat(id = 'UNE_2EAP_SEX_AGE_RT_A', filters = list(time = '2017', sex = 'SEX_T', classif1 = 'AGE_YTHADULT_YGE15')) %>% distinct(ref_area) %>% filter(!str_sub(ref_area, 1,1) %in% 'X')


region <- get_ilostat_toc(segment = 'ref_area', search = 'Annual') %>% select(ref_area, wb_income_group.label,ilo_region.label,  ilo_subregion_broad.label, ilo_subregion_detailed.label)  %>% filter(!str_sub(ref_area, 1,1) %in% 'X') 


EAP <- get_ilostat(id = 'EAP_2EAP_SEX_AGE_NB_A', filters = list(time = '2017', sex = 'SEX_T', classif1 = 'AGE_YTHADULT_YGE15')) %>% filter(!str_sub(ref_area, 1,1) %in% 'X') %>% select(ref_area, EAP2017 = obs_value)
EAP_income = EAP %>% left_join(region) %>% group_by(wb_income_group.label) %>% summarise(EAP2017_income = sum(EAP2017)) %>% ungroup 


country <- country %>% left_join(region, by = "ref_area")

X <- get_ilostat( id = 'EIP_NEET_SEX_RT_A', detail = 'bestsourceonly') %>% select(ref_area, time)





res <- country %>% left_join(X %>% count(ref_area, time) %>% spread(time, n) , by = "ref_area") %>% left_join(EAP , by = "ref_area") %>% save_ilo()


test <- country %>%  count(wb_income_group.label) %>% rename(new = n)
X %>% left_join(EAP) %>% 
		
			filter(!EAP2017 %in% NA) %>% 
			left_join(region) %>% 
			left_join(EAP_income) %>%
			distinct() %>% 
			group_by(wb_income_group.label, time) %>% 
			summarise(ref_EAP = sum(EAP2017) / first(EAP2017_income ) * 100) %>%
			ungroup() %>% left_join(test) %>% spread(time, ref_EAP)%>% select(-new)save_ilo()
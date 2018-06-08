
##################################################################
##################################################################

########### updating ILOEST database plse revised the path and file name

##################################################################
##################################################################


##################################################################											# OK for APRIL 2017
## POP and LFPR 5yrbands data from LFEP model !!stay stable during 2 year 
file_lfep <- 'J:\\TEM\\LF Model July 2017\\Final Datasets\\Master LFEP.dta'
file_lfep <- 'J:\\TEM\\LF Model July 2017\\Dissemination datasets\\Dissemination LFEP - Country data (1990-2050, including TOTALS).dta'
file_lfep_median <- 'J:\\TEM\\LF Model July 2017\\Dissemination datasets\\MedianAge_LF.dta'


##################################################################											# OK for APRIL 2017
## UR yth adult share of STE data from GET model master file !!update twice a year (OCT, APR) publish 2 (JAN / AUG !!!)
##################################################################
################################################################## PATCH
# file_get_master <- 'J:\\TEM\\GET Model November 2017\\Final Datasets\\Master Final GET database November 2017.dta'
# file_get_master <- 'J:\\TEM\\GET Model November 2017\\Final Datasets\\Master Final GET database November 2017_PATCH2.dta'
file_get_master <- 'J:\\TEM\\GET Model May 2018\\Final Datasets\\Master Final GET database May 2018_Patch.dta'

#file_get_master_patch <- 'J:\\TEM\\GET Model November 2017\\Final Datasets\\Master Final GET database November 2017 Philippines correction.dta'

## ECO details from GET model Extensiom !!update twice a year (OCT, APR) publish 2 (JAN / AUG !!!)			# OK for APRIL 2017
file_get_eco_details <- 'J:\\TEM\\EbySector Model\\Final Datasets\\MASTER Final GME - detailed Sector Database November 2017.dta'
file_get_eco_aggregate <- 'J:\\TEM\\EbySector Model\\Final Datasets\\MASTER Final GME - aggregate Sector Database November 2017.dta'
file_get_eco_sector <- 'J:\\TEM\\EbySector Model\\Final Datasets\\MASTER Final GME - Broad Sector Database November 2017.dta'



##################################################################
## OCU skill from GET model Extensiom !!update twice a year (OCT, APR) publish 2 (JAN / AUG !!!) 			# OK for APRIL 2017
##file_get_ocu_routine <- 'J:\\TEM\\GET Model Extension\\Final Datasets\\MASTER Final GME - Occupation (routine) Database November 2016.dta'
file_get_ocu_skill <- 'J:\\TEM\\GET Model Extension\\Final Datasets\\MASTER Final GME - Occupation (skills) Database November 2016.dta'

##################################################################											# OK for APRIL 2017
## CLASS from Stefan estimates !! update ??? we use distribution as reference
file_get_class <- 'J:\\TEM\\Ebyc Model v4 - October 2017\\Final Datasets\\EbyC v4 Master Employment by Class Estimates by Sub-components October 2017.dta'


SDG_GRP <- FALSE
SDG_TIME <- c(2000, 2017)


##################################################################											# OK for APRIL 2017
## GDP
# file_get_gdp <- 'J:\\TEM\\GET Model\\Final Datasets\\Productivity per country November 2016.dta'
file_get_gdp <- 'J:\\TEM\\GET Model November 2017\\Final Datasets\\Productivity per country November 2017.dta'


time_lfep <- c(1990, 2030)
time_get  <- c(1991, 2022)
round_val = 3
dir_file = paste0(ilo:::path$data,'REP_ILO/ILOEST/output/raw/')


save_file = TRUE


require(tidyverse)
require(stringr)
require(haven)
require(ilo)
init_ilo(-cl)
source(paste0(ilo:::path$data,'REP_ILO/ILOEST/do/REP_ILO.ILOEST_functions.r'))


reshape_lfep() 					# prepare file_lfep for processing:  reshape, map, create nb & ratio, 

POP_2POP_SEX_AGE_NB() 			# prepare pop_2wap_sex_age_nb 

EAP_2EAP_SEX_AGE_NB() 			# prepare eap_2eap_sex_age_nb

all_sex_age() 					# prepare EAP_2WAP_SEX_AGE_RT, EIP_2EIP_SEX_AGE_NB, EIP_2WAP_SEX_AGE_RT 

POP_2POP_SEX_AGE_GEO_NB() 		# prepare POP_2POP_SEX_AGE_GEO_NB

POP_2POP_GEO_NB() 				# prepare POP_2POP_GEO_NB 

rebuild_master() 				# prepare NEW_MASTER UR and co

EMP_2EMP_SEX_STE() 				# prepare STE

EMP_2EMP_SEX_ECO()				# prepare ECO sector

#SSD no value up to revision of master files !!!!

EMP_2EMP_SEX_OCU()				# prepare OCU occupation

GDP_NB()						# prepare GDP

EMP_2EMP_SEX_AGE_CLA()			# prepare income CLASS

EAP_2MDN_SEX_NB()				# prepare lfep age median indicator

##################### OTHER RATIO FOR KI

POP_2MLF_NOC_RT()				# POP_2MLF_NOC_RT Male labour force as % of working-age population -- ILO modeled estimates

POP_2FLF_NOC_RT()				# POP_2FLF_NOC_RT Female labour force as % of working-age population -- ILO modeled estimates

POP_2LDR_NOC_RT()				# POP_2LDR_NOC_RT Labour dependency ratio -- ILO modeled estimates

#################### rebuild ILOEST final ready to load

combine_ILOEST()

#################### rebuild SDG


# test <- c('ARM','BIH','DZA','IRQ','MNE','NIC','PER','PHL','PSE','QAT','SAU','STP','SYR','YEM')



# SDG_0852_SEX_AGE_RT
#		

# SDG_0111_SEX_AGE_RT + ref_area plus group "X01" "X02" "X03" "X04" "X05" "X06" "X10" "X13" "X21" "X26" "X34" "X36" "X40" "X45" "X49" "X56" "X64" "X70"
# SDG_0131_SEX_SOC_RT ######### don't do
# SDG_0821_NOC_RT     + ref_area plus group "X01" "X02" "X03" "X04" "X05" "X06" "X10" "X13" "X21" "X26" "X34" "X36" "X40" "X45" "X49" "X56" "X64" "X70"
# SDG_0852_SEX_AGE_RT	only  "X01" "X02" "X03" "X04" "X05" "X06" "X10" "X13" "X21" "X26" "X34" "X36" "X40" "X45" "X49" "X56" "X64" "X70"					
# SDG_1041_NOC_RT	equal to 			LAP_DGVA_NOC_RT 2000, 2017
# binding big files


SDG_0852_UNE_DEAP_SEX_AGE_RT()

SDG_0821_GDP_211P_NOC_RT()

SDG_0111_EMP_2EMP_SEX_AGE_CLA_DT()
			
SDG_0922_EMP_ECO_MAN_RT()		
				

rm(list=ls(all=TRUE)) 
invisible(gc(reset = TRUE))
q(save = "no", status = 0, runLast = FALSE)	
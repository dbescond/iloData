#' This Script reshapes data on the Labour Income Share from Europe commission / economic database, https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco_en
#' 
#' a link gives access to the set of the database pertaining to labour costs etc
#' https://ec.europa.eu/info/files/gross-domestic-product-income-approach-labour-costs_en
#' there a zip file is available for download, the zip file is ameco7_1.zip, and
#' the exctracted file is AMECO7.TXT
#' This Data is used to get the relation between the Country code and the Country Name used by AMECO
#' 
#' The following link is the one used to get the actual values of the data:
#' http://ec.europa.eu/economy_finance/ameco/Include/Query.cfm?serie=ALCD0&trn=1&agg=0&unite=0&ref=0&lstCntry=&lstYear=&code_serie=%27ALCD0%27&selection=0
#' 
#' To obtain this link follow the instructions:
#' 0) USE MOZILLA FIREFOX!
#' 1) Go to the AMECO database main page (first), and click to the AMECO online database link
#' 2) Select item 7, (GDP - Income Aproach Labour, costs), then select item 7.6 Adjusted Wage Share
#' 3) Of the two indicators choose ALCD0 ( [...Indicator name ...] at current market prices)
#' 4) Click on WebQuery(CurrentView), it opens an xlsx file with a long link in there, after clean up it boils down to the link above
#' 
#' @param work_directory inside the DATA repo, default is 'REP_EUAMECO'
#' @param check for automation, return a warning file at the root, default is TRUE
 #' @return csv files compare with ilostat database
#'
#' @examples
#' ## Not run:
#'	REP_EUAMECO.ANNUAL_LAP_input()
#' ## End(**Not run**)
#' @export
#' @rdname Micro_process

REP_EUAMECO.ANNUAL_LAP <- function (check = TRUE) {

  # SETUP; 
  ### redundant library calls for reference only
  work_directory = "REP_EUAMECO/ANNUAL_LAP"
  # work_directory <- "J:/COMMON/STATISTICS/DPAU/DATA/REP_EUROSTAT/AMECO"
  
  setwd(paste0(ilo:::path$data, work_directory))
  require(tidyverse)
  require(stringr)
  require(ilo)
  
  
  init_ilo()
  
  # STEP 0; Obtain a mapping from Ameco country name to country code
  ### not the text filtering are done in the hope that they are more robust to updates than the position of the variable of interest
  countrymap<- read_delim('input/AMECO7.TXT', delim = ';') %>% 
    select(-X65)  %>% 
    separate(CODE, 'ref_area', sep = '\\.', extra = 'drop') %>% 
    rename(SUBCHAPTER = `SUB-CHAPTER`) %>%
    filter(SUBCHAPTER=='06 Adjusted wage share', 
           str_detect(TITLE, "market prices"))  %>%
    select(ref_area,COUNTRY) %>%
    rename(ref_area.label=COUNTRY)
  
  
  # STEP 1 Loading from server
  Sys.setenv(http_proxy="http://proxy.ilo.ch:3128")
  
  X <-xml2:::read_html(httr:::GET("http://ec.europa.eu/economy_finance/ameco/Include/Query.cfm?serie=ALCD0&trn=1&agg=0&unite=0&ref=0&lstCntry=&lstYear=&code_serie=%27ALCD0%27&selection=0")) %>% 
    rvest:::html_table(header = TRUE) %>% # get dataset from html table 
    as.data.frame %>%                     # fixed list issue
    as.tbl %>% 
    rename(ref_area.label = Country)
  
  # STEP 2; Correspondence to ILOSTAT countries

  cou <- ilo$code$cl_country
  countrymap <- countrymap 
  X<- X %>% 
    inner_join(countrymap, by = "ref_area.label")
  
  
  X <- X %>% 
    left_join(
				cou %>% select(ref_area = code, label_en) %>% 
				mutate(test = 1), 
			by = "ref_area") %>% 
    filter( test %in% 1) %>% 
    select(-test,-Unit,-Label, -ref_area.label, -label_en)
  
  #STEP 3; reshape to long format
  
  X <-X %>% 
    gather(time, obs_value, -ref_area, na.rm = TRUE) %>% 
    as.tbl %>% 
    mutate(time = str_sub(time,2,-1) %>% as.numeric) %>% 
    filter(time < as.numeric(str_sub(Sys.time(),1,4)))
  
  # STEP 4; Generate non-correspondence missing variables (variables that in this case are not required to be matched to any other ILOSTAT subset)
  
  ### Note that for note_indicator the reference is taken from existing ILOSTAT data on wage share estimated from GDP, at market prices, and with employment income components= compensation of employees
  ### for note_source, since anual data, set to anual,  and from AMECO's metadata it follows that the concept falls under Definition (ESA 95) - which is often refered to as System of national accounts SNA
  
  
  # mapping of the source
  X <- X %>% left_join(
						ilo$code$cl_survey %>% 
						filter(str_detect(label_en,'AMECO'), str_sub(code,1,2) %in% 'XX') %>% 
						mutate(ref_area = str_sub(label_en,1,3)) %>%
						select(source = code, ref_area), 
				by = "ref_area")

  
  
  
  X <- X %>% 
     mutate(collection="YI", 
			obs_value = round(obs_value, 5), 
           indicator="LAP_DGVA_NOC_RT", 
           classif1="NOC_VALUE",
           note_indicator="T25:205_T26:1426_T27:213", 
           note_source="R1:3903_R1:3904")
  
  ### reorder
  X <- X %>% select_(.dots = c("collection", "ref_area", "indicator", "source", "classif1", "time", "obs_value", "note_indicator", "note_source"))
  
  
  
  # STEP 5; compare and Export to Output folder if need
  # warning could not detect country that no longer exist at the moment
 if(!check) {X %>% compare_ilo(collection, ref_area, source.type, indicator)}
 if(check) {X %>% compare_ilo(collection, ref_area, source.type, indicator, check)}

 
 
 X

}






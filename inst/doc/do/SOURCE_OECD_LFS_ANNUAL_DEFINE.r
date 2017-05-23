SOURCE_OECD_LFS_ANNUAL_DEFINE <- function (DataWithoutSource) {
  
  
  ReadMeSource <- readxl::read_excel(paste0("./ReadME_OECD_LFS.xlsx"), sheet="MappingSource") %>%
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
  DataWithSource
  
}
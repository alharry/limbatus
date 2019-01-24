# Query and data validation procedures

# Run setup file
source("analysis/setup.R")

# Prepare data
data <- read_excel("data/Data-dummy-lats-longs.xlsx")%>%
  
  # Data validation
  filter(!Tag %in% "175") %>% # Likely species ID error, no details other than length
  mutate(STL = ifelse(Tag %in% 189, 202, STL)) %>%
  mutate(Clasp.length = ifelse(Tag %in% 19, 156, Clasp.length)) %>%
  mutate(Clasp.length = ifelse(Tag %in% c("KH021208-2", "KH021208-8", "PM260508-3", "177", "BW110409-29", "BW110409-14", "BW120409-37", "48"), NA, Clasp.length)) %>% # Dubious clasper measurements
  filter(!Tag %in% "EB020609-9") %>% # ID conflict, possible sample mixup as unlikely to be BTP
  mutate(Clasp.calc = ifelse(Tag %in% "DW240309-25", NA, Clasp.calc)) %>% # DNA given as C. limbatus, unlikely to be immature at this length, no clasper length given 
  mutate(Clasp.calc = ifelse(Tag %in% 173, "y", Clasp.calc)) %>% # Clasp.length and STL inconsistent with immature, assumed to be an error
  mutate(Clasp.calc = ifelse(Tag %in% c(66, 187, 46), "n", Clasp.calc)) %>% # Small claspers unlikely to be partially calcified, assumed error
  mutate(Uter.stage = ifelse(Tag %in% "PS120609-2", "C", Uter.stage)) %>% # Purebred C. limbatus, largest individual measured, unlikely to be immature probably just non-pregnant 
  mutate(AgeAgree = ifelse(Tag %in% "BW290409-11", NA, AgeAgree)) %>% # Highly unlikely to be a 1+, assumed to be incorrectly aged
  mutate(Uter.stage = ifelse(Tag %in% "BW110410-18", "C", Uter.stage)) %>% # Relatively large female with 63mm uterus width, likely to be mature
  mutate(Uter.stage = ifelse(Tag %in% c("PS120509-2", "PS120509-4", "PS120509-5", "PS140509-6", "PS030609-10", "PS080609-9"), "C", Uter.stage)) %>% # Dubious that these were ovulating females, otherwise eggs probably would have been counted. More likely C3 with large developing eggs in the ovary or aborted young on capture

  # Possible hybrids or C. tilstoni identified as C. limbatus
  mutate(Clasp.length = ifelse(Tag %in% "608", NA, Clasp.length)) %>% # Clasper length and maturity stage consistent with mature male C. tilstoni, but clasp length could be a typo.  
  filter(!Tag %in% 178) %>% # Noted as mature male, but no clasper lengths or other corroborating evidence. 
  mutate(Uter.stage = ifelse(Tag %in% "PS120509-3", NA, Uter.stage)) %>% # F1 hybrid C. limbatus. Very large individual listed as non-pregnant, probably just observer error but can't be sure
  # EB030609-16 is a hybrid, somewhat smaller than other individuals (could conceivable be intermediate type but also could just be a typo)
  
  # Data query
  mutate(Sex = casefold(Sex)) %>%
  mutate(Sex = fct_recode(Sex, "Female" = "f", "Male" = "m")) %>% # Recode Sex
  
  # Location data
  mutate(Lat = ifelse(!is.na(LatDeg), -(LatDeg + LatMin/60), Lat)) %>%
  mutate(Long = ifelse(!is.na(LonDeg), LonDeg + LonMin/60, Long)) %>%
  
  # Maturity stage
  mutate(Clasp.calc = trimws(Clasp.calc)) %>% 
  mutate(Mat = ifelse(Uter.stage %in% c("A", "B")|Clasp.calc %in% c("n", "p"), 0, 1)) %>%
  mutate(Mat = ifelse(is.na(Uter.stage) & is.na(Clasp.calc), NA, Mat)) %>%
  mutate(Matern = ifelse(Uter.stage %in% c("D", "E", "F"), 1, 0)) %>%
  mutate(Matern = ifelse(Tag %in% "134122", 1, Matern)) %>%
  mutate(Matern = ifelse(is.na(Uter.stage), NA, Matern)) %>%
  mutate(Matern = ifelse(Tag %in% c("211080", "215969", "134119", "211079"), NA, Matern)) %>% # ABT 
  
  # Re-label sources of data
  mutate(Source = factor(Source, levels=c("QLD2", "QLD", "NSW1", "NSW2"))) %>%
  mutate(Source = fct_recode(Source,
                           "QLD: 2004 - 2007" = "QLD2",
                           "QLD: 2007" = "QLD",
                           "NSW: 2008 - 2010" = "NSW1",
                           "NSW: 2013" = "NSW2")) %>% 
  
  # Convert length measurements to cm
  mutate(STL = STL, FL = FL, PCL = PCL)

  # Separate out species
  data_all<-data %>% 
  # Add in labels for graphs  
    mutate(Species = fct_recode(Species,"C. limbatus" = "BTP", "C. tilstoni (QLD)" = "ABT")) %>%
    mutate(facet = fct_recode(Sex, `(a) Female` = "Female", `(b) Male` = "Male")) 
  
  # Qld C. tilstoni
  data_abt <- data_all %>%
    filter(Species %in% "C. tilstoni (QLD)") %>% 
    # Remove single purebred C. tilstoni from NSW (Not in Harry et al 2013)
    filter(!Tag %in% "BW110410-26") %>%
    # Add in label for graphs
    mutate(facet = fct_recode(Sex, `(a) Female` = "Female", `(b) Male` = "Male"))
  
  # C. limbatus
  data<-filter(data,Species%in%"BTP") %>% filter(!is.na(Sex)) %>%
    # Add in partial ages
    mutate(Month2 = ifelse(Month %in% c(12), Month - 11, Month + 1)) %>%
    mutate(Reader1 = Reader1 + Month2/12) %>%
    mutate(Reader2 = Reader2 + Month2/12) %>%
    mutate(AgeAgree = AgeAgree + Month2/12) %>%
    # Add in label for graphs
    mutate(facet = fct_recode(Sex, `(a) Female` = "Female", `(b) Male` = "Male")) 
  
# Save data
write_rds(data,"data/data_final_btp.rds")
write_rds(data_all,"data/data_final_both.rds")
write_rds(data_abt, "data/data_final_abt.rds")

# Clean up environment
rm(list = ls())

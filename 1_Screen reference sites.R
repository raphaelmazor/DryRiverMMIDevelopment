#########################
###Reference screening###
#########################

library(tidyverse)
library(ggplot2)

######LOAD DATA######

#Load GIS data

#Load habitat data
phab_df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_07252023.csv")

#Load bio data, for assessing sensitivity
arthro_df<-read_csv("Data/BioData/Arthros/arthropod_metircs_07252023.csv")



bug_mets<-c("Arth_Rich", "Arth_Abund", 
            "Insect_Rich", "Insect_RelRich","Insect_Abund", "Insect_RelAbund", 
            "Noninsect_Rich", "Noninsect_RelRich", "Noninsect_Abund", "Noninsect_RelAbund",
            "Coleo_Rich", "Coleo_RelRich", "Coleo_Abund", "Coleo_RelAbund", 
            "Araneae_Rich", "Araneae_RelRich", "Araneae_Abund", "Araneae_RelAbund", 
            "Araneae_Nat_Rich", "Araneae_Nat_RelRich", "Araneae_Nat_Abund", "Araneae_Nat_RelAbund",
            "Formicidae_Rich", "Formicidae_RelRich", "Formicidae_Abund", "Formicidae_RelAbund",
            "Formicidae_Nat_Rich", "Formicidae_Nat_RelRich", "Formicidae_Nat_Abund", "Formicidae_Nat_RelAbund", 
            "CAF_Rich", "CAF_RelRich", "CAF_Abund", "CAF_RelAbund", 
            "CAF_Nat_Rich", "CAF_Nat_RelRich", "CAF_Nat_Abund", "CAF_Nat_RelAbund", 
            "Nonnat_KRich", "Nonnat_Rich", "Nonnat_RelRich", "Nonnat_KAbund", "Nonnat_Abund", "Nonnat_RelAbund", 
            "NonnatSynanth_KRich", "NonnatSynanth_Rich", "NonnatSynanth_RelRich", 
            "NonnatSynanth_KAbund", "NonnatSynanth_Abund", "NonnatSynanth_RelAbund", 
            "LinEpi_Rich", "LinEpi_RelRich", "LinEpi_Abund", "LinEpi_RelAbund", 
            "Aquatic_KRich", "Aquatic_Rich", "Aquatic_RelRich", "Aquatic_KAbund", "Aquatic_Abund", "Aquatic_RelAbund", 
            "StratumFinal_KRich", "StratumFinal_KAbund",
            "Ground_Rich", "Ground_RelRich", "Ground_Abund", "Ground_RelAbund", 
            "Herbaceous_Rich", "Herbaceous_RelRich", "Herbaceous_Abund", "Herbaceous_RelAbund", 
            "Arboreal_Rich", "Arboreal_RelRich", "Arboreal_Abund", "Arboreal_RelAbund", 
            "FFG_KRich", "FFG_KAbund", 
            "Detritivore_Rich", "Detritivore_RelRich", "Detritivore_Abund","Detritivore_RelAbund", 
            "Fungivore_Rich", "Fungivore_RelRich", "Fungivore_Abund", "Fungivore_RelAbund",
            "DetrFung_Rich", "DetrFung_RelRich","DetrFung_Abund", "DetrFung_RelAbund", 
            "Predator_Rich", "Predator_RelRich", "Predator_Abund", "Predator_RelAbund", "FFG_d_KRich", "FFG_d_KAbund", "PredatorGround_Rich", "PredatorGround_RelRich", "PredatorGround_Abund","PredatorGround_RelAbund", 
            "BodySize_KRich", "BodySize_KAbund", "BodySizeSmall_Rich", "BodySizeSmall_RelRich", "BodySizeSmall_Abund", "BodySizeSmall_RelAbund", "BodySizeMedium_Rich", "BodySizeMedium_RelRich", "BodySizeMedium_Abund", "BodySizeMedium_RelAbund", "BodySizeLarge_Rich", "BodySizeLarge_RelRich", "BodySizeLarge_Abund", "BodySizeLarge_RelAbund", "BodySizeLargest", "BodySizeAverage", 
            "Dispersal_KRich", "Dispersal_KAbund", "DisperserPoor_Rich", "DisperserPoor_RelRich", "DisperserPoor_Abund", "DisperserPoor_RelAbund", "DisperserGood_Rich", "DisperserGood_RelRich", "DisperserGood_Abund", "DisperserGood_RelAbund", 
            "TempTol_KRich", "TempTol_KAbund", "TempTolHigh_Rich", "TempTolHigh_RelRich", "TempTolHigh_Abund", "TempTolHigh_RelAbund", "TempTolAverage", "TempTolMax")

setdiff(bug_mets, names(arthro_df))
setdiff(names(arthro_df), bug_mets)

arth_df_long<-arthro_df %>%
  select(StationCode, SampleDate, CollectionMethodCode, all_of(bug_mets)) %>%
  pivot_longer(cols=all_of(bug_mets), names_to="BugMet", values_to="BugMet_value", values_drop_na = T) %>%
  mutate(BugMet2 = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",BugMet),
                             CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",BugMet),
                             T~BugMet),
         Method = case_when(CollectionMethodCode=="TerInvt_T_DS"~("Trap"),
                            CollectionMethodCode=="TerInvt_V_DS"~("Veg"),
                            T~"Other"),
         MetricForm = case_when(str_detect(BugMet,"_Rich")~"Rich",
                                str_detect(BugMet,"_RelRich")~"RelRich",
                                str_detect(BugMet,"_Abund")~"Abund",
                                str_detect(BugMet,"_RelAbund")~"RelAbund",
                                T~"Other")) %>% #Other includes largest, average, and max
  clipr::write_clip()
  #Get rid of these interim calc metrics
  filter(!str_detect(BugMet,"KRich")) %>%
  filter(!str_detect(BugMet,"KAbund"))

arth_df_long %>% clipr::write_clip()

# arth_df_long %>% filter(MetricForm=="Other") %>% select(BugMet2) %>% unique()
# arth_df_long %>%   group_by(MetricForm) %>% tally()

#####ASSESS BIO RESPONSES TO STRESS#####

#Calculate Spearman's Rho between all bio metrics and all hab/gis metrics [stressors only]
#Create scatterplots of strong relationships

##Sepeicifically, look at phab stress measures
#How correlated are they to eachother?
#Look at hab stress metrics and identify appropriate cutoffs through visual inspections of relationships
#Can you pick the "best" one?
#How concordant are they based on cutoffs?
#Is one more sensitive than the others?

hab_stress_mets<-c("HumanActivity_Ext",	"HumanActivity_Int",	"HumanActivity_Prox",	"HumanActivity_Prox_SWAMP")

library(corrplot)
stress_df<-phab_df %>%
  mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
  select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
  #just work with the stress measures
  pivot_longer(cols=all_of(hab_stress_mets), names_to="PHABMet", values_to = "PHABMet_value", values_drop_na = T)
  
stress_df %>%
  inner_join()
             
             #%>%
             # mutate(SampleDate=lubridate::mdy(SampleDate))
             ) %>%
  filter(CollectionMethodCode=="TerInvt_T_DS") %>%
  # ggplot(aes(x=PHABMet_value, y=BugMet_value))+
  # # geom_point()+
  # geom_smooth()+
  # facet_grid(BugMet~PHABMet, scales="free")+
  # theme_bw()+
  # theme(strip.text.y=element_text(angle=0))+
  # scale_x_sqrt()
  
  ggplot(aes(x=PHABMet_value, y=BugMet_value))+
  # geom_point()+
  geom_smooth(aes(color=PHABMet), method=lm)+
  facet_wrap(~BugMet, scales="free")+
  theme_bw()+
  theme(strip.text.y=element_text(angle=0))+
  coord_cartesian(xlim=c(0,30))+
  scale_color_brewer(palette="Set1")



phab_df %>%
  rename(HumanActivity_Prox=HumanActivity_Prox.x) %>%
  select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
  mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
  pivot_longer(cols=all_of(hab_stress_mets), names_to="PHABMet", values_to = "PHABMet_value", values_drop_na = T) %>%
  
  inner_join(arthro_df %>%
               select(StationCode, SampleDate, CollectionMethodCode,
                      ends_with("_Rich")) %>%
               pivot_longer(cols=ends_with("_Rich"), names_to="BugMet", values_to="BugMet_value", values_drop_na = T) %>%
               mutate(BugMet2 = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",BugMet),
                                          CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",BugMet),
                                          T~BugMet))
  ) %>%
  group_by(PHABMet, BugMet, BugMet2) %>%
  summarise(Rho = cor(PHABMet_value, BugMet_value, use="pairwise.complete", method="spearman")) %>%
  ungroup() %>%
  mutate(RhoSq = Rho^2) %>%
  arrange(-RhoSq) %>%
  mutate(Method = case_when(str_detect(BugMet2,"V_")~"Veg",
                            str_detect(BugMet2,"T_")~"Trap",
                            T~"Other")) %>%
  
  ggplot(aes(x=BugMet, y=Rho))+
  geom_point(aes(color=PHABMet, shape=Method, group=Method,
                 size=RhoSq>.1), position=position_dodge())+
  geom_hline(yintercept=0)+
  scale_size_manual(values=c(1,2))+
  # facet_wrap(~Method)+
  coord_flip()



####SCREEN

#Start with Ode et al. 2016 screens
#Replace w1_hall with best phab metric(s)
#Adjust gis screens if necessary based on observed relationships with biology



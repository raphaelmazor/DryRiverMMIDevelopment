#########################
###Reference screening###
#########################

library(tidyverse)
library(ggplot2)
library(clipr)
######LOAD DATA######

#Load GIS data
gis_df<-read_csv("Data/NonBioData/GIS/All_Sites_Bug_Bryo_RB4_RB9_GIS.csv")
# skimr::skim(gis_df)

#Load habitat data
phab_df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_07272023.csv")

#Load bio data, for assessing sensitivity
arthro_df<-read_csv("Data/BioData/Arthros/arthropod_metircs_07252023.csv")
bryo_df<-read_csv("Data/BioData/Bryos/bryo_metrics_07262023_v2.csv")


bug_mets_lu<-read_csv("Data/BioData/Arthros/bug_metric_lu_07252023.csv") %>%
  mutate(Metric_short=str_sub(Metric, 3, nchar(Metric)))

#Review data gaps
#sites with bug data but no bryo data
setdiff(arthro_df$StationCode, bryo_df$StationCode ) %>% write_clip() 

#sites with bryo data but no bug data
#Should be a lot outside socal
setdiff( bryo_df$StationCode, arthro_df$StationCode ) %>% write_clip() 

# Sites with bio data (bugs or bryos) but no gis data
setdiff( c(bryo_df$StationCode, arthro_df$StationCode ), gis_df$StationCode ) %>% write_clip() 

# Sites with bio data (bugs or bryos) but no habitat data
setdiff( c(bryo_df$StationCode, arthro_df$StationCode ), phab_df $StationCode ) %>% write_clip() 

#sites with habitat data, but no bio data
setdiff(phab_df $StationCode , c(bryo_df$StationCode, arthro_df$StationCode )) %>% write_clip() 


arthro_df$StationCode %>%
  setdiff(phab_df$StationCode) %>%
  setdiff(bryo_df$StationCode)

bryo_df$StationCode %>%
  setdiff(phab_df$StationCode) %>%
  setdiff(arthro_df$StationCode)

# bug_mets<-c("Arth_Rich", "Arth_Abund", 
#             "Insect_Rich", "Insect_RelRich","Insect_Abund", "Insect_RelAbund", 
#             "Noninsect_Rich", "Noninsect_RelRich", "Noninsect_Abund", "Noninsect_RelAbund",
#             "Coleo_Rich", "Coleo_RelRich", "Coleo_Abund", "Coleo_RelAbund", 
#             "Araneae_Rich", "Araneae_RelRich", "Araneae_Abund", "Araneae_RelAbund", 
#             "Araneae_Nat_Rich", "Araneae_Nat_RelRich", "Araneae_Nat_Abund", "Araneae_Nat_RelAbund",
#             "Formicidae_Rich", "Formicidae_RelRich", "Formicidae_Abund", "Formicidae_RelAbund",
#             "Formicidae_Nat_Rich", "Formicidae_Nat_RelRich", "Formicidae_Nat_Abund", "Formicidae_Nat_RelAbund", 
#             "CAF_Rich", "CAF_RelRich", "CAF_Abund", "CAF_RelAbund", 
#             "CAF_Nat_Rich", "CAF_Nat_RelRich", "CAF_Nat_Abund", "CAF_Nat_RelAbund", 
#             "Nonnat_KRich", "Nonnat_Rich", "Nonnat_RelRich", "Nonnat_KAbund", "Nonnat_Abund", "Nonnat_RelAbund", 
#             "NonnatSynanth_KRich", "NonnatSynanth_Rich", "NonnatSynanth_RelRich", 
#             "NonnatSynanth_KAbund", "NonnatSynanth_Abund", "NonnatSynanth_RelAbund", 
#             "LinEpi_Rich", "LinEpi_RelRich", "LinEpi_Abund", "LinEpi_RelAbund", 
#             "Aquatic_KRich", "Aquatic_Rich", "Aquatic_RelRich", "Aquatic_KAbund", "Aquatic_Abund", "Aquatic_RelAbund", 
#             "StratumFinal_KRich", "StratumFinal_KAbund",
#             "Ground_Rich", "Ground_RelRich", "Ground_Abund", "Ground_RelAbund", 
#             "Herbaceous_Rich", "Herbaceous_RelRich", "Herbaceous_Abund", "Herbaceous_RelAbund", 
#             "Arboreal_Rich", "Arboreal_RelRich", "Arboreal_Abund", "Arboreal_RelAbund", 
#             "FFG_KRich", "FFG_KAbund", 
#             "Detritivore_Rich", "Detritivore_RelRich", "Detritivore_Abund","Detritivore_RelAbund", 
#             "Fungivore_Rich", "Fungivore_RelRich", "Fungivore_Abund", "Fungivore_RelAbund",
#             "DetrFung_Rich", "DetrFung_RelRich","DetrFung_Abund", "DetrFung_RelAbund", 
#             "Predator_Rich", "Predator_RelRich", "Predator_Abund", "Predator_RelAbund", "FFG_d_KRich", "FFG_d_KAbund", "PredatorGround_Rich", "PredatorGround_RelRich", "PredatorGround_Abund","PredatorGround_RelAbund", 
#             "BodySize_KRich", "BodySize_KAbund", "BodySizeSmall_Rich", "BodySizeSmall_RelRich", "BodySizeSmall_Abund", "BodySizeSmall_RelAbund", "BodySizeMedium_Rich", "BodySizeMedium_RelRich", "BodySizeMedium_Abund", "BodySizeMedium_RelAbund", "BodySizeLarge_Rich", "BodySizeLarge_RelRich", "BodySizeLarge_Abund", "BodySizeLarge_RelAbund", "BodySizeLargest", "BodySizeAverage", 
#             "Dispersal_KRich", "Dispersal_KAbund", "DisperserPoor_Rich", "DisperserPoor_RelRich", "DisperserPoor_Abund", "DisperserPoor_RelAbund", "DisperserGood_Rich", "DisperserGood_RelRich", "DisperserGood_Abund", "DisperserGood_RelAbund", 
#             "TempTol_KRich", "TempTol_KAbund", "TempTolHigh_Rich", "TempTolHigh_RelRich", "TempTolHigh_Abund", "TempTolHigh_RelAbund", "TempTolAverage", "TempTolMax")

# setdiff(bug_mets, names(arthro_df))
# setdiff(names(arthro_df), bug_mets)
# 
# 
# #Convert arth data to long format and correct the metric name
# 
# arth_df_long<-arthro_df %>%
#   select(StationCode, SampleDate, CollectionMethodCode, all_of(bug_mets_lu$Metric_short)) %>%
#   pivot_longer(cols=all_of(bug_mets_lu$Metric_short), 
#                names_to="Metric_short", 
#                values_to="BugMet_value", 
#                values_drop_na = T) %>%
#   mutate(Metric = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",Metric_short),
#                             CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",Metric_short),
#                             T~Metric_short)) %>%
#   #Get rid of these interim calc metrics
#   filter(!str_detect(Metric,"KRich")) %>%
#   filter(!str_detect(Metric,"KAbund")) %>%
#   inner_join(bug_mets_lu %>% select(Metric, Method, MetricForm, MetricGroup=Group))
# 
# arth_df_long %>% clipr::write_clip()
# 
# # arth_df_long %>% filter(MetricForm=="Other") %>% select(BugMet2) %>% unique()
# arth_df_long %>%   group_by(MetricForm) %>% tally()
# 
# #####ASSESS BIO RESPONSES TO STRESS#####
# 
# #Calculate Spearman's Rho between all bio metrics and all hab/gis metrics [stressors only]
# #Create scatterplots of strong relationships
# 
# ##Sepeicifically, look at phab stress measures
# #How correlated are they to eachother?
# #Look at hab stress metrics and identify appropriate cutoffs through visual inspections of relationships
# #Can you pick the "best" one?
# #How concordant are they based on cutoffs?
# #Is one more sensitive than the others?
# 
# #Identify habitat stress metrics
# hab_stress_mets<-c("HumanActivity_Ext",	"HumanActivity_Int",	"HumanActivity_Prox",	"HumanActivity_Prox_SWAMP")
# 
# #Create a long-format dataframe with just these stress metrics
# phabstress_df<-phab_df %>%
#   mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
#   select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
#   #just work with the stress measures
#   pivot_longer(cols=all_of(hab_stress_mets), names_to="PHABMet", values_to = "PHABMet_value", values_drop_na = T) %>%
#   group_by(PHABMet) %>%
#   mutate(MetMax=max(PHABMet_value)) %>%
#   ungroup() %>%
#   mutate(PHABMet_value_rescaled=PHABMet_value/MetMax)
# # skimr::skim(phabstress_df)
# 
# #Join with the long arthropod data to create scatterplots
# arth_phabstress<-inner_join(arth_df_long, phabstress_df, relationship = "many-to-many")
# 
# phabstress_v_bugmets_richness<-ggplot(data=arth_phabstress %>%
#                                         filter(MetricForm=="Rich") ,
#                                       aes(x=PHABMet_value_rescaled, y=BugMet_value))+
#   geom_smooth(aes(color=PHABMet),  se=F)+
#   facet_grid(MetricGroup~Method, scales="free")+
#   theme_bw()+
#   theme(strip.text.y = element_text(angle=0))+
#   ggtitle("Richness metrics")
# ggsave(phabstress_v_bugmets_richness, filename="Figures/phabstress_v_bugmets_richness.png", height=8, width=8)
# 
# phabstress_v_bugmets_relrichness<-ggplot(data=arth_phabstress %>%
#                                            filter(MetricForm=="RelRich") ,
#                                          aes(x=PHABMet_value_rescaled, y=BugMet_value))+
#   geom_smooth(aes(color=PHABMet),  se=F)+
#   facet_grid(MetricGroup~Method, scales="free")+
#   theme_bw()+
#   theme(strip.text.y = element_text(angle=0))+
#   ggtitle("Relative richness metrics")
# ggsave(phabstress_v_bugmets_relrichness, filename="Figures/phabstress_v_bugmets_relrichness.png", height=8, width=8)
# 
# phabstress_v_bugmets_abund<-ggplot(data=arth_phabstress %>%
#                                      filter(MetricForm=="Abund") ,
#                                    aes(x=PHABMet_value_rescaled, y=BugMet_value))+
#   geom_smooth(aes(color=PHABMet),  se=F)+
#   facet_grid(MetricGroup~Method, scales="free")+
#   theme_bw()+
#   theme(strip.text.y = element_text(angle=0))+
#   ggtitle("Abundance metrics")
# ggsave(phabstress_v_bugmets_abund, filename="Figures/phabstress_v_bugmets_abund.png", height=8, width=8)
# 
# phabstress_v_bugmets_relabund<-ggplot(data=arth_phabstress %>%
#                                         filter(MetricForm=="RelAbund") ,
#                                       aes(x=PHABMet_value_rescaled, y=BugMet_value))+
#   geom_smooth(aes(color=PHABMet),  se=F)+
#   facet_grid(MetricGroup~Method, scales="free")+
#   theme_bw()+
#   theme(strip.text.y = element_text(angle=0))+
#   ggtitle("Relative abundance metrics")
# ggsave(phabstress_v_bugmets_relabund, filename="Figures/phabstress_v_bugmets_relabund.png", height=8, width=8)
# 
# phabstress_v_bugmets_other<-ggplot(data=arth_phabstress %>%
#                                      filter(MetricForm=="Other") ,
#                                    aes(x=PHABMet_value_rescaled, y=BugMet_value))+
#   geom_smooth(aes(color=PHABMet),  se=F)+
#   facet_grid(MetricGroup~Method, scales="free")+
#   theme_bw()+
#   theme(strip.text.y = element_text(angle=0))+
#   ggtitle("Other metrics")
# ggsave(phabstress_v_bugmets_other, filename="Figures/phabstress_v_bugmets_other.png", height=8, width=8)
# 
# phabstress_df %>%
#   select(PHABMet, MetMax)  %>%
#   unique()
# 
# 
# 
# 
# # Propose cutoffs that divide the data set into ~thirds:
# # cutoffs_df<-crossing(
# #   Ext = 1:24,
# #   Int=1:24,
# #   Prox=seq(from=1, to=10, by=.5)
# # )
# # 
# # cutoffs_df$Low_n <- sapply(1:nrow(cutoffs_df), function(i){
# #   xdf = phab_df %>%
# #     filter(StationCode %in% arth_df_long$StationCode) %>%
# #     mutate(Stress=case_when(HumanActivity_Ext< cutoffs_df$Ext[i] &
# #                               HumanActivity_Int< cutoffs_df$Int[i] & 
# #                               # HumanActivity_Prox<8 &
# #                               HumanActivity_Prox_SWAMP<cutoffs_df$Prox[i] ~"Low",
# #                             T~"Not Low"))
# #   nrow(xdf %>%filter(Stress=="Low"))
# # })
# # 
# # cutoffs_df %>% filter(Low_n>=12)
# 
# phab_df2<-phab_df %>%
#   mutate(ReachStress = case_when(HumanActivity_Ext<12 &
#                                    HumanActivity_Int<12 & 
#                                    # HumanActivity_Prox<8 &
#                                    HumanActivity_Prox_SWAMP<6 ~"Low",
#                                  HumanActivity_Ext>=20~"High",
#                                  HumanActivity_Int>=20~"High",
#                                  # HumanActivity_Prox>=20~"High",
#                                  HumanActivity_Prox_SWAMP>=10~"High",
#                                  T~"Med" )) %>%
#   inner_join(arthro_df %>% select(StationCode) %>% unique()) %>%
#   group_by(ReachStress) %>% tally()
# 
# 
# ggplot(data=phabstress_df, aes(x=PHABMet_value))+
#   geom_histogram(aes(fill=PHABMet))
# 
# 
# phab_df %>%
#   skimr::skim()
# 
# library(corrplot)
# 
# 
# 
# ggplot(aes(x=PHABMet_value, y=BugMet_value))+
#   # geom_point()+
#   geom_smooth(aes(color=PHABMet), method=lm)+
#   facet_wrap(~BugMet, scales="free")+
#   theme_bw()+
#   theme(strip.text.y=element_text(angle=0))+
#   coord_cartesian(xlim=c(0,30))+
#   scale_color_brewer(palette="Set1")
# 
# 
# 
# phab_df %>%
#   rename(HumanActivity_Prox=HumanActivity_Prox.x) %>%
#   select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
#   mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
#   pivot_longer(cols=all_of(hab_stress_mets), names_to="PHABMet", values_to = "PHABMet_value", values_drop_na = T) %>%
#   
#   inner_join(arthro_df %>%
#                select(StationCode, SampleDate, CollectionMethodCode,
#                       ends_with("_Rich")) %>%
#                pivot_longer(cols=ends_with("_Rich"), names_to="BugMet", values_to="BugMet_value", values_drop_na = T) %>%
#                mutate(BugMet2 = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",BugMet),
#                                           CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",BugMet),
#                                           T~BugMet))
#   ) %>%
#   group_by(PHABMet, BugMet, BugMet2) %>%
#   summarise(Rho = cor(PHABMet_value, BugMet_value, use="pairwise.complete", method="spearman")) %>%
#   ungroup() %>%
#   mutate(RhoSq = Rho^2) %>%
#   arrange(-RhoSq) %>%
#   mutate(Method = case_when(str_detect(BugMet2,"V_")~"Veg",
#                             str_detect(BugMet2,"T_")~"Trap",
#                             T~"Other")) %>%
#   
#   ggplot(aes(x=BugMet, y=Rho))+
#   geom_point(aes(color=PHABMet, shape=Method, group=Method,
#                  size=RhoSq>.1), position=position_dodge())+
#   geom_hline(yintercept=0)+
#   scale_size_manual(values=c(1,2))+
#   # facet_wrap(~Method)+
#   coord_flip()
# 


####SCREEN

#Start with Ode et al. 2016 screens
#Replace w1_hall with best phab metric(s)
#Adjust gis screens if necessary based on observed relationships with biology

hab_stress_mets<-c("HumanActivity_Ext",	"HumanActivity_Int",	"HumanActivity_Prox",	"HumanActivity_Prox_SWAMP")

#Calculate terciles
phab_df %>%
  mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
  select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
  filter(StationCode %in% arthro_df$StationCode) %>%
  pivot_longer(cols=all_of(hab_stress_mets)) %>%
  group_by(name) %>%
  summarise(
    T1 = quantile(value, probs=0.333, na.rm=T),
    T2 = quantile(value, probs=0.667, na.rm=T)
  )


phabstress_df2<-phab_df %>%
  mutate(SampleDate=lubridate::mdy(SampleDate)) %>%
  select(StationCode, SampleDate, all_of(hab_stress_mets)) %>%
  # skimr::skim()
  #just work with the stress measures
  mutate(StationCode,
         # SampleDate,
         #Use ~terciles, round up
         
         ReachStress = 
           case_when(HumanActivity_Ext<10 &
                       HumanActivity_Int<10 & 
                       HumanActivity_Prox_SWAMP<4 ~0,
                     HumanActivity_Ext>=18~2,
                     HumanActivity_Int>=19~2,
                     HumanActivity_Prox_SWAMP>=7~2,
                     T~1 )) %>%
           # case_when(HumanActivity_Ext<10 &
           #             HumanActivity_Int<10 & 
           #             HumanActivity_Prox_SWAMP<6 ~0,
           #           HumanActivity_Ext>=16~2,
           #           HumanActivity_Int>=16~2,
           #           HumanActivity_Prox_SWAMP>=12~2,
           #           T~1 )) %>%
  group_by(StationCode) %>%
  summarise(ReachStressMax = max(ReachStress, na.rm=T),
            HumanActivity_Int_Max =max(HumanActivity_Int, na.rm = T),
            HumanActivity_Ext_Max =max(HumanActivity_Ext, na.rm = T),
            HumanActivity_Prox_Max =max(HumanActivity_Prox_SWAMP, na.rm = T))  %>%
  ungroup()

phabstress_df2 %>% group_by(ReachStressMax) %>% tally()

gis_stress_df<-gis_df %>%
  # names()
  select(StationCode,
         starts_with(c("ag_","code_21", "urban","road","paved","permanmande","invdamdist","mines") %>%sort())) %>%
  # skimr::skim()
  mutate(GIS_stress_Ode = 
           case_when(ag_2011_1k<3 & ag_2011_5k<3 & ag_2011_ws<3 &
                       code_21_2011_1k<7 & code_21_2011_5k<7 & code_21_2011_ws<10 &
                       urban_2011_1k<3 & urban_2011_5k<3 & urban_2011_ws<3 &
                       urban_2011_1k + ag_2011_1k <5 & urban_2011_5k + ag_2011_5k <5 & urban_2011_ws + ag_2011_ws<5 &
                       roaddens_1k<2 & roaddens_5k<2 & roaddens_ws<2 &
                       paved_int_1k<5 & paved_int_5k<10 & paved_int_ws<50 &
                       # invdamdist > .1 &
                       mines_5k ==0 ~0,
                     
                     ag_2011_1k+urban_2011_1k>=50~2, ag_2011_5k+ urban_2011_5k>=50~2, ag_2011_ws+urban_2011_ws>=50~2,
                     code_21_2011_1k>=50~2, code_21_2011_5k >=50~2, code_21_2011_ws>=50~2,
                     # urban_2011_1k>=50~2, urban_2011_5k >=50~2, urban_2011_ws>=50~2,
                     
                     T~1)
  ) 
# group_by(GIS_stress_Ode) %>% tally()
gis_stress_df %>%
  inner_join(arthro_df) %>%
  group_by(GIS_stress_Ode) %>%
  tally()


site_status_df<-tibble(
  StationCode = c(phab_df$StationCode, gis_df$StationCode) %>%
    unique() %>% sort()
) %>%
  left_join(    phabstress_df2  ) %>%
  left_join(gis_stress_df) %>%
  transmute(StationCode,
            ReachStressMax, GIS_stress_Ode,
            RefStatusFinal = case_when(ReachStressMax==2~"High",
                                       GIS_stress_Ode==2~"High",
                                       ReachStressMax==1~"Med",
                                       GIS_stress_Ode==1~"Med",
                                       ReachStressMax==0 & GIS_stress_Ode ~"Low",
                                       GIS_stress_Ode==0~"Low",
                                       is.na(GIS_stress_Ode) ~"Unknown",
                                       T~"Other"
            ))
site_status_df %>%
  group_by(RefStatusFinal) %>% tally()

site_status_df %>%
  filter(StationCode %in% arthro_df$StationCode) %>%
  select(ReachStressMax, GIS_stress_Ode, StationCode) %>%
  unique() %>%
  group_by(ReachStressMax, GIS_stress_Ode) %>%
  tally() %>% 
  pivot_wider(names_from=ReachStressMax, values_from = n, values_fill = 0) %>%
  rename(LowStress_reach=`0`, MedStress_reach=`1`, HighStress_reach=`2`) %>%
  mutate(WatershedStress = case_when(GIS_stress_Ode==0~"LowStress_shed",
                                     GIS_stress_Ode==1~"MedStress_shed",
                                     GIS_stress_Ode==2~"HighStress_shed", 
                                     T~"Other")) %>%
  select(-GIS_stress_Ode)

site_status_df %>%
  inner_join(arthro_df) %>%
  select(StationCode, RefStatusFinal) %>% 
  unique() %>%
  group_by(RefStatusFinal) %>% 
  tally()

ggplot(data=site_status_df, aes(x=GIS_stress_Ode, y=ReachStressMax))+
  geom_point( position = position_jitter(height=.2, width=.2)) +
  geom_smooth(method=lm)

write_csv(site_status_df, "Data/NonBioData/site_status_df.csv")

library(sf)
site_status_df %>%
  inner_join(gis_df %>%
               select(StationCode, new_lat, new_long)) %>%
  st_as_sf(coords=c("new_long","new_lat"),
           crs=4326)  %>%
  ggplot()+
  geom_sf(aes(color=RefStatusFinal))

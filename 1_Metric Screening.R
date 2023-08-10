####Assess metric response to stress

library(tidyverse)
library(ggplot2)

#Load GIS data

#Load habitat data
phab_df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_07272023.csv")
skimr::skim(phab_df)

ref_sites<-read_csv("Data/NonBioData/site_status_df.csv")
ref_sites %>% group_by(RefStatusFinal) %>% tally()

#Load bio data, for assessing sensitivity
bug_mets_lu<-read_csv("Data/BioData/Arthros/bug_metric_lu_07252023.csv") %>%
  mutate(Metric_short=str_sub(Metric, 3, nchar(Metric)))

arthro_df<-read_csv("Data/BioData/Arthros/arthropod_metircs_07252023.csv")
arth_df_long<-arthro_df %>%
  select(StationCode, SampleDate, CollectionMethodCode, all_of(bug_mets_lu$Metric_short)) %>%
  pivot_longer(cols=all_of(bug_mets_lu$Metric_short), 
               names_to="Metric_short", 
               values_to="MetricValue", 
               values_drop_na = T) %>%
  mutate(Metric = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",Metric_short),
                            CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",Metric_short),
                            T~Metric_short)) %>%
  #Get rid of these interim calc metrics
  filter(!str_detect(Metric,"KRich")) %>%
  filter(!str_detect(Metric,"KAbund")) %>%
  inner_join(bug_mets_lu %>% select(Metric, Method, MetricForm, MetricGroup=Group))

bryo_mets_lu<-read_csv("Data/BioData/Bryos/bryo_metric_lu.csv")
bryo_df<-read_csv("Data/BioData/Bryos/bryo_metrics_07262023_v2.csv")
# bryo_df<-read_csv("Data/BioData/Bryos/Final_Bryophyte_Output.csv") 
skimr::skim(bryo_df)

bryo_df_long<-bryo_df %>%
  select(-SampleID, -CollectionMethodCode, -Replicate) %>%
  pivot_longer(cols=c(-StationCode, -SampleDate),
               names_to = "Metric_short", values_to="MetricValue", values_drop_na = T) %>%
  filter(MetricValue > -Inf & MetricValue<Inf) %>%
  mutate(CollectionMethodCode="Bryophyte",
         Method="Bryo",
         MetricForm="Bryo", MetricGroup="Bryo", #Until I get lookup
         Metric= paste0("B_",Metric_short))


# 
# junk<-lm(MetricValue~StressLevel, 
#          data=arth_df_long %>% na.omit() %>%
#            filter(MetricValue>-Inf) %>%
#            left_join(ref_sites %>% select(StationCode, StressLevel)))
# junk<-t.test(
#   MetricValue~StressLevel, 
#   data=
#     arth_df_long %>% 
#     filter(MetricValue>-Inf) %>%
#     inner_join(
#       ref_sites %>% 
#         select(StationCode, StressLevel) ) %>%
#     filter(StressLevel %in% c("Low","High"))
# )
# junk %>% broom::glance()
# 
# junk2<-junk %>% broom::glance()
# junk2$f
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmodes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

bio_data_df<-arth_df_long %>%
  bind_rows(bryo_df_long) %>%
  filter(MetricValue>-Inf) %>%
  filter(!str_detect(Metric_short, "_KRich")) %>%
  filter(!str_detect(Metric_short, "_KAbund")) %>%
  left_join(ref_sites %>% select(StationCode, RefStatusFinal)) %>%
  mutate(RefStatus.f = factor(RefStatusFinal, levels=c("Low","Med","High")),
         RefStatus.f2 =case_when(RefStatusFinal=="Low"~"Low",T~"MedHigh") %>%
           factor(levels=c("Low","MedHigh"))
         )

metric_summary<-bio_data_df  %>%
  group_by(Method, Metric, Metric_short, MetricGroup, MetricForm) %>%
  summarise(    n = length(MetricValue),
                n_value = sum(!is.na(MetricValue)),
                n_missing = sum(is.na(MetricValue) ),
                n_ref = MetricValue[RefStatusFinal=="Low"] %>% length(),
                n_int = MetricValue[RefStatusFinal=="Med"] %>% length(),
                n_high = MetricValue[RefStatusFinal=="High"] %>% length(),
                
                mean_ref = MetricValue[RefStatusFinal=="Low"] %>% mean(na.rm=T),
                mean_int = MetricValue[RefStatusFinal=="Med"] %>% mean(na.rm=T),
                mean_high = MetricValue[RefStatusFinal=="High"] %>% mean(na.rm=T),
                
                sd_ref = MetricValue[RefStatusFinal=="Low"] %>% sd(na.rm=T),
                sd_int = MetricValue[RefStatusFinal=="Med"] %>% sd(na.rm=T),
                sd_high = MetricValue[RefStatusFinal=="High"] %>% sd(na.rm=T),
                
                response_direction = case_when(mean_ref>mean_high~"Decrease",
                                               mean_ref<mean_high~"Increase",
                                               T~"No response"),
                
                unique_values = unique(MetricValue) %>% length(),
                
                
                PctDom = sum(MetricValue %in% getmode(MetricValue))/length(MetricValue),
                
                
                # F_stat = lm(MetricValue~RefStatusFinal) %>%
                #   broom::glance() %>%
                #   select(statistic) %>% 
                #   unlist(),
                # 
                # t_stat = t.test(MetricValue~RefStatusFinal,
                # data= .%>% filter(RefStatusFinal!="Med")) %>%
                # broom::glance() %>%
                # select(statistic) %>%
                # unlist()
  ) %>%
  
  ungroup() 

# bio_data_df_long<-arth_df_long %>%
# bind_rows(bryo_df_long) %>%
# filter(MetricValue>-Inf) %>%
# left_join(ref_sites %>% select(StationCode, RefStatusFinal)) 

metric_summary$t_stat <-sapply(1:nrow(metric_summary), function(i){
  vals.i=metric_summary$unique_values[i]
  n_ref.i = metric_summary$n_ref[i]
  n_high.i = metric_summary$n_high[i]
  if(vals.i==1 | n_ref.i <=1 | n_high.i <=1)
    0
  else
  {
    met.i = metric_summary$Metric[i]
    # print(met.i)
    xdf =  bio_data_df %>%
      # arth_df_long %>%
      # bind_rows(bryo_df_long) %>%
      # left_join(ref_sites %>% select(StationCode, RefStatusFinal)) %>%
      filter(MetricValue>-Inf) %>%
      filter(Metric==met.i) %>%
      filter(RefStatusFinal %in% c("Low","High"))
    t_xdf = t.test(MetricValue~RefStatusFinal, data = xdf)
    t_xdf$statistic
  }
})



# metric_summary$n_meth<-sapply(1:nrow(metric_summary), function(i){
#   met.i=metric_summary$Metric[i]
#   meth.i=metric_summary$Method[i]
#   bio_data_df %>%
#     filter(Method==meth.i) %>%
#     select(StationCode, SampleDate) %>%
#     unique() %>%
#     nrow()
#   })

metric_summary$PassScreens<-metric_summary$PctDom<.95 & abs(metric_summary$t_stat)>=2 & metric_summary$n_missing==0

library(randomForest)

#Trap metrics RF
trap_mets<-metric_summary$Metric[metric_summary$Method=="Trap" & metric_summary$PassScreens & metric_summary$n_missing==0]
trap_rf_dat<-bio_data_df %>%
  filter(Metric %in% trap_mets) %>%
  filter(!is.na(RefStatus.f)) %>%
  select(StationCode, SampleDate, Metric, MetricValue, RefStatus.f) %>%
  pivot_wider(names_from=Metric, values_from=MetricValue) %>%
  select(RefStatus.f, all_of(trap_mets))
trap_rf_dat<-trap_rf_dat[, colSums(is.na(trap_rf_dat)) == 0] #Get rid of metrics with NA values
skimr::skim(trap_rf_dat)
set.seed(1)
trap_rf<-randomForest(RefStatus.f~.,
                      data=trap_rf_dat %>% filter(RefStatus.f %in% c("Low","High")) %>% droplevels(),
                      importance=T, proximity=T)
varImpPlot(trap_rf)


#Veg metrics RF
veg_mets<-metric_summary$Metric[metric_summary$Method=="Veg" & metric_summary$PassScreens & metric_summary$n_missing==0]
veg_rf_dat<-bio_data_df %>%
  filter(Metric %in% veg_mets) %>%
  filter(!is.na(RefStatus.f)) %>%
  select(StationCode, SampleDate, Metric, MetricValue, RefStatus.f) %>%
  pivot_wider(names_from=Metric, values_from=MetricValue) %>%
  select(RefStatus.f, all_of(veg_mets))
veg_rf_dat<-veg_rf_dat[, colSums(is.na(veg_rf_dat)) == 0] #Get rid of metrics with NA values
skimr::skim(veg_rf_dat)
set.seed(1)
veg_rf<-randomForest(RefStatus.f~.,
                      data=veg_rf_dat %>% filter(RefStatus.f %in% c("Low","High")) %>% droplevels(),
                      importance=T, proximity=T)
varImpPlot(veg_rf)


#bryo metrics RF
bryo_mets<-metric_summary$Metric[metric_summary$Method=="Bryo" & metric_summary$PassScreens & metric_summary$n_missing==0]
bryo_rf_dat<-bio_data_df %>%
  filter(Metric %in% bryo_mets) %>%
  filter(!is.na(RefStatus.f)) %>%
  select(StationCode, SampleDate, Metric, MetricValue, RefStatus.f) %>%
  pivot_wider(names_from=Metric, values_from=MetricValue) %>%
  select(RefStatus.f, all_of(bryo_mets))
bryo_rf_dat<-bryo_rf_dat[, colSums(is.na(bryo_rf_dat)) == 0] #Get rid of metrics with NA values
skimr::skim(bryo_rf_dat)
set.seed(1)
bryo_rf<-randomForest(RefStatus.f~.,
                     data=bryo_rf_dat %>% filter(RefStatus.f %in% c("Low","High")) %>% droplevels(),
                     importance=T, proximity=T)
varImpPlot(bryo_rf)

rf_importance<-
  trap_rf %>% importance() %>%
  as_tibble() %>%
  mutate(Metric=trap_rf %>% importance() %>% row.names(),
         Method="Trap") %>%
  bind_rows(
    veg_rf %>% importance() %>%
      as_tibble() %>%
      mutate(Metric=veg_rf %>% importance() %>% row.names(),
             Method="Veg")
  ) %>%
  bind_rows(
    bryo_rf %>% importance() %>%
      as_tibble() %>%
      mutate(Metric=bryo_rf %>% importance() %>% row.names(),
             Method="Bryo")
  ) %>%
  rename(LowImp=Low, HighImp=High) %>%
  group_by(Method) %>%
  mutate(MDA_Rank = rank(MeanDecreaseAccuracy),
         Gini_Rank = rank(MeanDecreaseGini))

metric_summary2 <- metric_summary %>%
  left_join(rf_importance)



write_csv(metric_summary2, "NotForGit/metric_summary2.csv")


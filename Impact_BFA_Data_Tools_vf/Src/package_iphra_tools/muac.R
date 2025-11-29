if(!is.null(nut)){
    cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",2), collapse=""), " ", "MUAC / NUTRITION"))
    nut <-  nut %>% 
            mutate(fpc = nrow(hh_roster)/population_estimation )
            if(as.numeric( population_estimation ) > 10000){
                # On suppose que la population est assez grande pour que la correction de population finie ne soit pas importante.        
                nut_survey <- srvyr::as_survey_design(nut)
            } else {
                # On définit le fpc
               nut_survey <- srvyr::as_survey_design(nut, fpc = fpc)
            }
    
    ##Prevalence of acute malnutrition based on MUAC cut off's (and/or oedema) and by sex
    proportions <- c("gam_muac_noflag","mam_muac_noflag","sam_muac_noflag")
    results_prop <- data.frame()
                    for (i in proportions) {
                      results2 <- nut_survey %>% 
                                  srvyr::summarise(num_sample = sum(!!rlang::sym(i),  na.rm = TRUE),
                                                   Prop = srvyr::survey_mean(!!rlang::sym(i), na.rm = TRUE, vartype = "ci")) %>% 
                                  mutate(Name = i) %>% relocate(Name, .before = 1)
                      results_prop <- rbind(results_prop,results2)                  
                    }
    
    results_prop <- results_prop %>% 
      mutate(Prop = paste0("(",num_sample,") ",round(Prop*100,2), " / [",round(Prop_low*100,2), " - ",round(Prop_upp * 100,2),"] (95% C.I.)")) %>% 
      select(-c(Prop_low,Prop_upp,num_sample)) %>% 
      rename(All = "Prop")
    results_prop_sex <- data.frame()
    for (i in proportions) {
      results2 <- nut_survey %>% 
        group_by(sex) %>% 
        srvyr::summarise(num_sample = sum(!!rlang::sym(i), 
            na.rm = TRUE),
                         Prop = srvyr::survey_mean(!!rlang::sym(i), 
            na.rm = TRUE, vartype = "ci")) %>% 
        mutate(Name = i,
               sex = case_when(sex == 1~"Homme",
                               sex == 2~"Femme")) %>% 
        relocate(Name, .before = 1)
      results_prop_sex <- rbind(results_prop_sex,results2)
    }
    results_prop_sex <- results_prop_sex %>% 
      mutate(Prop_low = ifelse(Prop_low<0,0,Prop_low)) %>% 
      mutate(Prop = paste0("(",num_sample,") ",round(Prop*100,2), " / [",round(Prop_low*100,2), " - ",round(Prop_upp * 100,2),"] (95% C.I.)")) %>% 
      select(-c(Prop_low,Prop_upp,num_sample)) %>% 
      tidyr::pivot_wider(names_from = sex, values_from = Prop)
    
    results_prop_final <- results_prop %>% 
      left_join(results_prop_sex) %>% 
      mutate(AMN_MUAC_Indicator = case_when(Name == "gam_muac_noflag"~"Prevalence of global malnutrition\n(< 125 mm and/or oedema)",
                              Name == "mam_muac_noflag"~"Prevalence of moderate malnutrition\n(< 125 mm and >115 mm, no oedema)",
                              Name == "sam_muac_noflag"~"Prevalence of severe malnutrition\n(< 115 mm and/or oedema)")) %>% 
      select(-Name) %>% 
      relocate(AMN_MUAC_Indicator, .before= 1)
      
    
    
    ## Prevalence of acute malnutrition based on MUAC-for-age z-score cut off's (and/or oedema) and by sex
    proportions <- c("global_mfaz_noflag","moderate_mfaz_noflag","severe_mfaz_noflag")
    results_prop_mfaz <- data.frame()
    for (i in proportions) {
      results2 <- nut_survey %>% 
        srvyr::summarise(num_sample = sum(!!rlang::sym(i), 
            na.rm = TRUE),
                         Prop = srvyr::survey_mean(!!rlang::sym(i), 
            na.rm = TRUE, vartype = "ci")) %>% 
        mutate(Name = i) %>% 
        relocate(Name, .before = 1)
      results_prop_mfaz <- rbind(results_prop_mfaz,results2)
    }
    
    results_prop_mfaz <- results_prop_mfaz %>% 
                         mutate(Prop = paste0("(",num_sample,") ",round(Prop*100,2), " / [",round(Prop_low*100,2), " - ",round(Prop_upp * 100,2),"] (95% C.I.)")) %>% 
                         select(-c(Prop_low,Prop_upp,num_sample)) %>% 
                         rename(All = "Prop")
                         results_prop_mfaz_sex <- data.frame()
                      for (i in proportions) {
                        results2 <- nut_survey %>% 
                          group_by(sex) %>% 
                          srvyr::summarise(num_sample = sum(!!rlang::sym(i), 
                              na.rm = TRUE),
                                           Prop = srvyr::survey_mean(!!rlang::sym(i), 
                              na.rm = TRUE, vartype = "ci")) %>% 
                          mutate(Name = i,
                                 sex = case_when(sex == 1~"Homme",
                                                 sex == 2~"Femme")) %>% 
                          relocate(Name, .before = 1)
                        results_prop_mfaz_sex <- rbind(results_prop_mfaz_sex,results2)
                      }
                      results_prop_mfaz_sex <- results_prop_mfaz_sex %>% 
                        mutate(Prop_low = ifelse(Prop_low<0,0,Prop_low)) %>% 
                        mutate(Prop = paste0("(",num_sample,") ",round(Prop*100,2), " / [",round(Prop_low*100,2), " - ",round(Prop_upp * 100,2),"] (95% C.I.)")) %>% 
                        select(-c(Prop_low,Prop_upp,num_sample)) %>% 
                        tidyr::pivot_wider(names_from = sex, values_from = Prop)
                      
                      results_prop_final_mfaz <- results_prop_mfaz %>% 
                        left_join(results_prop_mfaz_sex) %>% 
                        mutate(AMN_MUAC_Indicator = case_when(Name == "severe_mfaz_noflag"~"Prevalence of severe malnutrition\n(< -3 MUAC-for-age z-score and/or oedema)",
                                                Name == "moderate_mfaz_noflag"~"Prevalence of moderate malnutrition\n(>= -3 and < -2 MUAC-for-age z-score, no oedema)",
                                                Name == "global_mfaz_noflag"~"Prevalence of global malnutrition\n(< -2 MUAC-for-age z-score and/or oedema)")) %>%  select(-Name) %>% 
                        relocate(AMN_MUAC_Indicator, .before= 1)
      
    mean <- c("muac_noflag","mfaz_noflag")
    
    results_mean <- data.frame()
    
    # results <- data.frame()
    for (i in mean) {
      results2 <- nut_survey %>% 
                  srvyr::summarise(Mean = srvyr::survey_mean(!!rlang::sym(i), na.rm = TRUE, vartype = "ci")) %>% 
                  mutate(Name = i) %>%
                  mutate_at(vars(starts_with("Mean_")),~round(.,2)) %>% 
                  relocate(Name, .before = 1)
                  results_mean <- rbind(results_mean,results2)
    }
    
    
    
    ##Prevalence of acute malnutrition based on MUAC cut off's (and/or oedema) and by Age_group
    proportions <- c("gam_muac_noflag","mam_muac_noflag","sam_muac_noflag")
    results_prop_age_group <- data.frame()
    for (i in proportions) {
      results2 <- nut_survey %>% 
                  mutate(age_group = as.character(cut(as.numeric(age_months), 
                                              breaks = c(5,17,29,41,53,59),
                                              labels = c("06-17", "18-29", "30-41", "42-53", "54-59")))
                         ) %>% 
                  group_by(age_group) %>% 
                  srvyr::summarise(num_sample = sum(!!rlang::sym(i), 
                                                    na.rm = TRUE),
                                                    Prop = srvyr::survey_mean(!!rlang::sym(i), 
                                                    na.rm = TRUE, vartype = "ci")
                                   ) %>% 
                  mutate(Name = i) %>% 
                  relocate(Name, .before = 1)
                results_prop_age_group <- rbind(results_prop_age_group,results2)
    }
    ## Total age_Group
    age_group <- nut_survey %>% 
      mutate(age_group = as.character(cut(as.numeric(age_months), 
                                  breaks = c(5,17,29,41,53,59),
                                  labels = c("06-17", "18-29", "30-41", "42-53", "54-59")
                          ))) %>% 
                          group_by(age_group) %>% 
                          srvyr::summarise(Total = n()) %>% 
                          filter(!is.na(age_group))
    
    ## calculate eodema
    edema <-  nut_survey %>% 
              mutate(age_group = as.character(cut(as.numeric(age_months), 
                                          breaks = c(5,17,29,41,53,59),
                                          labels = c("06-17", "18-29", "30-41", "42-53", "54-59")))
                                  ) %>% 
                                  group_by(age_group) %>% 
                                  srvyr::summarise(edema_total = sum(flag_edema_pitting,na.rm=T)) %>% 
                                  filter(!is.na(age_group))
    
    num_sample <- results_prop_age_group %>% 
                  filter(!is.na(age_group)) %>% 
                  group_by(age_group) %>% 
                  summarise(num_sample = sum(num_sample,na.rm=T)) %>% 
                  left_join(age_group) %>% 
                  left_join(edema) %>% 
                  mutate(Normal = paste0(Total-num_sample, " / ", round(((Total-num_sample)/Total) *100,2),"%")) %>% 
                  mutate(Oedema = paste0(edema_total, " / ", round(((edema_total)/Total) *100,2),"%")) %>% 
                  select(-c(num_sample,edema_total))
    
    
    results_prop_age_group_final <- results_prop_age_group %>% 
                                    filter(!is.na(age_group)) %>% 
                                    mutate(Prop_low = ifelse(Prop_low<0,0,Prop_low)) %>% 
                                    mutate(Prop = paste0("(",num_sample,") ",round(Prop*100,2), " / [",round(Prop_low*100,2), " - ",round(Prop_upp * 100,2),"] (95% C.I.)")) %>% 
                                    select(-c(Prop_low,Prop_upp,num_sample))%>% 
                                    tidyr::pivot_wider(names_from = Name, values_from = Prop) %>% 
                                    left_join(num_sample) %>% 
                                    relocate(Total,.before = 2)
}

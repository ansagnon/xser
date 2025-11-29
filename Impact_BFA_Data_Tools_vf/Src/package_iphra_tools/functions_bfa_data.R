## Transformer les variables de santé mentale en variable catégorielle nominale
## L'objectif ici est d'afficher ces variables dans un ordre qui facilite la lecture 
# 1- Jamais
# 2- Rarement
# 3- De temps en temps
# 4- La plupart du temps
# 5- Tout le temps 

# “Parfois ou plus” = temps_entemps, pluspart_temps, tout_temps
# “Souvent ou plus” = pluspart_temps, tout_temps
# bfa_levels <- c('jamais', 'rarement', 'temps_entemps', 'pluspart_temps', 'tout_temps')
# 

# Ordre pour comparaison
bfa_levels_ordered <- c('jamais', 'rarement', 'temps_entemps', 'pluspart_temps', 'tout_temps')

data.list$main <- data.list$main %>%
                  mutate(
                        # Au moins un comportement "parfois ou plus"
                        sante_mentale_globale_parfois = apply(
                          select(., starts_with("sante_mentale_")), 1, 
                          function(x) { any(match(x, bfa_levels_ordered) >= match("temps_entemps", bfa_levels_ordered))}
                        ),
                        
                        # Au moins un comportement "souvent ou plus"
                        sante_mentale_globale_souvent = apply(
                          select(., starts_with("sante_mentale_")), 1, 
                          function(x) { any(match(x, bfa_levels_ordered) >= match("pluspart_temps", bfa_levels_ordered))}
                        ), .after = sante_mentale_6
                  ) %>% 
                  mutate(
                        sante_mentale_globale_parfois = ifelse(sante_mentale_globale_parfois, "Parfois ou plus", "Inferieur à Parfois"),
                        sante_mentale_globale_souvent = ifelse(sante_mentale_globale_souvent, "Souvent ou plus", "Inferieur à Souvent")
                  )


# View( data.list$main %>% select(all_of(c("sante_mentale_1", "sante_mentale_2", "sante_mentale_3", "sante_mentale_4", "sante_mentale_5", "sante_mentale_6",
#                                       "sante_mentale_globale_parfois", "sante_mentale_globale_souvent"))
#                                 ) )


## L'idée de cette fonction est de récupérer le nom de colonne dans chaque sheet
get_person_id_col_name <- function(sheet_name) {
      dplyr::case_when(
             sheet_name == "hh_roster" ~ "person_id",
             sheet_name == "ind_health" ~ "health_person_id",
             sheet_name == "child_nutrition" ~ "child_person_id",
             TRUE ~ NA_character_  
      )
}



## Formatage des indicateurs standard
## 
convert_table_to_results <- function(df, analysis_type = "Indicateur standard") {
  
  # Cas particuliers | fsl_hdds_cat
  if(names(df)[1]  %in%  c("fsl_hdds_cat", "fsl_hhs_cat_ipc", "fsl_fc_phase", "fsl_hhs_cat_ipc") ) {

    return(
           
       df %>% rename(
                    analysis_var_value = names(df)[1] ,
                    n = num_samples ,
                    stat = Percentage
              ) %>%
              mutate(
                    analysis_var = as.character(names(df)[1]) ,
                    analysis_type = analysis_type,
                    group_var = NA_character_,
                    group_var_value = NA_character_,
                    stat_low = NA_character_,
                    stat_upp = NA_character_,
                    n_total = NA_real_,
                    n_w = NA_real_,
                    n_w_total = NA_real_,
                    analysis_key = NA_character_
              ) %>%
              select( all_of( c("analysis_type", "analysis_var", "analysis_var_value", "group_var", "group_var_value",
                              "stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total","analysis_key"))
                      )           
           )
  }
  
  
  # Tableaux de moyenne
  if (all(c("Mean", "Mean_low", "Mean_upp") %in% colnames(df))) {
    return(
      df %>%
        transmute(
          analysis_type = analysis_type,
          analysis_var = !!sym(names(.)[1]),
          analysis_var_value = "mean",
          group_var = NA_character_,
          group_var_value = NA_character_,
          stat = as.character(Mean),
          stat_low = as.character(Mean_low),
          stat_upp = as.character(Mean_upp),
          n = NA_real_,
          n_total = NA_real_,
          n_w = NA_real_,
          n_w_total = NA_real_,
          analysis_key = NA_character_
        )
    )
  }


  # Tableaux de proportions
  if (ncol(df) > 2) {

    df2 <- df %>%
      # On ne convertit pas les colonnes — on laisse les types d'origine
      pivot_longer(
        cols = -1,
        names_to = "analysis_var_value",
        values_to = "stat"
      ) %>%
      rename(analysis_var = 1) %>%
      mutate(
        analysis_type = analysis_type,
        group_var = NA_character_,
        group_var_value = NA_character_,
        stat = stat,
        stat_low = as.character(NA_real_),
        stat_upp = as.character(NA_real_),
        n = NA_real_,
        n_total = NA_real_,
        n_w = NA_real_,
        n_w_total = NA_real_,
        analysis_key = NA_character_
      ) %>%
      select(
        analysis_type, analysis_var, analysis_var_value,
        group_var, group_var_value,
        stat, stat_low, stat_upp,
        n, n_total, n_w, n_w_total,
        analysis_key
      )

    return(df2)
  }

  # ---- CAS : Aucun format reconnu ----
  stop("Format de table non reconnu dans convert_table_to_results().")
}

####################################################################################
# Formatage des résultats muac et mfaz
# 

## Reformatage des résultats | MUAC & MFAZ
pivot_results_full <- function(df_name) {
  # df_name = "results_prop"
  print(df_name)
  pivot_db <- get(df_name)
  key_col <- names(pivot_db)[1]
  cols    <- names(pivot_db)[-1]

  # Cas 1 - moyenne
  if (all(c("Mean", "Mean_low", "Mean_upp") %in% cols)) {
    result <- pivot_db %>%
              mutate(across(everything(), as.character)) %>%
              transmute(
                    analysis_var = !!sym(key_col),
                    analysis_var_value = "Mean",
                    stat     = Mean,
                    stat_low = Mean_low,
                    stat_upp = Mean_upp
              )

  } 
  else if(all(c("Prop", "Prop_low", "Prop_upp") %in% cols)) {
    result <- pivot_db %>%
              mutate(across(everything(), as.character)) %>%
              transmute(
                    analysis_var = !!sym(key_col),
                    analysis_var_value = !!sym(names(pivot_db)[2]) ,
                    stat     = Prop,
                    stat_low = Prop_low,
                    stat_upp = Prop_upp,
                    n = num_sample
              )

  } else {
    # Cas 2 - proportion
    result <- pivot_db %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
            cols = all_of(cols),
            names_to = "group_var_value",
            values_to = "stat"
      ) %>%
      rename(analysis_var_value = !!sym(key_col)) %>%
      mutate(
            Indicator = df_name,
            analysis_var = ifelse(as.character(key_col) != "Name", as.character(key_col), NA_character_ ),
            group_var = "sex",
            stat_low = NA,
            stat_upp = NA
      ) %>%
      select(Indicator, analysis_var, analysis_var_value, group_var, group_var_value, stat, stat_low, stat_upp)
  }

  return(result)
}
#
# analysis_supp_mfaz <- map_df(list_analysis_supp_mfaz, pivot_results_full)


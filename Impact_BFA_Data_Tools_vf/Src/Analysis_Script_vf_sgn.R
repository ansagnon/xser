# 1- Préparation de l'espace de travail ----

# Repertoire actuel de travial
getwd()

# Chargement des librairies necessaires

## Chargement du package analysis tool de impact Initiatives :Infos disponible ici : https://github.com/impact-initiatives/analysistools
# devtools::install_github("impact-initiatives/impactR4PHU")
# devtools::install_github("impact-initiatives/analysistools")
# devtools::install_github("https://github.com/impact-initiatives/addindicators", force = TRUE)

 
## Chargement des librairies disponible ici
pacman::p_load("addindicators",
               "analysistools",
               "openxlsx", 
               "dplyr",
               "srvyr",
               "tidyr",
               "stringr",
               "purrr"
               )

options(scipen = 999)
source("Src/package_iphra_tools/functions_phu.R")

# Contenu du répertoire actuel de travail
dir("Input/")

# Chargement de la base de données 
# data.list <- list(
#           main = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "main"),
#           hh_roster = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "hh_roster"),
#           ind_health = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "ind_health"),
#           child_nutrition = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "child_nutrition")
# )
# population_estimation <- 27630 # Non_PDI

data.list <- list(
          main = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "main"),
          hh_roster = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "hh_roster"),
          ind_health = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "ind_health"),
          child_nutrition = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "child_nutrition")
)
population_estimation <-41197 # PDI


# Chargement de l'outil kobo
getSheetNames("Input/Kobo_nutrisafe.xlsx")
kobo_survey <- read.xlsx(xlsxFile = "Input/Kobo_nutrisafe.xlsx", sheet = "survey") %>% tibble::as_tibble()
kobo_survey <- kobo_survey %>%
               separate(col = "type", into = c("type", "list_name"), sep = " ") %>% 
               select(type, list_name, name, label)
 
kobo_choices <- read.xlsx(xlsxFile = "Input/Kobo_nutrisafe.xlsx", sheet = "choices") %>% 
                group_by(list_name) %>%
                summarize( names = list(name), labels = list(label))


kobo_tool <- kobo_survey %>% left_join(kobo_choices, by = "list_name")


# Chargement de l'échantillonnage
getSheetNames("Input/Point_échantillonnage_final_10_12_2024.xlsx")
sampling <-  read.xlsx(xlsxFile = "Input/Point_échantillonnage_final_10_12_2024.xlsx", sheet = 1)

# Création de variables supplémentaire nécessaires pour l'analyse de données
source("Src/package_iphra_tools/format_dataset.R")
source("Src/package_iphra_tools/muac.R")
source("Src/package_iphra_tools/functions_bfa_data.R")
source("Src/package_iphra_tools/add_hwise.R")



## Indice de sécurité Hydrique
data.list$main <- add_hwise(data = data.list$main,
                      hwise_worry_col = "wash_wise_worry", hwise_plans_col = "wash_wise_plans", hwise_hands_col = "wash_wise_hands", hwise_drink_col = "wash_wise_drink",
                      # hwise_interrupt_col = NULL, hwise_clothes_col = NULL, hwise_food_col = NULL, hwise_body_col = NULL, hwise_angry_col = NULL,
                      # hwise_sleep_col = NULL, hwise_none_col = NULL, hwise_shame_col = NULL,
                      never_val = "never", rarely_val = "rarely", sometimes_val = "sometimes", often_val = "often", always_val = "always"
)
table(data.list$main$wash_wise_drink)

# Chargement du dap
getSheetNames("Input/dap_nutrisafe.xlsx")
# dap <- read.xlsx(xlsxFile = "Input/dap_nutrisafe.xlsx", sheet = "survey")
dap <- read.xlsx(xlsxFile = "Input/dap_nutrisafe.xlsx", sheet = "template")

# Chargement de la liste des variables | Analyses Supplémentaires FSL
## Il s'agit de variables pour lesquels nous ferons des analyses bien que ces variables soit absentes du dap
list_analysis_supp <- c('hdds_table', 'hdds_cat_table', 'hdds_score_table', 'hhs_table', 'hhs_tabl_Freq',
                        'hhs_cat_table',  'fc_phase_table')


var_lookup <- tibble()
for (sheet in names(data.list)){
    var_lookup <- rbind(var_lookup, tibble(variable = names(data.list[[sheet]]), datasheet = sheet)) %>% 
                  distinct(variable, .keep_all = T) %>% filter(stringr::str_detect(variable, "(/)|(___)", T))
}


if(!"datasheet" %in% names(dap)) dap <- dap %>% left_join(var_lookup, by = c("analysis_var" = "variable") )


missing_sheets <- dap %>% filter(is.na(datasheet)) %>% pull(analysis_var)
if(length(missing_sheets) > 0) {
    warning("These variables are missing from data: ", paste(missing_sheets))
    dap <- dap %>% filter(!analysis_var %in% missing_sheets)
} 
rm(missing_sheets, kobo_survey, kobo_choices)
 
# 2- Plan échantillonnage & calcul de poids ----

## Il s'agit d'un sondage aléatoire en grappe et à deux degrés
## Etape 1 : Tirage des hexagones à enquêter parmi les hexagones du centre ville de commune de Piela
## Etape 2 : Tirage des ménages parmi les ménages des hexagones sélectionnes à l'étape 1
## Etape 3 : prob_inclusion_final & calcul de poids

# nbr_hexagone_tire <- 23
# nbr_hexagone_total <- 87
# # nbr_pop_piela <- 79448
# 
# 
# sampling <- sampling %>%
#             mutate(
#                 prob_inclusion_hexagone = nbr_hexagone_tire / nbr_hexagone_total,
#                 prob_inclusion_menage = nbr_menage_tire / nbr_menage,
#                 prob_inclusion_final = prob_inclusion_hexagone * prob_inclusion_menage,
#                 weights = 1 / prob_inclusion_final,
#                 rowname = as.character(rowname)
#             )
# 
# 
# data.list$main <- data.list$main %>%
#                   left_join( sampling %>% select(rowname, nbr_pop, weights), by = c("cluster" = "rowname" ) ) %>% 
#                   # On accorde un poids de 1 aux PDI (echantillonnage non probabiliste) meme si on reste prudent dans l'interprétation
#                   mutate( weights = ifelse(is.na(weights), 1, weights), nbr_pop = ifelse(is.na(nbr_pop), 1, nbr_pop)  )
# 
# 
# # count(data.list$main, residency_status, cluster, weights )
# # View(data.list$main )
# 
# 
# ## 2-a Prise en compte des poids
# 
# 
# ## 2-b Prise en compte du plan d'échantillonnage
# design <- list(
#           main = data.list$main %>% as_survey_design(ids = cluster, weights = weights, pps = FALSE, variance = "HT" ),
#           hh_roster = data.list$hh_roster %>% as_survey_design(),
#           ind_health = data.list$ind_health %>%  as_survey_design(),
#           child_nutrition = data.list$child_nutrition %>% as_survey_design()
# )

design <- list(
          main = data.list$main %>% as_survey_design(),
          hh_roster = data.list$hh_roster %>% as_survey_design(),
          ind_health = data.list$ind_health %>%  as_survey_design(),
          child_nutrition = data.list$child_nutrition %>% as_survey_design()
)




## 3- Analyse de données ----

results <- NULL
for (i in 1:nrow(dap)) {
    print(i)
    sub_dap <- dap %>% slice(i)
    id  <- sub_dap$id
    Secteur <- sub_dap$secteur
    Sous_secteur <- sub_dap$sous_secteur
    analysis_var <- sub_dap$analysis_var
    analysis_var_sheet <- sub_dap$datasheet
    analysis_type <- sub_dap$analysis_type
    Indicator <- sub_dap$Indicator
    group_var <- sub_dap$group
    group_var_sheet <- var_lookup %>% filter(variable == group_var) %>% pull(datasheet) %>% unique()
    level <- sub_dap$level
    
    ## Initialisation du survey design a utiliser
    sub_data.list <- design
    ## Ne considérer pour l'analyse que les réponses différentes de NA
    sub_data.list[[analysis_var_sheet]]$variables <- sub_data.list[[analysis_var_sheet]]$variables %>% filter(!is.na(!!sym(analysis_var)) & !!sym(analysis_var) != "nsp") 
    ## Ne faire aucune analyse si le dataframe privé de NA est vide
    if(is.null(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]]) || length(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]]) == 0) next
    
    
    # -------------------- Gestion des jointures
    if(length(group_var_sheet) == 1 &&  !is.na(group_var_sheet) && group_var_sheet != analysis_var_sheet){

        # Jointure entre une variable située dans la feuille main et une variable située dans une feuille de boucle individuelle
        if(analysis_var_sheet == "main" || group_var_sheet == "main" ) {
            sub_database <- sub_data.list[[group_var_sheet]]$variables %>% select(all_of(c( "uuid", group_var ))) %>% 
                            left_join(sub_data.list[[analysis_var_sheet]]$variables %>% select(all_of(c( "uuid", analysis_var ))), 
                                      by = "uuid", keep = FALSE)
        } 
        
        # Jointure entre deux variables situées dans des feuilles de boucles individuelles
        else {
            person_id_1 <- get_person_id_col_name(sheet_name = analysis_var_sheet)
            person_id_2 <- get_person_id_col_name(sheet_name = group_var_sheet)
            
            join_cols <- c("uuid" = "uuid")
            join_cols[person_id_1] <- person_id_2
            
            # sub_database <- sub_data.list[[analysis_var_sheet]]$variables %>% 
            #                 select(uuid, all_of(person_id_1), all_of(analysis_var)) %>% 
            #                 left_join( sub_data.list[[group_var_sheet]]$variables %>% select(uuid, all_of(person_id_2), all_of(group_var)), 
            #                            by = join_cols, keep = FALSE
            #                 )
            
            # Le script format_dataset.R a permis de créer daans chacune des feuilles les variables de grouping.
            # Ce qui fait que la jointure n'est plus necessaire.
            # Aussi la variable age group n'a pas la meme valeur dans les differentes sheets.
            sub_database <- sub_data.list[[analysis_var_sheet]]$variables %>% 
                            select(uuid, all_of(person_id_1), all_of(analysis_var), all_of(group_var))         
            
        }
        # Dans tous les cas 
        sub_data.list[['multiple_sheets']] <- as_survey_design(.data = sub_database)
        analysis_var_sheet <- "multiple_sheets"         
    }
    
    # -------------------  Analyse de données  
    if(analysis_type == "mean"){
          sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]] <- as.numeric(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]])
          results <- bind_rows( results, 
                                create_analysis_mean(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level) )
    } 
    else if(analysis_type == "median") {
          sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]] <- as.numeric(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]])
          results <- bind_rows( results, 
                                create_analysis_median(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level))
    } 
    else if(analysis_type == "select_one"){
          # A faire uniquement si nous disposons de la liste complète des modalités dans l'outil kobo
          # Dans ce cas transformer la variable en factor pour afficher les modalités dans le meme ordre que celui utilisé par kobo
          # Et afficher les labels de modalités plutot que les names
          kobo_var_info <- kobo_tool %>% filter(name == analysis_var)
          if(nrow(kobo_var_info) == 1){
                  if( !is.null(kobo_var_info$names[[1]]) & !is.null(kobo_var_info$labels[[1]]) & (length(kobo_var_info$names[[1]]) == length(kobo_var_info$labels[[1]])) ){
                          ## Transformer en facteur pour afficher les modalités de réponses dans le meme ordre que celui utilisé dans l'outil kobo
                          sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]] <- factor(x = sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]], 
                                                                                                    levels = kobo_var_info$names[[1]] ,
                                                                                                    labels = kobo_var_info$labels[[1]] ,
                                                                                                    ordered = FALSE
                                                                                                  ) 
                  }
          }         
          results <- bind_rows( results, 
                                create_analysis_prop_select_one( design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level) %>% 
                                mutate(analysis_var_value = as.character(analysis_var_value))
                               )
    } 
    else if(analysis_type == "select_multiple"){
          results_SM <- create_analysis_prop_select_multiple(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level, sm_separator = "/")
          kobo_var_info <- kobo_tool %>% filter(name == analysis_var)
          
          # Afficher les labels de variable plutot que les modalités de variable
          if (nrow(kobo_var_info) == 1) {
                dict <- tibble( analysis_var_value = kobo_var_info$names[[1]], analysis_var_label = kobo_var_info$labels[[1]])
              
                results_SM <- results_SM %>%
                              left_join(dict, by = "analysis_var_value") %>%
                              mutate(analysis_var_value = analysis_var_label) %>% 
                              select(-analysis_var_label)
          }
        
          results <- bind_rows(results, results_SM)
    } 
    else if(analysis_type == "ratio") {
          results <- bind_rows( results, 
                                create_analysis_ratio( design = sub_data.list[[analysis_var_sheet]], group_var = NA, analysis_var_numerator, analysis_var_denominator,numerator_NA_to_0 = TRUE,filter_denominator_0 = TRUE,level = 0.95))
    } 
}


# Analyse supplémentaires liées au FSL2
results_analysis_supp <- NULL
for (variable in list_analysis_supp) {
    df <- get(variable)
    results_analysis_supp <- bind_rows( results_analysis_supp, convert_table_to_results(df))
}

# Analyse supplémentaire mfaz & muac

list_analysis_supp_mfaz <- c("results_prop_final", "results_prop_age_group_final", 
                             "results_prop_final_mfaz", "results_prop_mfaz_sex", "results_mean")

analysis_supp_mfaz <- map_df(list_analysis_supp_mfaz, pivot_results_full)


results_vf <- bind_rows( results %>% mutate(across(everything(), ~ ifelse(is.na(.x), NA, .x))) %>% 
                                     mutate(stat = paste0(as.character(round(stat * 100, 2)), "%"),
                                            stat_low = paste0(as.character(round(stat_low * 100, 2)), "%"),
                                            stat_upp = paste0(as.character(round(stat_upp * 100, 2)), "%")
                                     ),
                                     results_analysis_supp,
                                     analysis_supp_mfaz
                                     )                         



# write.xlsx(x = results_vf, file = 'Output/resultats_analyse_nutrisafe_non_pdi_test.xlsx')
write.xlsx(x = results_vf, file = 'Output/resultats_analyse_nutrisafe_pdi_test.xlsx')







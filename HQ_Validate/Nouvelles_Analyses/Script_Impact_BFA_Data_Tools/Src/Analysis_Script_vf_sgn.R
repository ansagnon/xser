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
               "srvyr"
               )

options(scipen = 999)
source("Src/package_iphra_tools/functions_phu.R")

# Contenu du répertoire actuel de travail
dir("Input/")

# Chargement de la base de données 
library(openxlsx)

data.list <- list(
          main = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "main"),
          hh_roster = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "hh_roster"),
          ind_health = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "ind_health"),
          child_nutrition = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_non_pdi.xlsx", sheet = "child_nutrition")
)

# data.list <- list(
#           main = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "main"),
#           hh_roster = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "hh_roster"),
#           ind_health = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "ind_health"),
#           child_nutrition = read.xlsx("Input/BFA_ACF_IPHRA_final_anonymized_data241218_pdi.xlsx", sheet = "child_nutrition")
# )



# Chargement de l'outil kobo
getSheetNames("Input/Kobo_nutrisafe.xlsx")
kobo_tool <- read.xlsx(xlsxFile = "Input/Kobo_nutrisafe.xlsx", sheet = "survey")

# Chargement de l'échantillonnage
getSheetNames("Input/Point_échantillonnage_final_10_12_2024.xlsx")
sampling <-  read.xlsx(xlsxFile = "Input/Point_échantillonnage_final_10_12_2024.xlsx", sheet = 1)

# Création de variables supplémentaire nécessaires pour l'analyse de données
source("Src/package_iphra_tools/format_dataset.R")


# Chargement du dap
getSheetNames("Input/dap_nutrisafe.xlsx")
# dap <- read.xlsx(xlsxFile = "Input/dap_nutrisafe.xlsx", sheet = "survey")
dap <- read.xlsx(xlsxFile = "Input/dap_nutrisafe.xlsx", sheet = "template")
  
unique(data.list$child_nutrition$child_sex)

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
#rm(missing_sheets)

get_person_id_col_name <- function(sheet_name) {
      dplyr::case_when(
        sheet_name == "hh_roster" ~ "person_id",
        sheet_name == "ind_health" ~ "health_person_id",
        sheet_name == "child_nutrition" ~ "child_person_id",
        TRUE ~ NA_character_  
      )
}

 
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
    Secteur <- sub_dap$Secteur
    Sous_secteur <- sub_dap$Sous_secteur
    analysis_var <- sub_dap$analysis_var
    analysis_var_sheet <- sub_dap$datasheet
    analysis_type <- sub_dap$analysis_type
    Indicator <- sub_dap$Indicator
    group_var <- sub_dap$group
    group_var_sheet <- var_lookup %>% filter(variable == group_var) %>% pull(datasheet) %>% unique()
    level <- sub_dap$level
    sub_data.list <- design
    sub_data.list[[analysis_var_sheet]] <- sub_data.list[[analysis_var_sheet]] %>% filter(!is.na(.data[[analysis_var]]))
    if (is.null(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]]) || length(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]]) == 0) next


    
    # -------------------- Gestion des jointures
    
    if(length(group_var_sheet) == 1 &&  !is.na(group_var_sheet) && group_var_sheet != analysis_var_sheet){
        # vérifier si l'une des deux feuilles est 'main'
        if(analysis_var_sheet == "main" || group_var_sheet == "main" ) {
            sub_database <- sub_data.list[[group_var_sheet]]$variables %>% select(all_of(c( "uuid", group_var ))) %>% 
                            left_join(sub_data.list[[analysis_var_sheet]]$variables %>% select(all_of(c( "uuid", analysis_var ))), 
                                      by = "uuid", keep = FALSE)
            
        } else {
            person_id_1 <- get_person_id_col_name(sheet_name = analysis_var_sheet)
            person_id_2 <- get_person_id_col_name(sheet_name = group_var_sheet)   
            join_cols <- c("uuid" = "uuid")         # toujours sur uuid
            join_cols[person_id_1] <- person_id_2  # jointure dynamique sur les identifiants
            
            
            sub_database <- sub_data.list[[analysis_var_sheet]]$variables %>% 
                            select(all_of(c("uuid", person_id_1, analysis_var ))) %>% 
                            left_join(sub_data.list[[group_var_sheet]]$variables %>% select(all_of(c("uuid", person_id_2, group_var ))), 
                                      by = join_cols , keep = FALSE) 
        }
        
        sub_data.list[['multiple_sheets']] <- as_survey_design(.data = sub_database)
        analysis_var_sheet <- "multiple_sheets"         
    }

        
    # -------------------    
    
    if(analysis_type == "mean"){
          sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]] <- as.numeric(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]])
          results <- bind_rows( results, 
                                create_analysis_mean(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level) )
    } else if(analysis_type == "median") {
          sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]] <- as.numeric(sub_data.list[[analysis_var_sheet]]$variables[[analysis_var]])
          results <- bind_rows( results, 
                                create_analysis_median(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level))
    } else if(analysis_type == "select_one"){
          results <- bind_rows( results, 
                                create_analysis_prop_select_one( design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level))
    } else if(analysis_type == "select_multiple"){
          results <- bind_rows(  results, 
                                 create_analysis_prop_select_multiple(design = sub_data.list[[analysis_var_sheet]], group_var = group_var, analysis_var = analysis_var, level = level, sm_separator = "/"))
    } else if(analysis_type == "ratio") {
          results <- bind_rows( results, 
                                create_analysis_ratio( design = sub_data.list[[analysis_var_sheet]], group_var = NA, analysis_var_numerator, analysis_var_denominator,numerator_NA_to_0 = TRUE,filter_denominator_0 = TRUE,level = 0.95))
    } 
}



results <- results %>%
           mutate(across(everything(), ~ {
              x <- .
              x[is.na(x)] <- NA
              x
          }))

# 
write.xlsx(x = results, file = 'Output/resultats_analyse_nutrisafe_non_pdi_test.xlsx')
# write.xlsx(x = results, file = 'Output/resultats_analyse_nutrisafe_pdi_test.xlsx')





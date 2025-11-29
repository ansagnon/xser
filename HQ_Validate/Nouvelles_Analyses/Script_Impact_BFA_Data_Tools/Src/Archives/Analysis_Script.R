# 1- Préparation de l'espace de travail ----

# Repertoire actuel de travial
getwd()

# Chargement des librairies necessaires

## Chargement du package analysis tool de impact Initiatives :Infos disponible ici : https://github.com/impact-initiatives/analysistools
# devtools::install_github("impact-initiatives/impactR4PHU")
# devtools::install_github("impact-initiatives/analysistools")
# devtools::install_github("https://github.com/impact-initiatives/addindicators", force = TRUE)
# install.packages("analysistools")
 
## Chargement des librairies disponible ici
pacman::p_load("addindicators",
               "analysistools", 
               "openxlsx", 
               "dplyr"
               )


source("Src/package_iphra_tools/functions_phu.R")



# Contenu du répertoire actuel de travail
dir("Input/")

# Chargement de la base de données 
library(openxlsx)

data.list <- list(
          main = read.xlsx("Input/BFA_ACF_IPHRA_PIELA_final_anonymized_data250114.xlsx", sheet = "main"),
          hh_roster = read.xlsx("Input/BFA_ACF_IPHRA_PIELA_final_anonymized_data250114.xlsx", sheet = "hh_roster"),
          ind_health = read.xlsx("Input/BFA_ACF_IPHRA_PIELA_final_anonymized_data250114.xlsx", sheet = "ind_health"),
          child_nutrition = read.xlsx("Input/BFA_ACF_IPHRA_PIELA_final_anonymized_data250114.xlsx", sheet = "child_nutrition")
)

# Chargement du dap
getSheetNames("Input/kobo_acf_dap_in_dev_PH.xlsx")
dap <- read.xlsx(xlsxFile = "Input/kobo_acf_dap_in_dev_PH.xlsx", sheet = "survey")

# Chargement de l'outil kobo
getSheetNames("Input/Kobo_nutrisafe.xlsx")
kobo_tool <- read.xlsx(xlsxFile = "Input/Kobo_nutrisafe.xlsx", sheet = "survey")

# Chargement de l'échantillonnage
getSheetNames("Input/Point_échantillonnage_final_10_12_2024.xlsx")
sampling <-  read.xlsx(xlsxFile = "Input/Point_échantillonnage_final_10_12_2024.xlsx", sheet = 1) %>% 
             select(rowname, pop, menage ) %>% 
             distinct(rowname, .keep_all = TRUE) %>% 
             rename("strate_name" = rowname, "nbr_population" = pop, "nbr_menage" = menage )


# Création de variables supplémentaire nécessaires pour l'analyse de données
source("Src/package_iphra_tools/format_dataset.R")

 

# 2- Plan échantillonnage & calcul de poids ----

## 2-a Prise en compte des poids

# db_main <- db_main %>% 
#            add_weights(
#                sample_data = sampling,
#                strata_column_dataset = "cluster",
#                strata_column_sample = "strate_name",
#                population_column = "nbr_population",
#                weight_column = "weights"
#            )
# 
# 
# 
# ## 2-b Prise en compte du plan d'échantillonnage
# db_main <- db_main %>% 
#            as_survey_design(
#                 ids = NULL,
#                 probs = NULL,
#                 strata = NULL,
#                 variables = NULL,
#                 fpc = NULL,
#                 nest = FALSE,
#                 check_strata = !nest,
#                 weights = NULL,
#                 pps = FALSE,
#                 variance = c("HT", "YG")
#            )
# 
# !(dap %>% pull( analysis_var )) %in% c(names(db_main), names(db_hh_roster), names(db_child_nutrition), names(db_ind_health)) 



design = data.list$main %>% srvyr::as_survey_design()



## 3- Analyse de données ----

results <- NULL
for (i in 1:nrow(dap)) {
    sub_dap <- dap %>% slice(i)
    id  <- dap$id
    Secteur <- sub_dap$Secteur
    Sous_secteur <- sub_dap$Sous_secteur
    analysis_var <- sub_dap$analysis_var
    analysis_type <- sub_dap$analysis_type
    group_var <- sub_dap$group_var
    Indicator <- sub_dap$Indicator
    dataset <- sub_dap$dataset
    level <- sub_dap$level
    

    if(analysis_type == "mean"){
          design$variables[[analysis_var]] <- as.integer(design$variables[[analysis_var]])
          results <- bind_rows( results, 
                                create_analysis_mean(design = design, group_var = group_var, analysis_var = analysis_var, level = level) )
    } else if(analysis_type == "median") {
          results <- bind_rows( results, 
                                create_analysis_median(design = design, group_var = group_var, analysis_var = analysis_var, level = level))
    } else if(analysis_type == "prop_select_one"){
          results <- bind_rows( results, 
                                create_analysis_prop_select_one( design = design, group_var = group_var, analysis_var = analysis_var, level = level))
    } else if(analysis_type == "prop_select_multiple"){
          results <- bind_rows(  results, 
                                 create_analysis_prop_select_multiple(design = design, group_var = group_var, analysis_var = analysis_var, level = level, sm_separator = "/"))
    } else if(analysis_type == "ratio") {
          results <- bind_rows( results, 
                                create_analysis_ratio( design, group_var = NA, analysis_var_numerator, analysis_var_denominator,numerator_NA_to_0 = TRUE,filter_denominator_0 = TRUE,level = 0.95))
    } 
    
}

setdiff(dap$analysis_var, unique(c(names(db_main), names(db_hh_roster), names(db_ind_health) , names(db_child_nutrition))) )  

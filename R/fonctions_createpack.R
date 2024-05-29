# Importation des bibliotheques necessaires
library(haven)
library(readxl)
library(dplyr)

#1.  Fonction pour lire les donnees et renommer les colonnes
read_and_prepare_data <- function(produit_path, conversion_path) {
  produit_dta <- read_dta(produit_path)
  conversion_data <- read_excel(conversion_path)
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03b_", produit)] <- "uniteID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03c_", produit)] <- "tailleID"
  list(produit = produit_dta, conversion_data = conversion_data)

}
produit_path = "database/fruits.dta"
conversion_path = "database/Table_de_conversion_phase_2.xlsx"
produit = "fruits"


dta <- read_and_prepare_data(produit_path, conversion_path)


#2.  Fonction pour faire le merge des donnees
merge_data <- function(produit_dta, conversion_data, produit) {
  conversion_selected <- conversion_data[c("produitID", "uniteID", "tailleID", "poids")]
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  produit_merge <- merge(produit_dta, conversion_selected, by = c("produitID", "uniteID", "tailleID"), all.x = TRUE)
  produit_merge <- produit_merge %>% filter(!is.na(produit_merge[[paste0("s07Bq07a_", produit)]]))

  produit_merge$poids <- as.numeric(produit_merge$poids)
  produit_merge[[paste0("s07Bq03a_", produit)]] <- as.numeric(produit_merge[[paste0("s07Bq03a_", produit)]])
  produit_merge$qte_cons_kg <- (produit_merge$poids * produit_merge[[paste0("s07Bq03a_", produit)]]) / 1000

  return(produit_merge)
}

produit_merge <- merge_data(dta[["produit"]], dta[["conversion_data"]], produit)
View(produit_merge)

#3.  Fonction pour traiter les donnees d'achats

process_achats <- function(produit_merge, produit) {
  achats_cols <- c("produitID", paste0("s07Bq07a_", produit), paste0("s07Bq07b_", produit), paste0("s07Bq07c_", produit), paste0("s07Bq08_", produit),"poids")
  produit_achats <- produit_merge[achats_cols]
  colnames(produit_achats)[2:5] <- c("qte", "uniteID", "tailleID", "Valeur")
  produit_achats <- produit_achats %>%
    group_by(produitID, qte, uniteID, tailleID) %>%
    mutate(Valeur = mean(Valeur)) %>%
    ungroup() %>%
    unique()
  names(produit_merge)[names(produit_merge) == paste0("s07Bq03a_", produit)] <- "qte"
  produit_merge <- merge(produit_merge, produit_achats, by = c("produitID", "uniteID", "tailleID", "qte"), all.x = TRUE)
  taux_unmatching <- sum(is.na(produit_merge$Valeur)) / length(produit_merge$Valeur) * 100
  print(taux_unmatching)

  return(viandes_achats = produit_achats)
}

datag <- process_achats(produit_merge, produit)
View(datag)

#PharmCat 2.33.1

if (!require(remotes)) install.packages('remotes')

if (!require(RxNormR)) remotes::install_github("mpancia/RxNormR")
library(RxNormR)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(purrr)) install.packages('purrr')
library(purrr)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

if (!require(openxlsx)) install.packages('openxlsx')
library(openxlsx)

if (!require(haven)) install.packages('haven')
library(haven)

if (!require(rxnorm)) remotes::install_github("nt-williams/rxnorm")
library(rxnorm)

#####Zombie RxCUIs##############################################################

if (file.exists("REMAPPED_RXCUIS.txt")) {
  norxcuidata = unique(read.delim("REMAPPED_RXCUIS.txt", header = TRUE, colClasses = c("MED_MNEMONIC"="character")))
  norxcuidata['MED_MNEMONIC'] = lapply(norxcuidata['MED_MNEMONIC'], trimws, which = "right")
} else {
  norxcuidata = data.frame(
  MED_MNEMONIC=c("FISH OIL 300 M1 EACH"),
  RXCUI=c("831574"),
  RXNORM_DESCRIPTION=c("docosahexaenoic acid 144 MG / eicosapentaenoic acid 216 MG / vitamin E 2 UNT Oral Capsule"),
  RXNORM_STATUS=c("Obsolete"),
  RELATIONSHIP=c("HAS_INGREDIENT"),
  REMAPPED_RXCUI=c("618597"),
  REMAPPED_DESCRIPTION=c("eicosapentaenoate"))
}
rxcuifinal = read.delim("./TEMP/rxcuifinal.txt", header = TRUE, colClasses = "character", quote = "")
atc_classes = read_stata("ATC_Classes.dta")

norxcuis = unique(norxcuidata['REMAPPED_RXCUI'])

for (i in 1:nrow(norxcuis)) {
  if (is.na(as.numeric(norxcuis[i, 'REMAPPED_RXCUI']))) {
    norxcuis[i, 'GENERIC'] = NA
    norxcuis[i, 'GENERIC_NAME'] = NA
    norxcuis[i, 'ATC'] = NA
    message(i, "/", nrow(norxcuis), " RxCUI ", norxcuis[i, 'REMAPPED_RXCUI'], " is not an RxCUI!")
  } else if (get_rxcui_status(norxcuis[i, 'REMAPPED_RXCUI']) == "Quantified") {
    norxcuis[i, 'GENERIC'] = NA
    norxcuis[i, 'GENERIC_NAME'] = NA
    norxcuis[i, 'ATC'] = NA
    message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " is unquantified! Manual search required!")
  } else if (get_rxcui_status(norxcuis[i, 'REMAPPED_RXCUI']) == "Obsolete") {
    norxcuis[i, 'GENERIC'] = NA
    norxcuis[i, 'GENERIC_NAME'] = NA
    norxcuis[i, 'ATC'] = NA
    message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " is obsolete! Manual search required!")
  } else if (get_rxcui_status(norxcuis[i, 'REMAPPED_RXCUI']) == "NotCurrent") {
    norxcuis[i, 'GENERIC'] = NA
    norxcuis[i, 'GENERIC_NAME'] = NA
    norxcuis[i, 'ATC'] = NA
    message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " is Not current! Manual search required!")
  } else if (get_rxcui_status(norxcuis[i, 'REMAPPED_RXCUI']) == "Remapped") {
    message(i, "/", nrow(norxcuis), " Remapping ",norxcuis[i, 'REMAPPED_RXCUI'])
    message("Remapped to ", get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']))
    if (is.na(as.numeric(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])))) {
      norxcuis[i, 'GENERIC'] = NA
      norxcuis[i, 'GENERIC_NAME'] = NA
      norxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " remapped to ", get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']), " is not an RxCUI!")
    } else if (get_rxcui_status(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])) == "Quantified") {
      norxcuis[i, 'GENERIC'] = NA
      norxcuis[i, 'GENERIC_NAME'] = NA
      norxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " remapped to ", get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']), " is unquantified! Manual search required!")
    } else if (get_rxcui_status(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])) == "Obsolete") {
      norxcuis[i, 'GENERIC'] = NA
      norxcuis[i, 'GENERIC_NAME'] = NA
      norxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " remapped to ", get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']), " is obsolete! Manual search required!")
    } else if (get_rxcui_status(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])) == "NotCurrent") {
      norxcuis[i, 'GENERIC'] = NA
      norxcuis[i, 'GENERIC_NAME'] = NA
      norxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(norxcuis), " RxCUI ",norxcuis[i, 'REMAPPED_RXCUI'], " remapped to ", get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']), " is Not current! Manual search required!")
    } else if (is.null(rx_related_tty(as.numeric(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
      norxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
      norxcuis[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']))
      norxcuis[i, 'ATC'] = get_atc(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']))
    } else {
      norxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
      norxcuis[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']))
      norxcuis[i, 'ATC'] = get_atc(get_remapped_rxcui(norxcuis[i, 'REMAPPED_RXCUI']))
    }
  } else {
    message(i, "/", nrow(norxcuis), " RxCUI ", norxcuis[i, 'REMAPPED_RXCUI'])
    if (is.null(rx_related_tty(as.numeric(norxcuis[i, 'REMAPPED_RXCUI']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
      if (is.null(rx_related_tty(as.numeric(norxcuis[i, 'REMAPPED_RXCUI']), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
        norxcuis[i, 'GENERIC'] = NA
      } else {
        norxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(norxcuis[i, 'REMAPPED_RXCUI']), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
      }
      norxcuis[i, 'GENERIC_NAME'] = get_rx(norxcuis[i, 'GENERIC'])
      norxcuis[i, 'ATC'] = list(list(get_atc(norxcuis[i, 'GENERIC'])))
    } else {
      norxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(norxcuis[i, 'REMAPPED_RXCUI']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
      norxcuis[i, 'GENERIC_NAME'] = get_rx(norxcuis[i, 'GENERIC'])
      norxcuis[i, 'ATC'] = list(list(get_atc(norxcuis[i, 'GENERIC'])))
    }
  }
}

norxcuifinal = left_join(norxcuidata, norxcuis, by="REMAPPED_RXCUI")
norxcuifinal = select(norxcuifinal, -RXNORM_STATUS, -RELATIONSHIP, -REMAPPED_RXCUI, -REMAPPED_DESCRIPTION)
norxcuifinal = unique(norxcuifinal)
norxcuifinal = subset(norxcuifinal, !is.na(GENERIC))
norxcuifinal$RXCUI = as.character(norxcuifinal$RXCUI)
norxcuifinal = unique(unnest_longer(norxcuifinal, ATC, keep_empty = TRUE))

norxcuifinal = left_join(norxcuifinal, atc_classes, by=join_by("ATC" == "LEVEL4"))

norxcuifinal = group_by(norxcuifinal, MED_MNEMONIC) %>%
  fill(RXCUI, .direction = "downup") %>%
  fill(RXNORM_DESCRIPTION, .direction = "downup")%>%
  fill(GENERIC, .direction = "downup")%>%
  fill(GENERIC_NAME, .direction = "downup")%>%
  fill(ATC, .direction = "downup")%>%
  fill(LEVEL4_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL3, .direction = "downup")%>%
  fill(LEVEL3_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL2, .direction = "downup")%>%
  fill(LEVEL2_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL1, .direction = "downup")%>%
  fill(LEVEL1_DESCRIPTION, .direction = "downup")

combfinal = bind_rows(rxcuifinal, norxcuifinal)

combfinal = group_by(combfinal, MED_MNEMONIC) %>%
  fill(RXCUI, .direction = "downup") %>%
  fill(RXNORM_DESCRIPTION, .direction = "downup")%>%
  fill(GENERIC, .direction = "downup")%>%
  fill(GENERIC_NAME, .direction = "downup")%>%
  fill(ATC, .direction = "downup")%>%
  fill(LEVEL4_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL3, .direction = "downup")%>%
  fill(LEVEL3_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL2, .direction = "downup")%>%
  fill(LEVEL2_DESCRIPTION, .direction = "downup")%>%
  fill(LEVEL1, .direction = "downup")%>%
  fill(LEVEL1_DESCRIPTION, .direction = "downup")

combfinal = unique(combfinal)

manualreview = subset(combfinal, is.na(GENERIC) & str_length(RXCUI) < 13)

manualreview$MED_MNEMONIC = paste("'", manualreview$MED_MNEMONIC, "',", sep = "")

manualreview = select(manualreview, MED_MNEMONIC, RXCUI, RXNORM_DESCRIPTION)

write.csv(manualreview, "NeedsFollowUp.csv", row.names = FALSE)

write.table(combfinal, file = "./TEMP/combfinal.txt", sep = '\t', quote = FALSE)

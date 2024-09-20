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

load("./TEMP/SQLVAR.RData")

rxcuidata = unique(read_tsv("RXCUIS.txt", col_names = TRUE, trim_ws = TRUE, show_col_types = FALSE, col_types = cols(.default = col_character())))

rxcuidata = rxcuidata[, 1:3]

rxcuidata['RXNORM_DESCRIPTION'] = lapply(rxcuidata['RXNORM_DESCRIPTION'], trimws, which = "left")

rxcuidata['MED_MNEMONIC'] = lapply(rxcuidata['MED_MNEMONIC'], trimws, which = "right")

atc_classes = read_stata("ATC_Classes.dta")

if (file.exists("./TEMP/jsondl.RData")) {
  jsondl = readRDS("./TEMP/jsondl.RData")
} else {
  jsondl = unique(rxcuidata['RXCUI'])
  for (i in 1:nrow(jsondl)) {
    if (is.na(as.numeric(jsondl[i, 'RXCUI']))) {
      jsondl[i, 'GENERIC'] = NA
      jsondl[i, 'GENERIC_NAME'] = NA
      jsondl[i, 'ATC'] = NA
      message(i, "/", nrow(jsondl), " RxCUI ", jsondl[i, 'RXCUI'], " is not an RxCUI!")
    } else if (get_rxcui_status(jsondl[i, 'RXCUI']) == "Quantified") {
        jsondl[i, 'GENERIC'] = NA
        jsondl[i, 'GENERIC_NAME'] = NA
        jsondl[i, 'ATC'] = NA
        message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " is unquantified! Manual search required!")
    } else if (get_rxcui_status(jsondl[i, 'RXCUI']) == "Obsolete") {
      jsondl[i, 'GENERIC'] = NA
      jsondl[i, 'GENERIC_NAME'] = NA
      jsondl[i, 'ATC'] = NA
      message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " is obsolete! Manual search required!")
    } else if (get_rxcui_status(jsondl[i, 'RXCUI']) == "NotCurrent") {
      jsondl[i, 'GENERIC'] = NA
      jsondl[i, 'GENERIC_NAME'] = NA
      jsondl[i, 'ATC'] = NA
      message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " is Not current! Manual search required!")
    } else if (get_rxcui_status(jsondl[i, 'RXCUI']) == "Remapped") {
      message(i, "/", nrow(jsondl), " Remapping ",jsondl[i, 'RXCUI'])
      message("Remapped to ", get_remapped_rxcui(jsondl[i, 'RXCUI']))
      if (is.na(as.numeric(get_remapped_rxcui(jsondl[i, 'RXCUI'])))) {
        jsondl[i, 'GENERIC'] = NA
        jsondl[i, 'GENERIC_NAME'] = NA
        jsondl[i, 'ATC'] = NA
        message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " remapped to ", get_remapped_rxcui(jsondl[i, 'RXCUI']), " is not an RxCUI!")
      } else if (get_rxcui_status(get_remapped_rxcui(jsondl[i, 'RXCUI'])) == "Quantified") {
        jsondl[i, 'GENERIC'] = NA
        jsondl[i, 'GENERIC_NAME'] = NA
        jsondl[i, 'ATC'] = NA
        message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " remapped to ", get_remapped_rxcui(jsondl[i, 'RXCUI']), " is unquantified! Manual search required!")
      } else if (get_rxcui_status(get_remapped_rxcui(jsondl[i, 'RXCUI'])) == "Obsolete") {
        jsondl[i, 'GENERIC'] = NA
        jsondl[i, 'GENERIC_NAME'] = NA
        jsondl[i, 'ATC'] = NA
        message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " remapped to ", get_remapped_rxcui(jsondl[i, 'RXCUI']), " is obsolete! Manual search required!")
      } else if (get_rxcui_status(get_remapped_rxcui(jsondl[i, 'RXCUI'])) == "NotCurrent") {
        jsondl[i, 'GENERIC'] = NA
        jsondl[i, 'GENERIC_NAME'] = NA
        jsondl[i, 'ATC'] = NA
        message(i, "/", nrow(jsondl), " RxCUI ",jsondl[i, 'RXCUI'], " remapped to ", get_remapped_rxcui(jsondl[i, 'RXCUI']), " is Not current! Manual search required!")
      } else if (is.null(rx_related_tty(as.numeric(get_remapped_rxcui(jsondl[i, 'RXCUI'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
        jsondl[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(jsondl[i, 'RXCUI'])), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
        jsondl[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(jsondl[i, 'RXCUI']))
        jsondl[i, 'ATC'] = list(list(get_atc(get_remapped_rxcui(jsondl[i, 'RXCUI']))))
      } else {
        jsondl[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(jsondl[i, 'RXCUI'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
        jsondl[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(jsondl[i, 'RXCUI']))
        jsondl[i, 'ATC'] = list(list(get_atc(get_remapped_rxcui(jsondl[i, 'RXCUI']))))
      }
    } else {
        message(i, "/", nrow(jsondl), " RxCUI ", jsondl[i, 'RXCUI'])
        if (is.null(rx_related_tty(as.numeric(jsondl[i, 'RXCUI']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
          if (is.null(rx_related_tty(as.numeric(jsondl[i, 'RXCUI']), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
            jsondl[i, 'GENERIC'] = NA
          } else { 
            jsondl[i, 'GENERIC'] = rx_related_tty(as.numeric(jsondl[i, 'RXCUI']), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
          }
          jsondl[i, 'GENERIC_NAME'] = get_rx(jsondl[i, 'GENERIC'])
          jsondl[i, 'ATC'] = list(list(get_atc(jsondl[i, 'GENERIC'])))
        } else {
          jsondl[i, 'GENERIC'] = rx_related_tty(as.numeric(jsondl[i, 'RXCUI']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
          jsondl[i, 'GENERIC_NAME'] = get_rx(jsondl[i, 'GENERIC'])
          jsondl[i, 'ATC'] = list(list(get_atc(jsondl[i, 'GENERIC'])))
      }
    }
  }
  
  saveRDS(jsondl, file = "./TEMP/jsondl.RData")
}


rxcuifinal = left_join(rxcuidata, jsondl, by="RXCUI")

rxcuifinal[rxcuifinal == "?"] = NA


message("Checking Descriptions")
for (i in 1:nrow(rxcuifinal)) {
  message(i, "/", nrow(rxcuifinal), " RxCUI ", rxcuifinal[i, 'RXCUI'])
  if (rxcuifinal[i, "RXCUI"] != "" & !is.na(rxcuifinal[i, "RXNORM_DESCRIPTION"])) {
    if (rxcuifinal[i, "RXNORM_DESCRIPTION"] == rxcuifinal[i, "RXCUI"]) {
      rxcuifinal[i, "RXNORM_DESCRIPTION"] = get_rx(rxcuifinal[i, "RXCUI"])
    }
  }
}

concept = data.frame()

for (i in 1:nrow(rxcuifinal)) {
  message(i, "/", nrow(rxcuifinal), " RxCUI ", rxcuifinal[i, 'RXCUI'])
  if (is.na(rxcuifinal[i, "GENERIC"])) {
    if (!is.na(rxcuifinal[i, "RXNORM_DESCRIPTION"])) {
      concept[i, "RXCUI"] = rxcuifinal[i, "RXCUI"]
      transfer = rx_rxcui_name(str_to_title(rxcuifinal[i, "RXNORM_DESCRIPTION"]), allsrc = 1, search = 2)
      if (length(transfer[["idGroup"]]) > 0) {
        concept[i, "REMAP"] = list(list(transfer[["idGroup"]]))
      } else {
        concept[i, "REMAP"] = NA
      }
    }
  }
}

concept = unique(subset(concept, !is.na(REMAP) & REMAP != "NULL" & nchar(RXCUI) < 10))

rxcuifinal = left_join(rxcuifinal, concept, by="RXCUI")

rxcuis = subset(rxcuifinal, REMAP != "NULL" & is.na(GENERIC))

if(nrow(rxcuis) != 0) {
  rxcuis = unique(unnest_longer((unnest_longer(rxcuis, REMAP)), REMAP, keep_empty = TRUE))
  
  rxcuis = select(rxcuis, -REMAP_id)
  
  for (i in 1:nrow(rxcuis)) {
    if (is.na(as.numeric(rxcuis[i, 'REMAP']))) {
      rxcuis[i, 'GENERIC'] = NA
      rxcuis[i, 'GENERIC_NAME'] = NA
      rxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(rxcuis), " RxCUI ", rxcuis[i, 'REMAP'], " is not an RxCUI!")
    } else if (get_rxcui_status(rxcuis[i, 'REMAP']) == "Quantified") {
      rxcuis[i, 'GENERIC'] = NA
      rxcuis[i, 'GENERIC_NAME'] = NA
      rxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " is unquantified! Manual search required!")
    } else if (get_rxcui_status(rxcuis[i, 'REMAP']) == "Obsolete") {
      rxcuis[i, 'GENERIC'] = NA
      rxcuis[i, 'GENERIC_NAME'] = NA
      rxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " is obsolete! Manual search required!")
    } else if (get_rxcui_status(rxcuis[i, 'REMAP']) == "NotCurrent") {
      rxcuis[i, 'GENERIC'] = NA
      rxcuis[i, 'GENERIC_NAME'] = NA
      rxcuis[i, 'ATC'] = NA
      message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " is Not current! Manual search required!")
    } else if (get_rxcui_status(rxcuis[i, 'REMAP']) == "Remapped") {
      message(i, "/", nrow(rxcuis), " Remapping ",rxcuis[i, 'REMAP'])
      message("Remapped to ", get_remapped_rxcui(rxcuis[i, 'REMAP']))
      if (is.na(as.numeric(get_remapped_rxcui(rxcuis[i, 'REMAP'])))) {
        rxcuis[i, 'GENERIC'] = NA
        rxcuis[i, 'GENERIC_NAME'] = NA
        rxcuis[i, 'ATC'] = NA
        message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " remapped to ", get_remapped_rxcui(rxcuis[i, 'REMAP']), " is not an RxCUI!")
      } else if (get_rxcui_status(get_remapped_rxcui(rxcuis[i, 'REMAP'])) == "Quantified") {
        rxcuis[i, 'GENERIC'] = NA
        rxcuis[i, 'GENERIC_NAME'] = NA
        rxcuis[i, 'ATC'] = NA
        message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " remapped to ", get_remapped_rxcui(rxcuis[i, 'REMAP']), " is unquantified! Manual search required!")
      } else if (get_rxcui_status(get_remapped_rxcui(rxcuis[i, 'REMAP'])) == "Obsolete") {
        rxcuis[i, 'GENERIC'] = NA
        rxcuis[i, 'GENERIC_NAME'] = NA
        rxcuis[i, 'ATC'] = NA
        message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " remapped to ", get_remapped_rxcui(rxcuis[i, 'REMAP']), " is obsolete! Manual search required!")
      } else if (get_rxcui_status(get_remapped_rxcui(rxcuis[i, 'REMAP'])) == "NotCurrent") {
        rxcuis[i, 'GENERIC'] = NA
        rxcuis[i, 'GENERIC_NAME'] = NA
        rxcuis[i, 'ATC'] = NA
        message(i, "/", nrow(rxcuis), " RxCUI ",rxcuis[i, 'REMAP'], " remapped to ", get_remapped_rxcui(rxcuis[i, 'REMAP']), " is Not current! Manual search required!")
      } else if (is.null(rx_related_tty(as.numeric(get_remapped_rxcui(rxcuis[i, 'REMAP'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
        rxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(rxcuis[i, 'REMAP'])), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
        rxcuis[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(rxcuis[i, 'REMAP']))
        rxcuis[i, 'ATC'] = list(list(get_atc(get_remapped_rxcui(rxcuis[i, 'REMAP']))))
      } else {
        rxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(get_remapped_rxcui(rxcuis[i, 'REMAP'])), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
        rxcuis[i, 'GENERIC_NAME'] = get_rx(get_remapped_rxcui(rxcuis[i, 'REMAP']))
        rxcuis[i, 'ATC'] = list(list(get_atc(get_remapped_rxcui(rxcuis[i, 'REMAP']))))
      }
    } else {
      message(i, "/", nrow(rxcuis), " RxCUI ", rxcuis[i, 'REMAP'])
      if (is.null(rx_related_tty(as.numeric(rxcuis[i, 'REMAP']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
        if (is.null(rx_related_tty(as.numeric(rxcuis[i, 'REMAP']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]])) {
          rxcuis[i, 'GENERIC'] = NA
        } else {
          rxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(rxcuis[i, 'REMAP']), "IN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]  
        }
        rxcuis[i, 'GENERIC_NAME'] = get_rx(rxcuis[i, 'GENERIC'])
        rxcuis[i, 'ATC'] = list(list(get_atc(rxcuis[i, 'GENERIC'])))
      } else {
        rxcuis[i, 'GENERIC'] = rx_related_tty(as.numeric(rxcuis[i, 'REMAP']), "MIN")[["relatedGroup"]][["conceptGroup"]][[1]][["conceptProperties"]][[1]][["rxcui"]]
        rxcuis[i, 'GENERIC_NAME'] = get_rx(rxcuis[i, 'GENERIC'])
        rxcuis[i, 'ATC'] = list(list(get_atc(rxcuis[i, 'GENERIC'])))
      }
    }
  }
  
  rxcuis = select(rxcuis, -REMAP)
  
  rxcuis$ATC[is.na(rxcuis$ATC)] = list(NULL) 
  
  rxcuis = unique(rxcuis)
  
  rxcuis = filter(group_by(rxcuis, RXCUI), !is.na(GENERIC) | all(is.na(GENERIC)))
  
  rxcuis = subset(rxcuis, !is.na(GENERIC))
  
  rxcuis_dup = as.data.frame(split(filter(group_by(rxcuis, MED_MNEMONIC, RXCUI), n()>1), ~ave(MED_MNEMONIC, MED_MNEMONIC, FUN = seq_along)))

  rxcuis_unq = filter(group_by(rxcuis, MED_MNEMONIC, RXCUI), n()<2)
  
}

rxcuifinal = select(rxcuifinal, -REMAP)

if(nrow(rxcuis_unq) != 0) {
  rxcuifinal = rows_patch(rxcuifinal, rxcuis_unq, by = c("MED_MNEMONIC", "RXCUI"), unmatched = "ignore")
  rxcuifinal = bind_rows(rxcuifinal, rxcuis_unq)
  rxcuifinal = distinct(rxcuifinal)
}

if(nrow(rxcuis_dup) != 0) {
  
  rxcuifinal = rows_patch(rxcuifinal, rxcuis_dup[[1]], by = c("MED_MNEMONIC", "RXCUI"), unmatched = "ignore")
  
  rxcuis_dup = rxcuis_dup[-1]
  
  for(i in 1:length(rxcuis_dup)) {
      rxcuifinal = bind_rows(rxcuifinal, rxcuis_dup[[i]])
  }
  rxcuifinal = distinct(rxcuifinal)
}


rxcuifinal = unique(unnest_longer(rxcuifinal, ATC, keep_empty = TRUE))

rxcuifinal = left_join(rxcuifinal, atc_classes, by=join_by("ATC" == "LEVEL4"))

rxcuifinal = group_by(rxcuifinal, MED_MNEMONIC) %>%
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

alwayspull = unique(subset(rxcuifinal, is.na(RXCUI), select = "MED_MNEMONIC"))
alwayspull = subset(alwayspull, !is.na(MED_MNEMONIC))
write.table(alwayspull, file = "./TEMP/alwayspull.txt", sep = '\t', quote = FALSE)
alwayspull$MED_MNEMONIC = paste0("'", alwayspull$MED_MNEMONIC, "',")
write.table(alwayspull, "Alwayspull.txt", row.names = FALSE, col.names = FALSE, sep = '\t', quote = FALSE)


rxcuifinal = subset(rxcuifinal, !is.na(RXCUI))

zombiecheck = subset(rxcuifinal, is.na(GENERIC) & nchar(RXCUI) < 10)
zombiecheck = unique(select(zombiecheck, MED_MNEMONIC))

if (dim(zombiecheck)[1] == 0) {
zombiecheck = data.frame(MED_MNEMONIC=c('THALIDAMIDE'))
}

zombiecheck$MED_MNEMONIC = ifelse(grepl("'", zombiecheck$MED_MNEMONIC), paste0('"', zombiecheck$MED_MNEMONIC, '",'), paste0("'", zombiecheck$MED_MNEMONIC, "',"))

       
################################################################################
if (file.exists('4_Zombie_RxCUI_SQL.sql')) {
  file.remove('4_Zombie_RxCUI_SQL.sql')
}

file.create('4_Zombie_RxCUI_SQL.sql')
sqlout = file('4_Zombie_RxCUI_SQL.sql', open = 'a')

writeLines(c('--PharmCat 2.2 ZOMBIES',
             '',
             'SELECT',
             'MED_MNEMONIC,',
             'RXCUI,',
             'RXNORM_DESCRIPTION,',
             'ref.Concept_Status_Desc AS RXNORM_STATUS,',
             'refrelation.Relationship_Mnem_CS AS RELATIONSHIP,',
             'refrelation.Related_Source_Code AS REMAPPED_RXCUI,',
             'refrelation.Related_Source_Desc AS REMAPPED_DESCRIPTION'),
           sep = "\r", sqlout)
writeLines(c('FROM `hca-usr-gbc-gme-prod.',paste0(THREEFOUR),'.SYMEDTABLE` medtable'), sep = "", sqlout)
writeLines(c('',
             '',
             'LEFT JOIN hca-hin-prod-cur-clinical.edwcl_views.ref_rxnorm ref'),
           sep = "\r", sqlout)
writeLines(c('ON medtable.RXCUI = CAST(ref.RxNorm_Id AS STRING)'), sep = "", sqlout)
writeLines(c('',
             '',
             'LEFT JOIN hca-hin-prod-cur-clinical.edwcl_views.ref_rxnorm_relationship refrelation'),
           sep = "\r", sqlout)
writeLines(c('ON medtable.RXCUI = CAST(refrelation.RxNorm_Id AS STRING)'), sep = "", sqlout)
writeLines(c('',
             '',
             'WHERE refrelation.Relationship_Mnem_CS IN(',
             "'HAS_QUAN_FORM',",
             "'HAS_GENERIC_DRUG',",
             "'HAS_INGREDIENT',",
             "'RXN_CONTAINS',",
             "'RXN_HAS_BOSS',",
             "'HASPARENT'"),
           sep = "\r", sqlout)
writeLines(c(') AND medtable.MED_MNEMONIC IN('), sep = "", sqlout)
writeLines('', sep = "\r", sqlout)
for (i in 1:nrow(zombiecheck)) {
  if (i == nrow(zombiecheck)){
    writeLines(substring(as.character(zombiecheck[i, 'MED_MNEMONIC']), 1, nchar(as.character(zombiecheck[i, 'MED_MNEMONIC']))-1), sep = "\r", sqlout)
  } else {
    writeLines(as.character(zombiecheck[i, 'MED_MNEMONIC']), sep = "\r", sqlout)
  }
}
writeLines(')', sep = "", sqlout)
close(sqlout)
################################################################################
write.table(zombiecheck, "ZOMBIE_RXCUIS.txt", row.names = FALSE, col.names = FALSE, sep = '\t', quote = FALSE)

write.table(rxcuifinal, file = "./TEMP/rxcuifinal.txt", sep = '\t', quote = FALSE)
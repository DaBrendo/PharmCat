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


medcat = read_excel("Categorizations.xlsx")
medcat$CATEGORIZATION = as.character(medcat$CATEGORIZATION)
atc_classes = read_stata("ATC_Classes.dta")
combfinal = read.delim("./TEMP/combfinal.txt", header = TRUE, colClasses = "character", quote = "")

categorydf = data.frame()

for (i in 1:nrow(medcat)) {
  
  if (medcat[i, 'TYPE'] == "MED") {
    
    message("Medication: ", as.character(medcat[i, 'CATEGORIZATION']))
    
    concept = rx_rxcui_name(str_to_title(as.character(medcat[i, 'CATEGORIZATION'])), allsrc = 1, search = 2)
    
    if(is.null(concept[["idGroup"]][["rxnormId"]])) {
      message(as.character(medcat[i, 'CATEGORIZATION']), "WAS NOT FOUND!")
    } else {
      message("Starting concept gathering...")
      
      for (z in 1:length(concept[["idGroup"]][["rxnormId"]])) {
        message("RXCUI:", concept[["idGroup"]][["rxnormId"]][[z]])
        allrel = rx_allrelated(concept[["idGroup"]][["rxnormId"]][[z]])
        relate_list = flatten(allrel[["allRelatedGroup"]])
        relate_list[['rxcui']] = NULL
        conceptdf = flatten(relate_list)
        conceptdf = conceptdf[names(conceptdf) != "tty"]
        conceptdf = lapply(conceptdf, function(x) bind_rows(x))
        conceptdf = as.data.frame(bind_rows(conceptdf))
        if (dim(conceptdf)[1] == 0) {
          message("RXCUI:", concept[["idGroup"]][["rxnormId"]][[z]], " WAS NOT FOUND, GENERATING DUMMY!")
          conceptdf = data.frame(
            rxcui=c(concept[["idGroup"]][["rxnormId"]][[z]]),
            name=c(as.character(medcat[i, 'CATEGORIZATION'])),
            synonym=c("TEST"),
            tty=c("TEST"),
            language=c("TEST"),
            supress=c("TEST"),
            umlscui=c("TEST"))
        }
        conceptdf = select(conceptdf, rxcui)
        conceptdf$CATEGORY = as.character(medcat[i, 'CATEGORIZATION'])
        conceptdf = rename(conceptdf, RXCUI = rxcui)
        categorydf = rbind(categorydf, conceptdf)
      }
    }
  } else if (medcat[i, 'TYPE'] == "ATC") {
    message("ATC: ", as.character(medcat[i, 'CATEGORIZATION']))
    if (nchar(medcat[i, 'CATEGORIZATION']) == 5) {
      message("Level 4 Class")
      atcdf = subset(atc_classes, LEVEL4 == as.character(medcat[i, 'CATEGORIZATION']))
      atcdf = select(atcdf, LEVEL4, LEVEL4_DESCRIPTION)
      atcdf = left_join(atcdf, combfinal, by = join_by('LEVEL4' == 'ATC'), relationship = "many-to-many")
      atcdf = select(atcdf, RXCUI, LEVEL4)
      atcdf = rename(atcdf, CATEGORY = LEVEL4)
    } else if (nchar(medcat[i, 'CATEGORIZATION']) == 4) {
      message("Level 3 Class")
      atcdf = subset(atc_classes, LEVEL3 == as.character(medcat[i, 'CATEGORIZATION']))
      atcdf = select(atcdf, LEVEL3, LEVEL3_DESCRIPTION)
      atcdf = left_join(atcdf, combfinal, by = 'LEVEL3', relationship = "many-to-many")
      atcdf = select(atcdf, RXCUI, LEVEL3)
      atcdf = rename(atcdf, CATEGORY = LEVEL3)
    } else if (nchar(medcat[i, 'CATEGORIZATION']) == 3) {
      message("Level 2 Class")
      atcdf = subset(atc_classes, LEVEL2 == as.character(medcat[i, 'CATEGORIZATION']))
      atcdf = select(atcdf, LEVEL2, LEVEL2_DESCRIPTION)
      atcdf = left_join(atcdf, combfinal, by = 'LEVEL2', relationship = "many-to-many")
      atcdf = select(atcdf, RXCUI, LEVEL2)
      atcdf = rename(atcdf, CATEGORY = LEVEL2)
    } else if (nchar(medcat[i, 'CATEGORIZATION']) == 1) {
      message("Level 1 Class")
      atcdf = subset(atc_classes, LEVEL1 == as.character(medcat[i, 'CATEGORIZATION']))
      atcdf = select(atcdf, LEVEL1, LEVEL1_DESCRIPTION)
      atcdf = left_join(atcdf, combfinal, by = 'LEVEL1', relationship = "many-to-many")
      atcdf = select(atcdf, RXCUI, LEVEL1)
      atcdf = rename(atcdf, CATEGORY = LEVEL1)
    } else {
      message("ATC: ", as.character(medcat[i, 'CATEGORIZATION']), "was not found!")
    }
    categorydf = bind_rows(categorydf, atcdf)
  }
}

categorydf = unique(categorydf)

write.table(categorydf, file = "./TEMP/categorydf.txt", sep = '\t', quote = FALSE)
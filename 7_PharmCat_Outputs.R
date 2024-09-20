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

combfinal = read.delim("./TEMP/combfinal.txt", header = TRUE, colClasses = "character", quote = "")
categorydf = read.delim("./TEMP/categorydf.txt", header = TRUE, colClasses = "character", quote = "")
alwayspull = read.delim("./TEMP/alwayspull.txt", header = TRUE, colClasses = "character", quote = "")


output = left_join(combfinal, categorydf, relationship = "many-to-many")

output$SQL_MNEMONIC = paste0("'",output$MED_MNEMONIC,"',")

alwayspull$SQL_MNEMONIC = paste0("'",alwayspull$MED_MNEMONIC,"',")

output = select(output, SQL_MNEMONIC, MED_MNEMONIC, RXCUI, RXNORM_DESCRIPTION, CATEGORY, GENERIC, GENERIC_NAME, ATC:LEVEL1_DESCRIPTION)

output = bind_rows(output, alwayspull)

categorized = subset(output, !is.na(CATEGORY))

categorized = bind_rows(categorized, alwayspull)

write.csv(output, "All_Mnemonics.csv", row.names = FALSE)
write.csv(categorized, "Categorized_Only.csv", row.names = FALSE)

file.remove("./TEMP/combfinal.txt")
file.remove("./TEMP/alwayspull.txt")
file.remove("./TEMP/categorydf.txt")
file.remove("./TEMP/rxcuifinal.txt")
file.remove("./TEMP/jsondl.RData")
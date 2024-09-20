#PharmCat 2.33.1

THREEFOUR = '34ID'
PRJCTVW = 'POPULATION_TABLE_HERE'
INPATIENT = TRUE
HOMEMED = TRUE

dir.create("./TEMP", showWarnings = FALSE)
save(THREEFOUR, PRJCTVW, file = "./TEMP/SQLVAR.RData")

if (file.exists('2_RxCUI_SQL.sql')) {
  file.remove('2_RxCUI_SQL.sql')
}

file.create('2_RxCUI_SQL.sql')
sqlout = file('2_RxCUI_SQL.sql', open = 'a')

writeLines('--PharmCat 2.2 TABLE', sqlout)
writeLines(c('--CREATE OR REPLACE VIEW `hca-usr-gbc-gme-prod.',paste0(THREEFOUR),'.SYMEDTABLE` AS'), sep = "", sqlout)

if (INPATIENT) {
  writeLines(c(
    '',
    'SELECT DISTINCT',
    "Cast(Coalesce((CASE WHEN rxadmin.SRC_SYS_REF_CD IN ('CERNER','EPIC') THEN rxqty.SRC_SYS_ORGNL_CD end), rxadmin.SRC_SYS_ORGNL_CD) AS STRING) AS MED_MNEMONIC,",
    'Coalesce(Sym1.Standard_Code, Substring(DCRef.Clinical_Phrm_RXM_RXNORM_Code, 2)) AS RXCUI,',
    'Coalesce(Sym1.Standard_Code_Desc, Cast(Norm.RxNorm_Id AS STRING)) AS RXNORM_DESCRIPTION'),
    sep = "\r", sqlout)
  writeLines(c('FROM `hca-usr-gbc-gme-prod.',paste0(THREEFOUR),'.',paste0(PRJCTVW),'` pop'), sep = "", sqlout)
  writeLines(c(
    '',
    '',
    'INNER JOIN `hca-hin-prod-cur-clinical.edwcdm_views.mdctn_admn_dtl` rxadmin',
    'ON rxadmin.PATIENT_DW_ID = pop.patient_dw_id',
    'AND rxadmin.COID = pop.coid',
    '',
    'INNER JOIN `hca-hin-prod-cur-clinical.edwcdm_views.MDCTN_ADMN_TM_QTY` rxqty',
    'ON rxadmin.MDCTN_ADMN_SK = rxqty.MDCTN_ADMN_SK',
    'AND rxadmin.PATIENT_DW_ID = rxqty.PATIENT_DW_ID',
    "AND rxadmin.VLD_TO_TS = '9999-12-31 00:00:00'",
    '',
    'LEFT JOIN  `hca-hin-prod-cur-clinical.edwcdm_views.ref_symedical_value_set` sym1',
    'ON rxadmin.coid = sym1.Coid',
    "AND Cast(Coalesce((CASE WHEN rxadmin.SRC_SYS_REF_CD IN ('CERNER','EPIC') THEN rxqty.SRC_SYS_ORGNL_CD end), rxadmin.SRC_SYS_ORGNL_CD) AS STRING) = sym1.source_term_code",
    "AND sym1.map_domain = 'MED'",
    '',
    'LEFT JOIN `hca-hin-prod-cur-clinical.edwcl_views.ref_clinical_phrm_rxm_code_map` DCRef',
    'ON pop.COID = DCRef.COID',
    'AND rxadmin.SRC_SYS_ORGNL_CD = DCRef.Clinical_Phrm_RXM_Mnemonic_CS',
    '',
    'LEFT JOIN `hca-hin-prod-cur-clinical.edwcl_views.ref_rxnorm` Norm',
    'ON Substring(DCRef.Clinical_Phrm_RXM_RXNORM_Code, 2) = Cast(Norm.RxNorm_Id AS STRING)',
    '',
    'WHERE rxadmin.ADMN_SUBST_STS_REF_CD IN(',
    "'CP',",
    "'13',",
    "'1404',",
    "'1',",
    "'1309',",
    "'12015',",
    "'12',",
    "'1209',",
    "'1405',",
    "'1408',",
    "'1305',",
    "'36269759',",
    "'12019',",
    "'13010',",
    "'1406',",
    "'362697599',",
    "'12012',",
    "'2946731',",
    "'12018',",
    "'1202',",
    "'CP',",
    "'6'",
    ')',
    '',
    ''),
    sep = "\r", sqlout)
}

if (INPATIENT & HOMEMED) {
  writeLines('UNION ALL', sep = "\r", sqlout)
}

if (HOMEMED) {
  writeLines(c(
    '',
    '',
    'SELECT DISTINCT',
    'Med.Clinical_Phrm_Mnem_CS AS MED_MNEMONIC,',
    'Coalesce(Cast(Substring(DCRef.Clinical_Phrm_RXM_RXNORM_Code, 2) AS STRING), Sym1.Standard_Code) AS RXCUI,',
    'Coalesce(Norm.RxNorm_Desc, Sym1.Standard_Code_Desc) AS RXNORM_DESCRIPTION'),
    sep = "\r", sqlout)
  writeLines(c('FROM `hca-usr-gbc-gme-prod.',paste0(THREEFOUR),'.',paste0(PRJCTVW),'` pop'), sep = "", sqlout)
  writeLines(c(
    '',
    '',
    'INNER JOIN `hca-hin-dev-cur-clinical.edwcl_views.clinical_patient_med_list` Med',
    'ON pop.Patient_dw_id=Med.Patient_dw_id',
    'AND pop.Coid=Med.Coid',
    "AND Med.Clinical_Systems_Module_Code NOT IN ('TEV','IAT')",
    '',
    'LEFT JOIN `hca-hin-prod-cur-clinical.edwcl_views.ref_clinical_phrm_rxm_code_map` DCRef',
    'ON Med.COID = DCRef.COID',
    'AND Med.Clinical_Phrm_Mnem_CS = DCRef.Clinical_Phrm_RXM_Mnemonic_CS',
    '',
    'LEFT JOIN `hca-hin-prod-cur-clinical.edwcdm_views.ref_symedical_value_set` sym1',
    'ON Med.COID = sym1.COID',
    'AND Med.Clinical_Phrm_Mnem_CS = sym1.source_term_code',
    "AND sym1.map_domain = 'MED'",
    '',
    'LEFT JOIN `hca-hin-prod-cur-clinical.edwcl_views.ref_rxnorm` Norm',
    'ON Substring(DCRef.Clinical_Phrm_RXM_RXNORM_Code, 2) = Cast(Norm.RxNorm_Id AS STRING)'),
    sep = "\r", sqlout)
}

close(sqlout)
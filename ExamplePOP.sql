REPLACE VIEW BDO6585.PHARMCATTEST AS
SELECT
fp.Patient_DW_Id
,fp.Company_Code
,fp.Coid
,fp.Admission_Date
FROM EDWCDM_Views.Fact_Patient FP

INNER JOIN EDWCL_Views.Person_CL p
ON fp.Patient_Person_DW_Id = p.Person_DW_Id

INNER JOIN EDWCDM_PC_Views.Patient_Diagnosis  PD		--if limiting on diagnosis codes
ON PD.Patient_DW_Id = FP.Patient_DW_Id  	   
AND PD.COID = FP.COID
AND PD.Diag_Mapped_Code NOT = 'Y'
AND pd.Diag_Cycle_Code = 'F'
AND PD.Diag_Type_Code='0'		--ICD-10 CM codes
AND pd.Diag_Code IN(
'I5181'
)	

WHERE fp.Admission_Date BETWEEN '2023-10-30' AND '2023-10-31'		--adjust dates as needed
AND fp.Discharge_Date BETWEEN '2023-10-30' AND '2023-12-31'
AND ((Cast((Cast((fp.Admission_Date(FORMAT'YYYY-MM')) AS CHAR(7)) || '-01') AS DATE) - Cast((Cast((p.Person_Birth_Date(FORMAT'YYYY-MM')) AS CHAR(7)) || '-01') AS DATE))/365) > '17'
AND fp.Company_Code='H'
AND fp.final_bill_date <> '0001-01-01'
AND fp.final_bill_date IS NOT NULL

GROUP BY 1,2,3,4
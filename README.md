# Demonstration property tax regressivity analysis for Dewitt Charter Township, Michigan

### Methodological notes:
* The workflow limits to properties where `Parcels.propstatus` = `'Active'`; `Parcels.exemptcode` = `'TAXABLE'`; only residential properties (`Parcels.propclass` of 401 and 410); and years 2018 to 2023 (using `ParcelMaster.lastSaleDate` field), and most recent sale for each property. Outliers limited by standard IAAO IQR outlier filter for AV ratios and we removed properties with sale price under $10,000.
* Sales transactions were inflation adjusted using the All transactions HPI for the Lansing metro area.


### Sales Ratio Study for Detroit, Michigan

* *Data source:* The following study uses 2023 sales available at this [link](https://data.detroitmi.gov/datasets/property-sales-1/explore?showTable=true) and the 2024 assessment roll available at this [link](https://data.detroitmi.gov/datasets/property-sales-1/explore?showTable=true).
* *Sales universe:* 
  * Transactions considered arm's length based on the following classes `"03-ARM'S LENGTH"`, `"19-MULTI PARCEL ARM'S LENGTH"`, `"03-ARMS LENGTH"`, `"11-FROM LENDING INSTITUTION EXPOSED"`, `"11-FROM LANDING INSTITUTION EXPOSED"`
  * For residential property classes where `PROPCLASSDESC` equals `"RESIDENTIAL-IMPROVED"` or `"RESIDENTIAL CONDOMINIUMS"` and `TAXSTATUS` is `'TAXABLE'`
  * Sales happening between 2023-04-01 to 2024-02-26.
  * For properties that sold for more than $1,000 and were assessed over $1,000
  * Excludes properties with an assessment value to sales ratio more than 1.5 times the interquartile range.
* For a link to the report see this link: https://mansueto-institute.github.io/
* The study consists of 2,892 properties sold between 2023-04-01 to 2024-02-26. 

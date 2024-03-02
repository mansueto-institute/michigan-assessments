### Sales Ratio Study for Detroit, Michigan

* *Data source:* The following study uses 2023 sales available at this [link](https://data.detroitmi.gov/datasets/property-sales-1/explore?showTable=true) and the 2024 assessment roll available at this [link](https://drive.google.com/file/d/1pD89pezh6huc75B63sPLitD4woWlyeJK/view?ts=65c50b0b).
* *Sales universe:* 
  * Transactions considered arm's length based on the following classes `"03-ARM'S LENGTH"`, `"19-MULTI PARCEL ARM'S LENGTH"`, `"03-ARMS LENGTH"`, `"11-FROM LENDING INSTITUTION EXPOSED"`, `"11-FROM LANDING INSTITUTION EXPOSED"`
  * For residential property classes such that `PROPCLASSDESC` is `"RESIDENTIAL-IMPROVED"` or `"RESIDENTIAL CONDOMINIUMS"` and `TAXSTATUS` is `'TAXABLE'`
  * The sale year equals 2023 and there are no duplicate transactions for a given property parcel
  * For properties located in Detroit such that `TAXPCITY` is `'DETROIT'`
  * For properties that sold for more than $1,000 and were assessed over $1,000
  * Excludes properties with an assessment value to sales ratio greater than 1.5 the interquartile range.
* For a link to the report see this link: https://mansueto-institute.github.io/
* The study consists of 3,140 properties sold between 4/1/23 to 3/31/24. 

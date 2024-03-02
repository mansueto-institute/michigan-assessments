
library(tidyverse)
library(cmfproperty)
library(lubridate)
library(readxl)

assessroll <- read_csv('/Users/nm/Desktop/Projects/work/cmf-reports/detroit-lansing-data/detroit/Tentative_Assessment_Roll_(2024).csv')
# Sales data: https://data.detroitmi.gov/datasets/property-sales-1/explore?showTable=true 
sales <- read_csv('/Users/nm/Desktop/Projects/work/cmf-reports/detroit-lansing-data/detroit/Property_Sales.csv')

sales <- sales %>%
  filter(term_of_sale %in% c("03-ARM'S LENGTH", "19-MULTI PARCEL ARM'S LENGTH", "03-ARMS LENGTH", "11-FROM LENDING INSTITUTION EXPOSED", "11-FROM LANDING INSTITUTION EXPOSED")) %>%
  mutate(
    sale_date_sr = as.Date(str_sub(sale_date, 1, 10), format = "%Y/%m/%d"),
    sale_year_sr = as.integer(str_sub(sale_date, 1, 4))) %>%
  filter(sale_year_sr == 2023 | sale_year_sr == 2024) %>%
  group_by(parcel_number) %>%
  mutate(dup_count = row_number(desc(sale_date))) %>%
  ungroup() %>%
  filter(dup_count == 1) %>%
  select(parcel_number, sale_year_sr, sale_date_sr, sale_price, term_of_sale) 
  
assessroll_full <- assessroll %>%
  mutate(sale_year_ar = as.integer(str_sub(SALEDATE, 1,4)),
         sale_date_ar = as.Date(str_sub(SALEDATE, 1,10), format = "%Y/%m/%d")#,
  ) %>%
  left_join(., sales %>% select(parcel_number, sale_year_sr, sale_date_sr, sale_price, term_of_sale), by = c('PARCELNO' = 'parcel_number')) %>%
  select(PARCELNO, propclass, TAXPCITY, PROPCLASSDESC, TAXSTATUS, SALEPRICE, SALEDATE, ASSESSEDVALUE, TAXABLEVALUE, sale_year_ar, sale_date_ar, sale_year_sr, sale_date_sr, sale_price, term_of_sale) %>%
  filter(sale_year_ar >= 2023 | sale_year_sr >= 2023) %>%
  mutate(sale_price_coalesced = coalesce(sale_price, SALEPRICE),
         sale_date_coalesced = coalesce(sale_date_sr, sale_date_ar)
         ) %>%
  filter(PROPCLASSDESC %in% c("RESIDENTIAL-IMPROVED", "RESIDENTIAL CONDOMINIUMS"),
         TAXSTATUS == 'TAXABLE') %>%
  group_by(PARCELNO) %>%
  mutate(dup = row_number(desc(sale_date_coalesced))) %>%
  ungroup() %>%
  filter(dup == 1) %>%
  filter(TAXPCITY == 'DETROIT') %>% 
  #mutate(file_year = 2023) %>%
  filter(sale_price_coalesced >= 1000 & ASSESSEDVALUE >= 1000) %>%
  select(PARCELNO, propclass, TAXPCITY, PROPCLASSDESC, TAXSTATUS, sale_price_coalesced, sale_date_coalesced, ASSESSEDVALUE, TAXABLEVALUE)

assessroll_full <- assessroll_full %>%
  mutate(sale_bucket = case_when(sale_price_coalesced <= 50000 ~ '1 - <$50K',
                                 sale_price_coalesced > 50000 & sale_price_coalesced <= 100000 ~ '2 - $50K-$100K',
                                 sale_price_coalesced > 100000 & sale_price_coalesced <= 200000 ~ '3 - $100K-$200K', 
                                 sale_price_coalesced > 200000 & sale_price_coalesced <= 400000 ~ '4 - $200K-$400K', 
                                 sale_price_coalesced > 400000 ~ '5 - $400K+')) %>%
  mutate(sale_year = lubridate::year(sale_date_coalesced)) %>%
  #mutate_at(vars(sale_year), as.integer) %>%
  mutate(sale_quintile = ntile(sale_price_coalesced, 5)) %>%
  group_by(sale_quintile) %>%
  mutate(median_quintile_sale_price = median(sale_price_coalesced)) %>%
  ungroup()

assessroll_full <- assessroll_full %>%
  mutate(av_ratio = ASSESSEDVALUE/sale_price_coalesced) %>%
  group_by(sale_year) %>%
  mutate(
    quartile_1 = quantile(x = av_ratio, 1 / 4),
    quartile_3 = quantile(x = av_ratio, 3 / 4),
    iqr = quartile_3 - quartile_1,
    lower_trim_point = quartile_1 - (iqr * 1.5),
    upper_trim_point = quartile_3 + (iqr * 1.5)
  ) %>%
  ungroup() %>%
  mutate(non_outlier_flag_iaao = case_when(av_ratio > lower_trim_point & av_ratio < upper_trim_point ~ 1, 
                                       TRUE ~ as.integer(0))) %>%
  filter(non_outlier_flag_iaao == 1)

assessroll_full_input <- assessroll_full  %>%
  select(PARCELNO, sale_price_coalesced, ASSESSEDVALUE, sale_year) %>%
  filter(sale_year == 2023)

ratios <- cmfproperty::reformat_data(
  assessroll_full_input,
  sale_col = "sale_price_coalesced",
  assessment_col = "ASSESSEDVALUE",
  sale_year_col = "sale_year",
)

cmfproperty::make_report(ratios, 
                         jurisdiction_name = "Detroit, Michigan",
                         output_dir = "/Users/nm/Desktop/Projects/work/cmf-reports/detroit-lansing-data") 


# -------------------------------------------------------------------------


# https://cmf-uchicago.github.io/cmfproperty/reference/index.html
# https://detroitmi.gov/departments/office-chief-financial-officer/ocfo-divisions/office-assessor/sales-study#:~:text=2024%20All%20Transactions%20Data
# https://data-wayne.opendata.arcgis.com/datasets/2023-assessments-wayne/about
# https://www.waynecounty.com/departments/mb/equalization/annual-assessment-data.aspx


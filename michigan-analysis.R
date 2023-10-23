
library(tidyverse)
library(lubridate)
library(readxl)
# devtools::install_github("cmf-uchicago/cmfproperty")
library(cmfproperty)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# FHFA HPI for Three-Digit ZIP Codes (All-Transactions Index)
hpi_df<- list('year' = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
              'hpi_3zip' = c(135.50, 131.40, 129.88, 135.09, 140.87, 151.13, 156.41, 167.34, 177.28, 187.02, 193.97, 217.88, 249.00, 268.22),
              'index_3zip' = c( 1.979483395, 2.041248097, 2.06513705, 1.985491154, 1.904024988, 1.774763449, 1.714851992, 1.602844508, 1.512973827, 1.434178163, 1.382791153, 1.231044612, 1.077188755, 1),
              'hpi_metro' = c(133.2, 127.71, 125.06, 128.49, 134.06, 143.33, 148.43, 159.2, 168.59, 177.61, 185.64, 207.34, 237.26, 253.62),
              'index_metro' = c(1.904054054, 1.985905567, 2.027986566, 1.973850105, 1.891839475, 1.769483011, 1.708684228, 1.593090452, 1.504359689, 1.427960137, 1.366192631, 1.223208257, 1.06895389, 1)) %>%
  as.data.frame()

# https://dewittmi.gov/city-assessor/
dewitt <- read_excel('DEWITT TOWNSHIP FOIA EXPORT.xlsx')

df <- dewitt %>%
  filter(Parcels.propstatus == 'Active',
         Parcels.exemptcode == 'TAXABLE',
         # ParcelMaster.propcity == 'DEWITT',
         Parcels.propclass %in% c(401) ) %>% # 410 is mobile homes
  mutate_at(vars(ParcelMaster.propzip), list(as.character)) %>%
  mutate(zip3 = str_sub(ParcelMaster.propzip, 1,3)) %>%
  mutate(sale_date = ymd(ParcelMaster.lastSaleDate)) %>%
  mutate(sale_year = year(sale_date)) %>%
  select(zip3, ParcelMaster.propzip, ParcelMaster.propcity, Parcels.propclass, Parcels.pnum, ParcelReadonly.mborsev_1, sale_year, ParcelMaster.lastSalePrice, ParcelMaster.lastSaleDate, sale_date) %>%
  filter(sale_year >= 2018) %>%
  group_by(Parcels.pnum) %>%
  mutate(most_recent_sale = row_number(desc(sale_date))) %>%
  ungroup() %>%
  filter(most_recent_sale == 1) %>%
  rename(assessed_value = ParcelReadonly.mborsev_1,
         sale_price = ParcelMaster.lastSalePrice) %>%
  left_join(., hpi_df, by = c('sale_year' = 'year')) %>%
  mutate(sale_price_hpi = sale_price * index_metro,
         av_ratio = assessed_value / sale_price,
         av_ratio_hpi = assessed_value / sale_price_hpi) %>%
  filter(sale_price > 10000 & !is.na(sale_price), 
         assessed_value > 10000 & !is.na(assessed_value))

# Outlier rule according to IAAO standards (1.5 X IQR procedure to identify outlier ratios)
# https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf#page=54
df <- df %>%
  group_by(sale_year) %>%
  mutate(
    quartile_1 = quantile(x = av_ratio, 1 / 4),
    quartile_3 = quantile(x = av_ratio, 3 / 4),
    iqr = quartile_3 - quartile_1,
    lower_trim_point = quartile_1 - (iqr * 1.5),
    upper_trim_point = quartile_3 + (iqr * 1.5)
  ) %>%
  ungroup() %>%
  mutate(outlier_flag_iaao = case_when(av_ratio > lower_trim_point & av_ratio < upper_trim_point ~ 1, 
                                       TRUE ~ as.integer(0))) %>%
  filter(outlier_flag_iaao == 1)


# df <- df %>%
#   group_by(sale_year) %>%
#   mutate(
#     zscore = (sale_price - mean(sale_price))/sd(sale_price),
#     mean_zscore = mean(zscore),
#     sd_zscore = sd(zscore),
#     lower_zscore = mean_zscore - (3 * sd_zscore),
#     upper_zscore = mean_zscore + (3 * sd_zscore),
#     sale_price_outlier = ifelse(zscore > lower_zscore & zscore < upper_zscore, 1, 0)
#   )


# df %>% group_by(sale_price_outlier) %>% tally()

df <- df %>%
  select(Parcels.pnum, assessed_value, sale_year, sale_price, sale_date, sale_price_hpi, av_ratio, av_ratio_hpi)
  
ratios <-
  cmfproperty::reformat_data(
    df,
    sale_col = "sale_price_hpi",
    assessment_col = "assessed_value",
    sale_year_col = "sale_year",
  )

cmfproperty::make_report(ratios, 
                         jurisdiction_name = "Dewitt Charter Township, Michigan",
                         output_dir = paste0(getwd(), "/reports") )




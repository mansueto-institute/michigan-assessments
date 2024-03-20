

library(tidyverse)
library(cmfproperty)
library(lubridate)
library(readxl)
library(scales)
library(patchwork)
library(tidymodels)

# Inflation adjustments ---------------------------------------------------

# https://www.michigan.gov/treasury/-/media/Project/Websites/treasury/STC/Bulletins/2023/Bulletin-16-of-2023---Inflation-Rate-Multiplier-for-2024.pdf?rev=f423a9acbe454ee4b253a8b2d0a56b9b&hash=5026C95F56D2AD98991EB417B821C1A2
inflation_adj <- list(year = c(2021, 2022, 2023, 2024),
                      inflation_rate_multipliers = c(1.014, 1.033, 1.05, 1.05),
                      index_2024 = c(1.1389, 1.1025, 1.0500, 1.0000)
                      ) %>% as.data.frame()

# Sales used --------------------------------------------------------------

# https://detroitmi.gov/document/2024-valuation-sales-used
sales_training <- read_excel('/Users/nm/Desktop/detroit-study/data/2024 Resi Arms Length transactions.xlsx') %>%
  rename_all(list(tolower)) %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  mutate(
    sale_date_sr = as.Date(str_sub(sale_date, 1, 10), format = "%Y-%m-%d"),
    sale_year_sr = as.integer(str_sub(sale_date, 1, 4))) %>%
  # ensure no duplicates
  group_by(parcel_number) %>%
  mutate(dup_count = row_number(desc(sale_date))) %>%
  ungroup() %>%
  filter(dup_count == 1) %>%
  select(parcel_number, sale_year_sr, sale_date_sr, sale_price, terms_of_sale) %>%
  # join in inflation index
  left_join(., inflation_adj %>% select(year, index_2024), by = c('sale_year_sr'='year')) %>%
  mutate(sale_price_2024 = sale_price * index_2024) %>%
  # remove nulls in sale price
  filter(!is.na(sale_price_2024)) %>%
  rename(sale_date_training = sale_date_sr, 
         sale_price_training = sale_price, 
         sale_price_2024_training = sale_price_2024) %>%
  select(parcel_number, sale_year_sr, sale_date_training, sale_price_training, sale_price_2024_training) #%>%
  #mutate(sale_date_training_char = as.character(sale_date_training))

min(sales_training$sale_date_training)
max(sales_training$sale_date_training)

# Recent sales data -------------------------------------------------------

# Recent sales data: https://data.detroitmi.gov/datasets/property-sales-1/explore?showTable=true 
sales_validation <- read_csv('/Users/nm/Desktop/detroit-study/data/Property_Sales.csv') %>%
  filter(term_of_sale %in% c("03-ARM'S LENGTH", "19-MULTI PARCEL ARM'S LENGTH", "03-ARMS LENGTH", "11-FROM LENDING INSTITUTION EXPOSED", "11-FROM LANDING INSTITUTION EXPOSED")) %>%
  mutate(
    sale_date_sr = as.Date(str_sub(sale_date, 1, 10), format = "%Y/%m/%d"),
    sale_year_sr = as.integer(str_sub(sale_date, 1, 4))) %>%
  # restrict to post 2023
  filter(sale_year_sr >= 2023) %>%
  # ensure no duplicates
  group_by(parcel_number) %>%
  mutate(dup_count = row_number(desc(sale_date))) %>%
  ungroup() %>%
  filter(dup_count == 1) %>%
  select(parcel_number, sale_year_sr, sale_date_sr, sale_price, term_of_sale, economic_condition_factor_neigh)  %>%
  # join in inflation index
  left_join(., inflation_adj %>% select(year, index_2024), by = c('sale_year_sr'='year')) %>%
  mutate(sale_price_2024 = sale_price * index_2024)  %>%
  # remove nulls in sale price
  filter(!is.na(sale_price_2024)) %>%
  rename(sale_date_validation = sale_date_sr, 
         sale_price_validation = sale_price, 
         sale_price_2024_validation = sale_price_2024) %>%
  select(parcel_number, sale_year_sr, sale_date_validation, sale_price_validation, sale_price_2024_validation, economic_condition_factor_neigh) #%>%
  #mutate(sale_date_validation_char = as.character(sale_date_validation))

min(sales_validation$sale_date_validation)
max(sales_validation$sale_date_validation)

# Assessment roll ---------------------------------------------------------

# https://data.detroitmi.gov/datasets/detroitmi::tentative-assessment-roll-2024/about
assessroll <- read_csv('/Users/nm/Desktop/detroit-study/data/Tentative_Assessment_Roll_(2024).csv')

assessroll_clean <- assessroll %>%
  rename_all(list(tolower)) %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  mutate(sale_date_ar = as.Date(str_sub(saledate, 1,10), format = "%Y/%m/%d"),
         sale_year_ar = as.integer(str_sub(saledate, 1, 4))) %>%
  # filter(sale_year_ar >= 2021) %>%
  mutate(sale_date_ar_char = as.character(sale_date_ar)) %>%
  #select(parcelno, propclass, propclassdesc, taxstatus, saleprice, saledate, sale_date_ar, sale_year_ar, assessedvalue, taxablevalue) %>%
  select(parcelno, propclass, propclassdesc, taxstatus, assessedvalue, taxablevalue) %>%
  #left_join(., inflation_adj %>% select(year, index_2024), by = c('sale_year_ar'='year')) %>%
  #mutate(sale_price_assessroll_2024 = saleprice * index_2024) %>%
  filter(propclassdesc %in% c("RESIDENTIAL-IMPROVED", "RESIDENTIAL CONDOMINIUMS"),
         taxstatus == 'TAXABLE') 

sales_study_validation <- assessroll_clean %>%
  # filter(sale_date_ar >= as.Date('2023-04-01', format = "%Y-%m-%d")) %>%
  inner_join(., sales_validation, by = c('parcelno'='parcel_number')) %>% 
  mutate(sale_date_coalesced = coalesce(sale_date_validation)) %>% # , sale_date_ar
  filter(!is.na(sale_date_coalesced)) %>%
  mutate(sale_date_range = case_when(sale_date_coalesced >= as.Date('2023-04-01', format = "%Y-%m-%d") ~ paste0('2023-04-01 to ',max(sales_validation$sale_date_validation)),
                                     sale_date_coalesced >= as.Date('2021-04-01', format = "%Y-%m-%d") & sale_date_coalesced <= as.Date('2023-03-31', format = "%Y-%m-%d") ~ '2021-04-01 to 2023-03-31',
                                     TRUE ~ as.character('outside study'))) %>%
  # filter(sale_date_range != 'outside study') %>%
  filter(sale_date_range == paste0('2023-04-01 to ',max(sales_validation$sale_date_validation))) %>%
  mutate(sale_price_coalesced = coalesce(sale_price_2024_validation)) %>% # , sale_price_assessroll_2024
  filter(!is.na(sale_price_coalesced)) %>%
  # ensure no duplicates
  group_by(parcelno, sale_date_range) %>%
  mutate(dup = row_number(desc(sale_date_coalesced))) %>%
  ungroup() %>%
  filter(dup == 1) %>%
  # filter above $1k price cutoff
  filter(sale_price_coalesced >= 1000 & assessedvalue >= 1000) %>%
  select(parcelno, propclass, propclassdesc, taxstatus, assessedvalue, sale_date_coalesced, sale_date_range, sale_price_coalesced, economic_condition_factor_neigh) %>%
  # bucketize price
  mutate(sale_bucket = case_when(sale_price_coalesced <= 50000 ~ '1 - <$50K',
                                 sale_price_coalesced > 50000 & sale_price_coalesced <= 100000 ~ '2 - $50K-$100K',
                                 sale_price_coalesced > 100000 & sale_price_coalesced <= 200000 ~ '3 - $100K-$200K', 
                                 sale_price_coalesced > 200000 & sale_price_coalesced <= 400000 ~ '4 - $200K-$400K', 
                                 sale_price_coalesced > 400000 ~ '5 - $400K+')) %>%
  mutate(sale_year = lubridate::year(sale_date_coalesced)) %>%
  group_by(sale_date_range) %>%
  mutate(sale_quintile = ntile(sale_price_coalesced, 5)) %>%
  ungroup() %>%
  group_by(sale_date_range, sale_quintile) %>%
  mutate(median_quintile_sale_price = median(sale_price_coalesced)) %>%
  ungroup() %>%
  # IQR filter
  mutate(av_ratio = assessedvalue/sale_price_coalesced) %>%
  group_by(sale_date_range) %>%
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
  filter(non_outlier_flag_iaao == 1) %>%
  select(parcelno, assessedvalue, sale_date_coalesced, sale_date_range, sale_price_coalesced, economic_condition_factor_neigh, sale_bucket, sale_year, sale_quintile, median_quintile_sale_price, av_ratio)


sales_study_training <- assessroll_clean %>%
  # filter(sale_date_ar >= as.Date('2021-04-01', format = "%Y-%m-%d") ) %>%
  inner_join(., sales_training, by = c('parcelno'='parcel_number')) %>% 
  mutate(sale_date_coalesced = coalesce(sale_date_training)) %>% #, sale_date_ar
  filter(!is.na(sale_date_coalesced)) %>%
  mutate(sale_date_range = case_when(sale_date_coalesced >= as.Date('2023-04-01', format = "%Y-%m-%d") ~ '2023-04-01 to 2024-02-29',
                                     sale_date_coalesced >= as.Date('2021-04-01', format = "%Y-%m-%d") & sale_date_coalesced <= as.Date('2023-03-31', format = "%Y-%m-%d") ~ '2021-04-01 to 2023-03-31',
                                     TRUE ~ as.character('outside study'))) %>%
  # filter(sale_date_range != 'outside study') %>%
  filter(sale_date_range == '2021-04-01 to 2023-03-31') %>%
  mutate(sale_price_coalesced = coalesce(sale_price_2024_training)) %>% # , sale_price_assessroll_2024
  filter(!is.na(sale_price_coalesced)) %>%
  # ensure no duplicates
  group_by(parcelno, sale_date_range) %>%
  mutate(dup = row_number(desc(sale_date_coalesced))) %>%
  ungroup() %>%
  filter(dup == 1) %>%
  # filter above $1k price cutoff
  filter(sale_price_coalesced >= 1000 & assessedvalue >= 1000) %>%
  select(parcelno, propclass, propclassdesc, taxstatus, assessedvalue, sale_date_coalesced, sale_date_range, sale_price_coalesced) %>%
  # bucketize price
  mutate(sale_bucket = case_when(sale_price_coalesced <= 50000 ~ '1 - <$50K',
                                 sale_price_coalesced > 50000 & sale_price_coalesced <= 100000 ~ '2 - $50K-$100K',
                                 sale_price_coalesced > 100000 & sale_price_coalesced <= 200000 ~ '3 - $100K-$200K', 
                                 sale_price_coalesced > 200000 & sale_price_coalesced <= 400000 ~ '4 - $200K-$400K', 
                                 sale_price_coalesced > 400000 ~ '5 - $400K+')) %>%
  mutate(sale_year = lubridate::year(sale_date_coalesced)) %>%
  # make quintiles
  group_by(sale_date_range) %>%
  mutate(sale_quintile = ntile(sale_price_coalesced, 5)) %>%
  ungroup() %>%
  group_by(sale_date_range, sale_quintile) %>%
  mutate(median_quintile_sale_price = median(sale_price_coalesced)) %>%
  ungroup() %>%
  mutate(av_ratio = assessedvalue/sale_price_coalesced) %>%
  # IQR filter
  group_by(sale_date_range) %>%
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
  filter(non_outlier_flag_iaao == 1) %>%
  select(parcelno, assessedvalue, sale_date_coalesced, sale_date_range, sale_price_coalesced, sale_bucket, sale_year, sale_quintile, median_quintile_sale_price, av_ratio)

# Analytical tables -------------------------------------------------------

out_path = '/Users/nm/Desktop/detv3/'
#folder_out = 'validation'
#assessroll_full_input <- sales_study_validation 
folder_out = 'training'
assessroll_full_input <- sales_study_training

side_analysis <- assessroll_full_input %>%
  mutate(av_ratio = assessedvalue/ sale_price_coalesced,
         av_ratio_level = case_when(av_ratio > .80 ~ 'Over 80% selling price',
                                    av_ratio > .60 & av_ratio <= .80 ~ '60% to 80% selling price',
                                    av_ratio > .50 & av_ratio <= .60 ~ '50% to 60% selling price',
                                    av_ratio > .40 & av_ratio <= .50 ~ '40% to 50% selling price',
                                    av_ratio > .20 & av_ratio <= .40 ~ '20% to 40% selling price',
                                    av_ratio <= .20 ~ 'Under 20% selling price',
                                    TRUE ~ ''),
         assessment_overunder = case_when(av_ratio > .5 ~ 'Over 50% selling price',
                                          av_ratio <= .5 ~ '50% or less selling price'),
         sale_decile = ntile(sale_price_coalesced, 10),
         sale_quintile = ntile(sale_price_coalesced, 5),
         sale_quartile = ntile(sale_price_coalesced, 4),
         sale_tercile = ntile(sale_price_coalesced, 3),
         ) %>%
  mutate(count = 1)

# decile 50% over / under
decile_50_thresh <- side_analysis %>% 
  group_by(sale_decile, assessment_overunder) %>%
  summarize(count_by_over_under_assess = sum(count),
            median_sale_price = median(sale_price_coalesced),
            mean_sale_price = mean(sale_price_coalesced)) %>%
  ungroup() %>%
  group_by(sale_decile) %>%
  mutate(count_total = sum(count_by_over_under_assess)) %>%
  ungroup() %>%
  mutate(share = count_by_over_under_assess / count_total) %>%
  mutate(assessment_overunder = factor(assessment_overunder, (c('50% or less selling price','Over 50% selling price')))) %>%
  arrange(sale_decile, factor(assessment_overunder, levels = c('50% or less selling price','Over 50% selling price'))) %>%
  group_by(sale_decile) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% 
  ungroup() 

# decile detailed thresholds
decile_detailed_thresh <- side_analysis %>% 
  group_by(sale_decile, av_ratio_level) %>%
  summarize(count_by_av_ratio_level = sum(count),
            median_sale_price = median(sale_price_coalesced),
            mean_sale_price = mean(sale_price_coalesced)) %>%
  ungroup() %>%
  group_by(sale_decile) %>%
  mutate(count_total = sum(count_by_av_ratio_level)) %>%
  ungroup() %>%
  mutate(share = count_by_av_ratio_level / count_total) %>%
  mutate(av_ratio_level = factor(av_ratio_level, rev(c('Under 20% selling price', '20% to 40% selling price', '40% to 50% selling price', '50% to 60% selling price', '60% to 80% selling price', 'Over 80% selling price')))) %>%
  arrange(sale_decile, factor(av_ratio_level, levels = c('Under 20% selling price', '20% to 40% selling price', '40% to 50% selling price', '50% to 60% selling price', '60% to 80% selling price', 'Over 80% selling price'))) %>%
  group_by(sale_decile) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% 
  ungroup() 

# decile only
decile_only <- side_analysis %>% 
  group_by(sale_decile) %>%
  summarize(median_sale_price = dollar(median(sale_price_coalesced)),
            mean_sale_price = dollar(round(mean(sale_price_coalesced),0)),
            min_sale_price = min(sale_price_coalesced),
            max_sale_price = max(sale_price_coalesced),
            median_av_ratio = round(median(av_ratio),3),
            mean_av_ratio = round(mean(av_ratio),3),
            count = sum(count)) %>%
  ungroup() 

# quintile only
quintile_only <- side_analysis %>% 
  group_by(sale_quintile) %>%
  summarize(median_sale_price = dollar(median(sale_price_coalesced)),
            mean_sale_price = dollar(round(mean(sale_price_coalesced),0)),
            median_av_ratio = round(median(av_ratio),3),
            mean_av_ratio = round(mean(av_ratio),3),
            count = sum(count)) %>%
  ungroup() 

# above 6 decile
above_6_decile <- side_analysis %>% 
  mutate(above_6 = case_when(sale_decile >= 6 ~ 'above 6',
                             TRUE ~ 'below')) %>%
  group_by(above_6, av_ratio_level) %>%
  summarize(median_sale_price = dollar(median(sale_price_coalesced)),
            mean_sale_price = dollar(round(mean(sale_price_coalesced),0)),
            min_sale_price = min(sale_price_coalesced),
            max_sale_price = max(sale_price_coalesced),
            median_av_ratio = round(median(av_ratio),3),
            mean_av_ratio = round(mean(av_ratio),3),
            count = sum(count)) %>%
  ungroup() %>%
  group_by(above_6) %>%
  mutate(share = count / sum(count)) %>%
  ungroup()

# topline
topline <- side_analysis %>% 
  summarize(median_sale_price = dollar(median(sale_price_coalesced)),
            mean_sale_price = dollar(round(mean(sale_price_coalesced),0)),
            median_av_ratio = round(median(av_ratio),3),
            mean_av_ratio = round(mean(av_ratio),3),
            count = sum(count)) %>%
  ungroup() 

# writexl::write_xlsx(
#   list('above_6_decile' = above_6_decile, 'topline' = topline, 'decile_only' = decile_only, 'quintile_only' = quintile_only), 
#            col_names = TRUE, format_headers = TRUE,
#   path = paste0(out_path,folder_out,'/tables.xlsx'))

# Regressivity charts -----------------------------------------------------

(decile_viz1 <- ggplot(data = decile_50_thresh, aes(x = sale_decile, y = share, fill = assessment_overunder)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label= percent(share, accuracy = 1L) ), position=position_dodge(width=0.9), vjust=-0.25, fontface = "bold", size = 4) +
  scale_x_continuous(expand = c(.01,.01), breaks = c(1,2,3,4,5,6,7,8,9,10), name = 'Sale decile') +
  scale_y_continuous(expand = c(.0,.04), name = 'Share of sales', labels = percent_format(1L)) +
  labs(subtitle = paste0('Share of properties at each sale price decile above or below statutory mandated assessment level of 50%\nSales study spans ',unique(side_analysis$sale_date_range))) +
  scale_fill_manual(values = (c("#3288BD", "#D53E4F")), name = "",
                  breaks = c('50% or less selling price','Over 50% selling price')) + 
  theme_classic() + 
  theme(legend.position = 'bottom',
        plot.subtitle = element_text(size = 13, hjust = .5,  face = "bold", color = "#333333"),
        legend.text = element_text(size = 13, face = "bold", color = "#333333"),
        axis.text = element_text(size = 13, face = "bold", color = "#333333"), axis.title = element_text(size = 13, face = "bold", color = "#333333")
        ))
  
(decile_viz2 <- ggplot(data = decile_detailed_thresh, aes(x = sale_decile, y = share, fill = av_ratio_level)) +
    geom_bar(position="stack", stat="identity", color = 'white', linewidth = .5) +
    geom_text(data = decile_detailed_thresh, 
              aes(label=ifelse(share >= 0.05, paste0(round(share*100,0),"% (",comma(count_by_av_ratio_level),")"),""), 
                  y = pos_id_share, x = sale_decile), color = '#333333', fontface = "bold", size = 4) +
    scale_x_continuous(expand = c(.01,.01), breaks = c(1,2,3,4,5,6,7,8,9,10), name = 'Sale decile') +
    scale_y_continuous(expand = c(.0,.04), name = 'Share of sales', labels = percent_format(1L) ) +
  labs(subtitle = paste0('Share of properties at each sale price decile above or below statutory mandated assessment level of 50%\nSales study spans ',unique(side_analysis$sale_date_range))) +
    scale_fill_manual(values = rev(c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD")), name = "",
                      breaks=c('Under 20% selling price', '20% to 40% selling price', '40% to 50% selling price', 
                               '50% to 60% selling price', '60% to 80% selling price', 'Over 80% selling price')) +
    theme_classic() +
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
    theme(legend.position = 'bottom',
          plot.subtitle = element_text(size = 13, hjust = .5,  face = "bold", color = "#333333"),
          legend.text = element_text(size = 13, face = "bold", color = "#333333"),
          axis.text = element_text(size = 13, face = "bold", color = "#333333"), axis.title = element_text(size = 13, face = "bold", color = "#333333")
          ))

ggsave(plot = decile_viz1, filename = paste0(out_path,folder_out,'/decile_1.png'), dpi = 300, width = 10, height = 7)
ggsave(plot = decile_viz2, filename = paste0(out_path,folder_out,'/decile_2.png'), dpi = 300, width = 10, height = 7)

#ggsave(plot = decile_plot, filename = '/Users/nm/Desktop/decile_det.pdf', width = 17, height = 7)

(scatter1 <- ggplot(data = side_analysis, aes(x = sale_price_coalesced, y = av_ratio, color = av_ratio)) +
  geom_point(alpha = .5) +
  geom_hline(yintercept = .5, alpha = .6) +
  viridis::scale_color_viridis(name = 'Assessment to\nsale price ratio') +
  labs(subtitle = paste0('Assessed value ratio versus sale price indicating outlier high value properties\nSales study spans ',unique(side_analysis$sale_date_range))) +
  scale_y_continuous(labels = percent_format(), name = 'Assessed value to sale price ratio') +
  scale_x_continuous(expand = c(0,0), limits = c(0,1100000), labels = dollar_format(), name = 'Sale price') +
  theme_bw() + theme(
    plot.subtitle = element_text(size = 13, hjust = .5,  face = "bold", color = "#333333"),
    legend.text = element_text(size = 13, face = "bold", color = "#333333"),
    axis.text = element_text(size = 13, face = "bold", color = "#333333"), axis.title = element_text(size = 13, face = "bold", color = "#333333"),
    legend.title = element_text(size = 11, face = "bold", color = "#333333"),
    legend.position = 'bottom'))

ggsave(plot = scatter1 , filename = paste0(out_path,folder_out,'/scatter1.png'), dpi = 300, width = 8.5, height = 6)

(scatter2 <- ggplot(data = side_analysis, aes(x = log10(sale_price_coalesced), y = av_ratio, color = av_ratio)) +
  geom_point(alpha = .5) +
  geom_hline(yintercept = .5, alpha = .6) +
  viridis::scale_color_viridis(name = 'Assessment to\nsale price ratio') +
  labs(subtitle = 'Assessed value ratio versus sale price indicating outlier high value properties') +
  scale_y_continuous(labels = percent_format(), name = 'Assessed value to sale price ratio') +
  scale_x_continuous(expand = c(0,0),
    breaks = c(log10(1), log10(10), log10(100), log10(1000), log10(10000), log10(100000), log10(1000000),  log10(10000000)),
    labels = c('$1', '$10', '$100', '$1K', '$10K', '$100K', '$1M', '$10M'), name = 'Sale price'
  ) +
    theme_bw() + theme(
      plot.subtitle = element_text(size = 13, hjust = .5,  face = "bold", color = "#333333"),
      legend.text = element_text(size = 13, face = "bold", color = "#333333"),
      axis.text = element_text(size = 13, face = "bold", color = "#333333"), axis.title = element_text(size = 13, face = "bold", color = "#333333"),
      legend.title = element_text(size = 11, face = "bold", color = "#333333"),
      legend.position = 'bottom'))


av_average <- assessroll_full_input %>% 
  mutate(av_ratio = assessedvalue/sale_price_coalesced,
         sales_ntile = ntile(x = sale_price_coalesced, n = 100),
         count = 1) %>%
  group_by(sales_ntile, sale_date_range) %>%
  summarize(
    count = sum(count),
    av_ratio = mean(av_ratio),
    sale_price_coalesced = mean(sale_price_coalesced),
    .groups = 'keep'
  ) %>%
  ungroup()


(ratio_chart <- ggplot(data = av_average, aes(x = sale_price_coalesced, y = av_ratio)) +
  geom_hline(yintercept = .5, color = "#333333", alpha = .8, linetype = 2) +
  geom_smooth(method = "loess", formula = "y ~ x", aes(weight = count), se = FALSE, color = '#FF6F91', alpha = .5) +
  geom_point(alpha = .7, aes(size = count), color = '#333333') +
  # scale_color_manual(values = c("#66c2a5", "#8da0cb")) +
  scale_y_continuous(name = "Average assessed value to sale price ratio",  expand = c(0, 0)) + # limits = c(0, .3),
  scale_x_continuous(name = "Sale price\n(percentiled point estimates)", oob = scales::squish, expand = c(0, 20), 
                     breaks = c(0, 50000, 100000, 250000, 500000, 750000, 1000000), 
                     labels = c("", '$50K', '$100K', "$250K", "$500K", "$750K", "$1M")) + 
  scale_size_binned(name = "Number of sales", range = c(0, 3), n.breaks = 3, breaks = waiver(), labels = comma_format()) +
  labs(
    subtitle = paste0("Sales from ",unique(av_average$sale_date_range)," compared to 2024 assessment level in Detroit")
  ) +
  theme_classic() +
  theme(
    plot.subtitle = element_text(size = 13, hjust = .5,  face = "bold", color = "#333333"), # 
    plot.caption = element_text(size = 12, hjust = 0),
    legend.key.width = unit(40, "pt"),
    legend.position = "none", 
    #legend.text = element_text(size = 13, color = "#333333"),
    #legend.title = element_blank(),
    strip.text = element_text(size = 13), strip.background = element_blank(),
    axis.text = element_text(size = 10, face = "bold", color = "#333333"), axis.title = element_text(size = 12, face = "bold", color = "#333333"),
    plot.margin = margin(t = 15, r = 20, b = 10, l = 15)
  ))

ggsave(plot = ratio_chart, filename = paste0(out_path,folder_out,'/ratio_chart.png'), dpi = 300, width = 8.5, height = 6)

# IAAO Stats --------------------------------------------------------------

# https://github.com/cmf-uchicago/cmfproperty/blob/master/R/iaao_stats.R
# https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf#page=57

cod_df <-  assessroll_full_input %>% 
  mutate(av_ratio = assessedvalue/sale_price_coalesced,
         count = 1) %>%
  mutate(abs_dif = abs(av_ratio - median(av_ratio))) %>%
  summarise(
    cod = ((sum(abs_dif) / n()) / median(av_ratio)),
    n = n()
  )
(cod <- paste0('COD: ',round(cod_df$cod,4),' n-size: ', comma(cod_df$n)))

# -------------------------------------------------------------------------

prd_df <- assessroll_full_input %>% 
  mutate(av_ratio = assessedvalue/sale_price_coalesced,
         count = 1) %>%
  summarise(
    mean_av = mean(av_ratio , na.rm = TRUE),
    median_av = median(av_ratio ),
    weighted_mean_av = weighted.mean(av_ratio , sale_price_coalesced),
    prd = mean_av / weighted_mean_av,
    n = n()
  )
(prd <- paste0('PRD: ',round(prd_df$prd,4),' n-size: ', comma(prd_df$n)))

# -------------------------------------------------------------------------

prb_df <- assessroll_full_input %>% 
  mutate(av_ratio = assessedvalue/sale_price_coalesced,
         log2 = log(2),
         count = 1) %>%
  mutate(cod = 100 * sum(abs(av_ratio - stats::median(av_ratio)))/(n() * stats::median(av_ratio)),
         prd = mean(av_ratio, na.rm = TRUE)/stats::weighted.mean(av_ratio, sale_price_coalesced, na.rm = TRUE),
         prb_value = 0.50 * (assessedvalue/median(av_ratio)) + 0.50 * sale_price_coalesced,
         prb_ln_value = log(prb_value)/log(2),
         prb_pct_diff = (av_ratio - median(av_ratio))/median(av_ratio)
         )

lm_model <- linear_reg() %>% 
  fit(prb_pct_diff ~ prb_ln_value,  data = prb_df ) %>%
  tidy(., conf.int = TRUE) 
print(lm_model)

prb_stat <- lm_model %>% filter(term == 'prb_ln_value') %>% select(estimate) 
(prb <- paste0('PRB: ',round(prb_stat$estimate,4)))

# https://docs.google.com/spreadsheets/d/1Q2srayEp7twFh3HBNjWACc_5kvNXCBbEol8nPZ5vRag/edit#gid=55387093
writexl::write_xlsx(
  list('above_6_decile' = above_6_decile, 'topline' = topline, 
       'decile_only' = decile_only, 'quintile_only' = quintile_only,
       'iaao_stats' = list(stat = c(prb, prd, cod, side_analysis %>% nrow())) %>% as.data.frame()
       ), 
  col_names = TRUE, format_headers = TRUE,
  path = paste0(out_path,folder_out,'/tables_',folder_out,'.xlsx'))


# -------------------------------------------------------------------------

#' #' @param data dataframe of data
#' #' @param assessment_value_col string of assessment_value_col name
#' #' @param sale_price_col string of sale_price_col name
#' #' @return dataframe 
#' gen_iaao_stats <- function(data, assessment_value_col, sale_price_col) { 
#'   df_iaao <- data %>% 
#'     rename_at(vars(c(assessment_value_col, sale_price_col)), function(x) c('assessment_value','sale_price')) %>% 
#'     mutate(av_ratio = assessment_value/sale_price,
#'            log2 = log(2),
#'            count = 1) %>%
#'     mutate(cod = 100 * sum(abs(av_ratio - stats::median(av_ratio)))/(n() * stats::median(av_ratio)),
#'            prd = mean(av_ratio, na.rm = TRUE)/stats::weighted.mean(av_ratio, sale_price, na.rm = TRUE),
#'            prb_value = 0.50 * (assessment_value/median(av_ratio)) + 0.50 * sale_price,
#'            prb_ln_value = log(prb_value)/log(2),
#'            prb_pct_diff = (av_ratio - median(av_ratio))/median(av_ratio)
#'     )
#'   prb_model <- linear_reg() %>% 
#'     fit(prb_pct_diff ~ prb_ln_value,  data = df_iaao) %>%
#'     tidy(., conf.int = TRUE) 
#'   iaao_out <- list('cod' = c(unique(df_iaao$cod)) ,
#'                    'prd' = c(unique(df_iaao$prd)) ,
#'                    'prb' = c(prb_model %>% filter(term == 'prb_ln_value') %>% select(estimate) %>% distinct() %>% pull())) %>%
#'     as.data.frame()
#'   return(iaao_out)
#' }
#' 
#' test <- gen_iaao_stats(data = data_input, assessment_value_col = 'assessed_value', sale_price_col = 'sale_price')

library(data.table)

huggies = diapers_market[brand_desc == "HUGGIES", ]

diaper_prod_ids <- huggies$prod_id

# iterate through each file
p1 = fread(file = "DF_999361007.csv", header = TRUE)
p1 = fread(file = "DF_999361008.csv", header = TRUE)
p1 = fread(file = "DF_999361009.csv", header = TRUE)
p1 = fread(file = "DF_999361010.csv", header = TRUE)

p1 = merge(p1,
           diapers_customers,
           all.x = TRUE,
           all.y = FALSE,
           by = "cust_id")

min_prices = huggies[, min(tran_prod_paid_amt/tran_prod_sale_qty), by = prod_id]
p1[! is.na(expected_revenue), expected_profit := (expected_revenue - (expected_quan*min_prices[1]))]

# delete babies whose expected values are zero 
# because we don't want to elimilate customers based on rough age estimates.
p1 = p1[expected_profit != 0, ]

yes = p1[is.na(expected_profit), final_score := prob]
no = p1[!is.na(expected_profit), final_score := expected_profit*prob]

write.csv(yes, file = "9993610<#>_score1.csv", row.names = FALSE)
write.csv(no, file = "9993610<#>_score2.csv", row.names = TRUE)

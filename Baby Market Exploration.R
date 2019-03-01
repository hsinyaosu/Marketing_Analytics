
tran <- fread('cleantransaction.csv')
prod <- fread('product_table.csv')

# all the categories relate to baby in the market
library(stringr)
baby_related <- unique(prod[which(str_extract(prod$category_desc_eng, "BABY")=="BABY")]$category_desc_eng)
baby_diaper <- unique(prod$category_desc_eng[which(prod$category_desc_eng=='DIAPERS')])
baby_related <- unique(prod[which((str_extract(prod$category_desc_eng, "BABY")=="BABY")|
                                    str_extract(prod$category_desc_eng, "DIAPERS")=="DIAPERS") ]$category_desc_eng)

##[1] "OUT. BABY HYGIENE ITEMS"      "BABY HYGIENE"                 "BABY SHAMPOO AND CONDITIONER" "BABY BATH PRODUCTS"          
# [5] "DIAPERS"                      "BABY FOOD"

# see how many brand has those categories in the market
unique(prod[which(prod$category_desc_eng %in% baby_related)]$brand_desc)

## [1] "PRIVATE LABEL" "DODOT"         "HALIBUT"       "DOVE"          "ULTRA SUAVE"   "HUGGIES"       "SARBEC"        "CORINE FARME" 
# [9] "JBABY"         "MIXA BEBE"     "NUTRIBEN"      "LIONFRESH"     "MITOSYL"       "LUTSINE" 

market_quantity <- sum(tran[which(tran$category_desc_eng %in% baby_related)]$tran_prod_sale_qty)
tran_baby_market <- tran[which(tran$category_desc_eng %in% baby_related)]
huggies_quantity <- sum(tran_baby_market[which(tran_baby_market$brand_desc == 'HUGGIES')]$tran_prod_sale_qty)

as.numeric(huggies_quantity)/as.numeric(market_quantity)
## Huggies market share in baby market 0.01879625 in terms of quantity


# Huggies product categories
unique(prod_huggies <- prod[which(prod$brand_desc == 'HUGGIES'),]$category_desc_eng)

## "OUT. BABY HYGIENE ITEMS", "DIAPERS" 

# Market share in the above two category
tran_huggies <- tran_baby_market[which(tran_baby_market$brand_desc == 'HUGGIES')]
huggies_categories <- unique(prod_huggies <- prod[which(prod$brand_desc == 'HUGGIES'),]$category_desc_eng)
tran_baby_huggies_category <- sum(tran_baby_market[which(tran_baby_market$category_desc_eng %in% huggies_categories)]$tran_prod_sale_qty)
as.numeric(huggies_quantity)/as.numeric(tran_baby_huggies_category)

## market share in those two categories is 0.02051488

# customers in baby related market
length(unique(tran_baby_market$cust_id))
length(unique(tran_huggies$cust_id))

## market size in terms of customer number is 7452
## market share for Huggies in terms of customer number is 1205

# Competitor directly compete with Huggies
unique(tran_baby_market[which(tran_baby_market$category_desc_eng %in% huggies_categories)]$brand_desc)

## "PRIVATE LABEL" "DODOT"         "DOVE"       "JBABY"         "CORINE FARME"  "LIONFRESH" 

# Competitor in Diaper category
unique(tran_baby_market[which(tran_baby_market$category_desc_eng == 'DIAPERS')]$brand_desc)

## "DODOT"         "PRIVATE LABEL" "HUGGIES" 

# unit price of huggies comparing to the avg market
hygiene_item_huggies <- mean(tran_huggies[which(tran_huggies$category_desc_eng == 'OUT. BABY HYGIENE ITEMS')]$prod_unit_price)
hygiene_market <- mean(tran_baby_market[which(tran_baby_market$category_desc_eng == 'OUT. BABY HYGIENE ITEMS')]$prod_unit_price)

## avg Huggies price for OUT. BABY HYGIENE ITEMS products 4.149284
## avg market price for OUT. BABY HYGIENE ITEMS products 1.703869

# unit price of huggies comparing to the avg market
diapers_item_huggies <- mean(tran_huggies[which(tran_huggies$category_desc_eng == 'DIAPERS')]$prod_unit_price)
diapers_market <- mean(tran_baby_market[which(tran_baby_market$category_desc_eng == 'DIAPERS')]$prod_unit_price)

## avg Huggies price for DIAPER products 8.531686
## avg market price for DIAPER products 9.406105
library(data.table)
require(lubridate)

products = fread(file = "product_table.csv", header = TRUE)
transactions = fread(file = "transaction_table.csv", header = TRUE)

all = merge(transactions,
            products,
            by = "prod_id",
            all.x = TRUE,
            all.y = FALSE)

diapers_market = all[category_desc_eng == "DIAPERS", ]

# clean dates

diapers_customers = diapers_market[, .(count = sum(tran_prod_sale_qty), 
                                       first_purchase = min(tran_id/100000000000)), by = cust_id]
diapers_customers = diapers_customers[order(count, decreasing = TRUE),]

diapers_customers[, first_purchase := as.character(first_purchase)]

diapers_customers[, first_purchase := (paste(substr(first_purchase,1,4),
                                             substr(first_purchase,5,6),
                                             substr(first_purchase,7,8),
                                             sep = "-"))]

diapers_customers[, first_purchase := as_date(first_purchase)]

diapers_market[, tran_dt_new := as.Date(tran_dt)]
diapers_market = diapers_market[order(tran_dt_new, decreasing = FALSE), ]

for (i in c(1:nrow(diapers_customers))){
    customer <- diapers_customers$cust_id[i]
    this_customer_transactions = diapers_market[cust_id == customer, ]
    num <- nrow(this_customer_transactions)
    lst <- c(1)
    zeros <- rep(0, num-1)
    lst <- c(lst, zeros)
    diapers_market[cust_id == customer, bool_first := lst]
}


diapers_market[cust_id == 81849885, ]
mostfrequentbuyer = diapers_market[cust_id == 81849885, ]
mostfrequentbuyer = mostfrequentbuyer[, .(prod_id, tran_dt, tran_prod_sale_amt, tran_prod_sale_qty)]

# -------------------------------------------------------

diapers_market = diapers_market[order(tran_dt, decreasing = FALSE), ]

unique_customers <- diapers_customers$cust_id
num_transactions <- diapers_customers$count

for (i in c(1:length(unique_customers))){
    cm = unique_customers[i]
    nt = num_transactions[i]
}

diapers_market = diapers_market[order(tran_dt, decreasing = FALSE)]
diapers_market[interval := (tran_dt - shift(tran_dt, fill = first(tran_dt))), by = cust_id]

# estimate age ------------------------------------------
# create table that is indexed by customer and subcategory
cust_subcat = diapers_market[, .(first_purchase_dt = min(tran_dt)), by = c("cust_id", "sub_category_desc")]

# keep only T1 - T6 and order from smallest to largest size
cust_subcat = cust_subcat[(sub_category_desc != "RESGUARDO BEBE") & (sub_category_desc != "FRALDA CUECA"), ]
cust_subcat[, int_subcat := as.integer(substr(sub_category_desc, 9, 9))]
cust_subcat = cust_subcat[order(int_subcat, decreasing = FALSE),]
table(cust_subcat$int_subcat)

# 
unique_customers <- diapers_customers$cust_id

# check loop logic
#i <- unique_customers[2]
#cust_subcat[cust_id == i, ]
#cust_subcat[cust_id == i, ][2]
#diapers_market[cust_id == 47749869, .(sub_category_desc, tran_dt)]

bdays <- c()
for (i in unique_customers){ # for each customer
    cssub = cust_subcat[cust_id == i, ][2]
    
    birthday <- cssub$first_purchase_dt
    birthday <- as.Date(birthday)

    if(cssub$int_subcat >= 3){
    	year(birthday) <- year(birthday) - (cssub$int_subcat - 2)
    }
    else if(cssub$int_subcat == 2){
    	month(birthday) <- month(birthday) - 4
    }
    else if(cssub$int_subcat == 1){
    	month(birthday) <- month(birthday) - 2
    }
    else{
    	print("error")
    }
    
    bdays <- c(bdays, birthday)
}

cust_birth = data.table(cust_id = unique_customers, birthday = bdays)

diapers_customers = merge(diapers_customers,
                          cust_birth,
                          by = "cust_id",
                          all.x = TRUE,
                          all.y = FALSE)

diapers_customers[is.na(birthday), birthday := first_purchase]
diapers_customers[, birthday := as_date(birthday)]
diapers_customers = diapers_customers[! is.na(birthday), ]


deadline = as.Date("20180401", "%Y%m%d")
diapers_customers[, age := round((deadline - birthday)/30)]

# estimate expected value -------------------------------
howmanymorediapers <- function(age){
    howmanymoremonths <- 37 - age
    if(howmanymoremonths <= 0){
        return(0)
    }
    else if(howmanymoremonths >= 36){ # 0-1 month olds
        howmanymorediapers
        return((howmanymoremonths - 35)*360 + 1200 + 1680 + 4320)
    }
    else if(howmanymoremonths >= 32){ # 1-5 month olds
        return((howmanymoremonths - 31)*300 + 1680 + 4320)
    }
    else if(howmanymoremonths >= 25){ # 5-12 month olds
        return((howmanymoremonths - 24)*240 + 4320)
    }
    else if(howmanymoremonths > 0){
        return(howmanymoremonths*180)
    }
    else{
        return(1000000)
    }
}

# get expected quantity
templst <- lapply(diapers_customers$age, howmanymorediapers)
diapers_customers[, expected_quan := as.numeric(templst)]

# get average sell price
temp = diapers_market[brand_desc == "HUGGIES" , .(tran_prod_paid_amt, tran_prod_sale_qty)]
avg_rev = mean(temp$tran_prod_paid_amt/temp$tran_prod_sale_qty)

diapers_customers[, expected_value := expected_quan*avg_rev]

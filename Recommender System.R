library(dplyr)
library(tidyr)
library(recommenderlab)


# read in data 
transaction <- read.csv("D:/Emory/Marketing Analytics/Project 1/transaction_table.csv")
product <- read.csv("D:/Emory/Marketing Analytics/Project 1/product_table.csv")
transaction_product <- inner_join(transaction,product, by="prod_id")

#create a table that ony includes baby products
baby_market <- transaction_product %>% filter(category_desc_eng %in% c('OUT. BABY HYGIENE ITEMS','DIAPERS','BABY BATH PRODUCTS','BABY FOOD','BABY HYGIENE','BABY SHAMPOO AND CONDITIONER'))
baby_market$tran_dt <- as.Date(baby_market$tran_dt)

#For each customer in the baby market, calculate purchasing frequency of the baby products they purchase (frequency=total/daydiff where daydiff is the difference in days between their first purchase and the last) 
baby_market_cust <- baby_market %>% select(cust_id,prod_id,tran_dt,tran_prod_sale_qty) %>% group_by(cust_id,prod_id) %>% mutate(total=sum(tran_prod_sale_qty),earliest=min(tran_dt), latest=max(tran_dt),daydiff=as.integer(difftime(latest,earliest,units="days")),frequency=total/daydiff) %>% filter(earliest != latest) 

baby_market_stats <- unique(baby_market_cust[,-c(3,4)])

#create affiliation matrix and use purchasing frequency as the rating
affiliation_df <- baby_market_cust %>% select(cust_id,prod_id, frequency)
affiliation_df <- unique(affiliation_df)
affiliation_df <- affiliation_df[complete.cases(affiliation_df),]
matrix = spread(affiliation_df,cust_id,frequency)
affiliation_matrix = matrix[, -1]
affiliation_matrix <- as.data.frame(affiliation_matrix)
rownames(affiliation_matrix) <- matrix$prod_id
ratings = as(as.matrix(affiliation_matrix),"realRatingMatrix")

#create evluation scheme of the recommender sytsem with a train-test split ratio of 80:20
e <- evaluationScheme(ratings, method="split", train=0.8, k=NULL, given=-1,goodRating=NA)

#create recommender models for both user-based and item-based collaborative filtering methods with cosine, Jacard and Pearson similrity measures. One thing worth noting is that for the 'recommenderlab' library, rows are customers and columns are products by default in the affiliation matrix so ratings of products are generated for each customer to see which products a given customer is more likely to buy. However, in our case, since our goal is to find out the potential customers for each Huggies product, we're actually "recommending" customers to products in our case. Therefore, our rows are products and our columns are customers. And the result is user-based CF is actually item-based CF in our case and vice versa. 

UBCF_cosine = Recommender(getData(e, "train"), method = "UBCF")
IBCF_cosine = Recommender(getData(e, "train"), method = "IBCF")
UBCF_jaccard = Recommender(getData(e, "train"), method = "UBCF", param = list(method = "Jaccard"))
IBCF_jaccard = Recommender(getData(e, "train"), method = "IBCF", param = list(method = "Jaccard"))
UBCF_pearson = Recommender(getData(e, "train"), method = "UBCF", param = list(method = "Pearson"))
IBCF_pearson = Recommender(getData(e, "train"), method = "IBCF", param = list(method = "Pearson"))

#Create recommendation ratings
p1 <- predict(UBCF_cosine, getData(e, "known"), type="ratings")
p2 <- predict(IBCF_cosine, getData(e, "known"), type="ratings")
p3 <- predict(UBCF_jaccard, getData(e, "known"), type="ratings")
p4 <- predict(IBCF_jaccard, getData(e, "known"), type="ratings")
p5 <- predict(UBCF_pearson, getData(e, "known"), type="ratings")
p6 <- predict(IBCF_pearson, getData(e, "known"), type="ratings")

#calcualte RMSE, MSE and MAE
error <- rbind(UBCF.cosine = calcPredictionAccuracy(p1, getData(e, "unknown")),
               IBCF.cosine = calcPredictionAccuracy(p2, getData(e, "unknown")),
               UBCF.jaccard = calcPredictionAccuracy(p3, getData(e, "unknown")),
               IBCF.jaccard = calcPredictionAccuracy(p4, getData(e, "unknown")),
               UBCF.pearson = calcPredictionAccuracy(p5, getData(e, "unknown")),
               IBCF.pearson = calcPredictionAccuracy(p6, getData(e, "unknown")))

#item-based collaborative filtering with Pearson similarity has the best overall score
#but as explained before, in our case, it is actually user-based collaborative #filtering because we flipped rows and columns
print(error)

#Find Huggies product IDs and display ratings for each Huggies product
#There're 7 Huggies products in total
unique(Huggies$prod_id)
rec.one = predict(IBCF_pearson, ratings["999163036",], type="ratings")
rec.two = predict(IBCF_pearson, ratings["999223699",], type="ratings")
rec.three = predict(IBCF_pearson, ratings["999223700",], type="ratings")
rec.four = predict(IBCF_pearson, ratings["999361007",], type="ratings")
rec.five = predict(IBCF_pearson, ratings["999361008",], type="ratings")
rec.six = predict(IBCF_pearson, ratings["999361009",], type="ratings")
rec.seven = predict(IBCF_pearson, ratings["999361010",], type="ratings")

#output the rating tables for each Huggies product
rec_999163036 <- as.data.frame(as(rec.one, "list"))
rec_999163036 <- add_rownames(rec_999163036, "cust_id")
colnames(rec_999163036) <- c("cust_id","rating")
write.csv(rec_999163036, 
          file = "D:/Emory/Marketing Analytics/Project 2/999163036.csv",
          row.names = FALSE, col.names = FALSE)

rec_999223699 <- as.data.frame(as(rec.two, "list"))
rec_999223699 <- add_rownames(rec_999223699, "cust_id")
colnames(rec_999223699) <- c("cust_id","rating")
write.csv(rec_999223699, 
          file = "D:/Emory/Marketing Analytics/Project 2/999223699.csv",
          row.names = FALSE, col.names = FALSE)

rec_999223700 <- as.data.frame(as(rec.three, "list"))
rec_999223700 <- add_rownames(rec_999223700, "cust_id")
colnames(rec_999223700) <- c("cust_id","rating")
write.csv(rec_999223700, 
          file = "D:/Emory/Marketing Analytics/Project 2/999223700.csv",
          row.names = FALSE, col.names = FALSE)


rec_999361007 <- as.data.frame(as(rec.four, "list"))
rec_999361007 <- add_rownames(rec_999361007, "cust_id")
colnames(rec_999361007) <- c("cust_id","rating")
write.csv(rec_999361007, 
          file = "D:/Emory/Marketing Analytics/Project 2/999361007.csv",
          row.names = FALSE, col.names = FALSE)


rec_999361008 <- as.data.frame(as(rec.five, "list"))
rec_999361008 <- add_rownames(rec_999361008, "cust_id")
colnames(rec_999361008) <- c("cust_id","rating")
write.csv(rec_999361008, 
          file = "D:/Emory/Marketing Analytics/Project 2/999361008.csv",
          row.names = FALSE, col.names = FALSE)


rec_999361009 <- as.data.frame(as(rec.six, "list"))
rec_999361009 <- add_rownames(rec_999361009, "cust_id")
colnames(rec_999361009) <- c("cust_id","rating")
write.csv(rec_999361009, 
          file = "D:/Emory/Marketing Analytics/Project 2/999361009.csv",
          row.names = FALSE, col.names = FALSE)


rec_999361010 <- as.data.frame(as(rec.seven, "list"))
rec_999361010 <- add_rownames(rec_999361010, "cust_id")
colnames(rec_999361010) <- c("cust_id","rating")
write.csv(rec_999361010, 
          file = "D:/Emory/Marketing Analytics/Project 2/999361010.csv",
          row.names = FALSE, col.names = FALSE)



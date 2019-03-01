library(data.table)
library(dplyr)
library(ggplot2)
setwd('C:/Users/alexs/Documents/Marketing Analytics/Group_Project/Project_2')
tran <- fread('cleantransaction.csv')
head(tran)
prod <- fread('product_table.csv')
head(prod)

# type of product in HUGGIES
prod_huggies <- prod[which(prod$brand_desc == 'HUGGIES'),]

# competitors seller diaper
tran_diaper <- tran[which(tran$category_desc =='FRALDAS'),]
tran_diaper_comp <- tran_diaper[-which(tran_diaper$brand_desc =='HUGGIES'),]
unique(tran_diaper_comp$brand_desc)

# product_id in DODOT diaper
prod_dodot_diaper <- prod[which(prod$brand_desc == 'DODOT' & prod$category_desc =='FRALDAS'),]

# product_id in PRIVATE LABEL
prod_pl_diaper <- prod[which(prod$brand_desc == 'PRIVATE LABEL' & prod$category_desc =='FRALDAS'),]

# look into customer of DODOT in tran
test <- tran_diaper_comp[which(tran_diaper_comp$subcategory_id =='93370'),]

tran_dodot <- tran_diaper[which(tran_diaper$brand_desc =='DODOT'),]
unique(tran_dodot$tran_prod_sale_qty)
hist(tran_dodot$tran_prod_sale_qty)
table(tran_dodot$tran_prod_sale_qty)

# identify regular customer
tran_dodot[, start_date := min(tran_dt), by= 'cust_id']
tran_dodot[, last_date := max(tran_dt), by = 'cust_id']
tran_dodot[, date_diff := as.Date(tran_dodot$last_date)-
       as.Date(tran_dodot$start_date)]
tran_dodot[, count_tran_id := uniqueN(tran_id), by= "cust_id" ]


# consider customer buying same product for over 2/3 of a year and equal or over 38 time purchases as regular customer
tran_dodot[, regular_cust := ifelse(tran_dodot$date_diff>270, ifelse(tran_dodot$count_tran_id>=38, 'yes', 'no'), 'no')]
dodot_regular <- unique(tran_dodot[which(tran_dodot$regular_cust == 'yes'),]$cust_id)

tran_dodot[, count_prod_id := uniqueN(prod_id), by = "cust_id"]

print(tran_dodot[which(tran_dodot$count_prod_id >= 6),])

# identify which product id is bigger size
hist(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$prod_id)
unique(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$cust_id)

data <- tran_dodot[which(tran_dodot$count_prod_id >= 6 & tran_dodot$regular_cust =='yes'),]

ggplot(data, aes(data$tran_dt,as.character(data$prod_id)))+ geom_point(colour = data$cust_id) 
ggplot(data, aes(data$tran_dt,as.character(data$sub_category_desc)))+ geom_point(colour = data$cust_id)+
  labs(title="Regular Customer of Dodot ", x ="Time", y = "Category")+
  theme(axis.text.x = element_blank())

unique(data$prod_id)

# unique(prod_pl_diaper$sub_category_desc)

## bigger size <- sub_category_desc == c("FRALDA T5","FRALDA T6")
## other <- c("FRALDA CUECA")

tran_dodot_big <- tran_dodot[which(tran_dodot$sub_category_desc %in% c('FRALDA T5','FRALDA T6')),]

tran_dodot_big[, count_tran_id := uniqueN(tran_id), by= "cust_id"]

# create date_diff for tran_dodot_big
tran_dodot_big[, start_date := min(tran_dt), by= 'cust_id']
tran_dodot_big[, last_date := max(tran_dt), by = 'cust_id']
tran_dodot_big[, date_diff := as.Date(tran_dodot_big$last_date)-
                 as.Date(tran_dodot_big$start_date)]

# set conditions for determine customer who do not need to purchase diaper in 2018
tran_dodot_big[, eliminate_cust := ifelse(tran_dodot_big$date_diff>180, ifelse(tran_dodot_big$count_tran_id >= 6,
                                                                               ifelse(tran_dodot_big$last_date > as.Date('2017-06-01', format="%Y-%m-%d"), 'yes','no'),'no'), 'no')]


eliminate_cust <- as.data.frame(tran_dodot_big[which(tran_dodot_big$eliminate_cust=='yes'),])
eliminate_cust_dodot <- unique(eliminate_cust$cust_id)

tran_dodot_big[which(tran_dodot_big$cust_id=='799924'),][order(tran_dt)]

## eliminate customer list
# [1]   799924  1539803  6019835  8169802 10629613 11599946 12189899 14669832 15499629 27879910 31429680 32239659 38279998 38919993 42569954
# [16] 52319807 54099615 61889803 63189976 64239835 66159554 67089984 68029961 68959870 70069949 71389946 74289807 78339680 79669589 80899687
# [31] 81779810 86709958 87089903 87489615 88659885 91669577 91769669 98589856 99269913  1369928  3119890 12719976 18069947 20589880 22729979
# [46] 23509865 25079599 31089912 33869812 36669711 38519883 41519646 41709665 41739754 42779672 44819958 45029889 46149862 48129784 52639869
# [61] 54059803 56999610 57569716 60529787 66699775 68149871 72679677 78239888 80909980 84909677 89299811 92869591 93059555 98709618


# look into customer of PRIVATE LABEL in tran

tran_dodot <- tran_diaper[which(tran_diaper$brand_desc =='PRIVATE LABEL'),]
unique(tran_dodot$tran_prod_sale_qty)
hist(tran_dodot$tran_prod_sale_qty)
table(tran_dodot$tran_prod_sale_qty)

# identify loyal customer
tran_dodot[, start_date := min(tran_dt), by= 'cust_id']
tran_dodot[, last_date := max(tran_dt), by = 'cust_id']
tran_dodot[, date_diff := as.Date(tran_dodot$last_date)-
             as.Date(tran_dodot$start_date)]
tran_dodot[, count_tran_id := uniqueN(tran_id), by= "cust_id" ]


# consider customer buying same product for over 2/3 of a year and equal or over 38 time purchases as regular customer
tran_dodot[, regular_cust := ifelse(tran_dodot$date_diff>270, ifelse(tran_dodot$count_tran_id>=38, 'yes', 'no'), 'no')]
dodot_regular <- unique(tran_dodot[which(tran_dodot$regular_cust == 'yes'),]$cust_id)

tran_dodot[, count_prod_id := uniqueN(prod_id), by = "cust_id"]

print(tran_dodot[which(tran_dodot$count_prod_id >= 6),])

# identify which product id is bigger size
hist(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$prod_id)
unique(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$cust_id)

data <- tran_dodot[which(tran_dodot$count_prod_id >= 6 & tran_dodot$regular_cust =='yes'),]

ggplot(data, aes(data$tran_dt,as.character(data$prod_id)))+ geom_point(colour = data$cust_id) 
ggplot(data, aes(data$tran_dt,as.character(data$sub_category_desc)))+ geom_point(colour = data$cust_id) + 
  labs(title="Regular Customer of Private Label ", x ="Time", y = "Category")+
  theme(axis.text.x = element_blank())
unique(data$prod_id)

# unique(prod_pl_diaper$sub_category_desc)

## bigger size <- sub_category_desc == c("FRALDA T5","FRALDA T6")
## other <- c("FRALDA CUECA","RESGUARDO BEBE")

tran_dodot_big <- tran_dodot[which(tran_dodot$sub_category_desc %in% c('FRALDA T5','FRALDA T6')),]

unique(tran_dodot_big$sub_category_desc)

tran_dodot_big[, count_tran_id := uniqueN(tran_id), by= "cust_id"]

# create date_diff for tran_dodot_big
tran_dodot_big[, start_date := min(tran_dt), by= 'cust_id']
tran_dodot_big[, last_date := max(tran_dt), by = 'cust_id']
tran_dodot_big[, date_diff := as.Date(tran_dodot_big$last_date)-
            as.Date(tran_dodot_big$start_date)]

# set conditions for determine customer who do not need to purchase diaper in 2018
tran_dodot_big[, eliminate_cust := ifelse(tran_dodot_big$date_diff>180, ifelse(tran_dodot_big$count_tran_id >= 6,
                                                                               ifelse(tran_dodot_big$last_date > as.Date('2017-06-01', format="%Y-%m-%d"), 'yes','no'),'no'), 'no')]

eliminate_cust <- as.data.frame(tran_dodot_big[which(tran_dodot_big$eliminate_cust=='yes'),])
eliminate_cust_pl <- unique(eliminate_cust$cust_id)

tran_dodot_big[which(tran_dodot_big$cust_id=='329968'),][order(tran_dt)]

## eliminate customer list
# [1]   329968   699812   969975  3689815  3879648  4309605  6079930  7399797  7639542 10229970 11529836 12709994 14579809 14669762 16359967
# [16] 21549823 22029987 23100000 23369710 23989990 25509893 25899797 28039946 28739765 29639764 31469798 32199633 32289868 33119617 33129968
# [31] 33309653 33799842 34809650 36859904 37009633 39589641 41979833 44379899 44809845 45019696 45019977 45249531 45519715 45559604 45679898
# [46] 46319846 47119983 47379958 48429907 50069642 50099788 51999870 54799832 54949955 55709907 57520000 58919615 59649884 60569799 61089639
# [61] 62149945 63649948 64409709 65689935 66039611 67809958 70159937 70279969 70839933 73229998 73549824 75679996 78649817 79659939 79809829
# [76] 80139861 80439521 80909980 81269713 81849885 82099697 83240000 83259986 83619996 84349578 85069816 85729942 85949697 87019827 87429999
# [91] 88169655 88959959 89249557 89869626 90049797 90119828 97809684 99309654 99409972   799924  5039834  5139925  6259653  8229636  9919642
# [106] 11119662 11389560 11459993 11469670 11599946 12129579 13169926 13739810 14179738 14189592 14299896 14639599 16909799 18769972 18909750
# [121] 19549791 20009976 20509604 20589880 20909719 21159531 21489560 21849939 22849582 23679575 24489713 24509693 25069562 25669531 26249568
# [136] 27989743 28629904 29759594 30529672 31039819 31079522 31419913 32239659 33139540 33229854 34589863 34649782 35219758 35439967 36259638
# [151] 38769971 39689967 40819762 41539590 42509986 43469577 45029889 45649777 46289575 47749869 48889801 49719951 50729606 50979878 52879917
# [166] 56119821 57569716 57599576 57719658 58939977 60039707 61069976 62299906 63059929 63139548 63609863 63709996 63839683 64239709 65549871
# [181] 65579845 65949616 67829568 68149871 68169950 69429786 69769566 70679598 70949630 71679855 72419745 73139771 74569708 75569703 75979514
# [196] 77329948 77649985 77889863 77919579 78499661 78599978 79129744 79329949 79999656 80429707 80899687 81619979 81659873 81689633 83919699
# [211] 84109976 86019710 88479960 88659885 88899674 89299811 89639644 89929931 89969638 92059982 92339697 92869957 96479520 97759939 98469775
# [226] 98589856 98849789 99229897  1649869  8259629 13379843 30619950 40039730 48069561 48979985 51549800 52659736 55589990 67229965 73639655
# [241] 74019963 80669666 81469859 94549984 96779944 97909841 99259910 11679751 16059766 41379703 51669643 67849867 71809698 78339680 86569519
# [256] 93099895 98999735

# combining eliminate customer 
eliminate_cust_dodot<-as.data.frame(eliminate_cust_dodot)
eliminate_cust_pl<- as.data.frame(eliminate_cust_pl)
colnames(eliminate_cust_dodot) <-"id"
colnames(eliminate_cust_pl)= "id"
customer_list_rm <- unique(rbind(eliminate_cust_dodot ,eliminate_cust_pl))

write.csv(customer_list_rm, file="customer_list_rm.csv")

# exclude customer in CF list
df_999163036 <- fread('999163036.csv')
df_999223699 <- fread('999223699.csv')
df_999223700 <- fread('999223700.csv')
df_999361007 <- fread('999361007.csv')
df_999361008 <- fread('999361008.csv')
df_999361009 <- fread('999361009.csv')
df_999361010 <- fread('999361010.csv')  

# DF_999163036
temp <- data.frame()
for(i in 1:nrow(df_999163036)){
  if(!(df_999163036[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999163036[i][,1])
  }
}

temp <- left_join(temp,df_999163036,by='cust_id')

DF_999163036 <- as.data.table(temp)

write.csv(DF_999163036, file="DF_999163036.csv")

# DF_999223699
temp <- data.frame()
for(i in 1:nrow(df_999223699)){
  if(!(df_999223699[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999223699[i][,1])
  }
}

temp <- left_join(temp,df_999223699,by='cust_id')

DF_999223699 <- as.data.table(temp)

write.csv(DF_999223699, file="DF_999223699.csv")

# DF_999223700
temp <- data.frame()
for(i in 1:nrow(df_999223700)){
  if(!(df_999223700[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999223700[i][,1])
  }
}

temp <- left_join(temp,df_999223700,by='cust_id')

DF_999223700 <- as.data.table(temp)

write.csv(DF_999223700, file="DF_999223700.csv")

# DF_999361007
temp <- data.frame()
for(i in 1:nrow(df_999361007)){
  if(!(df_999361007[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999361007[i][,1])
  }
}

temp <- left_join(temp,df_999361007,by='cust_id')

DF_999361007 <- as.data.table(temp)

write.csv(DF_999361007, file="DF_999361007.csv")

# DF_999361008
temp <- data.frame()
for(i in 1:nrow(df_999361008)){
  if(!(df_999361008[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999361008[i][,1])
  }
}

temp <- left_join(temp,df_999361008,by='cust_id')

DF_999361008 <- as.data.table(temp)

write.csv(DF_999361008, file="DF_999361008.csv")

# DF_999361009
temp <- data.frame()
for(i in 1:nrow(df_999361009)){
  if(!(df_999361009[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999361009[i][,1])
  }
}

temp <- left_join(temp,df_999361009,by='cust_id')

DF_999361009 <- as.data.table(temp)

write.csv(DF_999361009, file="DF_999361009.csv")

# DF_999361010
temp <- data.frame()
for(i in 1:nrow(df_999361010)){
  if(!(df_999361010[i][,1] %in% customer_list_rm$id)){
    temp <- rbind(temp,df_999361010[i][,1])
  }
}

temp <- left_join(temp,df_999361010,by='cust_id')

DF_999361010 <- as.data.table(temp)

write.csv(DF_999361010, file="DF_999361010.csv")

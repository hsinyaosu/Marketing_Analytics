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


# consider customer buying same product for over half a year and equal or over 3 time purchases as loyal customer
tran_dodot[, regular_cust := ifelse(tran_dodot$date_diff>270, ifelse(tran_dodot$count_tran_id>=38, 'yes', 'no'), 'no')]
dodot_regular <- unique(tran_dodot[which(tran_dodot$regular_cust == 'yes'),]$cust_id)

tran_dodot[, count_prod_id := uniqueN(prod_id), by = "cust_id"]

print(tran_dodot[which(tran_dodot$count_prod_id >= 6),])

# identify which product id is bigger size
hist(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$prod_id)
unique(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$cust_id)

data <- tran_dodot[which(tran_dodot$count_prod_id >= 6 & tran_dodot$regular_cust =='yes'),]

ggplot(data, aes(data$tran_dt,as.character(data$prod_id)))+ geom_point(colour = data$cust_id) 
ggplot(data, aes(data$tran_dt,as.character(data$sub_category_desc)))+ geom_point(colour = data$cust_id)
unique(data$prod_id)

# unique(prod_pl_diaper$sub_category_desc)

## bigger size <- sub_category_desc == c("FRALDA T5","FRALDA T6")
## other <- c("FRALDA CUECA")

tran_dodot_big <- tran_dodot[which(tran_dodot$sub_category_desc %in% c('FRALDA T5','FRALDA T6')),]

tran_dodot_big[, count_tran_id := uniqueN(tran_id), by= "cust_id"]

tran_dodot_big[, eliminate_cust := ifelse(tran_dodot_big$date_diff>180, ifelse(tran_dodot_big$count_tran_id >= 6,
                                                                               ifelse(tran_dodot_big$last_date < as.Date('2017-12-31', format="%Y-%m-%d"), 'yes','no'),'no'), 'no')]


eliminate_cust <- as.data.frame(tran_dodot_big[which(tran_dodot_big$eliminate_cust=='yes'),])
eliminate_cust_dodot <- unique(eliminate_cust$cust_id)

tran_dodot_big[which(tran_dodot_big$cust_id=='799924'),][order(tran_dt)]

## eliminate customer 
# [1]   799924  1539803  6019835  8169802  8679783  9919642 10629613 11599946 12189899 14669832 15499629 15869799 27879910 31429680 32239659
# [16] 33139540 38279998 38919993 39349950 41449784 42569954 45769973 50259624 52319807 54099615 57799695 63189976 64239835 64629817 65969947
# [31] 66099545 66159554 67089984 68029961 68959870 70069949 71389946 71579537 74289807 78339680 79669589 80899687 81279937 81779810 86709958
# [46] 86799874 87089903 87489615 88659885 90119828 91669577 91769669 93999650 98589856 99269913  1369928  3119890  8259629 12719976 12899871
# [61] 14299896 15819895 18069947 20589880 20839975 22729979 23509865 24939852 25079599 30919926 31089912 33869812 36669711 38519883 39539765
# [76] 39849975 40049967 41519646 41709665 41739754 42779672 43249812 44379899 44819958 44989801 45029889 46149862 48129784 50099788 51349847
# [91] 52639869 52969671 54059803 56999610 57549928 57569716 59369945 60529787 65689935 66699775 67549933 67809958 67829997 68149871 68749573
# [106] 70159937 72679677 72999830 73019562 74759921 78059882 78239888 80909980 84319801 84909677 85949697 88129795 89299811 90909657 92869591
# [121] 93059555 98709618 13679928  2389794 11199831 23799921


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


# consider customer buying same product for over half a year and equal or over 3 time purchases as loyal customer
tran_dodot[, regular_cust := ifelse(tran_dodot$date_diff>270, ifelse(tran_dodot$count_tran_id>=38, 'yes', 'no'), 'no')]
dodot_regular <- unique(tran_dodot[which(tran_dodot$regular_cust == 'yes'),]$cust_id)

tran_dodot[, count_prod_id := uniqueN(prod_id), by = "cust_id"]

print(tran_dodot[which(tran_dodot$count_prod_id >= 6),])

# identify which product id is bigger size
hist(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$prod_id)
unique(tran_dodot[which(tran_dodot$count_prod_id >= 6),]$cust_id)

data <- tran_dodot[which(tran_dodot$count_prod_id >= 6 & tran_dodot$regular_cust =='yes'),]

ggplot(data, aes(data$tran_dt,as.character(data$prod_id)))+ geom_point(colour = data$cust_id) 
ggplot(data, aes(data$tran_dt,as.character(data$sub_category_desc)))+ geom_point(colour = data$cust_id)
unique(data$prod_id)

# unique(prod_pl_diaper$sub_category_desc)

## bigger size <- sub_category_desc == c("FRALDA T5","FRALDA T6")
## other <- c("FRALDA CUECA","RESGUARDO BEBE")

tran_dodot_big <- tran_dodot[which(tran_dodot$sub_category_desc %in% c('FRALDA T5','FRALDA T6')),]

tran_dodot_big[, count_tran_id := uniqueN(tran_id), by= "cust_id"]

tran_dodot_big[, eliminate_cust := ifelse(tran_dodot_big$date_diff>180, ifelse(tran_dodot_big$count_tran_id >= 6,
                                                                               ifelse(tran_dodot_big$last_date < as.Date('2017-12-31', format="%Y-%m-%d"), 'yes','no'),'no'), 'no')]


eliminate_cust <- as.data.frame(tran_dodot_big[which(tran_dodot_big$eliminate_cust=='yes'),])
eliminate_cust_pl <- unique(eliminate_cust$cust_id)

tran_dodot_big[which(tran_dodot_big$cust_id=='799924'),][order(tran_dt)]
## eliminate customer 
# [1]   329968   699812   969975  1829913  2799943  3689815  3779918  3879648  4309605  6079930  7399797  9329968  9749794 10229970 11529836
# [16] 12709994 14579809 14669762 14669832 15219892 16359967 19320000 20779661 21549823 22029987 23100000 23369710 23799921 23989990 25509893
# [31] 25749578 25899797 26809754 28039946 28739765 29579982 29639764 29859597 30769971 31009864 31469798 32199633 32289868 33119617 33129968
# [46] 33309653 33799842 34799710 34809650 35649822 36859904 37009633 39589641 40549931 41659638 41979833 43089796 44809845 45019696 45019977
# [61] 45519715 45559604 45679898 46319846 47119983 47379958 47389671 48429907 49929593 50069642 50099728 50099788 51999870 54799832 54949955
# [76] 55709907 57520000 58919615 59649884 60569799 61089639 62599880 63649948 64409709 65689935 65739938 66039611 66289964 67809958 68839859
# [91] 70159937 70279969 70839933 71499931 71659967 72379577 73229998 73349981 74069769 75679996 78239888 78649817 79659939 79809829 80139861
# [106] 80439521 80909980 81269713 81849885 82099697 83240000 83259986 83619996 84019574 84349578 85069816 85729942 85949697 86439820 87019827
# [121] 87429999 88169655 88269797 88959959 89249557 89759972 89869626 90049797 90119828 93779687 93999650 95359802 95689596 99309654 99409972
# [136]   799924  3009958  5039834  5109957  5139925  6259653  8229636  9919642 11119662 11389560 11459993 11469670 11599946 12129579 13169926
# [151] 13739810 14179738 14189592 14299896 14509676 14639599 16909799 17289971 18019542 18069947 18769972 19549791 20009976 20509604 20589880
# [166] 20909719 21159531 21489560 21849939 22849582 23529877 23679575 24489713 24509693 25069562 25669531 26249568 27129674 27989743 28119953
# [181] 28629904 29759594 30529672 31039819 31079522 31419913 32239659 33139540 33229854 34589863 34649782 35219758 35439967 36259638 38149980
# [196] 38609988 38769971 39089810 39599655 39689967 40819762 41539590 42279850 42509986 43299990 43469577 44369706 44489989 45029889 45409661
# [211] 45649777 45979995 46289575 46699634 47749869 48889801 49719951 50729606 50979878 51939992 52879917 54039964 54469953 55919621 56119821
# [226] 57569716 57599576 57719658 58309983 60039707 61069976 62299906 63059929 63139548 63609863 63709996 63839683 64239709 64239851 64709684
# [241] 65549871 65579845 65949616 67829568 68149871 68169950 69429786 69769566 70319955 70679598 70949630 71679855 72419745 73139771 73569895
# [256] 74569708 75569703 75979514 76849997 77329948 77649985 77889863 77919579 78499661 78599978 79129744 79329949 79999656 80089949 80429707
# [271] 80899687 81469663 81619979 81659873 81689633 83919699 84109976 86019710 86499802 88479960 88659885 89039775 89299811 89639644 89799600
# [286] 89929931 89969638 92059982 92339697 92869957 93289858 93479789 93539545 95779968 96479520 97759939 97979605 98399917 98469775 98589856
# [301] 98629985 98849789 99229897  1409661  1649869  2389981  8259629 13379843 14749832 15139814 17419739 27559914 27959865 30619950 36739956
# [316] 39089617 40039730 48069561 48979985 51549800 52659736 54539682 55589990 55699934 56139844 57569847 57799942 57949815 58959780 63149599
# [331] 67229965 69559945 70139914 70869780 73589829 73639655 74019963 80669666 81469859 89339811 89769630 89789816 94549984 95229569 97299984
# [346] 97909841 99259910  9729963 11679751 13409950 16059766 18739849 26759733 30189974 31599645 32719930 33459796 34319838 39679904 41379703
# [361] 46149671 50479791 59769826 61149783 63009935 67849867 75709600 78339680 86569519 89519798 93099895 98999735 99599996 99939955

# combining eliminate customer 
eliminate_cust_dodot<-as.data.frame(eliminate_cust_dodot)
eliminate_cust_pl<- as.data.frame(eliminate_cust_pl)
colnames(eliminate_cust_dodot) <-"id"
colnames(eliminate_cust_pl)= "id"
customer_list_rm <- unique(rbind(eliminate_cust_dodot ,eliminate_cust_pl))

write.csv(customer_list_rm, file="customer_list_rm.csv")


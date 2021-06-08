library(tibble)
library(bnlearn)
library(dplyr)
require(ggplot2)
data<-readRDS("amz_train.rds")
data$epoc = as.Date(as.character(data$epoc))

data_df <- data.frame(year = as.numeric(format(data$epoc, format = "%Y")),
                 month = as.numeric(format(data$epoc, format = "%m")),
                 day = as.numeric(format(data$epoc, format = "%d")))

data = data[,-2]
data = add_column(data, d = data_df[,1], .after = 1)
data = add_column(data, d = data_df[,2], .after = 2)
data = add_column(data, d = data_df[,3], .after = 3)
colnames(data)[2] = "year"
colnames(data)[3] = "month"
colnames(data)[4] = "day"

###a-) Number of Products, number of sellers (overall and by day)
length(unique(data$pid))
unique(data$pid)
length(unique(data$sid))
no_of_sellers = list()
for (i in c(1:9)){
    df = data %>% filter(data$pid==unique(data$pid)[i])
    count = length(unique(df$sid))
    no_of_sellers = append(no_of_sellers,count)
    
}

no_of_sellers
barplot(unlist(no_of_sellers),names.arg=c(1:9),xlab="Products",ylab="No_of_sellers",col="blue")

month_day = paste(data$month,data$day)
data = data[,-c(3,4)]
#data
data = add_column(data, month_day = month_day, .after = 1)
#data
product_count_byday = data %>% group_by(month_day) %>% summarise(product_count_byday = length(unique(pid)))
seller_count_byday = data %>% group_by(month_day) %>% summarise(seller_count_byday = length(unique(sid)))

barplot(seller_count_byday$seller_count_byday,names.arg=seller_count_byday$month_day,xlab="Date",ylab="No_of_sellers",col="green")
barplot(product_count_byday$product_count_byday,names.arg=product_count_byday$month_day,xlab="Date",ylab="No_of_products",col="red")

##b-) Average, max, min price, buy-box price, shipping cost by product

avg = data.frame( data %>% group_by(pid,month_day) %>% summarise(average = mean(price)))
max = data.frame(data %>% group_by(pid,month_day) %>% summarise(max = max(price)))
min = data.frame(data %>% group_by(pid,month_day) %>% summarise(min = min(price)))
df = data%>% filter(data$bbox=="success")
bbox_price = data.frame(df %>% group_by(pid,month_day) %>% summarise(bbox_price = mean(bbox_price)))

for (i in c(1:9)){
    avg = data.frame( data %>% group_by(pid,month_day) %>% summarise(average = mean(price)))
    max = data.frame(data %>% group_by(pid,month_day) %>% summarise(max = max(price)))
    min = data.frame(data %>% group_by(pid,month_day) %>% summarise(min = min(price)))
    df=data%>% filter(data$bbox=="success")
    bbox_price = data.frame(df %>% group_by(pid,month_day) %>% summarise(bbox_price = mean(bbox_price)))
    max=max[max$pid==unique(data$pid)[i],]
    avg=avg[avg$pid==unique(data$pid)[i],]
    min=min[min$pid==unique(data$pid)[i],]
    bbox_price=bbox_price[bbox_price$pid==unique(data$pid)[i],]
    if (i==2){
        bbox_price=rbind(bbox_price,data.frame(pid=unique(data$pid)[i],month_day='8 31',bbox_price=0))
        bbox_price=rbind(bbox_price,data.frame(pid=unique(data$pid)[i],month_day='9 1',bbox_price=0))
        bbox_price=rbind(bbox_price,data.frame(pid=unique(data$pid)[i],month_day='9 2',bbox_price=0))    
    }
    df = data.frame(avg$month_day,avg$average,max$max,min$min,bbox_price$bbox_price)
    g = ggplot(df, aes(avg$month_day,group=1))
    g = g + geom_line(aes(y=avg$average), colour="red")
    g = g + geom_line(aes(y=max$max), colour="green")
    g = g + geom_line(aes(y=min$min), colour="blue")
    g = g + geom_line(aes(y=bbox_price$bbox_price), colour="purple")
    print(g)
}


###############
#  shipping cost by product eklenmeli
##############




##c-) Seller ratings, positive feedbacks and counts, product ratings and counts by product
data %>% group_by(pid) %>% summarise(average_sid_rating = mean(sid_rating))
data %>% group_by(pid) %>% summarise(average_sid_pos_fb = mean(sid_pos_fb))
data %>% group_by(pid) %>% summarise(average_sid_rating_cnt = mean(sid_rating_cnt))
data %>% group_by(pid) %>% summarise(average_pid_rating = mean(pid_rating))
data %>% group_by(pid) %>% summarise(average_pid_rating_cnt = mean(pid_rating_cnt))

##d-) Percentage of buy-box successes when Amazon is the seller against every other seller (total and by product and week)
##by product
a = data %>% filter(sid=="amazon", box=="success") %>% group_by(pid) %>% summarise(No_of_Amazon_Win = length((sid))) 
a = a %>% add_row(pid="B00MVVI1FC" , No_of_Amazon_Win=0, .before = 5) 

b = data %>% filter(sid=="amazon")%>% group_by(pid) %>% summarise(is_Amazon_seller = length((sid)))

winper = a[,2]/b[,2] 
winper = winper   %>% add_column(a[,1], .before = 1) %>% add_row(pid="B00AMFLZLG" , No_of_Amazon_Win=NA, .before = 3)


##total
a = data %>% filter(sid=="amazon", box=="success") %>% group_by(pid) %>% summarise(No_of_Amazon_Win = length((sid)))
colSums(a[,2])
b = data %>% filter(sid=="amazon")%>% group_by(pid) %>% summarise(is_Amazon_seller = length((sid)))
colSums(b[,2])

winpertotal = colSums(a[,2])/colSums(b[,2])

winper = winper %>% add_row(pid="total", No_of_Amazon_Win=winpertotal)

###############
#Weekly ratio eklenmeli
##############


##e-) Prime, FBA, Page and Rank information by seller and by product
##is_Prime
data %>% filter(bbox=="success", is_prime=="yes") %>% group_by(pid) %>% summarise(two_condition = length((is_prime)))
data %>% filter(bbox=="success", is_prime=="yes") %>% group_by(sid) %>% summarise(two_condition = length((is_prime)))
##is_fba
data %>% filter(bbox=="success", is_fba=="yes") %>% group_by(pid) %>% summarise(two_condition = length((is_fba)))
data %>% filter(bbox=="success", is_fba=="yes") %>% group_by(sid) %>% summarise(two_condition = length((is_fba)))
##Rank
data %>% filter(bbox=="success") %>% group_by(rank) %>% summarise(no_of_bbox_success = length((rank)))
data %>% filter(bbox=="success") %>% group_by(pid,rank) %>% summarise(no_of_bbox_success = length((rank)))
data %>% filter(bbox=="success") %>% group_by(sid,rank) %>% summarise(no_of_bbox_success = length((rank)))
##Page
data %>% filter(bbox=="success") %>% group_by(page) %>% summarise(no_of_bbox_success = length((page)))
data %>% filter(bbox=="success") %>% group_by(pid,page) %>% summarise(no_of_bbox_success = length((page)))
data %>% filter(bbox=="success") %>% group_by(sid,page) %>% summarise(no_of_bbox_success = length((page)))


##g-) What are the scales on which the data in each column is measured on?
str(data)
summary(data)

boxplot(data$price)
boxplot(data$sid_rating)
boxplot(data$sid_pos_fb)
boxplot(data$sid_rating_cnt)
boxplot(data$shipping)
boxplot(data$pid_rating)
boxplot(data$pid_rating_cnt)

aa=data %>% filter(bbox=="success") %>% group_by(pid) %>% summarise(result = mean((sid_rating_cnt)))
bb=data %>% group_by(pid) %>% summarise(average_sid_rating_cnt = mean(sid_rating_cnt))
aa
bb


data %>% filter(bbox=="failure") %>% group_by(page) %>% summarise(no_of_bbox_success = length((page)))
data %>% filter(bbox=="success") %>% group_by(page) %>% summarise(no_of_bbox_success = length((page)))

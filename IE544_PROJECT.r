##Q1
library(tibble)
library(bnlearn)
library(dplyr)
require(ggplot2)
library(car)
library(gRain)
library(caret)

data<-readRDS("amz_train.rds")
data$epoc = as.Date(as.character(data$epoc))
data

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

###a
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
length(unique(data$rank))

month_day = paste(data$month,data$day)
data = data[,-c(3,4)]
#data
data = add_column(data, month_day = month_day, .after = 1)
#data
seller_count_byday = data %>% group_by(month_day) %>% summarise(seller_count_byday = length(unique(sid)))
product_count_byday = data %>% group_by(month_day) %>% summarise(product_count_byday = length(unique(pid)))

barplot(seller_count_byday$seller_count_byday,names.arg=seller_count_byday$month_day,xlab="Date",ylab="No_of_sellers",col="green")
barplot(product_count_byday$product_count_byday,names.arg=product_count_byday$month_day,xlab="Date",ylab="No_of_products",col="red")

##b

avg = data.frame( data %>% group_by(pid,month_day) %>% summarise(average = mean(price)))
max = data.frame(data %>% group_by(pid,month_day) %>% summarise(max = max(price)))
min = data.frame(data %>% group_by(pid,month_day) %>% summarise(min = min(price)))
df=data%>% filter(data$bbox=="success")
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
    g= g + labs(x = "Days",y= "Prices",title = unique(data$pid)[i])
    print(g)
}




##b
df3=data %>%group_by(pid) %>%summarise(shipping_average_byproduct = mean(shipping,na.rm=TRUE)) 
barplot(df3$shipping_average_byproduct,names.arg=df3$pid,xlab="Products",ylab="Average Shipping Cost",col="pink",ylim=c(0,10))


shipping_byday = data %>% group_by(month_day) %>% summarise(shipping_byday = mean(shipping,na.rm=TRUE))
barplot(shipping_byday$shipping_byday,names.arg=shipping_byday$month_day,xlab="Date",ylab="Average Shipping Cost",col="red",ylim=c(0,5))

##c
df1 = data %>% filter(data$bbox=="success")
df1 %>% group_by(pid) %>% summarise(average_sid_rating = mean(sid_rating))
df1 %>% group_by(pid) %>% summarise(average_sid_pos_fb = mean(sid_pos_fb))
df1 %>% group_by(pid) %>% summarise(average_sid_rating_cnt = mean(sid_rating_cnt))
df1 %>% group_by(pid) %>% summarise(average_pid_rating = mean(pid_rating))
df1 %>% group_by(pid) %>% summarise(average_pid_rating_cnt = mean(pid_rating_cnt))

##d
##by product
data %>% filter(sid=="amazon", bbox=="success") %>% group_by(pid) %>% summarise(No_of_Amazon_Win = length((sid)))
data %>% filter(sid=="amazon")%>% group_by(pid) %>% summarise(is_Amazon_seller = length((sid)))

##total
a=data %>% filter(sid=="amazon", bbox=="success") %>% group_by(pid) %>% summarise(No_of_Amazon_Win = length((sid)))
colSums(a[,2])
b=data %>% filter(sid=="amazon")%>% group_by(pid) %>% summarise(is_Amazon_seller = length((sid)))
colSums(b[,2])

c=(colSums(a[,2])/colSums(b[,2]))*100
c

##e
##Rank
data %>% filter(bbox=="success") %>% group_by(rank) %>% summarise(no_of_bbox_success = length((rank)))
data %>% filter(bbox=="success") %>% group_by(pid,rank) %>% summarise(no_of_bbox_success = length((rank)))
data %>% filter(bbox=="success") %>% group_by(sid,rank) %>% summarise(no_of_bbox_success = length((rank)))
##Page
data %>% filter(bbox=="success") %>% group_by(page) %>% summarise(no_of_bbox_success = length((page)))
data %>% filter(bbox=="success") %>% group_by(pid,page) %>% summarise(no_of_bbox_success = length((page)))

data %>% filter(bbox=="success") %>% group_by(sid,page) %>% summarise(no_of_bbox_success = length((page)))

##is_Prime
data %>% filter(bbox=="success") %>% group_by(pid)%>% summarise(length = length((bbox)))
data %>% filter(bbox=="success", is_prime=="yes") %>% group_by(pid) %>% summarise(two_condition = length((is_prime)))
data %>% filter(bbox=="success") %>% group_by(sid)%>% summarise(length = length((bbox)))

x=data %>% filter(bbox=="success", is_prime=="yes") %>% group_by(sid) %>% summarise(two_condition = length((is_prime)))
x
llist= list()
for (i in x$sid){
    df = data %>% filter(sid==i)
    df = df %>% filter(bbox=="success")
    llist= append(llist,nrow(df))}
llist
perc=(x$two_condition/unlist(llist))*100
#barplot(perc,names.arg=x$sid,xlab="SellerId",ylab="Number of Bbox Success when IsPrime==Yes",col="blue")


##is_fba
#data %>% filter(bbox=="success", is_fba=="yes") %>% group_by(pid) %>% summarise(two_condition = length((is_fba)))
y=data %>% filter(bbox=="success", is_fba=="yes") %>% group_by(sid) %>% summarise(two_condition = length((is_fba)))
y
llist2= list()
for (i in y$sid){
    df = data %>% filter(sid==i)
    df = df %>% filter(bbox=="success")
    llist2= append(llist2,nrow(df))}
llist2

##g
str(data)
summary(data)

#install.packages("car")
#boxplot(data$price)
#boxplot(data$sid_rating)
#Boxplot(data$sid_rating)
#boxplot(data$sid_pos_fb)
#boxplot(data$sid_rating_cnt)
#boxplot(data$shipping)
#boxplot(data$pid_rating)
boxplot(data$pid_rating_cnt)

# new data creation


data<-readRDS("amz_train.rds")
raw_data<-readRDS("amz_train.rds")
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

new_df_list = list()
for (i in c(1:nrow(data))){
    pid_name = data$pid[i]
    day1 = data$day[i]
    filtered_data = data %>% filter(data$pid==pid_name & data$day==day1)
    min_price = min(filtered_data$price)
    ratio = (data[i,'price']-min_price)/min_price
    new_df_list = append(new_df_list,ratio)
    
}
new_df_list
raw_data$price_gap_ratio = unlist(new_df_list)
raw_data
library(tidyr)
price_weight = 0.8
shipping_weight = 0.2

###Method 1
#data$shipping[is.na(data$shipping)] = mean(data$shipping,na.rm=TRUE)


####Method2
na_indexes = which(is.na(data$shipping))
shipping_list = list()
for (i in na_indexes){
    pid_name = data$pid[i]
    sid_name = data$sid[i]
    filtered_data = data %>% filter(data$pid==pid_name & data$sid==sid_name)
    filtered_data = drop_na(filtered_data)
    if (nrow(filtered_data)!=0){
        avg_shipping = mean(filtered_data$shipping)  
    }
    else{
        avg_shipping=0
    }
    data[i,'shipping'] = avg_shipping
}

tot_weighted_cost_list=list()
for (i in c(1:nrow(data))){
    tot_weighted_cost = data$price[i]*price_weight+data$price[i]*shipping_weight
    tot_weighted_cost_list = append(tot_weighted_cost_list,tot_weighted_cost)
    }
tot_weighted_cost_list


raw_data$tot_weighted_cost = unlist(tot_weighted_cost_list)
raw_data

is_Amazon = ifelse(raw_data$sid=="amazon", "1", "0")
raw_data$is_Amazon = is_Amazon
raw_data

raw_data = add_column(raw_data, d = data_df[,1], .after = 21)
raw_data = add_column(raw_data, d = data_df[,2], .after = 22)
raw_data = add_column(raw_data, d = data_df[,3], .after = 23)
raw_data
colnames(raw_data)[21] = "year"
colnames(raw_data)[22] = "month"
colnames(raw_data)[23] = "day"
raw_data

saveRDS(raw_data, file = "IE544_raw_data.rds")

## Question 3

traindata <- readRDS("IE544_raw_data.rds")
testdata <- readRDS("IE544_test_raw_data.rds")


train = traindata %>% select(is_Amazon, price_gap_ratio, sid_pos_fb, rank,  bbox )

#colnames(train)
str(train)

train$is_Amazon <- as.factor(train$is_Amazon)

train$price_gap_ratio<- cut(train$price_gap_ratio,
                       breaks = c(0,0.01 , 0.02, 0.12, 0.2, 0.4, 1, 1.5, 2, 20), 
                       include.lowest = TRUE,
                       right = TRUE )

# table(train$price_gap_ratio)




train$sid_pos_fb<- cut(train$sid_pos_fb,
                       breaks = c(0, 8.5, 9,  9.5, 10 ), 
                       include.lowest = TRUE,
                       right = TRUE )
# table(train$sid_pos_fb)

train$rank <- as.numeric(train$rank)
train$rank<- cut(train$rank,
                       breaks = c(0, 1, 2, 3 , 4 ,5, 7, 9, 12 ), 
                       include.lowest = TRUE,
                       right = TRUE )
# table(train$rank)

test = testdata %>% select(is_Amazon, price_gap_ratio, sid_pos_fb, rank )

test$is_Amazon <- as.factor(test$is_Amazon)


test$price_gap_ratio<- cut(test$price_gap_ratio,
                           breaks = c(0,0.01 , 0.02, 0.12, 0.2, 0.4, 1, 1.5, 2, 20), 
                           include.lowest = TRUE,
                           right = TRUE )

test$sid_pos_fb<- cut(test$sid_pos_fb,
                      breaks = c(0, 8.5, 9,  9.5, 10 ),
                      include.lowest = TRUE,
                      right = TRUE )



test$rank <- as.numeric(test$rank)
test$rank<- cut(test$rank,
                 breaks = c(0, 1, 2, 3 , 4 ,5, 7, 9, 12 ), 
                 include.lowest = TRUE,
                 right = TRUE )


str(test)





#colnames(train)

wlist <- data.frame(from = c("price_gap_ratio" , "is_Amazon" , "rank" , "sid_pos_fb" ,"is_Amazon"), 
                      to = c( "bbox" ,"bbox" ,"bbox","bbox" ,"sid_pos_fb"     )   )

blist <- data.frame(from = c( "is_Amazon" , "price_gap_ratio" , "price_gap_ratio"    ,"sid_pos_fb"  , "rank"    ,           "rank", "sid_pos_fb"  , "rank"    ) , 
                      to = c("price_gap_ratio" ,"is_Amazon" ,  "sid_pos_fb"   , "price_gap_ratio"   ,"price_gap_ratio"  ,"sid_pos_fb"  ,"rank"  , "is_Amazon"    )  )


# Score-Based Algorithms
# Hill Climbing
dag_hc = hc(train, whitelist = wlist, blacklist= blist  ) 
graphviz.plot(dag_hc)
score(dag_hc, train)


# Tabu Search
dag_tabu = tabu(train, whitelist = wlist, blacklist= blist  ) 
graphviz.plot(dag_tabu)
score(dag_tabu, train)



# Constraint-Based Algorithms
# Grow-Shrink
dag_cb = gs(train, undirected = FALSE, whitelist = wlist,  blacklist= blist  )
graphviz.plot(dag_cb)
score(dag_cb, train)


# Incremental Association 
dag_iamb = mmpc(train, undirected = FALSE, whitelist = wlist,  blacklist= blist  ) 
graphviz.plot(dag_iamb)
score(dag_iamb, train)


# Hybrid
# Max-Min Hill Climbing
dag_hyb <- mmhc(train,   whitelist = wlist ,  blacklist= blist   ) 
graphviz.plot(dag_hyb)
score(dag_hyb, train)



#cross-validation with k-fold
set.seed(100)


bn.cv(train, bn = "hc", algorithm.args = list(blacklist=blist),  method="k-fold")  
bn.cv(train, bn = "tabu", algorithm.args = list(blacklist=blist),  method="k-fold")
bn.cv(train, bn = "gs", algorithm.args = list(blacklist=blist),  method="k-fold")
bn.cv(train, bn = "iamb", algorithm.args = list(blacklist=blist),  method="k-fold")
bn.cv(train, bn = "mmhc", algorithm.args = list(blacklist=blist),  method="k-fold")

bn.cv(train, bn = "hc", algorithm.args = list(blacklist=blist),  method="hold-out")
bn.cv(train, bn = "tabu", algorithm.args = list(blacklist=blist),  method="hold-out")
bn.cv(train, bn = "gs", algorithm.args = list(blacklist=blist),  method="hold-out")
bn.cv(train, bn = "iamb", algorithm.args = list(blacklist=blist),  method="hold-out")
bn.cv(train, bn = "mmhc",  algorithm.args = list(blacklist=blist), method="hold-out")



#bootstrapping
arcs = boot.strength(train, R=40, algorithm = "tabu", algorithm.args = list(whitelist = wlist , blacklist= blist), cpdag=TRUE)
arcs<-arcs[(arcs$strength > 0.60) & (arcs$direction >= 0.5), ]
graphviz.plot(averaged.network(arcs))




#Question 4

#a

dag_tabu
dag <-model2network("[is_Amazon][price_gap_ratio][sid_pos_fb|is_Amazon][rank|is_Amazon:price_gap_ratio][bbox|is_Amazon:price_gap_ratio:sid_pos_fb:rank]")
graphviz.plot(dag)


trained <- bn.fit(dag,train)
print(trained) # parameters of model


#b

junction<-compile(as.grain(trained))
jev = setEvidence(junction, nodes = "is_Amazon", states = "1") # P( bbox=success | sid=amazon )
querygrain(jev, nodes = c("bbox"))  


#c - d

traindata$pred_bbox <-  predict(trained, node = "bbox", train)
all.equal(traindata$bbox,traindata$pred_bbox) 

confusionMatrix(traindata$bbox,traindata$pred_bbox)



testdata$pred_bbox <-  predict(trained, node = "bbox", test)
all.equal(testdata$bbox,testdata$pred_bbox ) 

confusionMatrix(testdata$bbox,testdata$pred_bbox)

#sum(is.na(testdata$pred_bbox))

library(tibble)
library(bnlearn)
library(dplyr)

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







a=unlist(new_df_list[1])
a

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

new_df=data.frame(data$pid,data$year,data$month,data$day,data$sid,unlist(new_df_list),data$sid_pos_fb,unlist(tot_weighted_cost),data$rank,data$bbox)
colnames(new_df)= c("pid","year","month","day","is_Amazon","price_gap_ratio","sid_pos_fb","tot_weighted_cost","rank","bbox")


is_Amazon = ifelse(new_df$is_Amazon=="amazon", "1", "0")
new_df$is_Amazon = is_Amazon
new_df

saveRDS(new_df, file = "new_data_IE544PROJECT.rds")



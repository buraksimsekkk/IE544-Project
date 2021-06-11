library(bnlearn)

data1 = readRDS("new_data_IE544PROJECT_son.rds")
data1 = data1[-c(1:4)]
data1 = data1[-c(4)]

#colnames(data1)
str(data1)

data1$is_Amazon <- as.factor(data1$is_Amazon)

data1$price_gap_ratio<- cut(data1$price_gap_ratio,
                       breaks = c(0,0.02, 0.04, 0.06, 0.15, 0.4, 0.6, 1, 1.5, 2, 20 ), 
                       include.lowest = TRUE,
                       right = TRUE )

data1$sid_pos_fb<- cut(data1$sid_pos_fb,
                            breaks = c(0, 7, 8, 9, 9.2, 9.4, 9.6, 9.7, 9.8, 9.9, 10 ), 
                            include.lowest = TRUE,
                            right = TRUE )




wlist <- data.frame(from = c( "price_gap_ratio","is_Amazon"), 
                      to = c(  "bbox", "bbox"))

blist <- data.frame(from = c("rank" , "price_gap_ratio" , "sid_pos_fb" , "bbox"), 
                      to = c("is_Amazon" , "is_Amazon", "is_Amazon" , "rank" ))




dag1 = hc(data1 , whitelist = wlist , blacklist= blist)
graphviz.plot(dag1)
score(dag1, data1)



dag2 = tabu(data1,blacklist= blist)
graphviz.plot(dag2)
score(dag2, data1)




#whitelist = wlist,

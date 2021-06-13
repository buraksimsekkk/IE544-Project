library(bnlearn)
library(gRain)
library(dplyr)
library(caret)

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




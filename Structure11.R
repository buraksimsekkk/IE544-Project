library(bnlearn)
library(gRain)

traindata <- readRDS("IE544_raw_data.rds")
testdata <- readRDS("IE544_test_raw_data.rds")


train = traindata %>% select(is_Amazon,  bbox )

#colnames(train)
str(train)

train$is_Amazon <- as.factor(train$is_Amazon)
train$price_gap_ratio<- cut(train$price_gap_ratio,
                       breaks = c(0,0.01 , 0.02, 0.12, 0.2, 0.4,  1, 1.5, 2, 20 ), 
                       include.lowest = TRUE,
                       right = TRUE )

# c(0, 0.04, 0.08, 0.12, 0.2, 0.4, 0.6, 1, 1.5, 2, 20 )




train$sid_pos_fb<- cut(train$sid_pos_fb,
                       breaks = c(0, 8.5, 9,  9.5,   10 ), 
                       include.lowest = TRUE,
                       right = TRUE )

train$rank <- as.numeric(train$rank)
train$rank<- cut(train$rank,
                       breaks = c(0 , 1 , 2 ,3 , 4 ,5  , 7, 9 ,12 ), 
                       include.lowest = TRUE,
                       right = TRUE )





test = testdata %>% select(is_Amazon)
test$price_gap_ratio<- cut(test$price_gap_ratio,
                           breaks = c(0,0.01 , 0.02, 0.12, 0.2, 0.4,  1, 1.5, 2, 20 ), 
                           include.lowest = TRUE,
                           right = TRUE )

test$sid_pos_fb<- cut(test$sid_pos_fb,
                      breaks = c(0, 8.5, 9,  9.5,    10 ),  
                      include.lowest = TRUE,
                      right = TRUE )

test$is_Amazon <- as.factor(test$is_Amazon)

test$rank <- as.numeric(test$rank)
test$rank<- cut(test$rank,
                 breaks = c(0 , 1 , 2 ,3 , 4 , 5, 7  , 9  ,12 ), 
                 include.lowest = TRUE,
                 right = TRUE )


str(test)





colnames(train)

wlist <- data.frame(from = c("price_gap_ratio" , "is_Amazon" , "rank" , "sid_pos_fb" ,"is_Amazon"), 
                      to = c( "bbox" ,"bbox" ,"bbox","bbox" ,"sid_pos_fb"     )   )

blist <- data.frame(from = c( "is_Amazon" , "price_gap_ratio" , "price_gap_ratio"    ,"sid_pos_fb"  , "rank"    ,"rank", "sid_pos_fb"  , "rank" ), 
                      to = c("price_gap_ratio" ,"is_Amazon" ,  "sid_pos_fb"   , "price_gap_ratio"   ,"price_gap_ratio"  ,"sid_pos_fb"  ,"rank" , "is_Amazon"  )  )

          

# Score-Based Algorithms
# Hill Climbing
dag_hc = hc(train  ) # , 
graphviz.plot(dag_hc)
score(dag_hc, train)


# Tabu Search
dag_tabu = tabu(train , blacklist= blist  ,whitelist = wlist )
graphviz.plot(dag_tabu)
score(dag_tabu, train)



# Constraint-Based Algorithms
# Grow-Shrink
dag_cb = gs(train, undirected = FALSE,  blacklist= blist ,whitelist = wlist ) # 
graphviz.plot(dag_cb)
score(dag_cb, train)


# Incremental Association 
dag_iamb = mmpc(train, undirected = FALSE, blacklist= blist, whitelist = wlist ) # 
graphviz.plot(dag_iamb)
score(dag_iamb, train)


# Hybrid
# Max-Min Hill Climbing
dag_hyb <- mmhc(train, blacklist= blist  , whitelist = wlist ) # 
graphviz.plot(dag_hyb)
score(dag_hyb, train)



#cross-validation with k-fold
str(train)
data.frame(train)
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



# bootstrapping
#arcs = boot.strength(train, R=50, algorithm = "hc", algorithm.args = list(whitelist = wlist , blacklist= blist), cpdag=TRUE)
#arcs<-arcs[(arcs$strength > 0.65) & (arcs$direction >= 0.5), ]
#graphviz.plot(averaged.network(arcs),layout="fdp")




###Question 4

#a

dag_hc
dag <-model2network("[is_Amazon][bbox|is_Amazon]")
graphviz.plot(dag)


trained <- bn.fit(dag,train)
print(trained)


#b

junction<-compile(as.grain(trained))
jev = setEvidence(junction, nodes = "is_Amazon", states = "1")
querygrain(jev, nodes = c("bbox"))


XYtrain = traindata %>% select(pid,epoc,sid,  rank, is_Amazon, bbox) 
XYtrain = add_column(XYtrain, train[,2], .after = 3) 
XYtrain = add_column(XYtrain, train[,3], .after = 4) 
XYtrain$epoc = as.Date(as.character(XYtrain$epoc))


testcheck <- XYtrain %>%
  group_by(epoc,pid)%>%
  select(pid,epoc,sid, is_Amazon,price_gap_ratio  , sid_pos_fb, rank )

str(testcheck)
testcheck$epoc <- as.factor(testcheck$epoc)
testcheck$sid <- as.factor(testcheck$sid)
testcheck$price_gap_ratio <- as.factor(testcheck$price_gap_ratio)
testcheck$sid_pos_fb      <- as.factor(testcheck$sid_pos_fb     )

str(testcheck)
testcheck <- data.frame(testcheck)


modified_traindata = select(traindata, -sid_pos_fb)
modified_traindata = add_column(modified_traindata, train[,1:3])

testcheck <- XYtrain[,6]
testcheck$is_Amazon <- as.factor(testcheck$is_Amazon)

str(testcheck)

traindata$pred_bbox <-  predict(trained, node = "bbox", testcheck)
all.equal(traindata$bbox,traindata$pred_bbox) 

confusionMatrix(traindata$bbox,traindata$pred_bbox)




testdata$pred_bbox <-  predict(trained, node = "bbox", test)
all.equal(testdata$bbox,testdata$pred_bbox ) 

confusionMatrix(testdata$bbox,testdata$pred_bbox)



sum(is.na(test))







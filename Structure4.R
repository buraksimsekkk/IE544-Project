library(bnlearn)
library(gRain)

rawdata<-readRDS("amz_train.rds")

data = readRDS("new_data_IE544PROJECT_son.rds")
data = add_column(data, rawdata[,2], .after = 1)
data = add_column(data, rawdata[,3], .after = 2)
data$epoc = as.Date(as.character(data$epoc))

train = data %>% select(is_Amazon,price_gap_ratio  , sid_pos_fb, rank ,  bbox )


#colnames(data)
str(train)

train$is_Amazon <- as.factor(train$is_Amazon)

train$price_gap_ratio<- cut(train$price_gap_ratio,
                       breaks = c(0, 0.04, 0.08, 0.12, 0.2, 0.4, 0.6, 1, 1.5, 2, 20 ), 
                       include.lowest = TRUE,
                       right = TRUE )

# c(0, 0.04, 0.08, 0.12, 0.2, 0.4, 0.6, 1, 1.5, 2, 20 )

train$sid_pos_fb<- cut(train$sid_pos_fb,
                            breaks = c(0, 7, 8, 9,   9.5,  10 ), 
                            include.lowest = TRUE,
                            right = TRUE )


colnames(train)

wlist <- data.frame(from = c("price_gap_ratio" , "is_Amazon" , "rank" , "sid_pos_fb" ,"is_Amazon"), 
                      to = c( "bbox" ,"bbox" ,"bbox","bbox", "sid_pos_fb"          )   )

blist <- data.frame(from = c("is_Amazon" , "price_gap_ratio" , "price_gap_ratio"  , "price_gap_ratio"         ), 
                      to = c("price_gap_ratio" ,"is_Amazon" ,  "sid_pos_fb" ,   "rank"              )  )

              #      , whitelist = wlist , blacklist= blist

# Hill Climbing
dag_hc = hc(train ,whitelist = wlist , blacklist= blist)
graphviz.plot(dag_hc)
score(dag_hc, train, type = "aic")
score(dag_hc,train, type = "bde")


dag_tabu = tabu(train, whitelist = wlist , blacklist= blist)
graphviz.plot(dag_tabu)
score(dag_tabu, train, type = "aic")
score(dag_tabu,train, type = "bde")


# #contraint based
dag_cb = gs(train, undirected = FALSE, whitelist = wlist , blacklist= blist)
graphviz.plot(dag_cb)
score(dag_cb, train, type = "aic")
score(dag_cb,train, type = "bde")

print(dag_cb)


# Hybrid
dag_hyb <- mmhc(train, whitelist = wlist , blacklist= blist)
graphviz.plot(dag_hyb)
score(dag_hyb, train, type = "aic")
score(dag_hyb,train, type = "bde")


#cross-validation with k-fold
str(train)
data.frame(train)
set.seed(100)

bn.cv(train, bn = "hc", algorithm.args = list(blacklist=blist),  method="hold-out")
bn.cv(train, bn = "mmhc",  algorithm.args = list(blacklist=blist), method="hold-out")
bn.cv(train, bn = "gs", algorithm.args = list(blacklist=blist),  method="hold-out")


#bootstrapping
arcs = boot.strength(train, R=50, algorithm = "hc", algorithm.args = list(whitelist = wlist , blacklist= blist), cpdag=TRUE)
arcs<-arcs[(arcs$strength > 0.65) & (arcs$direction >= 0.5), ]
graphviz.plot(averaged.network(arcs),layout="fdp")




###Question 4

#a

dag_hc
dag <-model2network("[is_Amazon][sid_pos_fb|is_Amazon][rank|is_Amazon:sid_pos_fb][price_gap_ratio|sid_pos_fb:rank][bbox|is_Amazon:price_gap_ratio:sid_pos_fb:rank]")
graphviz.plot(dag)


trained <- bn.fit( dag,train)
print(trained)

#bn.fit.barchart(trained$bbox)


#b

junction<-compile(as.grain(trained))
jev = setEvidence(junction, nodes = "is_Amazon", states = "1")
querygrain(jev, nodes = c("bbox"))


XYtrain <- mutate(train, rawdata[,1:3])
XYtrain$epoc = as.Date(as.character(XYtrain$epoc))


testcheck <- XYtrain %>%
  group_by(epoc,pid)%>%
  select(pid,epoc,sid, price_gap_ratio, sid_pos_fb, rank, is_Amazon)

str(testcheck)
testcheck$epoc <- as.factor(testcheck$epoc)
testcheck$sid <- as.factor(testcheck$sid)
testcheck$price_gap_ratio <- as.factor(testcheck$price_gap_ratio)
testcheck$sid_pos_fb      <- as.factor(testcheck$sid_pos_fb     )
testcheck$is_Amazon <- as.factor(testcheck$is_Amazon)
str(testcheck)
testcheck <- data.frame(testcheck)


modified_rawdata = select(rawdata, -sid_pos_fb)
modified_rawdata = add_column(modified_rawdata, train[,1:3])

qwe <- testcheck[,4:7]

modified_rawdata$pred.bbox <-  predict(trained, node = "bbox", qwe)
all.equal(modified_rawdata$bbox,modified_rawdata$pred.bbox) 

confusionMatrix(modified_rawdata$bbox,modified_rawdata$pred.bbox)



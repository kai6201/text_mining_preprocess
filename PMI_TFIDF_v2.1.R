#用RScript-Caller必寫
options(repos = "https://cran.rstudio.com")
if(!require(methods)){install.packages("methods")}

pkgs <- c("rio","readxl","dplyr","tidytext","igraph","ggraph","stringdist","taRifx")
pkgs_ins <- pkgs [!(pkgs %in% installed.packages()[,"Package"])]
if( length(pkgs_ins)>0 )
{
  for (i in 1:length(pkgs_ins))
  {
    install.packages(pkgs_ins[i])
  }
}

library(rio)
library(readxl)
library(tidytext)
library(dplyr)
library(igraph)
library(ggraph)
library(stringdist) #stringdist
library(taRifx) #remove.factors

cat("===== Word_Association =====\r\n")

#輸入檔案
location <- file.choose( )
location <- sub("clickme",replacement = "",location)
input_location <- paste0(location,"1_Input_Data")
output_location <- paste0(location,"2_Outcome")
setting_location <- paste0(location,"3_Setting")

#PMI count
setwd(input_location)
words_stat <- read_excel("words_stat.xlsx")
raw_data <- import("dimension_doc.csv") #raw_data = doc x term matrix

words_stat_select <- words_stat %>% filter(filter == 1) #文章篩選
sav <- colnames(raw_data) %in% (words_stat_select$ngram)
raw_data <- raw_data[,sav]

#執行計算
occurrence_data <- as.character()
pb_i <- txtProgressBar(min = 1, max = (ncol(raw_data)-1), style = 3)
for ( i in 1:(ncol(raw_data)-1))
{
  #比對資料i
  raw_data1 <- raw_data[,i] %>% as.data.frame() %>% remove.factors()
  colnames(raw_data1) <- colnames(raw_data[i])
  #比對資料2的資料集
  if(i == ncol(raw_data)-1)#當到數第2個col時，要命名
  {
    raw_data2 <- raw_data[,-1:-i] %>% as.data.frame() %>% remove.factors()
    colnames(raw_data2) <- colnames(raw_data[ncol(raw_data)])
  }
  else{raw_data2 <- raw_data[,-1:-i]}
  
  for ( j in 1:ncol(raw_data2))
  {
    #比對資料2之j
    occurrence_beg <- raw_data2[,j] %>% as.data.frame() %>% remove.factors()
    colnames(occurrence_beg) <- colnames(raw_data2[j])
    #比對資料合併
    occurrence <- cbind(raw_data1,occurrence_beg)
    var1 <- sum(raw_data1) #變數1加總
    var2 <- sum(occurrence_beg) #變數2加總
    #製作欄位
    occurrence_name <- cbind(colnames(raw_data1[1]),colnames(occurrence_beg[1])) %>% 
      as.data.frame() %>% remove.factors()
    #計算共同出現次數
    occurrence_sum <- rowSums(occurrence) %>% as.data.frame() %>% remove.factors()
    occurrence_count <- occurrence_sum[occurrence_sum[1]>1,]#兩欄位相加要>1，才會被篩出來
    
    occurrence_temp <- cbind(occurrence_name,length(occurrence_count),var1,var2)
    occurrence_data <- rbind(occurrence_data,occurrence_temp)
  }
  setTxtProgressBar(pb_i, i)
}
colnames(occurrence_data) <- c("Variable_1","Variable_2","Co-occurrence","variable_1 (Doc)","variable_2 (Doc)")

#PMI（Pointwise Mutual Information）計算
occurrence_data$PMI <- log2((occurrence_data[,3]*(sum(occurrence_data[,3])))/(occurrence_data[,4]*occurrence_data[,5]))
occurrence_data$PMI <- round(occurrence_data$PMI,2)
occurrence_data_PPMI <- occurrence_data %>% filter( PMI > 0) %>% arrange(desc(PMI))
occurrence_data_PPMI[,"index"] <- 1:nrow(occurrence_data_PPMI)

#排除重複相近詞
var_list <- occurrence_data_PPMI[,c("index","Variable_1","Variable_2")]
var_list[,"nchar1"] <- nchar(var_list[,"Variable_1"])
var_list[,"nchar2"] <- nchar(var_list[,"Variable_2"])
var_list[,"same"] <- ifelse(var_list[,"nchar1"]==var_list[,"nchar2"],1,0)
var_list[,"stringdist"] <- stringdist(var_list[,"Variable_1"],var_list[,"Variable_2"])
var_select <- var_list %>% filter( same == 0 & stringdist<2 )
PPMI_index <- occurrence_data_PPMI[,"index"] %in% var_select[,"index"]
occurrence_data_PPMI <- occurrence_data_PPMI[!PPMI_index,]
occurrence_data_PPMI <- select(occurrence_data_PPMI, -which(names(occurrence_data_PPMI) %in% "index"))

setwd(output_location)
export(occurrence_data_PPMI,"Co-occurrence.xlsx")

#PMI畫圖
windowsFonts(BL = windowsFont("Microsoft JhengHei"))
PPMI_graph <- occurrence_data_PPMI[,colnames(occurrence_data_PPMI) %in% c("Variable_1","Variable_2","PMI")]
PPMI_graph <- PPMI_graph %>% top_n(30,PMI)
#PPMI_graph$PMI <- round(PPMI_graph$PMI,0)
PPMI_graph <- graph_from_data_frame(PPMI_graph)
V(PPMI_graph)$Degree <- degree(PPMI_graph, mode = 'total')
#V(PPMI_graph)$popularity <- as.character(cut(degree(PPMI_graph, mode = 'total'), breaks = 3,labels = c('low', 'medium', 'high')))

PPMI_plot <- ggraph(PPMI_graph,layout = "linear", circular = TRUE) +
  coord_fixed() + 
  geom_edge_link2(aes(width = PMI,alpha = PMI),color="#adadad", show.legend = T) +
  scale_edge_width(range = c(0.5, 3)) +
  geom_node_point(aes(size= Degree),color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  #facet_edges(~PMI)+
  #facet_graph(PMI~popularity) +
  theme_void()

png(file = "PMI_top30.png")
print(PPMI_plot)
dev.off()


#TFIDF
setwd(input_location)
words_stat <- read_excel("words_stat.xlsx")
raw_data <- import("dimension_freq.csv") #raw_data = freq x term matrix
col_num <- raw_data[,1] %>% as.data.frame()
colnames(col_num) <- "No."

words_stat_select <- words_stat %>% filter(filter == 1) #文章篩選
sav <- colnames(raw_data) %in% (words_stat_select$ngram)
raw_data <- raw_data[,sav]

#TF&IDF count
tfidf_list <- as.list(1)
idf_list <- log2(nrow(raw_data) / words_stat_select$doc ) %>% as.list()
tf_list <- lapply(seq_len(ncol(raw_data)),function(x){raw_data[,x]})

#TFIDF count
for (i in 1:length(idf_list))
{
  tfidf_list[i] <- lapply(tf_list[i],function(x){x*idf_list[[i]]})
}

tfidf <- data.frame(matrix(unlist(tfidf_list), nrow=nrow(raw_data), byrow=F),stringsAsFactors=FALSE)
tfidf <- round(tfidf,2)
colnames(tfidf) <- colnames(raw_data)

#整理&輸出
tfidf <- cbind(col_num,tfidf)
setwd(output_location)
export(tfidf,"TFIDF.xlsx")
write.table(tfidf, file = "TFIDF.csv", sep = ",",row.names = FALSE) #csv
cat("\nkaikai\n")
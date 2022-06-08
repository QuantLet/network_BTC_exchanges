setwd("/Users/yifuwang/Documents/crypto_network")
library(PerformanceAnalytics)##Return.calculate
library(highfrequency)
## install.packages("lpSolve",destdir = "D:/r_packages/download")
## install.packages("tidyverse",destdir = "D:/r_packages/download")
## install.packages("PerformanceAnalytics",destdir = "D:/r_packages/download")
## install.packages("network",destdir = "D:/r_packages/download")
## install.packages("igraph",destdir = "D:/r_packages/download")
## install.packages("ggcorrplot",destdir = "D:/r_packages/download")
## install.packages("ggpubr",destdir = "D:/r_packages/download")

# think the difference of cryptocurrency and traditional assets

library(clime)
library(flare)
library(tidyverse)
library(ggcorrplot)
library(ggpubr)
# library(network)
library(igraph)
library(OpenMx)
library(magick)
# library(gridExtra) #grid.arrange
library(corrplot)
## install.packages("stargazer",destdir = "D:/r_packages/download")
## library(stargazer)

## define necessary functions 
### function for extracting the date information
extract_and_select_date<-function(df){
  date_ymd<-str_sub(df[,2],1,10)
  df<-cbind(df,date_ymd)
  df1<-filter(df,df$date_ymd>="2020-08-03"&df$date_ymd<='2022-03-31')
  new_df<-arrange(df1,df1[,1])
}

reserve_null<-function(df){
  df1<-filter(df,df$tradecount == "NULL")
}

LTM<-function(volumedata){
  ## whether the first row contains 0s
  LTM0<-sum(volumedata[1,])/100
  LTM_all<-rowSums(volumedata)/LTM0
}

{
  ## BTC
  Gemini_BTCUSD_1h <- read.csv(file = "Gemini_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitstamp_BTCUSD_1h <- read.csv(file = "Bitstamp_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_BTCUSD_1h <- read.csv(file = "Bitfinex_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Cex_BTCUSD_1h <- read.csv(file = "CEX_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Poloniex_BTCUSDT_1h <- read.csv(file = "Poloniex_BTCUSDT_1h.csv", header = TRUE, skip = 1)
  # Bittrex_BTCUSDT_1h <- read.csv(file = "Bittrex_BTCUSDT_1h.csv", header = TRUE, skip = 1)
  Exmo_BTCUSD_1h <- read.csv(file = "Exmo_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Binance_BTCUSDT_1h <- read.csv(file = "Binance_BTCUSDT_1h.csv", header = TRUE, skip = 1)
  Kucoin_BTCUSDT_1h <- read.csv(file = "Kucoin_BTCUSDT_1h.csv", header = TRUE, skip = 1)
  FTX_BTCUSD_1h <- read.csv(file = "FTX_BTCUSD_1h.csv", header = TRUE, skip = 1)
  
  BTC_1h_list <- list(Gemini_BTCUSD_1h,Bitstamp_BTCUSD_1h,Binance_BTCUSDT_1h,Kucoin_BTCUSDT_1h, FTX_BTCUSD_1h, Bitfinex_BTCUSD_1h, Exmo_BTCUSD_1h,Cex_BTCUSD_1h)
  n_BTC <- length(BTC_1h_list)
  BTC_1h_1<-lapply(BTC_1h_list, extract_and_select_date)
  names_exchanges <- c("Gemini", "Bitstamp", "Binance", "Kucoin", "FTX", "Bitfinex", "Exmo", "Cex")
  names(BTC_1h_1) <- names_exchanges
  ## loop to full join(merge) the data
  for (i in 1:length(BTC_1h_1)) {
    if (i==1){
      close_join <- cbind(BTC_1h_1[[i]][2], BTC_1h_1[[i]][7])
      colnames(close_join)[2] <- names_exchanges[i]
      volume_join <- cbind(BTC_1h_1[[i]][2], BTC_1h_1[[i]][8])
      colnames(volume_join)[2] <- names_exchanges[i]
    }
    else{
      close_to_join <- cbind(BTC_1h_1[[i]][2], BTC_1h_1[[i]][7])
      colnames(close_join)[2] <- names_exchanges[i]
      volume_to_join <- cbind(BTC_1h_1[[i]][2], BTC_1h_1[[i]][8])
      colnames(volume_join)[2] <- names_exchanges[i]
      close_join <- full_join(close_to_join, close_join, by = "date")
      volume_join <- full_join(volume_to_join, volume_join, by = "date")
    }
  }
}
colnames(close_join)[2] <- "Gemini"
colnames(volume_join)[2] <- "Gemini"

close_join_BTC <- close_join %>% arrange(date) %>% mutate(date_daily = as.Date(substr(date,1,10)))
close_BTC_df <- close_join_BTC %>% dplyr::select(-c(date,date_daily))
return_BTC_mt <- rbind(rep(0,ncol(close_BTC_df)),diff(log(as.matrix(close_BTC_df))))
return_BTC_df <- data.frame(close_join_BTC$date_daily, close_join_BTC$date, return_BTC_df)
names(return_BTC_df) <- c("date", "date_time", colnames(return_BTC_mt))

png("close_tsplot_Bitcoin_new.png", width = 1200, height = 600, bg = "transparent")

par(mfrow=c(2,4))


plot(return_BTC_df$Gemini~return_BTC_df$date,type = "l", main = "Gemini",xlab="", ylab="",ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Bitstamp~return_BTC_df$date,type = "l", main = "Bitstamp",xlab="", ylab="", ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Binance~return_BTC_df$date,type = "l", main = "Binance",xlab="", ylab="",ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Kucoin~return_BTC_df$date,type = "l", main = "Kucoin",xlab="", ylab="",ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$FTX~return_BTC_df$date,type = "l", main = "FTX",xlab="", ylab="", ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Bitfinex~return_BTC_df$date,type = "l", main = "Bitfinex",xlab="", ylab="",ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Exmo~return_BTC_df$date,type = "l", main = "Exmo",xlab="", ylab="", ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)
plot(return_BTC_df$Cex~return_BTC_df$date,type = "l", main = "Cex",xlab="", ylab="",ylim=c(-0.15,0.15), cex.axis = 2, cex.main = 2)

dev.off()

volume_join_BTC <- volume_join %>% arrange(date) %>% mutate(date_daily = as.Date(substr(date,1,10)))

png("Qtsplot_Bitcoin_new.png", width = 900, height = 600, bg = "transparent")

par(mfrow=c(2,4))

plot(volume_join_BTC$Gemini~volume_join_BTC$date_daily,type = "l", main = "Gemini",xlab="", ylab="",ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Bitstamp~volume_join_BTC$date_daily,type = "l", main = "Bitstamp",xlab="", ylab="", ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Binance~volume_join_BTC$date_daily,type = "l", main = "Binance",xlab="", ylab="",ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Kucoin~volume_join_BTC$date_daily,type = "l", main = "Kucoin",xlab="", ylab="",ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$FTX~volume_join_BTC$date_daily,type = "l", main = "FTX",xlab="", ylab="", ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Bitfinex~volume_join_BTC$date_daily,type = "l", main = "Bitfinex",xlab="", ylab="",ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Exmo~volume_join_BTC$date_daily,type = "l", main = "Exmo",xlab="", ylab="", ylim=c(0,490000000), cex.axis = 2, cex.main = 2)
plot(volume_join_BTC$Cex~volume_join_BTC$date_daily,type = "l", main = "Cex",xlab="", ylab="",ylim=c(0,490000000), cex.axis = 2, cex.main = 2)

dev.off()

### network centrality
n_loop <- length(unique(return_BTC_df$date))-30
evcent_BTC <- matrix(nrow = n_loop, ncol = n_BTC)
degree_cent_BTC <- matrix(nrow = n_loop, ncol = n_BTC)


date_start_fixed = "2020-08-03"
date_end_fixed = as.character(as.Date("2020-08-03")+n_loop+29)
fig = image_graph(width = 1200, height = 600, res = 256, bg = "transparent")
# for (t in 1:2) {
for (t in 1:n_loop) {
  # for (t in 1:(length(date_unique)-29)){
  ## extract the date for 1,7 and 30 days average
  # date_rolling_all <- seq.Date(from = (as.Date("2020-08-03")+t-1),to = (as.Date("2020-08-03")+t+29), by = "day")
  date_rolling_estimate <- as.Date("2020-08-03")+t+29
  date_rolling_1day <- as.Date("2020-08-03")+t+28
  date_rolling_7days <- seq.Date(from = (as.Date("2020-08-03")+t+22),to = (as.Date("2020-08-03")+t+28), by = "day")
  date_rolling_30days <- seq.Date(from = (as.Date("2020-08-03")+t-1),to = (as.Date("2020-08-03")+t+28), by = "day")
  
  ## select the data in this loop
  return_BTC_rolling_estimate <- return_BTC_df %>% dplyr::filter(date %in% date_rolling_estimate) %>% dplyr::select(-c(date,date_time))
  return_BTC_rolling_1day <- return_BTC_df %>% dplyr::filter(date %in% date_rolling_1day) %>% dplyr::select(-c(date,date_time))
  return_BTC_rolling_7days <- return_BTC_df %>% dplyr::filter(date %in% date_rolling_7days) %>% dplyr::select(-c(date,date_time))
  return_BTC_rolling_30days <- return_BTC_df %>% dplyr::filter(date %in% date_rolling_30days) %>% dplyr::select(-c(date,date_time))
  ## rolling window MHAR and plot
  
  
  tryCatch ({
    Rcov_BTC_1h_estimate_vech <- vech(t(chol(rCov(return_BTC_rolling_estimate))))
    Rcov_BTC_1h_1day_vech <- vech(t(chol(rCov(return_BTC_rolling_1day))))
    Rcov_BTC_1h_7days_vech <- vech(t(chol(rCov(return_BTC_rolling_7days)/7)))
    Rcov_BTC_1h_30days_vech <- vech(t(chol(rCov(return_BTC_rolling_30days)/30)))
    
    
    
    X_HAR <- cbind(rep(1,length(Rcov_BTC_1h_estimate_vech)),Rcov_BTC_1h_1day_vech,Rcov_BTC_1h_7days_vech,Rcov_BTC_1h_30days_vech)
    beta_HAR <- solve(t(X_HAR) %*% X_HAR) %*% t(X_HAR) %*% Rcov_BTC_1h_estimate_vech
    Y_HAR <- X_HAR %*% beta_HAR
    # back to cov matrix
    RC_HAR <- (vech2full(Y_HAR) * lower.tri(matrix(rep(1,n_BTC^2),nrow = n_BTC),diag = T))%*%(vech2full(Y_HAR)*upper.tri(matrix(rep(1, n_BTC^2),nrow = n_BTC),diag = T))
    
    precision_HAR = solve(RC_HAR)
    sdmatrix <- sqrt(diag(diag(precision_HAR)))
    pcor_rolling <- - solve(sdmatrix) %*% precision_HAR %*% solve(sdmatrix)
    diag(pcor_rolling) <- rep(1,ncol(pcor_rolling))
    pcor_rolling_0.1 <- pcor_rolling*(pcor_rolling>0.1)
    # pcor_sum
    # pcor_aver
    
    ### note the choice of lambda
    
    # clime_rolling_0.2 = clime(RC_HAR,sigma = TRUE,linsolver = "simplex",lambda = 0.2)
    # inv_rolling_0.2<-clime_rolling_0.2[["Omegalist"]][[1]]
    # sdmatrix <- sqrt(diag(diag(inv_rolling_0.2)))
    # pcor_rolling_0.2 <- solve(sdmatrix) %*% inv_rolling_0.2 %*% solve(sdmatrix)
    
    
    # clime_rolling_0.3 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
    # inv_rolling_0.3<-clime_rolling_0.3[["Omegalist"]][[1]]
    # sdmatrix <- sqrt(diag(diag(inv_rolling_0.3)))
    # pcor_rolling_0.3 <- solve(sdmatrix) %*% inv_rolling_0.3 %*% solve(sdmatrix)
    # 
    # clime_rolling_0.4 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.4)
    # inv_rolling_0.4<-clime_rolling_0.4[["Omegalist"]][[1]]
    # sdmatrix <- sqrt(diag(diag(inv_rolling_0.4)))
    # pcor_rolling_0.4 <- solve(sdmatrix) %*% inv_rolling_0.4 %*% solve(sdmatrix)
    
    # clime_rolling_0.5 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.5)
    # inv_rolling_0.5<-clime_rolling_0.5[["Omegalist"]][[1]]
    # sdmatrix <- sqrt(diag(diag(inv_rolling_0.5)))
    # pcor_rolling_0.5 <- solve(sdmatrix) %*% inv_rolling_0.5 %*% solve(sdmatrix)
    # 
    # clime_rolling_0.6 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.6)
    # inv_rolling_0.6<-clime_rolling_0.6[["Omegalist"]][[1]]
    # sdmatrix <- sqrt(diag(diag(inv_rolling_0.6)))
    # pcor_rolling_0.6 <- solve(sdmatrix) %*% inv_rolling_0.6 %*% solve(sdmatrix)
    
    # sdmatrix1 <- sqrt(diag(diag(rc_rolling)))
    # rcor_rolling <- solve(sdmatrix1) %*% rc_rolling %*% solve(sdmatrix1)
    # rcor_rolling[abs(rcor_rolling)<0.4] <- 0
    
    # p1<-ggcorrplot(pcor_rolling_0.2[,46:1], title = "λ=0.2")
    # p2<-ggcorrplot(pcor_rolling_0.3[,46:1], method = "square", type = "full", title = "λ=0.3")
    # p3<-ggcorrplot(pcor_rolling_0.4[,46:1], method = "square", type = "full", title = "λ=0.4")
    # p4<-ggcorrplot(rcor_rolling[,46:1], method = "square", type = "full", title = "realized correlation")
    # p5<-ggpubr::ggarrange(p1,p2,p3,p4,nrow = 2,ncol = 2,labels = c("A","B","C","D"))
    # 
    # ggsave(filename = paste("heatplot_assets_", t, ".png"), plot = p5)
    
    rownames(pcor_rolling) <- names_exchanges
    colnames(pcor_rolling) <- names_exchanges
    
    rownames(pcor_rolling_0.1) <- names_exchanges
    colnames(pcor_rolling_0.1) <- names_exchanges
    
    # layout(matrix(c(1,1,2,2,2), nrow = 1, ncol = 5, byrow = TRUE))
    
    # p_heatmap <-
    # ggcorrplot(pcor_rolling_0.1, method = "square", type = "full", title = as.character(as.Date("2020-08-03")+t+29), lab = TRUE)
    # ggsave(filename = paste("heatplot_all_BTC_CCs_",t,".png",sep = ""), plot = p_heatmap)
    #plot cor matrix
    # corrplot(pcor_rolling, order="original", method="circle", tl.pos="lt", type="upper",    
    #          tl.col="black", tl.cex=0.8, tl.srt=36, 
    #          cl.cex=0.6,
    #          addCoef.col="black", addCoefasPercent = TRUE,
    #          p.mat = 1-abs(pcor_rolling), sig.level=0.90, insig = "blank")
    
    network_BTC_all_t <- graph.adjacency(pcor_rolling_0.1, mode = "undirected", weighted = "TRUE", diag = FALSE)
    
    evcent_BTC_rcor<-eigen_centrality(network_BTC_all_t)
    evcent_BTC_rcor_v<-evcent_BTC_rcor$vector
    evcent_BTC[t,] <- evcent_BTC_rcor_v
    
    degree_BTC_pcor <- degree(network_BTC_all_t)
    degree_cent_BTC[t,] <- degree_BTC_pcor
    
    V(network_BTC_all_t)$name <- names_exchanges
    # png(paste("Networks_BTC_",t,".png",sep = ""), width = 600, height = 600, bg = "transparent")
    # plot(network_BTC_all_t, main = as.character(as.Date("2020-08-03")+t+29),
    #      layout = layout_in_circle,
    #      vertex.size = 40,          # Moderate nodes
    #      vertex.label = V(network_BTC_all_t)$name, # Set the labels
    #      vertex.label.cex = 0.9,   # Slightly smaller font
    #      # vertex.label.dist = 1,  # Offset the labels
    #      # vertex.label.color = "black"
    #      )
    # grid.arrange(p_heatmap, network_plot, ncol=2, nrow = 1)
    # dev.off()
  },
  error = function(e){
    message(paste("An error occurred for item", t,":\n"), e)
    })
}
dev.off()

animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/Network_", date_start_fixed, "_", 
                              date_end_fixed,".gif"))

## centrality overview
degree_cent_BTC_mean <- apply(na.omit(degree_cent_BTC), 2, mean)
degree_cent_BTC_med <- apply(na.omit(degree_cent_BTC), 2, median)
degree_cent_BTC_max <- apply(na.omit(degree_cent_BTC), 2, max)
degree_cent_BTC_min <- apply(na.omit(degree_cent_BTC), 2, min)

evcent_BTC_mean <- apply(na.omit(evcent_BTC), 2, mean)
evcent_BTC_med <- apply(na.omit(evcent_BTC), 2, median)
evcent_BTC_max <- apply(na.omit(evcent_BTC), 2, max)
evcent_BTC_min <- apply(na.omit(evcent_BTC), 2, min)


# CRIX return ACF
Binance_BTC_price <- Binance_BTCUSDT_1h$close
Binance_BTC_return <- diff(log(Binance_BTC_price))
Binance_BTC_return_abs <- abs(Binance_BTC_return)
png("Binance_BTC_return_abs_acf.png", width = 900, height = 900, bg = "transparent")
acf(Binance_BTC_return_abs, cex.axis = 2, cex.lab = 2, lag.max = 24*7, xlab = "Lag", ylab = "Sample ACF", main = "ACF of Binance BTC absolute return")
dev.off()

{
  ## read all the files in the directory
  # filenames <- list.files(pattern="*.csv", full.names=TRUE)
  # lapply(filenames, read.csv, header = TRUE, skip = 1)
  # res <- lapply(ldf, summary)
  # names(res) <- substr(filenames, 6, 30)
  
  
  
  ## US&UK
  ### GEMINI not OK
  gemini_BTCUSD_1h<-read.csv(file = "Gemini_BTCUSD_1h.csv", header = TRUE, skip = 1)
  gemini_ETHUSD_1h<-read.csv(file = "Gemini_ETHUSD_1h.csv", header = TRUE, skip = 1)
  gemini_LTCUSD_1h<-read.csv(file = "Gemini_LTCUSD_1h.csv", header = TRUE, skip = 1)
  gemini_ZECUSD_1h<-read.csv(file = "Gemini_ZECUSD_1h.csv", header = TRUE, skip = 1)

  gemini_1h_list<-list(gemini_BTCUSD_1h, gemini_ETHUSD_1h,gemini_LTCUSD_1h, gemini_ZECUSD_1h)

  gemini_1h_1<-lapply(gemini_1h_list, extract_and_select_date)
  n_row<-length(gemini_1h_1[[1]]$close)
  gemini_close_1h_p<-matrix(nrow = n_row,ncol = length(gemini_1h_1))
  for (i in 1:length(gemini_1h_1)) {
    gemini_close_1h_p[,i]<-gemini_1h_1[[i]]$close
  }
  colnames(gemini_close_1h_p)<-c("Gemini_BTCUSD_1h","Gemini_ETHUSD_1h","Gemini_LTCUSD_1h","Gemini_ZECUSD_1h")

  ### Bitstamp OK
  Bitstamp_BCHUSD_1h<-read.csv(file = "Bitstamp_BCHUSD_1h.csv", header = TRUE, skip = 1)
  Bitstamp_BTCUSD_1h<-read.csv(file = "Bitstamp_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitstamp_ETHUSD_1h<-read.csv(file = "Bitstamp_ETHUSD_1h.csv", header = TRUE, skip = 1)
  Bitstamp_LTCUSD_1h<-read.csv(file = "Bitstamp_LTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitstamp_XRPUSD_1h<-read.csv(file = "Bitstamp_XRPUSD_1h.csv", header = TRUE, skip = 1)

  Bitstamp_1h_list<-list(Bitstamp_BCHUSD_1h,Bitstamp_BTCUSD_1h,Bitstamp_ETHUSD_1h,Bitstamp_LTCUSD_1h,Bitstamp_XRPUSD_1h)

  Bitstamp_1h_1<-lapply(Bitstamp_1h_list, extract_and_select_date)
  n_row<-length(Bitstamp_1h_1[[1]]$close)
  Bitstamp_close_1h_p<-matrix(nrow = n_row,ncol = length(Bitstamp_1h_1))
  for (i in 1:length(Bitstamp_1h_1)) {
    Bitstamp_close_1h_p[,i]<-Bitstamp_1h_1[[i]]$close
  }
  colnames(Bitstamp_close_1h_p)<-c("Bitstamp_BCHUSD_1h","Bitstamp_BTCUSD_1h", "Bitstamp_ETHUSD_1h", "Bitstamp_LTCUSD_1h", "Bitstamp_XRPUSD_1h")

  ### Bitfinex not OK
  Bitfinex_BATUSD_1h<-read.csv(file = "Bitfinex_BATUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_BTCUSD_1h<-read.csv(file = "Bitfinex_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_DAIUSD_1h<-read.csv(file = "Bitfinex_DAIUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_DASHUSD_1h<-read.csv(file = "Bitfinex_DASHUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_EDOUSD_1h<-read.csv(file = "Bitfinex_EDOUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_EOSUSD_1h<-read.csv(file = "Bitfinex_EOSUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_ETHUSD_1h<-read.csv(file = "Bitfinex_ETHUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_ETPUSD_1h<-read.csv(file = "Bitfinex_ETPUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_LTCUSD_1h<-read.csv(file = "Bitfinex_LTCUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_NEOUSD_1h<-read.csv(file = "Bitfinex_NEOUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_OMGUSD_1h<-read.csv(file = "Bitfinex_OMGUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_REPUSD_1h<-read.csv(file = "Bitfinex_REPUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_TRXUSD_1h<-read.csv(file = "Bitfinex_TRXUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_XLMUSD_1h<-read.csv(file = "Bitfinex_XLMUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_XMRUSD_1h<-read.csv(file = "Bitfinex_XMRUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_XRPUSD_1h<-read.csv(file = "Bitfinex_XRPUSD_1h.csv", header = TRUE, skip = 1)
  Bitfinex_XVGUSD_1h<-read.csv(file = "Bitfinex_XVGUSD_1h.csv", header = TRUE, skip = 1)

  Bitfinex_1h_list<-list(Bitfinex_BATUSD_1h,Bitfinex_BTCUSD_1h,Bitfinex_DAIUSD_1h,Bitfinex_DASHUSD_1h,
                         Bitfinex_EDOUSD_1h,Bitfinex_EOSUSD_1h,Bitfinex_ETHUSD_1h,Bitfinex_ETPUSD_1h,
                         Bitfinex_LTCUSD_1h,Bitfinex_NEOUSD_1h,Bitfinex_OMGUSD_1h,Bitfinex_REPUSD_1h,
                         Bitfinex_TRXUSD_1h,Bitfinex_XLMUSD_1h,Bitfinex_XMRUSD_1h,Bitfinex_XRPUSD_1h,Bitfinex_XVGUSD_1h)

  Bitfinex_1h_1<-lapply(Bitfinex_1h_list, extract_and_select_date)
  n_row<-length(Bitfinex_1h_1[[1]]$close)
  Bitfinex_close_1h_p<-matrix(nrow = n_row,ncol = length(Bitfinex_1h_1))
  for (i in 1:length(Bitfinex_1h_1)) {
    Bitfinex_close_1h_p[,i]<-Bitfinex_1h_1[[i]]$close
  }
  colnames(Bitfinex_close_1h_p)<-c("Bitfinex_BATUSD_1h","Bitfinex_BTCUSD_1h", "Bitfinex_DAIUSD_1h", "Bitfinex_DASHUSD_1h",
                                   "Bitfinex_EDOUSD_1h", "Bitfinex_EOSUSD_1h","Bitfinex_ETHUSD_1h","Bitfinex_ETPUSD_1h",
                                   "Bitfinex_LTCUSD_1h", "Bitfinex_NEOUSD_1h", "Bitfinex_OMGUSD_1h","Bitfinex_REPUSD_1h",
                                   "Bitfinex_TRXUSD_1h", "Bitfinex_XLMUSD_1h", "Bitfinex_XMRUSD_1h","Bitfinex_XRPUSD_1h","Bitfinex_XVGUSD_1h")
  
  ### Cexio
  Cexio_BTCUSD_1h<-read.csv(file = "Cexio_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Cexio_ETHUSD_1h<-read.csv(file = "Cexio_ETHUSD_1h.csv", header = TRUE, skip = 1)
  Cexio_ETHUSD_1h<-read.csv(file = "Cexio_LTCUSD_1h.csv", header = TRUE, skip = 1)
  Cexio_XLMUSD_1h<-read.csv(file = "Cexio_XLMUSD_1h.csv", header = TRUE, skip = 1)
  Cexio_XRPUSD_1h<-read.csv(file = "Cexio_XRPUSD_1h.csv", header = TRUE, skip = 1)
  
  Cexio_1h_list<-list(Cexio_BTCUSD_1h,Cexio_ETHUSD_1h,Cexio_ETHUSD_1h,Cexio_XLMUSD_1h,Cexio_XRPUSD_1h)
  
  Cexio_1h_1<-lapply(Cexio_1h_list, extract_and_select_date)
  n_row<-length(Cexio_1h_1[[1]]$Close)
  Cexio_close_1h_p<-matrix(nrow = n_row,ncol = length(Cexio_1h_1))
  Cexio_volume_1h<-matrix(nrow = n_row,ncol = length(Cexio_1h_1))
  for (i in 1:length(Cexio_1h_1)) {
    Cexio_close_1h_p[,i]<-Cexio_1h_1[[i]]$Close
    Cexio_volume_1h[,i]<-Cexio_1h_1[[i]]$Volume.BTC
  }
  colnames(Cexio_close_1h_p)<-c("Cexio_BTCUSD_1h", "Cexio_ETHBTC_1h")
  colnames(Cexio_volume_1h)<-c("Cexio_BTCUSD_1h", "Cexio_ETHBTC_1h")
  
  ### Poloniex
  Poloniex_ETCETH_1h<-read.csv(file = "Poloniex_ETCETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_OMGETH_1h<-read.csv(file = "Poloniex_OMGETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_ZECETH_1h<-read.csv(file = "Poloniex_ZECETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_GNOETH_1h<-read.csv(file = "Poloniex_GNOETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_REPETH_1h<-read.csv(file = "Poloniex_REPETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_GASETH_1h<-read.csv(file = "Poloniex_GASETH_1h.csv", header = TRUE, skip = 1)
  Poloniex_ZRXETH_1h<-read.csv(file = "Poloniex_ZRXETH_1h.csv", header = TRUE, skip = 1)
  
  Poloniex_1h_list<-list(Poloniex_ETCETH_1h,Poloniex_OMGETH_1h, Poloniex_ZECETH_1h, Poloniex_GNOETH_1h, Poloniex_REPETH_1h)
  
  Poloniex_1h_1<-lapply(Poloniex_1h_list, extract_and_select_date)
  n_row<-length(Poloniex_1h_1[[1]]$Close)
  Poloniex_close_1h_p<-matrix(nrow = n_row,ncol = length(Poloniex_1h_1))
  Poloniex_volume_1h<-matrix(nrow = n_row,ncol = length(Poloniex_1h_1))
  for (i in 1:length(Poloniex_1h_1)) {
    Poloniex_close_1h_p[,i]<-Poloniex_1h_1[[i]]$Close
    Poloniex_volume_1h[,i]<-Poloniex_1h_1[[i]]$Volume.ETH
  }
  colnames(Poloniex_close_1h_p)<-c("Poloniex_ETCETH_1h", "Poloniex_OMGETH_1h", "Poloniex_ZECETH_1h", "Poloniex_GNOETH_1h", "Poloniex_REPETH_1h")  
  colnames(Poloniex_volume_1h)<-c("Poloniex_ETCETH_1h", "Poloniex_OMGETH_1h", "Poloniex_ZECETH_1h", "Poloniex_GNOETH_1h", "Poloniex_REPETH_1h")  
  ### Bittrex
  Bittrex_BTCUSD_1h<-read.csv(file = "Bittrex_BTCUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_ETHUSD_1h<-read.csv(file = "Bittrex_ETHUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_LTCUSD_1h<-read.csv(file = "Bittrex_LTCUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_NEOUSD_1h<-read.csv(file = "Bittrex_NEOUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_ETCUSD_1h<-read.csv(file = "Bittrex_ETCUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_OMGUSD_1h<-read.csv(file = "Bittrex_OMGUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_XMRUSD_1h<-read.csv(file = "Bittrex_XMRUSD_1h.csv", header = TRUE, skip = 1)
  Bittrex_DASHUSD_1h<-read.csv(file = "Bittrex_DASHUSD_1h.csv", header = TRUE, skip = 1)
  
  Bittrex_1h_list<-list(Bittrex_BTCUSD_1h, Bittrex_ETHUSD_1h, Bittrex_LTCUSD_1h, Bittrex_NEOUSD_1h, 
                        Bittrex_ETCUSD_1h, Bittrex_OMGUSD_1h, Bittrex_XMRUSD_1h, Bittrex_DASHUSD_1h)
  
  Bittrex_1h_1<-lapply(Bittrex_1h_list, extract_and_select_date)
  n_row<-length(Bittrex_1h_1[[1]]$Close)
  Bittrex_close_1h_p<-matrix(nrow = n_row,ncol = length(Bittrex_1h_1))
  Bittrex_volume_1h<-matrix(nrow = n_row,ncol = length(Bittrex_1h_1))
  for (i in 1:length(Bittrex_1h_1)) {
    Bittrex_close_1h_p[,i]<-Bittrex_1h_1[[i]]$Close
    Bittrex_volume_1h[,i]<-Bittrex_1h_1[[i]][,9]
  }
  colnames(Bittrex_close_1h_p)<-c("Bittrex_BTCUSD_1h", "Bittrex_ETHUSD_1h", "Bittrex_LTCUSD_1h", "Bittrex_NEOUSD_1h", "Bittrex_ETCUSD_1h", "Bittrex_OMGUSD_1h", "Bittrex_XMRUSD_1h", "Bittrex_DASHUSD_1h")
  colnames(Bittrex_volume_1h)<-c("Bittrex_BTCUSD_1h", "Bittrex_ETHUSD_1h", "Bittrex_LTCUSD_1h", "Bittrex_NEOUSD_1h", "Bittrex_ETCUSD_1h", "Bittrex_OMGUSD_1h", "Bittrex_XMRUSD_1h", "Bittrex_DASHUSD_1h")
  ## EU&Russia
  ### EXMO
  # Exmo_BTCUSD_1h<-read.csv(file = "Exmo_BTCUSD_1h.csv", header = TRUE, skip = 1)
  # Exmo_ETHUSD_1h<-read.csv(file = "Exmo_ETHUSD_1h.csv", header = TRUE, skip = 1)
  # Exmo_LTCUSD_1h<-read.csv(file = "Exmo_LTCUSD_1h.csv", header = TRUE, skip = 1)
  # Exmo_XRPUSD_1h<-read.csv(file = "Exmo_XRPUSD_1h.csv", header = TRUE, skip = 1)
  # Exmo_BTCEUR_1h<-read.csv(file = "Exmo_BTCEUR_1h.csv", header = TRUE, skip = 1)
  # Exmo_ETHEUR_1h<-read.csv(file = "Exmo_ETHEUR_1h.csv", header = TRUE, skip = 1)
  # Exmo_LTCEUR_1h<-read.csv(file = "Exmo_LTCEUR_1h.csv", header = TRUE, skip = 1)
  # Exmo_ETHBTC_1h<-read.csv(file = "Exmo_ETHBTC_1h.csv", header = TRUE, skip = 1)
  # 
  # Exmo_1h_list<-list(Exmo_BTCUSD_1h, Exmo_ETHUSD_1h, Exmo_LTCUSD_1h, Exmo_XRPUSD_1h, 
  #                       Exmo_BTCEUR_1h, Exmo_ETHEUR_1h, Exmo_LTCEUR_1h, Exmo_ETHBTC_1h)
  # 
  # Exmo_1h_1<-lapply(Exmo_1h_list, extract_and_select_date)
  # n_row<-length(Exmo_1h_1[[1]]$close)
  # Exmo_close_1h_p<-matrix(nrow = n_row,ncol = length(Exmo_1h_1))
  # for (i in 1:length(Exmo_1h_1)) {
  #   Exmo_close_1h_p[,i]<-Exmo_1h_1[[i]]$close
  # }
  # colnames(Exmo_close_1h_p)<-c("Exmo_BTCUSD_1h.csv", "Exmo_ETHUSD_1h.csv", "Exmo_LTCUSD_1h.csv", "Exmo_XRPUSD_1h.csv", 
  #                              "Exmo_BTCEUR_1h.csv", "Exmo_ETHEUR_1h.csv", "Exmo_LTCEUR_1h.csv", "Exmo_ETHBTC_1h.csv")
  
  ### Bitbay
  # Bitbay_BTCUSD_1h<-read.csv(file = "Bitbay_BTCUSD_1h.csv", header = TRUE, skip = 1)
  # Bitbay_LTCUSD_1h<-read.csv(file = "Bitbay_LTCUSD_1h.csv", header = TRUE, skip = 1)
  # Bitbay_ETHUSD_1h<-read.csv(file = "Bitbay_ETHUSD_1h.csv", header = TRUE, skip = 1)
  # Bitbay_BCHUSD_1h<-read.csv(file = "Bitbay_BCHUSD_1h.csv", header = TRUE, skip = 1)
  # Bitbay_LSKUSD_1h<-read.csv(file = "Bitbay_LSKUSD_1h.csv", header = TRUE, skip = 1)
  # Bitbay_BTCEUR_1h<-read.csv(file = "Bitbay_BTCEUR_1h.csv", header = TRUE, skip = 1)
  # Bitbay_LTCEUR_1h<-read.csv(file = "Bitbay_LTCEUR_1h.csv", header = TRUE, skip = 1)
  # Bitbay_ETHEUR_1h<-read.csv(file = "Bitbay_ETHEUR_1h.csv", header = TRUE, skip = 1)
  # Bitbay_BCHEUR_1h<-read.csv(file = "Bitbay_BCHEUR_1h.csv", header = TRUE, skip = 1)
  # Bitbay_LSKEUR_1h<-read.csv(file = "Bitbay_LSKEUR_1h.csv", header = TRUE, skip = 1)
  # 
  # Bitbay_1h_list<-list(Bitbay_BTCUSD_1h,Bitbay_LTCUSD_1h,Bitbay_ETHUSD_1h,Bitbay_BCHUSD_1h,
  #                        Bitbay_LSKUSD_1h,Bitbay_BTCEUR_1h,Bitbay_LTCEUR_1h,Bitbay_ETHEUR_1h,
  #                        Bitbay_BCHEUR_1h,Bitbay_LSKEUR_1h)
  # 
  # Bitbay_1h_1<-lapply(Bitbay_1h_list, extract_and_select_date)
  # n_row<-length(Bitbay_1h_1[[1]]$Close)
  # Bitbay_close_1h_p<-matrix(nrow = n_row,ncol = length(Bitbay_1h_1))
  # for (i in 1:length(Bitbay_1h_1)) {
  #   Bitbay_close_1h_p[,i]<-Bitbay_1h_1[[i]]$Close
  # }
  # colnames(Bitbay_close_1h_p)<-c("Bitbay_BTCUSD_1h", "Bitbay_LTCUSD_1h", "Bitbay_ETHUSD_1h", "Bitbay_BCHUSD_1h", 
  #                                "Bitbay_LSKUSD_1h", "Bitbay_BTCEUR_1h", "Bitbay_LTCEUR_1h", "Bitbay_ETHEUR_1h", 
  #                                "Bitbay_BCHEUR_1h", "Bitbay_LSKEUR_1h")
  
  ### Abucoins
  # Abucoins_BTCUSD_1h<-read.csv(file = "Abucoins_BTCUSD_1h.csv", header = TRUE, skip = 1)
  # Abucoins_BTCPLN_1h<-read.csv(file = "Abucoins_BTCPLN_1h.csv", header = TRUE, skip = 1)
  # Abucoins_ETHPLN_1h<-read.csv(file = "Abucoins_ETHPLN_1h.csv", header = TRUE, skip = 1)
  # Abucoins_BCHPLN_1h<-read.csv(file = "Abucoins_BCHPLN_1h.csv", header = TRUE, skip = 1)
  # Abucoins_XRPBTC_1h<-read.csv(file = "Abucoins_XRPBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_LTCBTC_1h<-read.csv(file = "Abucoins_LTCBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_ETHBTC_1h<-read.csv(file = "Abucoins_ETHBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_BCHBTC_1h<-read.csv(file = "Abucoins_BCHBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_SCBTC_1h<-read.csv(file = "Abucoins_SCBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_DASHBTC_1h<-read.csv(file = "Abucoins_DASHBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_ZECBTC_1h<-read.csv(file = "Abucoins_ZECBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_XMRBTC_1h<-read.csv(file = "Abucoins_XMRBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_REPBTC_1h<-read.csv(file = "Abucoins_REPBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_STRATBTC_1h<-read.csv(file = "Abucoins_STRATBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_XEMBTC_1h<-read.csv(file = "Abucoins_XEMBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_BTGBTC_1h<-read.csv(file = "Abucoins_BTGBTC_1h.csv", header = TRUE, skip = 1)
  # Abucoins_ETCBTC_1h<-read.csv(file = "Abucoins_ETCBTC_1h.csv", header = TRUE, skip = 1)
  # 
  # Abucoins_1h_list<-list(Abucoins_BTCUSD_1h,Abucoins_BTCPLN_1h,Abucoins_ETHPLN_1h,Abucoins_BCHPLN_1h,
  #                    Abucoins_XRPBTC_1h,Abucoins_LTCBTC_1h,Abucoins_ETHBTC_1h,Abucoins_BCHBTC_1h,
  #                    Abucoins_SCBTC_1h,Abucoins_DASHBTC_1h,Abucoins_ZECBTC_1h,Abucoins_XMRBTC_1h,
  #                    Abucoins_REPBTC_1h,Abucoins_STRATBTC_1h,Abucoins_XEMBTC_1h,Abucoins_BTGBTC_1h,
  #                    Abucoins_ETCBTC_1h)
  # 
  # 
  # Abucoins_1h_1<-lapply(Abucoins_1h_list, extract_and_select_date)
  # n_row<-length(Abucoins_1h_1[[1]]$Close)
  # Abucoins_close_1h_p<-matrix(nrow = n_row,ncol = length(Abucoins_1h_1))
  # for (i in 1:length(Abucoins_1h_1)) {
  #   Abucoins_close_1h_p[,i]<-Abucoins_1h_1[[i]]$Close
  # }
  # colnames(Abucoins_close_1h_p)<-c("Abucoins_BTCUSD_1h","Abucoins_BTCPLN_1h","Abucoins_ETHPLN_1h","Abucoins_BCHPLN_1h",
  #                              "Abucoins_XRPBTC_1h","Abucoins_LTCBTC_1h","Abucoins_ETHBTC_1h","Abucoins_BCHBTC_1h",
  #                              "Abucoins_SCBTC_1h","Abucoins_DASHBTC_1h","Abucoins_ZECBTC_1h","Abucoins_XMRBTC_1h",
  #                              "Abucoins_REPBTC_1h","Abucoins_STRATBTC_1h","Abucoins_XEMBTC_1h","Abucoins_BTGBTC_1h",
  #                              "Abucoins_ETCBTC_1h")

  ## Asian-Pacific
  ### Zaif
  # Zaif_BCHBTC_1h<-read.csv(file = "Zaif_BCHBTC_1h.csv", header = TRUE, skip = 1)
  # Zaif_ETHBTC_1h<-read.csv(file = "Zaif_ETHBTC_1h.csv", header = TRUE, skip = 1)
  # Zaif_MONABTC_1h<-read.csv(file = "Zaif_MONABTC_1h.csv", header = TRUE, skip = 1)
  # Zaif_XEMBTC_1h<-read.csv(file = "Zaif_XEMBTC_1h.csv", header = TRUE, skip = 1)
  # 
  # Zaif_BTCJPY_1h<-read.csv(file = "Zaif_BTCJPY_1h.csv", header = TRUE, skip = 1)
  # Zaif_XEMJPY_1h<-read.csv(file = "Zaif_XEMJPY_1h.csv", header = TRUE, skip = 1)
  # Zaif_BCHJPY_1h<-read.csv(file = "Zaif_BCHJPY_1h.csv", header = TRUE, skip = 1)
  # Zaif_ETHJPY_1h<-read.csv(file = "Zaif_ETHJPY_1h.csv", header = TRUE, skip = 1)
  # Zaif_MONAJPY_1h<-read.csv(file = "Zaif_MONAJPY_1h.csv", header = TRUE, skip = 1)
  # Zaif_ZAIFJPY_1h<-read.csv(file = "Zaif_ZAIFJPY_1h.csv", header = TRUE, skip = 1)
  # 
  # Zaif_1h_list<-list(Zaif_BCHBTC_1h,Zaif_ETHBTC_1h,Zaif_MONABTC_1h,Zaif_XEMBTC_1h,
  #                    Zaif_BTCJPY_1h,Zaif_XEMJPY_1h,Zaif_BCHJPY_1h,Zaif_ETHJPY_1h,Zaif_MONAJPY_1h,Zaif_ZAIFJPY_1h)
  # 
  # Zaif_1h_1<-lapply(Zaif_1h_list, extract_and_select_date)
  # n_row<-length(Zaif_1h_1[[1]]$Close)
  # Zaif_close_1h_p<-matrix(nrow = n_row,ncol = length(Zaif_1h_1))
  # for (i in 1:length(Zaif_1h_1)) {
  #   Zaif_close_1h_p[,i]<-Zaif_1h_1[[i]]$Close
  # }
  # colnames(Zaif_close_1h_p)<-c("Zaif_BCHBTC_1h","Zaif_ETHBTC_1h","Zaif_MONABTC_1h","Zaif_XEMBTC_1h","Zaif_BTCJPY_1h",
  #                              "Zaif_XEMJPY_1h","Zaif_BCHJPY_1h","Zaif_ETHJPY_1h","Zaif_MONAJPY_1h","Zaif_ZAIFJPY_1h")
  ### Okcoin
  Okcoin_LTCUSD_1h<-read.csv(file = "Okcoin_LTCUSD_1h.csv", header = TRUE, skip = 1)
  Okcoin_ETHUSD_1h<-read.csv(file = "Okcoin_ETHUSD_1h.csv", header = TRUE, skip = 1)
  Okcoin_BTCUSD_1h<-read.csv(file = "Okcoin_BTCUSD_1h.csv", header = TRUE, skip = 1)
  
  Okcoin_1h_list<-list(Okcoin_LTCUSD_1h,Okcoin_ETHUSD_1h,Okcoin_BTCUSD_1h)
  
  Okcoin_1h_1<-lapply(Okcoin_1h_list, extract_and_select_date)
  n_row<-length(Okcoin_1h_1[[1]]$Close)
  n_row1<-length(Okcoin_1h_list[[1]]$Close)
  Okcoin_close_1h_p<-matrix(nrow = n_row,ncol = length(Okcoin_1h_1))
  Okcoin_volume_1h0<-rep(NA,length(Okcoin_1h_list))
  Okcoin_volume_1h<-matrix(nrow = n_row,ncol = length(Okcoin_1h_1))
  for (i in 1:length(Okcoin_1h_1)) {
    Okcoin_close_1h_p[,i]<-Okcoin_1h_1[[i]]$Close
  }
  ### zeros on "2018-05-17", so we calaulate the LTM from the beginning,
  for (i in 1:length(Okcoin_1h_list)) {
    Okcoin_volume_1h0[i]<-Okcoin_1h_list[[i]]$Volume.USD[1]
  }
  for (i in 1:length(Okcoin_1h_1)) {
    Okcoin_volume_1h[,i]<-Okcoin_1h_1[[i]]$Volume.USD
  }
  colnames(Okcoin_close_1h_p)<-c("Okcoin_LTCUSD_1h","Okcoin_ETHUSD_1h","Okcoin_BTCUSD_1h")
  colnames(Okcoin_volume_1h)<-c("Okcoin_LTCUSD_1h","Okcoin_ETHUSD_1h","Okcoin_BTCUSD_1h")
  
  ### Okex
  Okex_EOSBTC_1h<-read.csv(file = "Okex_EOSBTC_1h.csv", header = TRUE, skip = 1)
  Okex_OMGBTC_1h<-read.csv(file = "Okex_OMGBTC_1h.csv", header = TRUE, skip = 1)
  Okex_NEOBTC_1h<-read.csv(file = "Okex_NEOBTC_1h.csv", header = TRUE, skip = 1)
  Okex_QTUMBTC_1h<-read.csv(file = "Okex_QTUMBTC_1h.csv", header = TRUE, skip = 1)
  Okex_ZECBTC_1h<-read.csv(file = "Okex_ZECBTC_1h.csv", header = TRUE, skip = 1)
  Okex_BCHBTC_1h<-read.csv(file = "Okex_BCHBTC_1h.csv", header = TRUE, skip = 1)
  Okex_DASHBTC_1h<-read.csv(file = "Okex_DASHBTC_1h.csv", header = TRUE, skip = 1)
  Okex_LTCBTC_1h<-read.csv(file = "Okex_LTCBTC_1h.csv", header = TRUE, skip = 1)
  Okex_ETCBTC_1h<-read.csv(file = "Okex_ETCBTC_1h.csv", header = TRUE, skip = 1)
  Okex_ETHBTC_1h<-read.csv(file = "Okex_ETHBTC_1h.csv", header = TRUE, skip = 1)
  
  Okex_1h_list<-list(Okex_EOSBTC_1h,Okex_OMGBTC_1h,Okex_NEOBTC_1h,
                     Okex_QTUMBTC_1h,Okex_ZECBTC_1h,Okex_BCHBTC_1h,Okex_DASHBTC_1h,
                     Okex_LTCBTC_1h,Okex_ETCBTC_1h,Okex_ETHBTC_1h)
  
  
  Okex_1h_1<-lapply(Okex_1h_list, extract_and_select_date)
  n_row<-length(Okex_1h_1[[1]]$Close)
  Okex_close_1h_p<-matrix(nrow = n_row,ncol = length(Okex_1h_1))
  Okex_volume_1h<-matrix(nrow = n_row,ncol = length(Okex_1h_1))
  for (i in 1:length(Okex_1h_1)) {
    Okex_close_1h_p[,i]<-Okex_1h_1[[i]]$Close
    Okex_volume_1h[,i]<-Okex_1h_1[[i]][,9]
  }                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
  colnames(Okex_close_1h_p)<-c("Okex_EOSBTC_1h","Okex_OMGBTC_1h","Okex_NEOBTC_1h",
                              "Okex_QTUMBTC_1h","Okex_ZECBTC_1h","Okex_BCHBTC_1h","Okex_DASHBTC_1h",
                              "Okex_LTCBTC_1h","Okex_ETCBTC_1h","Okex_ETHBTC_1h")
  colnames(Okex_volume_1h)<-c("Okex_EOSBTC_1h","Okex_OMGBTC_1h","Okex_NEOBTC_1h",
                              "Okex_QTUMBTC_1h","Okex_ZECBTC_1h","Okex_BCHBTC_1h","Okex_DASHBTC_1h",
                              "Okex_LTCBTC_1h","Okex_ETCBTC_1h","Okex_ETHBTC_1h")
  
  ### Bithumb
  Bithumb_BTGKRW_1h<-read.csv(file = "Bithumb_BTGKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_LTCKRW_1h<-read.csv(file = "Bithumb_LTCKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_ETHKRW_1h<-read.csv(file = "Bithumb_ETHKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_ZECKRW_1h<-read.csv(file = "Bithumb_ZECKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_ETCKRW_1h<-read.csv(file = "Bithumb_ETCKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_XMRKRW_1h<-read.csv(file = "Bithumb_XMRKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_BTCKRW_1h<-read.csv(file = "Bithumb_BTCKRW_1h.csv", header = TRUE, skip = 1)
  Bithumb_XRPKRW_1h<-read.csv(file = "Bithumb_XRPKRW_1h.csv", header = TRUE, skip = 1)
  
  Bithumb_1h_list<-list(Bithumb_BTGKRW_1h,Bithumb_LTCKRW_1h,Bithumb_ETHKRW_1h,Bithumb_ZECKRW_1h,
                        Bithumb_ETCKRW_1h,Bithumb_XMRKRW_1h,Bithumb_BTCKRW_1h,Bithumb_XRPKRW_1h)
  
  
  Bithumb_1h_1<-lapply(Bithumb_1h_list, extract_and_select_date)
  n_row<-length(Bithumb_1h_1[[1]]$Close)
  Bithumb_close_1h_p<-matrix(nrow = n_row,ncol = length(Bithumb_1h_1))
  Bithumb_volume_1h<-matrix(nrow = n_row,ncol = length(Bithumb_1h_1))
  for (i in 1:length(Bithumb_1h_1)) {
    Bithumb_close_1h_p[,i]<-Bithumb_1h_1[[i]]$Close
    Bithumb_volume_1h[,i]<-Bithumb_1h_1[[i]]$Volume.KRW
  }
  colnames(Bithumb_close_1h_p)<-c("Bithumb_BTGKRW_1h","Bithumb_LTCKRW_1h","Bithumb_ETHKRW_1h","Bithumb_ZECKRW_1h",
                                  "Bithumb_ETCKRW_1h","Bithumb_XMRKRW_1h","Bithumb_BTCKRW_1h","Bithumb_XRPKRW_1h")
  colnames(Bithumb_volume_1h)<-c("Bithumb_BTGKRW_1h","Bithumb_LTCKRW_1h","Bithumb_ETHKRW_1h","Bithumb_ZECKRW_1h",
                                  "Bithumb_ETCKRW_1h","Bithumb_XMRKRW_1h","Bithumb_BTCKRW_1h","Bithumb_XRPKRW_1h")
  
  ### Binance
  Binance_ADABTC_1h<-read.csv(file = "Binance_ADABTC_1h.csv", header = TRUE, skip = 1)
  Binance_BTCUSDT_1h<-read.csv(file = "Binance_BTCUSDT_1h.csv", header = TRUE, skip = 1)
  Binance_XLMBTC_1h<-read.csv(file = "Binance_XLMBTC_1h.csv", header = TRUE, skip = 1)
  Binance_EOSBTC_1h<-read.csv(file = "Binance_EOSBTC_1h.csv", header = TRUE, skip = 1)
  Binance_ETCBTC_1h<-read.csv(file = "Binance_ETCBTC_1h.csv", header = TRUE, skip = 1)
  Binance_ETHBTC_1h<-read.csv(file = "Binance_ETHBTC_1h.csv", header = TRUE, skip = 1)
  Binance_NEOBTC_1h<-read.csv(file = "Binance_NEOBTC_1h.csv", header = TRUE, skip = 1)
  Binance_XLMBTC_1h<-read.csv(file = "Binance_XLMBTC_1h.csv", header = TRUE, skip = 1)
  Binance_XMRBTC_1h<-read.csv(file = "Binance_XMRBTC_1h.csv", header = TRUE, skip = 1)
  Binance_XRPBTC_1h<-read.csv(file = "Binance_XRPBTC_1h.csv", header = TRUE, skip = 1)
  
  Binance_1h_list_1<-list(Binance_ADABTC_1h,Binance_BTCUSDT_1h,Binance_XLMBTC_1h,Binance_EOSBTC_1h,
                          Binance_ETCBTC_1h,Binance_ETHBTC_1h,Binance_LTCBTC_1h,
                          Binance_NEOBTC_1h,Binance_XLMBTC_1h,Binance_XMRBTC_1h,Binance_XRPBTC_1h)
  ### remove repeated dates(not null in tradeout)
  Binance_1h_list_1<-lapply(Binance_1h_list_1, reserve_null)
  
  Binance_1h_list<-Binance_1h_list_1
  
  Binance_1h_1<-lapply(Binance_1h_list, extract_and_select_date)
  n_row<-length(Binance_1h_1[[1]][,7])
  Binance_close_1h_p<-matrix(nrow = n_row,ncol = length(Binance_1h_1))
  Binance_volume_1h<-matrix(nrow = n_row,ncol = length(Binance_1h_1))
  for (i in 1:length(Binance_1h_1)) {
    Binance_close_1h_p[,i]<-Binance_1h_1[[i]][,7]
    Binance_volume_1h[,i]<-Binance_1h_1[[i]]$Volume.BTC
  }
  colnames(Binance_close_1h_p)<-c("Binance_ADABTC_1h","Binance_BTCUSDT_1h","Binance_XLMBTC_1h", "Binance_EOSBTC_1h", 
                                  "Binance_ETCBTC_1h", "Binance_ETHBTC_1h", "Binance_LTCBTC_1h", 
                                  "Binance_NEOBTC_1h","Binance_XLMBTC_1h", "Binance_XMRBTC_1h", "Binance_XRPBTC_1h")
  colnames(Binance_volume_1h)<-c("Binance_ADABTC_1h","Binance_BTCUSDT_1h","Binance_XLMBTC_1h", "Binance_EOSBTC_1h", 
                                 "Binance_ETCBTC_1h", "Binance_ETHBTC_1h", "Binance_LTCBTC_1h", 
                                 "Binance_NEOBTC_1h",  "Binance_XLMBTC_1h", "Binance_XMRBTC_1h", "Binance_XRPBTC_1h")
  
  ## other international
  ### Bitso
  Bitso_BTCMXN_1h<-read.csv(file = "Bitso_BTCMXN_1h.csv", header = TRUE, skip = 1)
  Bitso_ETHMXN_1h<-read.csv(file = "Bitso_ETHMXN_1h.csv", header = TRUE, skip = 1)
  
  Bitso_1h_list<-list(Bitso_BTCMXN_1h, Bitso_ETHMXN_1h)
  
  Bitso_1h_1<-lapply(Bitso_1h_list, extract_and_select_date)
  n_row<-length(Bitso_1h_1[[1]]$Close)
  Bitso_close_1h_p<-matrix(nrow = n_row,ncol = length(Bitso_1h_1))
  Bitso_volume_1h<-matrix(nrow = n_row,ncol = length(Bitso_1h_1))
  for (i in 1:length(Bitso_1h_1)) {
    Bitso_close_1h_p[,i]<-Bitso_1h_1[[i]]$Close
    Bitso_volume_1h[,i]<-Bitso_1h_1[[i]]$Volume.MXN
  }
  colnames(Bitso_close_1h_p)<-c("Bitso_BTCMXN_1h", "Bitso_ETHMXN_1h")
  colnames(Bitso_volume_1h)<-c("Bitso_BTCMXN_1h", "Bitso_ETHMXN_1h")
  
  ### Unocoin
  # Unocoin_BTCINR_1h<-read.csv(file = "Unocoin_BTCINR_1h.csv", header = TRUE, skip = 1)
  # 
  # Unocoin_1h_list<-list(Unocoin_BTCINR_1h)
  # 
  # Unocoin_1h_1<-lapply(Unocoin_1h_list, extract_and_select_date)
  # n_row<-length(Unocoin_1h_1[[1]]$Close)
  # Unocoin_close_1h_p<-matrix(nrow = n_row,ncol = length(Unocoin_1h_1))
  # for (i in 1:length(Unocoin_1h_1)) {
  #   Unocoin_close_1h_p[,i]<-Unocoin_1h_1[[i]]$Close
  # }
  # colnames(Unocoin_close_1h_p)<-c("Unocoin_BTCINR_1h")
  
  ### Bit2c
  # Bit2c_BTCILS_1h<-read.csv(file = "Bit2c_BTCILS_1h.csv", header = TRUE, skip = 1)
  # Bit2c_LTCILS_1h<-read.csv(file = "Bit2c_LTCILS_1h.csv", header = TRUE, skip = 1)
  # 
  # Bit2c_1h_list<-list(Bit2c_BTCILS_1h, Bit2c_LTCILS_1h)
  # 
  # Bit2c_1h_1<-lapply(Bit2c_1h_list, extract_and_select_date)
  # n_row<-length(Bit2c_1h_1[[1]]$Close)
  # Bit2c_close_1h_p<-matrix(nrow = n_row,ncol = length(Bit2c_1h_1))
  # for (i in 1:length(Bit2c_1h_1)) {
  #   Bit2c_close_1h_p[,i]<-Bit2c_1h_1[[i]]$Close
  # }
  # colnames(Bit2c_close_1h_p)<-c("Bit2c_BTCILS_1h", "Bit2c_LTCILS_1h")
  
  }

num_p_1day=24

date_all<-Binance_1h_1[[1]]$date_ymd

date_unique<-unique(date_all)

# xts_time<-strptime(xts_time, "%Y-%m-%d %I-%P")
# Binance_close_1h_p0<-cbind(Binance_close_1h_p,xts_date)
# Binance_close_1h_p_xts<-xts(Binance_close_1h_p0,order.by = xts_date)
# Binance_close_1h_r<-Return.calculate(Binance_close_1h_p, method = "log")


###all assets whole period
close_1h_p <- cbind(Cexio_close_1h_p, Poloniex_close_1h_p, Bittrex_close_1h_p, Okcoin_close_1h_p, 
                  Okex_close_1h_p, Bithumb_close_1h_p, Binance_close_1h_p, Bitso_close_1h_p)
nassets <- c(ncol(Cexio_close_1h_p), ncol(Poloniex_close_1h_p), ncol(Bittrex_close_1h_p), ncol(Okcoin_close_1h_p),
           ncol(Okex_close_1h_p), ncol(Bithumb_close_1h_p), ncol(Binance_close_1h_p), ncol(Bitso_close_1h_p))
index_assets <- cumsum(nassets)

close_1h_r0<-apply(log(close_1h_p), 2, diff)
### add a line of 0s to obtain a balanced sheet
close_1h_r<-rbind(rep(0,ncol(close_1h_p)),close_1h_r0)

### 
volume_1h<-cbind(Cexio_volume_1h, Poloniex_volume_1h, Bittrex_volume_1h, Okcoin_volume_1h, 
                 Okex_volume_1h, Bithumb_volume_1h, Binance_volume_1h, Bitso_volume_1h)
## write data for realized principle component
# write.table(close_1h_r, file = "close_1h_r.txt", row.names = FALSE, col.names = FALSE)


## load data for network analysis

### for loops according to the date (location) change to length(date_unique)-30
# setwd("E:\\workspace\\R\\realized_comoment\\heatplot_assets")
for (t in 1:2){
# for (t in 1:(length(date_unique)-29)){
  ## extract the date
  date_rolling <- date_unique[t:(t+30)]
  ## select the data in this loop
  n_Binance <- index_assets[length(index_assets)-1]-index_assets[length(index_assets)-2]+0
  close_1h_r_rolling <- close_1h_r[((t-1)*num_p_1day+1):((t+30)*num_p_1day),(index_assets[length(index_assets)-2]+1):index_assets[length(index_assets)-1]]
  
  ## rolling window MHAR and plot
  close_1h_r_estimate = close_1h_r_rolling[(nrow(close_1h_r_rolling)-num_p_1day+1):nrow(close_1h_r_rolling),]
  close_1h_r_1day = close_1h_r_rolling[(nrow(close_1h_r_rolling)-num_p_1day*2+1):(nrow(close_1h_r_rolling)-num_p_1day),]
  close_1h_r_7days = close_1h_r_rolling[(nrow(close_1h_r_rolling)-num_p_1day*8+1):(nrow(close_1h_r_rolling)-num_p_1day),]
  close_1h_r_30days = close_1h_r_rolling[1:(nrow(close_1h_r_rolling)-num_p_1day),]
  
  Rcov_close_1h_r_estimate_vech <- vech(t(chol(rCov(close_1h_r_estimate))))
  Rcov_close_1h_r_1day_vech <- vech(t(chol(rCov(close_1h_r_1day))))
  Rcov_close_1h_r_7days_vech <- vech(t(chol(rCov(close_1h_r_7days)/7)))
  Rcov_close_1h_r_30days_vech <- vech(t(chol(rCov(close_1h_r_30days)/30)))
  
  X_HAR <- cbind(rep(1,length(Rcov_close_1h_r_estimate_vech)),Rcov_close_1h_r_1day_vech,Rcov_close_1h_r_7days_vech,Rcov_close_1h_r_30days_vech)
  beta_HAR <- solve(t(X_HAR) %*% X_HAR) %*% t(X_HAR) %*% Rcov_close_1h_r_estimate_vech
  Y_HAR <- X_HAR %*% beta_HAR
  # back to cov matrix
  RC_HAR <- (vech2full(Y_HAR) * lower.tri(matrix(rep(1,n_Binance^2),nrow = n_Binance),diag = T))%*%(vech2full(Y_HAR)*upper.tri(matrix(rep(1, n_Binance^2),nrow = n_Binance),diag = T))
  
  precision_HAR = solve(RC_HAR)
  sdmatrix <- sqrt(diag(diag(precision_HAR)))
  pcor_rolling <- - solve(sdmatrix) %*% precision_HAR %*% solve(sdmatrix)

  
  ### note the choice of lambda
  
  # clime_rolling_0.2 = clime(RC_HAR,sigma = TRUE,linsolver = "simplex",lambda = 0.2)
  # inv_rolling_0.2<-clime_rolling_0.2[["Omegalist"]][[1]]
  # sdmatrix <- sqrt(diag(diag(inv_rolling_0.2)))
  # pcor_rolling_0.2 <- solve(sdmatrix) %*% inv_rolling_0.2 %*% solve(sdmatrix)

  
  # clime_rolling_0.3 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
  # inv_rolling_0.3<-clime_rolling_0.3[["Omegalist"]][[1]]
  # sdmatrix <- sqrt(diag(diag(inv_rolling_0.3)))
  # pcor_rolling_0.3 <- solve(sdmatrix) %*% inv_rolling_0.3 %*% solve(sdmatrix)
  # 
  # clime_rolling_0.4 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.4)
  # inv_rolling_0.4<-clime_rolling_0.4[["Omegalist"]][[1]]
  # sdmatrix <- sqrt(diag(diag(inv_rolling_0.4)))
  # pcor_rolling_0.4 <- solve(sdmatrix) %*% inv_rolling_0.4 %*% solve(sdmatrix)
  
  # clime_rolling_0.5 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.5)
  # inv_rolling_0.5<-clime_rolling_0.5[["Omegalist"]][[1]]
  # sdmatrix <- sqrt(diag(diag(inv_rolling_0.5)))
  # pcor_rolling_0.5 <- solve(sdmatrix) %*% inv_rolling_0.5 %*% solve(sdmatrix)
  # 
  # clime_rolling_0.6 = clime(rc_rolling,sigma = TRUE,linsolver = "simplex",lambda = 0.6)
  # inv_rolling_0.6<-clime_rolling_0.6[["Omegalist"]][[1]]
  # sdmatrix <- sqrt(diag(diag(inv_rolling_0.6)))
  # pcor_rolling_0.6 <- solve(sdmatrix) %*% inv_rolling_0.6 %*% solve(sdmatrix)
  
  # sdmatrix1 <- sqrt(diag(diag(rc_rolling)))
  # rcor_rolling <- solve(sdmatrix1) %*% rc_rolling %*% solve(sdmatrix1)
  # rcor_rolling[abs(rcor_rolling)<0.4] <- 0
  
  # p1<-ggcorrplot(pcor_rolling_0.2[,46:1], title = "λ=0.2")
  # p2<-ggcorrplot(pcor_rolling_0.3[,46:1], method = "square", type = "full", title = "λ=0.3")
  # p3<-ggcorrplot(pcor_rolling_0.4[,46:1], method = "square", type = "full", title = "λ=0.4")
  # p4<-ggcorrplot(rcor_rolling[,46:1], method = "square", type = "full", title = "realized correlation")
  # p5<-ggpubr::ggarrange(p1,p2,p3,p4,nrow = 2,ncol = 2,labels = c("A","B","C","D"))
  # 
  # ggsave(filename = paste("heatplot_assets_", t, ".png"), plot = p5)
  
  rownames(pcor_rolling) <- str_sub(colnames(Binance_close_1h_p), 1, str_length(colnames(Binance_close_1h_p))-6)
  colnames(pcor_rolling) <- str_sub(colnames(Binance_close_1h_p), 1, str_length(colnames(Binance_close_1h_p))-6)
  
  p_heatmap <-ggcorrplot(pcor_rolling, method = "square", type = "full", title = "Binance", lab = TRUE)
  # +
  #   ggplot2::labs(x = 'X label', y = 'Y label') +
  #   ggplot2::theme(
  #     axis.title.x = element_text(angle = 0, color = 'grey20'),
  #     axis.title.y = element_text(angle = 90, color = 'grey20')
  #   )
  ggsave(filename = paste("heatplot_all_Binance_CCs_",t,".png",sep = ""), plot = p_heatmap)
  
  network_rcor_all_t <- graph.adjacency(pcor_rolling, mode = "undirected", weighted = "TRUE", diag = FALSE)
  
  V(network_rcor_all_t)$name <- str_sub(colnames(Binance_close_1h_p), 1, str_length(colnames(Binance_close_1h_p))-6)
  png(paste("Networks_all_Binance_CCs_",t,".png",sep = ""), width = 600, height = 600, bg = "transparent")
  plot(network_rcor_all_t, main = "Networks of CCs",
       vertex.size = 4,          # Smaller nodes
       vertex.label = V(network_rcor_all_t)$name, # Set the labels
       vertex.label.cex = 0.8,   # Slightly smaller font
       vertex.label.dist = 0.4,  # Offset the labels
       vertex.label.color = "black")
  dev.off()
}

# gif
list.files(path="/Users/yifuwang/Documents/crypto_network/gif1", pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current dir


## LTM index
LTM_Cexio<-LTM(Cexio_volume_1h)
LTM_Poloniex<-LTM(Poloniex_volume_1h)
LTM_Bittrex<-LTM(Bittrex_volume_1h)

LTM_Okex<-LTM(Okex_volume_1h)
LTM_Bithumb<-LTM(Bithumb_volume_1h)
LTM_Binance<-LTM(Binance_volume_1h)
LTM_Bitso<-LTM(Bitso_volume_1h)


### zeros on "2018-05-17", so we calaulate the LTM from the beginning,
LTM_Okcoin0<-sum(Okcoin_volume_1h0)/100
LTM_Okcoin<-rowSums(Okcoin_volume_1h)/LTM_Okcoin0

LTM_ex_all<-cbind(LTM_Cexio,LTM_Poloniex,LTM_Bittrex,LTM_Okcoin,
                  LTM_Okex,LTM_Bithumb,LTM_Binance,LTM_Bitso)

# summary statistics
pc_mean<-apply(pc_ex_all,2,mean)
pc_median<-apply(pc_ex_all,2, median)
pc_max<-apply(pc_ex_all,2, max)
pc_min<-apply(pc_ex_all,2, min)
pc_skewness<-apply(pc_ex_all,2, skewness)
pc_kurtosis<-apply(pc_ex_all,2, kurtosis)

# realized network(correlation)
rc_ex_all<-rCov(pc_ex_all)
sdmatrix_pc <- sqrt(diag(diag(rc_ex_all)))
rcor_pc_all <- solve(sdmatrix_pc) %*% rc_ex_all %*% solve(sdmatrix_pc)
#rcor_all[abs(rcor_all)<0.4] <- 0
net_pc_rcor_all <- graph.adjacency(abs(rcor_pc_all),weighted=TRUE,diag=FALSE)

# realized network(partial correlation)
inv_pc_all<-solve(rc_ex_all)
sdmatrix_inv_pc <- sqrt(diag(diag(inv_pc_all)))
pcor_pc_all <- solve(sdmatrix_inv_pc) %*% inv_pc_all %*% solve(sdmatrix_inv_pc)
net_pc_pcor_all <- graph.adjacency(abs(pcor_pc_all),weighted=TRUE,diag=FALSE)

# analyze the network(table)
degree_pc_rcor<-degree(net_pc_rcor_all)
degree_pc_pcor<-degree(net_pc_pcor_all)

closeness_pc_rcor<-closeness(net_pc_rcor_all)
closeness_pc_pcor<-closeness(net_pc_pcor_all)

betweenness_pc_rcor<-betweenness(net_pc_rcor_all)
betweenness_pc_pcor<-betweenness(net_pc_pcor_all)

evcent_pc_rcor<-evcent(net_pc_rcor_all)
evcent_pc_rcor_v<-evcent_pc_rcor$vector
evcent_pc_pcor<-evcent(net_pc_pcor_all)
evcent_pc_pcor_v<-evcent_pc_pcor$vector
###
names(degree_pc_rcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(degree_pc_pcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(closeness_pc_rcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(closeness_pc_pcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(betweenness_pc_rcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(betweenness_pc_pcor)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(evcent_pc_rcor_v)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
names(evcent_pc_pcor_v)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
# plot the networks
par(mfrow=c(1,2))
# set.seed(1234)
l01<-layout.lgl(net_pc_rcor_all)
l02<-layout.lgl(net_pc_pcor_all)
par(mfrow=c(1,2))
plot(net_pc_rcor_all,layout=l01, vertex.label.color = "black",edge.arrow.size=0.1, main = "A: realized correlation",
     vertex.label=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso"))
plot(net_pc_pcor_all,layout=l02, vertex.label.color = "black",edge.arrow.size=0.1, main = "B: realized partial correlation",
     vertex.label=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso"))

colnames(rcor_pc_all)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
rownames(rcor_pc_all)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")

colnames(pcor_pc_all)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")
rownames(pcor_pc_all)=c("Cexio","Poloniex","Bittrex","Okcoin","Okex","Bithumb","Binance","Bitso")

p1_1<-ggcorrplot(rcor_pc_all[,8:1], title = "realized correlation")
p1_2<-ggcorrplot(pcor_pc_all[,8:1], method = "square", type = "full", title = "realized partial correlation")
p1_3<-ggpubr::ggarrange(p1_1,p1_2,nrow = 1,ncol = 2,labels = c("A","B"))

# summary statistics
c_mean<-apply(close_1h_r,2,mean)
c_median<-apply(close_1h_r,2, median)
c_max<-apply(close_1h_r,2, max)
c_min<-apply(close_1h_r,2, min)
c_skewness<-apply(close_1h_r,2, skewness)
c_kurtosis<-apply(close_1h_r,2, kurtosis)

c_summary<-cbind(c_mean,c_median,c_max,c_min,c_skewness,c_kurtosis)
# write.csv(c_summary,file = "summary_statistics.csv")

### names
# names<-c(rep("Okex",15),rep("Bithumb",8),rep("Binance",21))


rc_all<-rCov(close_1h_r)
### note the choice of lambda

clime_all_0.2 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.1)
inv_all_0.2<-clime_all_0.2[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.2)))
pcor_all_0.2 <- solve(sdmatrix) %*% inv_all_0.2 %*% solve(sdmatrix)
# colnames(pcor_all_0.2)<-names
# rownames(pcor_all_0.2)<-names

clime_all_0.3 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
inv_all_0.3<-clime_all_0.3[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.3)))
pcor_all_0.3 <- solve(sdmatrix) %*% inv_all_0.3 %*% solve(sdmatrix)

clime_all_0.4 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.4)
inv_all_0.4<-clime_all_0.4[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.4)))
pcor_all_0.4 <- solve(sdmatrix) %*% inv_all_0.4 %*% solve(sdmatrix)

clime_all_0.5 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.5)
inv_all_0.5<-clime_all_0.5[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.5)))
pcor_all_0.5 <- solve(sdmatrix) %*% inv_all_0.5 %*% solve(sdmatrix)

clime_all_0.6 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.6)
inv_all_0.6<-clime_all_0.6[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.6)))
pcor_all_0.6 <- solve(sdmatrix) %*% inv_all_0.6 %*% solve(sdmatrix)

sdmatrix1 <- sqrt(diag(diag(rc_all)))
rcor_all <- solve(sdmatrix1) %*% rc_all %*% solve(sdmatrix1)
rcor_all[abs(rcor_all)<0.4] <- 0

### cor heatplot
p1<-ggcorrplot(pcor_all_0.2[,64:1], title = "λ=0.2")
p2<-ggcorrplot(pcor_all_0.3[,64:1], method = "square", type = "full", title = "λ=0.3")
p3<-ggcorrplot(pcor_all_0.4[,64:1], method = "square", type = "full", title = "λ=0.4")
p4<-ggcorrplot(rcor_all[,64:1], method = "square", type = "full", title = "realized correlation")
p5<-ggpubr::ggarrange(p1,p2,p3,p4,nrow = 2,ncol = 2,labels = c("A","B","C","D"))

### threshold low
# dfcor[dfcor<0.5] <- 0
### generate network with adjacent matrix
par(mfrow=c(2,2))
net_pcor_0.2 <- graph.adjacency(abs(pcor_all_0.2),weighted=TRUE,diag=FALSE)
set.seed(123)
l1 <- layout_with_lgl(net_pcor_0.2)
plot(net_pcor_0.2, layout=l1, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))

net_pcor_0.3 <- graph.adjacency(abs(pcor_all_0.3),weighted=TRUE,diag=FALSE)
set.seed(123)
l2 <- layout_with_lgl(net_pcor_0.3)
plot(net_pcor_0.3, layout=l2, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))

net_pcor_0.4 <- graph.adjacency(abs(pcor_all_0.4),weighted=TRUE,diag=FALSE)
set.seed(123)
l3 <- layout_with_kk(net_pcor_0.4)
plot(net_pcor_0.4, layout=l3, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))

# net_pcor_0.5 <- graph.adjacency(abs(pcor_all_0.5),weighted=TRUE,diag=FALSE)
# set.seed(123)
# l4 <- layout_with_kk(net_pcor_0.5)
# plot(net_pcor_0.5, layout=l4, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))
# net_pcor_0.6 <- graph.adjacency(pcor_all_0.6,weighted=TRUE,diag=FALSE)
# net_pcor <- as.network(pcor_Binance,matrix.type = 'adjacency')
# plot(net_pcor)
net_rcor <- graph.adjacency(abs(rcor_all),weighted=TRUE,diag=FALSE)
set.seed(123)
l5 <- layout_with_lgl(net_rcor)
plot(net_rcor, layout=l5, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))
# analyze the network(table)
#degree_pc_rcor<-degree(net_pcor_0.3)
degree_pcor_0.3<-degree(net_pcor_0.3)

#closeness_pc_rcor<-closeness(net_pcor_0.3)
closeness_pcor_0.3<-closeness(net_pcor_0.3)

#betweenness_pc_rcor<-betweenness(net_pcor_0.3)
betweenness_pcor_0.3<-betweenness(net_pcor_0.3)
names(betweenness_pcor_0.3)=colnames(close_1h_p)

#evcent_pc_rcor<-evcent(net_pcor_0.3)
evcent_pcor_0.3<-evcent(net_pcor_0.3)

evcent__pcor_0.3_v<-evcent_pcor_0.3$vector
names(evcent__pcor_0.3_v)=colnames(close_1h_p)

## adjust the layout referring to ?igraph::layout
### vname as lable
### vname<-c(colnames(close_1h_p))
par(mfrow=c(2,3))
set.seed(1234)
plot(net_pcor_0.2,layout=layout_nicely, vertex.label.color = "black",edge.arrow.size=0.1, main = "A: λ=0.2")
set.seed(229)
plot(net_pcor_0.3,layout=layout.fruchterman.reingold, vertex.label.color = "black",edge.arrow.size=0.1, main = "B: λ=0.3")
plot(net_pcor_0.4,layout=layout.fruchterman.reingold, vertex.label.color = "black",edge.arrow.size=0.1, main = "C: λ=0.4")

plot(net_pcor_0.5,layout=layout.circle, vertex.label.color = "black",edge.arrow.size=0.1, main = "D: λ=0.5")
# set.seed(12345)
# plot(net_pcor_0.5,layout=layout.fruchterman.reingold, vertex.label.color = "black",edge.arrow.size=0.1)

plot(net_pcor_0.6,layout=layout.fruchterman.reingold, vertex.label.color = "black",edge.arrow.size=0.1, main = "E: λ=0.6")

plot(net_rcor,layout=layout.circle, vertex.label.color = "black",edge.arrow.size=0.1, main = "F: Realized Correlation")
# set.seed(1234)
# plot(net_rcor,layout=layout.fruchterman.reingold, vertex.label.color = "black",edge.arrow.size=0.1)

## subsample
close_1h_rt<-cbind(date_all,close_1h_r)
close_1h_pt<-cbind(date_all,close_1h_p)
close_1h_rt_down<-close_1h_rt[close_1h_rt[,1]>="2019-08-01"&close_1h_rt[,1]<='2019-09-30',]
close_1h_pt_down<-close_1h_pt[close_1h_pt[,1]>="2019-08-01"&close_1h_rt[,1]<='2019-09-30',]
close_1h_r_down<-close_1h_rt_down[,-1]
close_1h_rt_up<-close_1h_rt[close_1h_rt[,1]>="2019-02-01"&close_1h_rt[,1]<='2019-05-31',]
close_1h_pt_up<-close_1h_pt[close_1h_rt[,1]>="2019-02-01"&close_1h_pt[,1]<='2019-05-31',]
close_1h_r_up<-close_1h_rt_up[,-1]

plot(close_1h_pt_down[,"Bitso_BTCMXN_1h"]~as.Date(close_1h_pt_down[,1],"%Y-%m-%d"),type = "l",xlab="Time", ylab="Hourly Price")
plot(close_1h_pt_up[,"Bitso_BTCMXN_1h"]~as.Date(close_1h_pt_up[,1],"%Y-%m-%d"),type = "l",xlab="Time", ylab="Hourly Price")


rc_down<-rCov(close_1h_r_down)
clime_down_0.3 = clime(rc_down,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
inv_down_0.3<-clime_down_0.3[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_down_0.3)))
pcor_down_0.3 <- solve(sdmatrix) %*% inv_down_0.3 %*% solve(sdmatrix)

rc_up<-rCov(close_1h_r_up)
clime_up_0.3 = clime(rc_up,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
inv_up_0.3<-clime_up_0.3[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_up_0.3)))
pcor_up_0.3 <- solve(sdmatrix) %*% inv_up_0.3 %*% solve(sdmatrix)

clime_all_0.3 = clime(rc_all,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
inv_all_0.3<-clime_all_0.3[["Omegalist"]][[1]]
sdmatrix <- sqrt(diag(diag(inv_all_0.3)))
pcor_all_0.3 <- solve(sdmatrix) %*% inv_all_0.3 %*% solve(sdmatrix)

par(mfrow=c(1,2))
net_pcor_0.3 <- graph.adjacency(abs(pcor_all_0.3),weighted=TRUE,diag=FALSE)
set.seed(123)
l11 <- layout_with_lgl(net_pcor_0.3)
plot(net_pcor_0.3, layout=l2, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))

net_pcor_0.3 <- graph.adjacency(abs(pcor_all_0.3),weighted=TRUE,diag=FALSE)
set.seed(123)
l12 <- layout_with_lgl(net_pcor_0.3)
plot(net_pcor_0.3, layout=l2, vertex.size=5,vertex.label.color = "black",edge.arrow.size=0.01, vertex.label=colnames(close_1h_p))
# rolling estimate
{
date_0<-unique(date_all)
date_1<-date_0[-(1:7)]
num_days=nrow(close_1h_r)/24
num_edges_0.2=rep(0,num_days-7)
num_edges_0.3=rep(0,num_days-7)
num_edges_0.4=rep(0,num_days-7)
num_edges_0.5=rep(0,num_days-7)
num_edges_0.6=rep(0,num_days-7)
num_edges_rcor=rep(0,num_days-7)

## lambda = 0.2
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  clime_0_0.2 = clime(rc_0,sigma = TRUE,linsolver = "simplex",lambda = 0.2)
  inv_0_0.2<-clime_0_0.2[["Omegalist"]][[1]]
  sdmatrix0 <- sqrt(diag(diag(inv_0_0.2)))
  pcor_0_0.2 <- solve(sdmatrix0) %*% inv_0_0.2 %*% solve(sdmatrix0)
  net_pcor_0.2_0 <- graph.adjacency(pcor_0_0.2,weighted=TRUE,diag=FALSE)
  num_edges_0.2[i]=ecount(net_pcor_0.2_0)
}

## lambda = 0.3
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  clime_0_0.3 = clime(rc_0,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
  inv_0_0.3<-clime_0_0.3[["Omegalist"]][[1]]
  sdmatrix0 <- sqrt(diag(diag(inv_0_0.3)))
  pcor_0_0.3 <- solve(sdmatrix0) %*% inv_0_0.3 %*% solve(sdmatrix0)
  net_pcor_0.3_0 <- graph.adjacency(pcor_0_0.3,weighted=TRUE,diag=FALSE)
  num_edges_0.3[i]=ecount(net_pcor_0.3_0)
}

## lambda = 0.4
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  clime_0_0.4 = clime(rc_0,sigma = TRUE,linsolver = "simplex",lambda = 0.4)
  inv_0_0.4<-clime_0_0.4[["Omegalist"]][[1]]
  sdmatrix0 <- sqrt(diag(diag(inv_0_0.4)))
  pcor_0_0.4 <- solve(sdmatrix0) %*% inv_0_0.4 %*% solve(sdmatrix0)
  net_pcor_0.4_0 <- graph.adjacency(pcor_0_0.4,weighted=TRUE,diag=FALSE)
  num_edges_0.4[i]=ecount(net_pcor_0.4_0)
}

## lambda = 0.5
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  clime_0_0.5 = clime(rc_0,sigma = TRUE,linsolver = "simplex",lambda = 0.5)
  inv_0_0.5<-clime_0_0.5[["Omegalist"]][[1]]
  sdmatrix0 <- sqrt(diag(diag(inv_0_0.5)))
  pcor_0_0.5 <- solve(sdmatrix0) %*% inv_0_0.5 %*% solve(sdmatrix0)
  net_pcor_0.5_0 <- graph.adjacency(pcor_0_0.5,weighted=TRUE,diag=FALSE)
  num_edges_0.5[i]=ecount(net_pcor_0.5_0)
}

## lambda = 0.6
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  clime_0_0.3 = clime(rc_0,sigma = TRUE,linsolver = "simplex",lambda = 0.3)
  inv_0_0.3<-clime_0_0.3[["Omegalist"]][[1]]
  sdmatrix0 <- sqrt(diag(diag(inv_0_0.3)))
  pcor_0_0.3 <- solve(sdmatrix0) %*% inv_0_0.3 %*% solve(sdmatrix0)
  net_pcor_0.3_0 <- graph.adjacency(pcor_0_0.3,weighted=TRUE,diag=FALSE)
  num_edges_0.3[i]=ecount(net_pcor_0.3_0)
}

## correlation
for(i in 1:(num_days-7)){
  if(i == 1 | i == 2 | i == 5){
    cat(i,"loops complete\n")
  }
  close_1h_r0 = close_1h_r[i:(24*7+i),]
  rc_0<-rCov(close_1h_r0)
  sdmatrix1 <- sqrt(diag(diag(rc_0)))
  rcor_0 <- solve(sdmatrix1) %*% rc_0 %*% solve(sdmatrix1)
  rcor_0[abs(rcor_0)<0.4] <- 0
  net_rcor_0 <- graph.adjacency(rcor_0,weighted=TRUE,diag=FALSE)
  num_edges_rcor[i]=ecount(net_rcor_0)
}

}
## empirical results
### fig 1:1time series of prices and returns
# par(mfrow=c(2,2))
### Cexio
plot(close_1h_p[,"Cexio_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",main = "Cexio",xlab="Time", ylab="Hourly Price",ylim=c(min(Cexio_close_1h_p),max(Cexio_close_1h_p)))
lines(close_1h_p[,"Cexio_ETHBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="royalblue",lty=1)

par(mai=c(2,0.5,0.5,0.5))
par(mfrow=c(2,3))
xy=par("usr") ## the location of x axis and y axis
# par(mfrow=c(1,1))
### Poloniex
legend(x=xy[2]-xinch(0.1),y=xy[3]-yinch(0.2),
       legend=c("ETCETH","OMGETH","ZECETH","GNOETH","REPETH"),
       col=c("steelblue","deepskyblue","darkolivegreen","lightsalmon","coral"),
       lty=1,lwd=1.5,xpd=TRUE,ncol=3,bty= "n")
plot(close_1h_p[,"Poloniex_ETCETH_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col = "steelblue", main = "Poloniex",xlab="Time", ylab="Hourly Price",ylim=c(min(Poloniex_close_1h_p),max(Poloniex_close_1h_p)))
lines(close_1h_p[,"Poloniex_OMGETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="deepskyblue",lty=1)
lines(close_1h_p[,"Poloniex_ZECETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="darkolivegreen",lty=1)
lines(close_1h_p[,"Poloniex_GNOETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="lightsalmon",lty=1)
lines(close_1h_p[,"Poloniex_REPETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="coral",lty=1)

### Bittrex
# plot(close_1h_p[,"Bittrex_BTCUSD_1h"]~as.Date(xts_time,"%Y-%m-%d"),type = "l",main = "Bittrex",xlab="Time", ylab="Hourly Price",ylim=c(min(Bittrex_close_1h_p),max(Bittrex_close_1h_p)))
plot(close_1h_p[,"Bittrex_ETHUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col = "royalblue", main = "Bittrex",xlab="Time", ylab="Hourly Price",ylim=c(min(Bittrex_close_1h_p[,-1]),max(Bittrex_close_1h_p[,-1])))
#lines(close_1h_p[,"Bittrex_ETHUSD_1h"]~as.Date(xts_time,"%Y-%m-%d"),col="Blue",lty=1)
lines(close_1h_p[,"Bittrex_LTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="orangered",lty=1)
lines(close_1h_p[,"Bittrex_NEOUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="firebrick",lty=1)
lines(close_1h_p[,"Bittrex_ETCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="steelblue",lty=1)
lines(close_1h_p[,"Bittrex_OMGUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="deepskyblue",lty=1)
lines(close_1h_p[,"Bittrex_XMRUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="dimgray",lty=1)
lines(close_1h_p[,"Bittrex_DASHUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="brown",lty=1)

### Okcoin
plot(close_1h_p[,"Okcoin_LTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", col = "orangered", main = "Okcoin",xlab="Time", ylab="Hourly Price",ylim=c(min(Okcoin_close_1h_p[,-3]),max(Okcoin_close_1h_p[,-3])))
lines(close_1h_p[,"Okcoin_ETHUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),col="royalblue",lty=1)
# lines(close_1h_p[,"Okcoin_BTCUSD_1h"]~as.Date(xts_time,"%Y-%m-%d"),col="seagreen",lty=1)

### Okex
plot(close_1h_p[,"Okex_ETHBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",main = "Okex",col = "royalblue", xlab="Time", ylab="Hourly Price",ylim=c(min(Okex_close_1h_p),max(Okex_close_1h_p)))
#lty means line type
lines(close_1h_p[,"Okex_EOSBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="chocolate",lty=1)
lines(close_1h_p[,"Okex_OMGBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="deepskyblue",lty=1)
lines(close_1h_p[,"Okex_NEOBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="firebrick",lty=1)
lines(close_1h_p[,"Okex_BCHBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="turquoise",lty=1)
lines(close_1h_p[,"Okex_QTUMBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="chartreuse",lty=1)
lines(close_1h_p[,"Okex_ZECBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="purple",lty=1)
lines(close_1h_p[,"Okex_DASHBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="brown",lty=1)
lines(close_1h_p[,"Okex_LTCBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="orangered",lty=1)
lines(close_1h_p[,"Okex_ETCBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),col="steelblue",lty=1)

lines(close_1h_p[,"Okex_OMGETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="deepskyblue4",lty=3)
lines(close_1h_p[,"Okex_ETCETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="steelblue1",lty=3)
lines(close_1h_p[,"Okex_EOSETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="chocolate1",lty=3)
lines(close_1h_p[,"Okex_QTUMETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="chartreuse4",lty=3)
lines(close_1h_p[,"Okex_NEOETH_1h"]~as.Date(date_all,"%Y-%m-%d"),col="firebrick1",lty=3)

### Bithumb
plot(close_1h_p[,"Bithumb_BTGKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col = "olivedrab", main = "Bithumb",xlab="Time", ylab="Hourly Price",ylim=c(min(Bithumb_close_1h_p[,-7]),max(Bithumb_close_1h_p[,-7])))
#lty means line type
lines(close_1h_p[,"Bithumb_LTCKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="orangered",lty=1)
lines(close_1h_p[,"Bithumb_ETHKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="royalblue",lty=1)
lines(close_1h_p[,"Bithumb_ZECKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="purple",lty=1)
lines(close_1h_p[,"Bithumb_ETCKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="steelblue",lty=1)
lines(close_1h_p[,"Bithumb_XMRKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="dimgray",lty=1)
#lines(close_1h_p[,"Bithumb_BTCKRW_1h"]~as.Date(xts_time,"%Y-%m-%d"),col="lightgoldenrod",lty=1)
lines(close_1h_p[,"Bithumb_XRPKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),col="seagreen",lty=1)

### Binance
# parse time stamp
date_Binance_char <- Binance_1h_1[[1]]$datedate_Binance 
# which(is.na(date_Binance))
Binance_close_1h_r0<-apply(log(Binance_close_1h_p), 2, diff)
Binance_close_1h_r<-rbind(rep(0,ncol(Binance_close_1h_p)),Binance_close_1h_r0)
xts_1h_p <- xts(scale(Binance_close_1h_p), date_Binance)
xts_1h_r <- xts(Binance_close_1h_r, date_Binance)
col_palette <- c("deepskyblue4", "chocolate", "mediumpurple4", "slateblue", "purple",
                 "turquoise4", "skyblue4", "steelblue", "blue2", "navyblue",
                 "orangered3", "lightgoldenrod4", "coral3", "palevioletred4", "red3",
                 "springgreen4", "darkolivegreen", "tan4", "dodgerblue4", "brown", "grey30")

plot.xts(xts_1h_p, col = col_palette, main ="Cryptocurrencies in Binance", format.labels="%b-%Y")
addLegend("topright", on = 1, lty = rep(1,21), legend.names = colnames(Binance_close_1h_p), col = col_palette, ncol = 3)

### Bitso
# plot(close_1h_p[,"Bitso_BTCMXN_1h"]~as.Date(xts_time,"%Y-%m-%d"),type = "l",main = "Bitso",xlab="Time", ylab="Hourly Price",ylim=c(min(Bitso_close_1h_p),max(Bitso_close_1h_p)))
# lines(close_1h_p[,"Bitso_ETHMXN_1h"]~as.Date(xts_time,"%Y-%m-%d"),col="royalblue",lty=1)

### plot BTC alone
par(mfrow=c(2,3))
plot(close_1h_p[,"Bitso_BTCMXN_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="royalblue",main = "Bitso_BTC",xlab="Time", ylab="Hourly Price")
plot(close_1h_p[,"Binance_BTCUSDT_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="Blue",main = "Binance_BTC",xlab="Time", ylab="Hourly Price")
plot(close_1h_p[,"Bithumb_BTCKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="deepskyblue4",main = "Bithumb_BTC",xlab="Time", ylab="Hourly Price")
plot(close_1h_p[,"Okcoin_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="lightsteelblue",main = "Okcoin_BTC",xlab="Time", ylab="Hourly Price")
plot(close_1h_p[,"Bittrex_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="deepskyblue",main = "Bittrex_BTC",xlab="Time", ylab="Hourly Price")
plot(close_1h_p[,"Cexio_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l",col="steelblue",main = "Cexio_BTC",xlab="Time", ylab="Hourly Price")

### plot the difference instead (exchange rate or standardized??) by Prof. Haerdle, or just return (by Ren)
# diff_BTC_Binance_Bithumb <- scale(close_1h_p[,"Bitso_BTCMXN_1h"])-scale(close_1h_p[,"Bithumb_BTCKRW_1h"])
Binance_BTCUSDT_1h<-read.csv(file = "Binance_BTCUSDT_1h.csv", header = TRUE, skip = 1)

Binance_BTCUSDT_1h_list <- Binance_BTCUSDT_1h %>% reserve_null %>% extract_and_select_date
Binance_BTCUSDT_1h_r <- c(0, diff(log(Binance_BTCUSDT_1h_list$close)))

png("Rtsplot_Bitcoin.png", width = 900, height = 600, bg = "transparent")

par(mfrow=c(2,3))

plot(Binance_BTCUSDT_1h_r~as.Date(date_all,"%Y-%m-%d"),type = "l",main = "Binance",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(close_1h_r[,"Bitso_BTCMXN_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bitso",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(close_1h_r[,"Bithumb_BTCKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bithumb",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(close_1h_r[,"Okcoin_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Okcoin",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(close_1h_r[,"Bittrex_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bittrex",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(close_1h_r[,"Cexio_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Cexio",xlab="", ylab="", cex.axis = 2, cex.main = 2)

dev.off()

png("Qtsplot_Bitcoin.png", width = 900, height = 600, bg = "transparent")

par(mfrow=c(2,3))

plot(Binance_BTCUSDT_1h_list$Volume.USDT~as.Date(date_all,"%Y-%m-%d"),type = "l",main = "Binance",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(volume_1h[,"Bitso_BTCMXN_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bitso",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(volume_1h[,"Bithumb_BTCKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bithumb",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(volume_1h[,"Okcoin_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Okcoin",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(volume_1h[,"Bittrex_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Bittrex",xlab="", ylab="", cex.axis = 2, cex.main = 2)
plot(volume_1h[,"Cexio_BTCUSD_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "Cexio",xlab="", ylab="", cex.axis = 2, cex.main = 2)

dev.off()

## acf plot of returns
png("Binance_BTCUSDT_r_1h_acf.png", width = 900, height = 600, bg = "transparent")
plot(acf(Binance_BTCUSDT_1h_r), type = "h")
dev.off()
png("Bittrex_BTCUSD_r_1h_acf.png", width = 900, height = 600, bg = "transparent")
plot(acf(close_1h_r[,"Bittrex_BTCUSD_1h"]), type = "h")
dev.off()

par(mfrow=c(2,2))
plot(close_1h_r[,"Okex_ETHBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "A: Okex_ETHBTC", xlab="Time", ylab="Hourly Return")
plot(close_1h_r[,"Bithumb_ETHKRW_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "B: Bithumb_ETHKRW", xlab="Time", ylab="Hourly Return")
plot(close_1h_r[,"Binance_ETCBTC_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "C: Binance_ETCBTC", xlab="Time", ylab="Hourly Return")
plot(close_1h_r[,"Binance_BTCUSDT_1h"]~as.Date(date_all,"%Y-%m-%d"),type = "l", main = "D: Binance_BTCUSDT", xlab="Time", ylab="Hourly Return")

par(mfrow=c(2,3))
plot(num_edges_0.2~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "A: λ=0.2",xlab="Date", ylab="Number of Edges")
plot(num_edges_0.3~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "B: λ=0.3",xlab="Date", ylab="Number of Edges")
plot(num_edges_0.4~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "C: λ=0.4",xlab="Date", ylab="Number of Edges")
plot(num_edges_0.5~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "D: λ=0.5",xlab="Date", ylab="Number of Edges")
plot(num_edges_0.6~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "E: λ=0.6",xlab="Date", ylab="Number of Edges")
plot(num_edges_rcor~as.Date(date_1,"%Y-%m-%d"),type = "l",main = "F: Realized Correlation",xlab="Date", ylab="Number of Edges")


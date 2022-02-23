###################################################
## President approval rating
###################################################
## df <- read_excel("president.xlsx")
###################################################
## JAGS
###################################################
require(rjags)
require(rlist)
require(jsonlite)
require(haven) ## for as_factor
require(forcats) ## for fct_reorder
require(tidyquant)
require(ggplot2)
require(dplyr)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

##############################################
## type of approval data 
##############################################

candidate.list <- c("y1", "y2", "y3", "y4")
n.candidate <- length(candidate.list)
candidate.names <- c("good", "bad", "dk", "net_good")
candidate.korea <- c("긍정평가", "부정평가", "모름", "순긍정평가")
tune = 1
sigma.tune <- 1

##############################################
## Loop
##############################################
for(candidate in 1:n.candidate){
    model.name <- paste0("president_approve_", candidate.korea[candidate])
    subdata <- df[!is.na(unlist(df[, candidate.list[candidate]])), ]

    ## house rename
    subdata$house.new <- subdata$house_new
    subdata$house.new <- sapply(subdata$house.new,
                                function(x){as.factor(toString(x))})
    n.house <- length(unique(subdata$house.new))
    house.name <- unique(subdata$house.new)
    house.id <- as.numeric(subdata$house.new)
    house.freq <- table(subdata$house.new) %>% 
        as.data.frame() %>% 
        arrange(house.name)
        
    ## report 
    cat("\t", model.name, " House number is ", n.house, "\n")
     
    subdata$y <- as.numeric(unlist(subdata[, candidate.list[candidate]]))
    subdata$date <- as.Date(as.character(subdata$start.date))
    Date <- sort(unique(subdata$date))
    numeric.date <- as.numeric(as.factor(as.numeric(subdata$date) -
                                     sort(as.numeric(subdata$date))[1] + 1))

    n.period <- length(Date)
    N.obs <- length(subdata$y)
    
    df.jags <- data.frame(y = subdata$y, date = subdata$date,
                          size = subdata$response)
    df.jags$Date <- numeric.date
    df.jags <- df.jags[order(df.jags$Date),]
    df.jags$sd <- sd(df.jags$y)
    df.jags$var <- var(df.jags$y)
    df.jags$lo <- df.jags$y - 1.96*df.jags$sd[1]
    df.jags$up <- df.jags$y + 1.96*df.jags$sd[1]
    mean.y <- sapply(1:n.period, function(i){mean(df.jags$y[df.jags$Date == i])})
    survey.count <- sapply(1:n.period, function(i){length(df.jags$y[df.jags$Date == i])})
    var.y.raw <- (df.jags$y^2)/df.jags$size
    var.y <- sapply(1:n.period, function(j){mean(var.y.raw[df.jags$Date == j], na.rm=TRUE)})
    var.y <-  ifelse(is.na(var.y)|var.y==0, mean(var.y, na.rm=TRUE), var.y)
    sigma.upper <- sd(df.jags$y)/sigma.tune
    ## plot(mean.y)
    a01 <- mean.y[1] - tune*1.96*df.jags$sd[1]
    a02 <- min(100, mean.y[1] + tune*1.96*df.jags$sd[1])

    foo <- list(y=df.jags$y, prec=1/var.y, numeric.date=numeric.date,
                house.id=house.id, N.obs = N.obs, n.period=n.period, n.house=n.house,
                sigma.low = sigma.low, sigma.high = sigma.high, a01 = a01, a02 = a02,
                alpha= rep(NA, n.period), new.alpha = NA)


    cat("\t", model.name, " model is ready to run!\n")
    
    model <- jags.model("mbc_house.bug",
                        data = foo, n.chains = 5, n.adapt=1000)
    
    output1 <- coda.samples(model=model,
                            variable.names=c("alpha","house","sigma"),
                            n.iter=1000, thin=5)
    output <- list.rbind(output1)
    coef.names <- colnames(output)

##############################################
    ## 3. visualization
##############################################
    cat(model.name, " visualization and save !\n")
    
    alphaFirst <- output[,grep("alpha\\[", coef.names)]
    new.alphaFirst  <- output[,grep("new.alpha", coef.names)]
    
    sigmaFirst <- output[,grep("sigma", coef.names)]
    houseFirst <- output[,grep("house", coef.names)]
    alpha.bar <- apply(alphaFirst,2,mean)
    alpha.ci <- apply(alphaFirst, 2, quantile,c(.025,.975))
    new.alphaFirst.bar <- mean(new.alphaFirst)
    new.alphaFirst.ci <- quantile(new.alphaFirst, probs=c(.025,.975))
    houseEffects <- apply(houseFirst,2,mean)

    df.gg <- data.frame(date = Date,
                        mean = ifelse(alpha.bar<0, 0, alpha.bar),
                        lower = ifelse(alpha.ci[1,]<0, 0, alpha.ci[1,]),
                        upper = ifelse(alpha.ci[2,]<0, 0, alpha.ci[2,]))
    df_obs <- data.frame(date = Date, y = alpha.bar)
    df_obs_low <- data.frame(date = Date, y = alpha.ci[1,])
    df_obs_high <- data.frame(date = Date, y = alpha.ci[2,])
    df_month <-  aggregate(df_obs$y, list(format(df_obs$date, "%Y-%m-01")), mean)
    colnames(df_month) <- c("date", "mean")
    df_month$date <- as.Date(df_month$date)
    df_month$low <- aggregate(df_obs_low$y, list(format(df_obs$date, "%Y-%m-01")), mean)$x
    df_month$upper <- aggregate(df_obs_high$y, list(format(df_obs$date, "%Y-%m-01")), mean)$x

    good.news.prob <- mean(new.alphaFirst > 50)
    good.news.ci <- quantile(new.alphaFirst , probs=c(0.05, 0.95))

    G <- 100
    df.pred <- data.frame(date = as.Date(rep(df.gg$date[nrow(df.gg)]+1, 100)),
                          mean = new.alphaFirst[sample(1:nrow(alphaFirst), size=100)])
##############################################
    ## plot 1: trend line
##############################################    
## c("#00AFBB", "#E7B800", "#FC4E07")
    gg.df <- df.gg %>% ggplot(aes(x = date, y = mean)) +
        geom_ribbon(data = df_month, aes(ymin=low,ymax=upper),alpha=0.3) +
        geom_point(data = df_month, 
                   col = "#b2182b", alpha=0.7, size = 4) +
        ## geom_point(alpha = .65, size = 2) +
        geom_point(data =df.pred , 
                   col = "black", alpha=0.2, size = 4) +
        geom_line(data = df_month, 
                  col = "#b2182b", alpha=0.2, size = 1) + 
        theme(panel.grid.minor = element_blank(), legend.position="bottom", 
              plot.subtitle = element_text(size = 20)) + 
        labs(title= paste0("대통령 ", candidate.korea[candidate]),
             subtitle = paste0("(예측 평균: ", round(mean(new.alphaFirst), 2), ", 90% 확률구간: ",
                               round(good.news.ci, 2)[1], ", ", round(good.news.ci, 2)[2], ")"),
             y = "지지율", x="조사시점") + 
        {if(candidate !=4) geom_hline(yintercept=50, color="red", linetype="dotted", size=1) } +
        theme_jhp2()
    ggsave(filename=paste0(model.name, "_monthly_line.pdf"),
           plot=gg.df, width=12, height=7)  
    
##############################################
    ## plot 2: House effects
##############################################    
    houseEffects <- apply(houseFirst,2,mean)
    houseci <- apply(houseFirst,2,quantile,c(.025,.975))
    lower= round(houseci[1,], 2)
    upper= round(houseci[2,], 2)
    names(houseEffects) <- house.name
    houseName <- house.name
    names(houseEffects) <- sort(houseName)
    df.house <- data.frame(name = sub("\\..*", "", house.name),
                           bias = round(houseEffects, 2),
                           lower= round(houseci[1,], 2),
                           upper= round(houseci[2,], 2),
                           sign = ifelse(houseEffects>0&lower>0&upper>0,
                                         "positive",
                                  ifelse(houseEffects<0&lower<0&upper<0, "negative", "mixed")))
    
    df.house <- df.house %>% arrange(bias) 
    g1 <- df.house %>% ## filter(sign == "positive")%>%
        mutate(function_name = as_factor(name) %>% fct_reorder(bias)) %>%
        arrange(desc(function_name)) %>%
        ggplot(aes(x = bias, y = function_name, color = function_name)) +
        geom_segment(aes(xend = 0, yend = function_name), size = 2) +
        geom_point(size = 4) +
        geom_label(aes(label = round(bias, 2)), 
                   hjust = "inward", size = 3.5) +
        expand_limits(x = 0) +
        labs(
            title = paste0("개별 조사기관의 조사결과가 평균으로부터 많이 다른가?"),
            subtitle = paste0(candidate.korea[candidate], ":",
                              range(df.long.full$start.date)[1], "~", range(df.long.full$start.date)[2]),
            ## caption = "Source: SNU IR Data Center",
            y = "지지율", x="조사기관 고정값") + 
        scale_color_tq() +
        theme_jhp() +
        theme(legend.position = "none") 
    png(file=paste0(model.name, "_house_effect.png"),
        family="sans", width=800, height=650)
    g1;
    dev.off()
   
}

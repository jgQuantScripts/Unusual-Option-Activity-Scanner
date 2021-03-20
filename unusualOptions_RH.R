require("RobinHood");require("data.table");require("pbapply")
require("httr");require("dplyr");require("purrr")
PASS <- new.env()
assign("username","USERNAME",envir = PASS)
assign("password","PASSWORD",envir = PASS)
# *************************************************************************************
#                         Function to get Option Chains
# *************************************************************************************
mod_json <- function(x, type) {
  
  if (type == "toJSON") {
    x <- x %>% jsonlite::toJSON()
    x <- substr(x, 2, nchar(x) - 1)
    return(x)
  }
  
  if (type == "fromJSON") {
    x <- jsonlite::fromJSON(rawToChar(x$content))
    return(x)
  }
  
}
# Get option chains from RH
get_rh_options = function(RH, chain_symbol,expiration,type){
  
  # URL and token
  url = paste0("https://api.robinhood.com/options/instruments/",
               "?state=active",
               "&type=", type,
               "&chain_symbol=", chain_symbol,
               "&expiration_dates=", expiration)
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET data
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # retrives all options for that expiration
  ops = lapply(as.list(1:nrow(dta)), function(ii){
    tmpURL = paste0("https://api.robinhood.com/marketdata/options/",dta$id[ii],"/")
    # GET 
    dta2 <- GET(tmpURL,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token))
    # format return
    dta2 <- try(mod_json(dta2, "fromJSON"), silent = TRUE)
    if(!inherits(dta2,'try-error'))
    {
      dta2 = as.data.frame(t(do.call(rbind, lapply(dta2, as.data.frame))))
      dta2$strike_price <- as.numeric(dta$strike_price[ii])
      dta2$type <- dta$type[ii]
    }else{
      dta2 <- NULL
    }
    dta2
  })
  
  ops = rbindlist(ops,use.names = TRUE, fill=TRUE)
  ops$expiration_date = expiration
  ops$access_date = as.character(Sys.Date())
  
  # convert to numeric columns
  ops$adjusted_mark_price       = ops$adjusted_mark_price %>% as.numeric 
  ops$ask_price                 = ops$ask_price %>% as.numeric 
  ops$ask_size                  = ops$ask_size %>% as.numeric 
  ops$bid_price                 = ops$bid_price %>% as.numeric 
  ops$bid_size                  = ops$bid_size %>% as.numeric 
  ops$break_even_price          = ops$break_even_price %>% as.numeric 
  ops$last_trade_price          = ops$last_trade_price %>% as.numeric 
  ops$last_trade_size           = ops$last_trade_size %>% as.numeric 
  ops$mark_price                = ops$mark_price %>% as.numeric 
  ops$open_interest             = ops$open_interest %>% as.numeric 
  ops$previous_close_price      = ops$previous_close_price %>% as.numeric 
  ops$volume                    = ops$volume %>% as.numeric 
  ops$chance_of_profit_long     = ops$chance_of_profit_long %>% as.numeric 
  ops$chance_of_profit_short    = ops$chance_of_profit_short %>% as.numeric 
  ops$delta                     = ops$delta %>% as.numeric 
  ops$gamma                     = ops$gamma %>% as.numeric 
  ops$implied_volatility        = ops$implied_volatility %>% as.numeric 
  ops$rho                       = ops$rho %>% as.numeric 
  ops$theta                     = ops$theta %>% as.numeric 
  ops$vega                      = ops$vega %>% as.numeric 
  ops$high_fill_rate_buy_price  = ops$high_fill_rate_buy_price %>% as.numeric 
  ops$high_fill_rate_sell_price = ops$high_fill_rate_sell_price %>% as.numeric 
  ops$low_fill_rate_buy_price   = ops$low_fill_rate_buy_price %>% as.numeric 
  ops$low_fill_rate_sell_price  = ops$low_fill_rate_sell_price %>% as.numeric 
  ops$strike_price              = ops$strike_price %>% as.numeric 
  ops$high_price                = ops$high_price %>% as.numeric 
  ops$low_price                 = ops$low_price %>% as.numeric 
  
  # return data
  ops
}
### DETERMINE TRADING DAY: (local time)
DAYTODAY = function()
{
  NOW <- Sys.time()
  # if the time now is past the market close but less than midnight -> trading day will be the next day
  if(NOW > as.POSIXct(paste0(Sys.Date(), " 16:00:00")) & NOW < as.POSIXct(paste0(Sys.Date(), " 23:59:59")))
  {
    daytoday <- format(Sys.Date()+1, "%Y%m%d")
  }else{
    # otherwise TODAY is the trading day 
    daytoday <- format(Sys.Date(), "%Y%m%d")
  }
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(xx){
  ttt <- TMZ[xx] - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",TMZ[xx]));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",TMZ[xx]));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",TMZ[xx]));cat("\n")
    Sys.sleep(tt)
  }  
}
## Create sequence of times
getTMZ = function(TF)
{
  # time difference between current time zone and NY-time
  tmDIFF = round(as.numeric(difftime(Sys.time(),lubridate::force_tz(with_tz(Sys.time(),"EST")),
                                     units = "hours")),0)
  if(TF < 240)
  {
    # determines the trading day to start
    daytoday <- DAYTODAY()
    # IT WILL MAKE A DECISION AT THE CLOSE OF THE FIRST BAR! 
    START <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 09:30:00"))  # NY TIME
    START <- START + minutes(TF)                                                 # Adjust for Bar Time
    START <- START + hours(tmDIFF)                                               # local Time
    END <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 17:00:00"))    # NY TIME
    END <- END + hours(tmDIFF)                                                   # local Time
    # the following line will determine the start times for the algo
    TMZ <- seq(START,END, by=paste0("",TF," min"))
    # MAKE A DECISION RIGHT BEFORE THE CLOSE 
    lastBAR = as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 15:59:55")) # NY TIME - 5 secs before close
    TMZ[(length(TMZ)-1)] <- lastBAR + hours(tmDIFF)
    # ALGO STOP TIME
    stopALGO = as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:01:00")) # NY TIME - 1 min after close
    TMZ[(length(TMZ))] <- stopALGO + hours(tmDIFF)
  }
  if(TF==240)
  {
    # determines the trading day to start
    daytoday <- DAYTODAY()
    # IT WILL MAKE A DECISION AT THE CLOSE OF THE FIRST BAR! 
    START <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 13:00:00")) # NY TIME
    START <- START + hours(tmDIFF)                                               # local Time
    MID   <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:00:00")) # NY TIME
    MID <- MID + hours(tmDIFF)                                               # local Time
    END <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 17:00:00"))   # NY TIME
    END <- END + hours(tmDIFF)                                               # local Time
    # TMZ
    TMZ = c(START,MID,END)
    # MAKE A DECISION RIGHT BEFORE THE CLOSE 
    TMZ[(length(TMZ)-1)] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 15:59:50"))+ hours(tmDIFF) 
    # ALGO STOP TIME
    TMZ[(length(TMZ))] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:01:00")) + hours(tmDIFF) 
  }
  TMZ
}
# *************************************************************************************
#                          VARIABLE DECLARATION
# *************************************************************************************
# tickers to call
tickers = c("AMC","ARKK","VXX")
# Assign expiration
EXP = "2021-03-19"
# get time sequences for algo to start
TMZ = getTMZ(TF=5)
# get future times only
TMZ <- TMZ[TMZ>Sys.time()]
# ***********************************************************************************************
#                         GET UNUSUAL ACTIVITY START
# ***********************************************************************************************
SLEEEP(1)
SCAN <- pblapply(as.list(2:length(TMZ)), function(xx){
  # establish RH connection
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # get options - USE NEXT AVAILABLE EXPIRATION
  ops = lapply(as.list(tickers), function(tic){
    calls  = get_rh_options(RH=RH, chain_symbol = tic,expiration = EXP, type = "call")  
    puts   = get_rh_options(RH=RH, chain_symbol = tic,expiration = EXP, type = "put")
    rbind(calls,puts)
  })
  # logout
  logout(RH)
  # row bind options
  ops = rbindlist(ops,use.names = TRUE, fill = TRUE)
  
  # remove strikes with ZERO open interest 
  ops = subset(ops, ops$open_interest > 0)
  
  # calculate Unusual Options Activity
  ops$vol2OI = ops$volume/ops$open_interest  
  
  # order by decreasing vol2OI
  ops = ops[order(ops$vol2OI, decreasing = TRUE),]
  
  # output to console
  lmt = ops[,c("symbol","type","strike_price","volume","open_interest","vol2OI")]
  # add time column
  lmt = cbind(lmt,TMZ[xx])
  # print to console
  cat("\n\n\n")
  print(head(lmt,5))
  cat("\n\n\n")
  
  # **********************************************************************************************
  # Sleep until the next bar
  SLEEEP(xx)
})



library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

workingDirectory = getwd()
dataPath = "/../Data"
transactionsFile = "/transactionsJan2022.csv"

df<-read_csv(paste(workingDirectory, dataPath, transactionsFile, sep=""))

gasFile <- "/avg_gas.csv"

gas <- read_csv(paste(workingDirectory, dataPath, gasFile, sep=""))

aavePriceFile <- "/hourly_prices.csv"

aavePrices <- read_csv(paste(workingDirectory, dataPath, aavePriceFile, sep=""))

aavePrices <- aavePrices %>%
  dplyr::rename(hour = HOUR, priceUSD = PRICE) %>%
  select(hour, priceUSD)

## Helper functions
not_all_na <- function(x) any(!is.na(x))

activeCollateral <- function(usr, ts, collaterals) {
  userCollateral <- collaterals %>%
    filter(user == usr, timestamp <= ts) %>%
    group_by(reserve) %>%
    slice_max(timestamp) %>%
    mutate(enabledForCollateral = toState) %>%
    select(user, enabledForCollateral) %>%
    filter(enabledForCollateral == TRUE)
  return(userCollateral)
}

activeCollateralTypes <- function(usr, ts, collaterals, reserveTypes) {
  userCollateral <- collaterals %>%
    filter(user == usr, timestamp <= ts) %>%
    group_by(reserve) %>%
    slice_max(timestamp) %>%
    ungroup() %>%
    filter(toState == TRUE) %>%
    select(user, reserve) %>%
    left_join(reserveTypes, by = "reserve") %>%
    select(user, reserveType) %>%
    distinct()
  
  return(case_when(length(userCollateral$reserveType) == 2 ~ "Mixed",
                   "Stable" %in% userCollateral$reserveType ~ "Stable",
                   "Non-Stable" %in% userCollateral$reserveType ~ "Non-Stable",
                   TRUE ~ "None"))
}

## Add the gas fees to the transaction data
df <- df %>%
  mutate(Date = floor_date(as_datetime(timestamp), unit = "hour")) %>%
  left_join(gas, by = "Date") %>%
  dplyr::rename(gasFee = `Fee (USD)`)


## Create the basic dataframes for each transaction type:
borrows <- df %>%
  filter(type=="borrow") %>%
  select(where(not_all_na))

repays <- df %>%
  filter(type == "repay") %>%
  select(where(not_all_na))

deposits <- df %>%
  filter(type == "deposit") %>%
  select(where(not_all_na))

redeems <- df %>%
  filter(type == "redeem") %>%
  select(where(not_all_na))

swaps <- df %>%
  filter(type == "swap") %>%
  select(where(not_all_na))

collaterals <- df %>%
  filter(type == "collateral") %>%
  select(where(not_all_na))

liquidations <- df %>%
  filter(type == "liquidation") %>%
  select(where(not_all_na))

liquidationsPerformed <- liquidations %>%
  mutate(liquidatee = user) %>%
  mutate(liquidatee_alias = user_alias) %>%
  mutate(user = liquidator) %>%
  mutate(user_alias = liquidator_alias)

## Create some helpful, smaller dataframes that can easily be queried to find some useful info
stableCoins <- df %>%
  select(reserve, stableCoin) %>%
  distinct() %>%
  filter(stableCoin == TRUE) %>%
  select(reserve)

nonStableCoins <- df %>%
  select(reserve, stableCoin) %>%
  distinct() %>%
  filter(stableCoin == FALSE) %>%
  select(reserve)

reserveTypes <- df %>%
  select(reserve, stableCoin) %>%
  distinct() %>%
  mutate(reserveType = if_else(stableCoin == TRUE, "Stable", "Non-Stable")) %>%
  select(reserve, reserveType) %>%
  drop_na()

## Calculate user balances for relevant transaction types (not taking interest into account)

cumulativeDeposits <- deposits %>%
  mutate(user = onBehalfOf, user_alias = onBehalfOf_alias) %>%
  dplyr::group_by(user, reserve) %>%
  arrange(timestamp) %>%
  dplyr::summarise(user_alias, reserve, totalDeposited = sum(amountUSD)) %>%
  distinct()

cumulativeRedeems <- redeems %>%
  mutate(uner = onBehalfOf, user_alias = onBehalfOf_alias) %>%
  group_by(user, reserve) %>%
  dplyr::summarise(user_alias, reserve, totalRedeems = sum(amountUSD)) %>%
  distinct()

cumulativeBalances <- cumulativeDeposits %>%
  left_join(cumulativeRedeems, by = c("user", "reserve")) %>%
  mutate(cumulativeBalances = totalDeposited - totalRedeems)


# Compute aggregate liquidations

df2 <- left_join(df, reserveTypes, by="reserve") %>%
  distinct()

numLiqPerUser <- liquidations %>%
  group_by(user) %>%
  dplyr::summarise(numLiquidations = n())


aggregateLiquidations <- df2 %>%
  filter(user %in% numLiqPerUser$user) %>% # First, let's filter out all users who have never been liquidated.
  group_by(user) %>%                       # The next set of logic is to sort users' transactions by timestamp and pull out all liquidations that are
  arrange(timestamp) %>%                   # part of a consecutive set of liquidations.
  mutate(nextTransaction = lead(type)) %>%
  mutate(prevTransaction = lag(type)) %>%
  filter(type == "liquidation" & (nextTransaction == "liquidation" | prevTransaction == "liquidation"))  %>%
  mutate(liquidationDay = floor_date(as_datetime(timestamp), unit = "day")) %>% # Then we want to use some approximation for the timeframe of this liquidation event, so we naively group consecutive liquidations by the day on which they took place.
  group_by(user,liquidationDay) %>% # Doing this means that we can group by user and liquidationDay, which is functionally grouping by "liquidation event"
  mutate(liquidationDuration = max(timestamp) - min(timestamp)) %>% # Now we can compute some basic stats about the event.
  mutate(liquidationStart = min(timestamp), liquidationEnd = max(timestamp)) %>%
  mutate(liquidationStartDatetime = as_datetime(liquidationStart), liquidationEndDatetime = as_datetime(liquidationEnd)) %>%
  mutate(reserve = collateralReserve) %>%
  left_join(reserveTypes, by = "reserve") %>%
  dplyr::rename(collateralType = reserveType.y) %>%
  mutate(reserve = principalReserve) %>%
  left_join(reserveTypes, by = "reserve") %>%
  dplyr::rename(principalType = reserveType) %>%
  mutate(totalCollateralUSD = sum(amountUSDCollateral), totalPrincipalUSD = sum(amountUSDPrincipal))%>%
  dplyr::mutate(numLiquidations = n()) %>%
  dplyr::summarise(user_alias, numLiquidations, liquidationDuration, liquidationStart, liquidationEnd, liquidationStartDatetime, liquidationEndDatetime,
            collateralReserves = str_flatten(str_sort(unique(collateralReserve)), collapse = ","), 
            collateralTypes = str_flatten(str_sort(unique(collateralType)), collapse= ","),
            principalReserves = str_flatten(str_sort(unique(principalReserve)), collapse = ","),
            principalTypes = str_flatten(str_sort(unique(principalType)), collapse = ","),
            totalCollateralUSD, totalPrincipalUSD, liquidationType = str_c(principalTypes, collateralTypes, sep = ":")) %>%
  distinct()

rm(df2)

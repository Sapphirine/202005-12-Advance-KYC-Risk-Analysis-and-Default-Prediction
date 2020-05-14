library(randomForest)
library(dplyr)
library(tidyr)
library(caret)

data = read.csv("financial_data.csv")
drop = c('name', 'adsh', 'form', 'period', 'qtrs', 
         'abstract', 'datatype', 'instance', 'Ticker', 'fy', 'fp', 
         'uom', 'iprx', 'Name', 'country', 'S.P.Rating', 'cityba', 'custom',
         'sales.2', 'ebitda', 'tang.assets', 'cash', 'ltm.ebitda.gr', 'ebit',
         'tang.eq', 'value', 'sales.gr', 'net.income', 'debt', 'cfo/interest',
         'ebit/int', 'ebitda/int.1', 'net.income', 'cfo', 'capex', 'debt/t. bv',
         'industry')
select_data = data[,!(names(data) %in% drop)]

select_data[select_data == '#VALUE!'] = NA
select_data[select_data == 'NA'] = NA
select_data[select_data == '#DIV/0!'] = NA
# select_data[,c(1, 3:ncol(select_data))] <- as.numeric(str(select_data[,c(1, 3:ncol(select_data))]))

select_data = select_data[!is.na(select_data$Mkt.cap),]
select_data = select_data[!is.na(select_data$countryba),]

# select_data$countryba[select_data$countryba != 'US'] = 'other'

# select_data[,c(1, 3:ncol(select_data))] <- as.numeric(as.character(select_data[,c(1, 3:ncol(select_data))]))
# select_data = sapply(select_data, function(x)ifelse(is.na(x), mean(x, na.rm=TRUE), x))
select_data = data.frame(select_data)

for(i in c(1, 3:ncol(select_data))){
  select_data[,i] <- as.numeric(as.character(select_data[,i]))
}

for(i in 1:ncol(data)){
  select_data[,i][is.na(select_data[,i])] <- mean(select_data[,i], na.rm = TRUE)
}

country_list = list(select_data$countryba)[[1]]
for (i in 1:length(country_list)){
  if(country_list[i] != 'US'){
    country_list[i] = NA
  }
}


country_list = factor(country_list)

addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

country_list <- addLevel(country_list, "other")

for (i in 1:length(country_list)){
  if(is.na(country_list[i])){
    country_list[i] = 'other'
  }
}

select_data = select_data[,!names(select_data) == 'countryba']
select_data$countryba = country_list

# train-test split
set.seed(2383)
size = floor(0.75 * nrow(select_data))
train_index <- sample(seq_len(nrow(select_data)), size = size)
train = select_data[train_index,]
train_X = train %>% select(-c('Mkt.cap'))
test = select_data[-train_index,]
test_X = test %>% select(-c('Mkt.cap'))

rf = randomForest(
  Mkt.cap ~ .,
  data=train
)

save(rf, file = 'rf.rda')


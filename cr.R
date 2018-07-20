library(tidyverse)
library(xgboost)
train <- read_csv("train.csv")
test <- read_csv("test.csv")
sample <- read_csv("sample_submission.csv")
Id <- test[,1]

# Data input --------------------------------------------------------------

label <- train$Target
all <- rbind(select(train,c(-Id,-Target)),select(test,-Id), stringsAsFactors = F)
all <- all %>% select(-idhogar)

# NA handling -------------------------------------------------------------

#check which columns have na
names(all)[sapply(all, function(y) sum(length(which(is.na(y)))))!=0]

#set na to zero in v18q1 because that means 0 tablets
all$v18q1[is.na(all$v18q1)] <- 0

#set na to zero in educ fow now
all$SQBmeaned[is.na(all$SQBmeaned)] <- 0
all$meaneduc[is.na(all$meaneduc)] <- 0

# assume default is 0 years behind in school
all$rez_esc[is.na(all$rez_esc)] <- 0

#set rent to median if unknown for now.
all$v2a1[is.na(all$v2a1)] <- median(all$v2a1, na.rm = T)

#are there any nonnumeric columns, need to fix before xgboost
names(all)[!unlist(lapply(all, is.numeric))]

all$dependency[all$dependency=="yes"] <- 1
all$dependency[all$dependency=="no"] <- 0
all$dependency <- as.numeric(all$dependency)

all$edjefe[all$edjefe=="yes"] <- 1
all$edjefe[all$edjefe=="no"] <- 0
all$edjefe <- as.numeric(all$edjefe)

all$edjefa[all$edjefa=="yes"] <- 1
all$edjefa[all$edjefa=="no"] <- 0
all$edjefa <- as.numeric(all$edjefa)



#reset train and test to original size
train <- all[1:length(label),]
test <- all[(length(label)+1):nrow(all),]

# setup data for xgb
dtrain <- xgb.DMatrix(as.matrix(train), label = as.integer(label-1))
dtest<- xgb.DMatrix(as.matrix(test))

#xgboost model the data
params = list(objective = 'multi:softmax',
              num_class = 4,
              eta = 0.1)
xgb <- xgb.train(data = dtrain,
               params = params,
               nfold = 10,
               nround = 20)

target <- predict(xgb, dtest)


sub <- data.frame(Id = Id,target = target+1)


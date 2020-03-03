# Preprocess data from beginning
train.df = read.csv('preprocessing1/train.csv')
test.df = read.csv('preprocessing1/test.csv')


# Drop ID
train.df = subset(train.df, select = -Id)
test.df = subset(test.df, select = -Id)


# If a single type takes up more than 80% of an attribute, drop it
cols = colnames(train.df)
dominated.cols = c()

for (col in cols){
  if(class(train.df[[col]])=='factor'){
    x = summary(train.df[[col]])
    max.percent = x[which.max(x)] / dim(train.df)[1]
    if(max.percent > 0.8){  # if greater than 80%
      dominated.cols = append(dominated.cols,col)
      cat(col, 'dominant attribute takes ', max.percent*100, '%\n')
    }
  }
}

train.df = train.df[,!names(train.df)%in%dominated.cols, drop=F]
test.df = test.df[,!names(test.df)%in%dominated.cols, drop=F]

summary(train.df)


# Fill NAs with mode of each column

# Drop FireplaceQu, which has 690 NA, fill.na.with.mode has not effect on it
train.df = subset(train.df, select = -FireplaceQu)
test.df = subset(test.df, select = -FireplaceQu)

fill.na.with.mode = function(df){
  cols = colnames(df)
  for (col in cols){
    if(class(df[[col]])=='factor'){
      x = summary(df[[col]])
      col.mode = names(x[which.max(x)])
      df[[col]][  is.na(df[[col]]) ] = col.mode
    }
    else{
      df[[col]][  is.na(df[[col]]) ] = 0
    }
  }
  return (df)
}

train.df= fill.na.with.mode(train.df)
test.df = fill.na.with.mode(test.df)


# If a single type takes less than 7% percent, then change it to the mode of column
# I chose this number because, 70% is split as training and 10-fold CV
# To avoid all weak levels squeeze into the 1/10 valiadation part

replace.low.vals.with.mode = function(df){
  cols = colnames(df)
  for (col in cols){
    if(class(df[[col]])=='factor'){
      x = summary(df[[col]])
      col.mode = names(x[which.max(x)])
      for (i in c(1:length(x))){
        if(x[[i]] < 0.07*dim(df)[1]){ # if less than 7% thres
          df[[col]][  df[[col]]==names(x[i]) ] = col.mode  
        }
      }
    }
  }
  return (df)
}


train.df1 = replace.low.vals.with.mode(train.df)
test.df1 = replace.low.vals.with.mode(test.df)
summary(train.df1)

write.csv(train.df1,'train_2.csv',row.names = F)
write.csv(test.df1, 'test_2.csv', row.names = F)




df <- data.frame(key=c('1', '2', '3', '4', '5'),
                 name1=c('black','black','black','red','red'),
                 type1=c('chair','chair','sofa','sofa','plate'),
                 num1=c(4,5,12,4,3),
                 name2=c('black', 'red', 'black', 'green', 'blue'),
                 type2=c('chair','chair','sofa','bed','plate'),
                 num2=c(4,7,12,3,1),
                 name3=c('blue', 'green', 'black', 'blue', 'blue'),
                 type3=c('couch','chair','sofa','plate','plate'),
                 num3=c(12,8,12,4,1))



group_len <- 3
groups <- split(2:ncol(df), cut(2:ncol(df), 7))

stacked.df <- do.call(rbind, lapply(groups, function(cols) {
  group <- df[ , c(1, cols)]  
  names(group) <- c("key", "name", "type", "num")
  group
}))

df2 <- group_by(stacked.df, key, name, type, num) %>% 
  summarise(dupes = n() > 1, num_dupes = n())

?split
?cut
?lapply

patt <- c("test","10 Barrel")
lut  <- c("1 Barrel","10 Barrel Brewing","Harpoon 100 Barrel Series","resr","rest","tesr")


test_df <- data.frame(name=c('nancy j hill', 'nancy hill', 'jane smith', 'jane smithe', 'jane smyth' ),
                      email=c('big@addr.com', 'big@addr.com', 'small@addr.com', 'small@addr.com', 'small@addr.com'),
                      addr1=c('123 main st', '1234 main st', '742 evergreen terrace', '42 evergreen terrace', '742 evergreen terrace 42'),
                      addr2=c('13 main st', '12 main st', '742 evergren terrace', '42 evergreen terr', '742 evergreen terrace 4')
                      )

test_df$check1 <- lapply(test_df$email, agrep, x=c(test_df$addr1, test_df$addr2), max.distance=1, value = TRUE)
test_df$check2 <- lapply(test_df$email, agrep, x=c(test_df$addr1, test_df$addr2), max.distance=2, value = TRUE)
test_df$check3 <- lapply(test_df$email, agrep, x=c(test_df$addr1, test_df$addr2), max.distance=3, value = TRUE)


patt <- c("test","10 Barrel")
lut  <- c("1 Barrel","10 Barrel Brewing","Harpoon 100 Barrel Series","resr","rest","tesr")

for (i in 1:length(patt)) {
  print(agrep(patt[i],lut,max=2,v=T))
}

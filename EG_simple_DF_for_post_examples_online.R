# Create simple dataframe for examples on stackoverwank, etc
# Creates 3x5 dataframe 3col = x,n,y , 5rows = numbers inside each ( )
# Take copy for own needs
# mydata <- data.frame(x = c(20,35,45,55,70), n = rep(50,5), y = c(6,17,26,37,44))

eg_data <- data.frame(
addr1 = c('123 Main St','742 Evergreen Ter','8435 Roanoke Dr','1340 N State Pkwy') , 
addr2 = c('123 Main St Apt 4','742 Evergreen Terrace','8435 Roanoke Dr Unit 5','1340 N State Pkwy') , 
addr3 = c('123 Main St','742 Evergreen Ter #4','8435 Roanoke Dr Apt 5','1340 N State Pkwy') ,
match1_2 = c('0', '0', '0','1') ,
diff1_2 = c(' Apt 4', 'race', ' Unit 5', NA) ,
match1_3 = c('1', '0', '0','1') ,
diff1_3 = c(NA, ' #4', ' Apt 5', NA))

eg_data$addr_comp1 <- mapply(charmatch, eg_data$addr1, eg_data$addr2)
eg_data$addr_comp2 <- mapply(charmatch, eg_data$addr2, eg_data$addr1)


eg_data <- data.frame(
  addr1 = c('123 Main St','742 Evergreen Ter','8435 Roanoke Dr','1340 N State Pkwy') , 
  addr2 = c('123 Main St Apt 4','742 Evergreen Terrace','8435 Roanoke Dr Unit 5','1340 N State Pkwy'), stringsAsFactors = FALSE)
eg_data$addr_comp1_2 <- mapply(vsetdiff, strsplit(eg_data$addr1, split=""), strsplit(eg_data$addr2, split=""), SIMPLIFY = TRUE)
eg_data$addr_comp2_1 <- mapply(vsetdiff, strsplit(eg_data$addr2, split=""), strsplit(eg_data$addr1, split=""), SIMPLIFY = TRUE)
eg_data$addr_comp2_2 <- lapply(eg_data$addr_comp2_1, paste, collapse = '')
eg_data$addr_comp2_3 <- as.character(eg_data$addr_comp2_2)


eg_data$fix <- paste(eg_data$addr_comp2_1, collapse=', ')
eg_data$fix2 <- str_c(eg_data$addr_comp2_1, collapse=',')
eg_data$fix3 <- as.factor(eg_data$addr_comp2_1)
eg_data$fix4 <- lapply(eg_data$addr_comp2_1, unlist)
eg_data$fix5 <- (matrix(unlist(eg_data$addr_comp2_1), nrow=4, byrow=F))
eg_data$fix5 <- (matrix(unlist(eg_data$addr_comp2_1), nrow=4, byrow=F))
eg_data$fix6 <- unlist(eg_data$addr_comp2_1, use.names=FALSE, recursive=FALSE)
eg_data$fix7 <- matrix(unlist(eg_data$addr_comp2_1), ncol=1, byrow=FALSE)

z <- list(1:3,4:6,7:9)
z$two <- do.call(rbind,z)


eg_data$fix7 <- matrix(unlist(eg_data$addr_comp2_1), ncol=1, byrow=FALSE)

eg_data$addr12 <- strsplit(eg_data$addr1, split="")
eg_data$addr13 <- paste(eg_data$addr12, sep="")
vsetdiff(eg_data$addr1, eg_data$addr2)
vsetdiff(eg_data$addr2, eg_data$addr1)

myList <- list('A'=1, 'B'=2, 'C'=3)

eg_data$fix8 <- toString(eg_data$addr_comp2_1)

install.packages("vecsets")
library(vecsets)
eg_data$addr_comp1_2 <- mapply(charmatch, eg_data$addr1, eg_data$addr2)
eg_data$addr_comp2_1 <- mapply(charmatch, eg_data$addr2, eg_data$addr1)
eg_data$diff1_2 = c(NA, NA, NA, NA)
eg_data$diff2_1 = c(' Apt 4', 'race', ' Unit 5', NA)



eg_data <- data.frame(ID = c(1,1,1,2,2,3,3,4,4), place = c('green','blue','green','red','black','pink','pink','blue','red'))
eg_data$place <- as.character(eg_data$place)

eg_data$dupes2 <- duplicated(eg_data, (eg_data$ID & eg_data$place), incomparables = FALSE)

eg_data$dupes <- duplicated(eg_data[,(1:2)])


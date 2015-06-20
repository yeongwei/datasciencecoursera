# Empty data.frame
emptyDf <- data.frame()
emptyDf

# JOINING 2 data.frames (LEFT + RIGHT)
df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
df2 <- data.frame(x = c(4, 5, 6), y = c("d", "e", "f")) 
data.frame(df, df2)

# UNION 2 data.frames
rbind(df, df2)

# CONSOLIDATE n data.frames
emptyDf <- data.frame(emptyDf, df) # THIS WONT WORK!!!

# Set value in data.frames based on specific column conditions
df$x[df$x == 1] <- 100
df

# Replace punctuation
sampleTxt <- "AB!@#$%^&*()XZ"
gsub("[[:punct:]]", ".", sampleTxt)

sampleTxt <- "AB()"
gsub("\\(\\)$", "", sampleTxt)


# plyr
library(plyr)
?ddply

# aggregate
df3 <- data.frame(
  a = c("a", "a", "b", "c", "d"),
  b = c(1, 1, 1, 2, 2),
  c = c(10, 10, 10, 10, 10))

?aggregate
aggregate(df3[, 3], by = list(df3$a, df3$b), FUN = sum, na.rm = TRUE)

# date/time
class(Sys.time())
format()
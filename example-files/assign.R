

assign <- read.csv('assign.csv', stringsAsFactors = F)

descriptor <- assign[which(assign$type=="descriptor"),]$RENAME
outcome <- assign[which(assign$type=="outcome"),]$RENAME

df <- matrix(rep(NA, 7*12), nrow = 12)

df <- sapply(1:12, function(x) df[x, ] = c(sample(descriptor, 4, replace=F), sample(outcome, 3, replace=F)))
df <- t(df)

df <- data.frame(df)

names(df) <- c("descriptor1", "descriptor2", "descriptor3", "descriptor4", "outcome1", "outcome2", "outcome3")

write.csv(df, "week1-assign.csv")

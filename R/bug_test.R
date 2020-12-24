library(ggradar2)

sample = readRDS(paste0(getwd(),"/dfsample"))
df_habibi = head(sample, 4)[,c(1,3,4,5,6)]
ggradar2::ggradar2(sample, multiplots = TRUE)

sample$group = str(c(1:nrow(sample)))


data(mtcars)
# Extract the group names. Otherwise, the first column will be chosen as the group names.
group = row.names(mtcars)
df = cbind(group,mtcars)
# The radar chart is not a nice presentation if you want to compare too many groups. Thus here
# we only focus on 4 groups.
dftest = head(df,4)
# To better distinguish two different styles, 6 groups are selected for illustration.
dftest = dftest[,1:7]

# Extract 3 brands of cars out of the data frame
facettest <- df[c(1,2,4,5,8:14),]
# Set the subgroup names
facet1 <- mapply(rep,c('Mazda','Hornet','Merc'),c(2,2,7))
facet1 <- Reduce(c,facet1)
facettest <- cbind(facettest,facet1)

ggradar2(facettest,multiplots = TRUE)

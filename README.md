# ggradar2
ggradar2 is a gg-function to draw a radar plot for data analysis. It is based on [ggradar](https://github.com/ricardo-bion/ggradar) by Ricardo Bion and [CreateRadialPlot](http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html) by Paul Williamson. It has been extended with more features and functionalities. 

# Install ggradar2
Run the code with package `devtools` installed.

```R
devtools::install_github("xl0418/ggradar2",dependencies=TRUE)
```

# Update 13th June 2022

## Draw the confidence interval (error bar)

A new feature is implemented in the plot function. Now, one can plot the confidence interval on the radar plot by providing `ci = ci.df`

```R
library(ggradar2)
library(ggplot2)
data(mtcars)
mtcars.sub <- subset(mtcars,select = c("mpg", "cyl", "disp", "hp", "drat"))[1:4,]

# Generate the upper boundary and the lower boundary of the confidence interval.
# Note that the "type"" column indiactes the boundary types.

ci.h <- apply(mtcars.sub, 2, function(x) x + abs(rnorm(length(x), 0, x/10)))
ci.h <- as.data.frame(ci.h)
ci.h$type <- "h"
ci.l <- apply(mtcars.sub, 2, function(x) x - abs(rnorm(length(x), 0, x/10)))
ci.l <- as.data.frame(ci.l)
ci.l$type <- "l"

ci.df <- rbind(ci.h, ci.l)

ggradar2::ggradar2(mtcars.sub, ci = ci.df, group.line.width = 1, group.point.size = 2)

```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/plot_radar2_2.png)

Now, this feature is only valid for single radar plotting not for multi-radars yet.

## The customized scales

The scales of the grid lines can be customized now. 

For example, one can set 10 grid lines.

```R
ggradar2::ggradar2(mtcars.sub, ci = ci.df, gridline.label = seq(0, 100, 10),group.line.width = 1, group.point.size = 2)
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/plot_3.png)


More features are coming soon.


# Use ggradar2
## Load data.

```R
library(ggradar2)
data(mtcars)
# Extract the group names. Otherwise, the first column will be chosen as the group names.
group = row.names(mtcars)
df = cbind(group,mtcars)
# The radar chart is not a nice presentation if you want to compare too many groups. Thus here 
# we only focus on 4 groups.
dftest = head(df,4)
# To better distinguish two different styles, 6 groups are selected for illustration.
dftest = dftest[,1:7]
```

## Help document
The interpretation of the arguments are listed [here](https://xl0418.github.io/2018/12/07/2018-12-07-ggradar2helpdocument/).

Note that some arguments may be depreciated. 

## Radar charts gallery
### Default style
By default

```R
ggradar2(dftest)
```

returns 

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/roundfill.png)

### No fill with round grid
If you don't want to fill the polygon, run

```R
ggradar2(dftest,polygonfill = FALSE)
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/roundnofill.png)

### Web type

#### Depreciated. Replaced by the customized scales `gridline.label = seq(0, 100, 10)`

A new web type 'lux' has been added by '''webtype'''

mini type

```R
ggradar2(dftest,webtype = 'mini')
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/mini.png)

luxurious type

```R
ggradar2(dftest,webtype = 'lux')
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/lux.png)


### Gird line trend
Use `grid.line.trend = 'increase'` to plot an outward-increasing grid lines.

```R
ggradar2(dftest,style = 'sharp',webtype = 'lux',
              group.line.width = 0.5,grid.line.trend = 'increase',gridline.min.linetype = 'solid',
              gridline.max.linetype = 'solid',gridline.min.colour = 'black',gridline.max.colour='black')
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/trend.png)


### Full score
Use `fullscore = c(...)` to set the full score to each variable.

```R
fullscore <- c(100,10,300,150,10,10)
a <- ggradar2(dftest,fullscore = fullscore)
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/fullscore.png)




### Sharp grid
A new style has been added. Call out the straight line style by running 

```R
ggradar2(dftest,style = 'sharp')
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/straightfill.png)

### Sharp grid without fill
Get rid of the fill

```R
ggradar2(dftest,style = 'sharp',polygonfill = FALSE)
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/straightnofill.png)

### Removing the legend 

```R
ggradar2(dftest,style = 'sharp',polygonfill = FALSE,plot.legend = FALSE)
```

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/nolegend.png)

### Multiple plots by subgroups

```R
# Extract 3 brands of cars out of the data frame
facettest <- df[c(1,2,4,5,8:14),]
# Set the subgroup names
facet1 <- mapply(rep,c('Mazda','Hornet','Merc'),c(2,2,7))
facet1 <- Reduce(c,facet1)
facettest <- cbind(facettest,facet1)

ggradar2(facettest,multiplots = TRUE)
```
Notice that the column name for the subgroups should be 'facet1'. Otherwise, ggradar2 could not recognize it.

![default](https://github.com/xl0418/ggradar2gallery/blob/master/README_figures/multipleplotsbig.png)


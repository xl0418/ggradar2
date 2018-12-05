ggradar2 <- function(plot.data,
                    base.size=15,
                    webtype = 'mini',
                    axis.labels="",
                    grid.min=0,  #10,
                    grid.max=1,  #100,
                    centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                    plot.extent.x.sf=1,
                    plot.extent.y.sf=1.2,
                    x.centre.range=0.02*(grid.max-centre.y),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=6,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.min=TRUE,
                    label.gridline.mid=TRUE,
                    label.gridline.max=TRUE,
                    axis.label.offset=1.15,
                    axis.label.size=5,
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=3,
                    group.colours=NULL,
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    legend.title="",
                    plot.legend=TRUE,
                    plot.title="",
                    legend.text.size=14,
                    style = "round",
                    polygonfill = TRUE,
                    polygonfill.transparency = 0.2,
                    multiplots = 'none',
                    pannelbackground = TRUE) {

  library(ggplot2)

  # Check if subgroup is given and load the data
  if(multiplots == '1D'){
    if(length(which(colnames(plot.data) == 'facet1'))==0){
      return('Error: no subgroup is applied.')
    }else{
      plot.data <- as.data.frame(plot.data)
      facet1ind <- which(colnames(plot.data) == 'facet1')
      facet1df <- plot.data$facet1
      facet1df <- factor(facet1df,levels = as.vector(unique(facet1df)))
      plot.data <- plot.data[,-facet1ind]
    }
  }else if(multiplots == 'none') {
    plot.data <- as.data.frame(plot.data)
  }else{
    return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
  }
  # Check if the group names are given. If not, choose the first column as the
  # group name.
  if(is.null(plot.data$group)==FALSE){
    plot.data$group <- as.factor(as.character(plot.data$group))
  }else{
    plot.data[,1] <- as.factor(as.character(plot.data[,1]))
    names(plot.data)[1] <- "group"
  }

  col_group = which(colnames(plot.data)=='group')
  # Extract names of the variables from the data frame except the group names.
  var.names <- colnames(plot.data)[-col_group]
  df_variables <-  plot.data[,-col_group]
  df_variables <- data.frame(lapply(df_variables,
                                    function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/grid.max)))
  plot.data <-  cbind(plot.data$group,df_variables)
  names(plot.data)[1] <- 'group'

  # Check if the axis labels are properly set up.
  if(axis.labels == ""){
    axis.labels <- var.names
  }else{
    if (length(axis.labels) != ncol(plot.data)-1)
      return("Error: 'axis.labels' contains the wrong number of axis labels")
  }


  #calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf


  #Declare required internal functions

  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

    path <- df[,1]
    path <- factor(path,levels = as.vector(path))

    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]

    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]


        graphData=rbind(graphData, data.frame(group=i,
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i,
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)

  # mini type for web plotting
  if(webtype == 'mini'){
    values.radar <- c("0%", "50%", "100%")
    grid.mid <- 0.5
    # (e) Create Circular grid-lines + labels
    #caclulate the cooridinates required to plot circular grid-lines for three user-specified
    #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
    gridline <- NULL
    gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
    gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
    gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)

    #gridline labels
    gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                     text=as.character(grid.min))
    gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                     text=as.character(grid.max))
    gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                     text=as.character(grid.mid))

  }else if(webtype == 'lux'){
    # luxurious web type
    values.radar <- c("0%", "20%", "40%", "60%", "80%", "100%")
    grid.mid1 <- 0.2
    grid.mid2 <- 0.4
    grid.mid3 <- 0.6
    grid.mid4 <- 0.8

    # (e) Create Circular grid-lines + labels
    #caclulate the cooridinates required to plot circular grid-lines for three user-specified
    #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
    gridline <- NULL
    gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
    gridline$mid1$path <- funcCircleCoords(c(0,0),grid.mid1+abs(centre.y),npoints = 360)
    gridline$mid2$path <- funcCircleCoords(c(0,0),grid.mid2+abs(centre.y),npoints = 360)
    gridline$mid3$path <- funcCircleCoords(c(0,0),grid.mid3+abs(centre.y),npoints = 360)
    gridline$mid4$path <- funcCircleCoords(c(0,0),grid.mid4+abs(centre.y),npoints = 360)
    gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)

    #gridline labels
    gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                     text=as.character(grid.min))
    gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                     text=as.character(grid.max))
    gridline$mid1$label <- data.frame(x=gridline.label.offset,y=grid.mid1+abs(centre.y),
                                     text=as.character(grid.mid1))
    gridline$mid2$label <- data.frame(x=gridline.label.offset,y=grid.mid2+abs(centre.y),
                                     text=as.character(grid.mid2))
    gridline$mid3$label <- data.frame(x=gridline.label.offset,y=grid.mid3+abs(centre.y),
                                     text=as.character(grid.mid3))
    gridline$mid4$label <- data.frame(x=gridline.label.offset,y=grid.mid4+abs(centre.y),
                                     text=as.character(grid.mid4))
  }else{
    return("Error: 'webtype' only contains two types ('mini' and 'lux') so far.  ")
  }




  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=base.size) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))


  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  if(multiplots == '1D'){
    facet_vec <- factor(unique(facet1df),levels = as.vector(unique(facet1df)))
    no.facet <- length(facet_vec)
    multiaxislabel <- cbind(axis$label[rep(seq_len(nrow(axis$label)), no.facet),],rep(facet_vec,each = nrow(axis$label)))
    names(multiaxislabel)[4] <- 'facet1'
    base <- ggplot(multiaxislabel) + xlab(NULL) + ylab(NULL) + coord_equal() +
      geom_text(data=subset(multiaxislabel,multiaxislabel$x < (-x.centre.range)),
                aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
      scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
      scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))+facet_wrap(~facet1)
  }else if(multiplots == 'none'){
    base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
      geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
                aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
      scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
      scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  }else{
    return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
  }

    if(style == "round"){
      if(webtype == 'mini'){
        # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
        base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                                  lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                                  lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                                  lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
      }else if(webtype == 'lux'){
        # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
        base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                                  lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$mid1$path,aes(x=x,y=y),
                                  lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$mid2$path,aes(x=x,y=y),
                                  lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$mid3$path,aes(x=x,y=y),
                                  lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$mid4$path,aes(x=x,y=y),
                                  lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                                  lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)

      }else{
        return("Error: 'webtype' only contains two types ('mini' and 'lux') so far.  ")
      }

    }else if(style == "straight"){
      if(webtype == 'mini'){
        # ... + straight grid-lines at 'min', 'mid' and 'max' y-axis values
        # Extract the coordinates of the inner points and the outer points
        oddindex <-  seq(1,nrow(axis$path),2)
        evenindex <- seq(2,nrow(axis$path),2)
        axis$innerpath <- axis$path[oddindex,]
        axis$outerpath <- axis$path[evenindex,]
        axis$innerpath <- rbind(axis$innerpath,head(axis$innerpath,1))
        axis$outerpath <- rbind(axis$outerpath,head(axis$outerpath,1))
        # Calculate the coordinates of the middle points
        axis$middlepath <- (axis$innerpath+axis$outerpath)/2
        # Draw the straight path of each layer
        base <- base + geom_path(data = axis$innerpath,aes(x=x,y=y),
                                 lty=gridline.min.linetype,colour=gridline.max.colour,size=grid.line.width)+
          geom_path(data = axis$outerpath,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)+
          geom_path(data = axis$middlepath,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.mid.colour,size=grid.line.width)
      }else if(webtype == 'lux'){
        # ... + straight grid-lines at 'min', 'mid' and 'max' y-axis values
        # Extract the coordinates of the inner points and the outer points
        oddindex <-  seq(1,nrow(axis$path),2)
        evenindex <- seq(2,nrow(axis$path),2)
        axis$innerpath <- axis$path[oddindex,]
        axis$outerpath <- axis$path[evenindex,]
        axis$innerpath <- rbind(axis$innerpath,head(axis$innerpath,1))
        axis$outerpath <- rbind(axis$outerpath,head(axis$outerpath,1))
        # Calculate the coordinates of the middle points
        axis$middle1path <- (-axis$innerpath+axis$outerpath)/5+axis$innerpath
        axis$middle2path <- (-axis$innerpath+axis$outerpath)*2/5+axis$innerpath
        axis$middle3path <- (-axis$innerpath+axis$outerpath)*3/5+axis$innerpath
        axis$middle4path <- (-axis$innerpath+axis$outerpath)*4/5+axis$innerpath

        # Draw the straight path of each layer
        base <- base + geom_path(data = axis$innerpath,aes(x=x,y=y),
                                 lty=gridline.min.linetype,colour=gridline.max.colour,size=grid.line.width)
        base <- base + geom_path(data = axis$outerpath,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
        base <- base +   geom_path(data = axis$middle1path,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +   geom_path(data = axis$middle2path,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +   geom_path(data = axis$middle3path,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.mid.colour,size=grid.line.width)
        base <- base +   geom_path(data = axis$middle4path,aes(x=x,y=y),
                    lty=gridline.min.linetype,colour=gridline.mid.colour,size=grid.line.width)

      }else{
        return("Error: 'webtype' only contains two types ('mini' and 'lux') so far.  ")
      }

    }else{
      return("Error: 'style' should be specified...")
    }


    # + axis labels for any vertical axes [abs(x)<=x.centre.range]
    base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
    # + axis labels for any vertical axes [x>x.centre.range]
    base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
    # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
    base <- base + theme_clear

    if(style == 'round'){
      #  + background circle against which to plot radar data
      base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                                  fill=background.circle.colour,
                                  alpha=background.circle.transparency)
    }else if(style == 'straight'){
      #  + background polygon  against which to plot radar data
      base <- base + geom_polygon(data=axis$outerpath,aes(x,y),
                                  fill=background.circle.colour,
                                  alpha=background.circle.transparency)
    }else{
      return("Error: 'style' should be specified...")
    }


    # + radial axes
    base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                             colour=axis.line.colour)

    # Draw path for different facets.
    if(multiplots == '1D'){

      multigrouppath <- cbind(group$path,rep(facet1df,each=nrow(group$path)/nrow(plot.data)))
      names(multigrouppath)[4] <- 'facet1'
      # fill different facets
      if(polygonfill){
        base <- base + geom_polygon(data=multigrouppath,aes(x=x,y=y,col = factor(group), fill = factor(group)),
                                    alpha=polygonfill.transparency,show.legend = F)+
          facet_wrap(~facet1)
      }

      # ... + group (cluster) 'paths'
      base <- base + geom_path(data=multigrouppath,aes(x=x,y=y,group=group,colour=group),
                               size=group.line.width)+
        facet_wrap(~facet1)

      # ... + group points (cluster data)
      base <- base + geom_point(data=multigrouppath,aes(x=x,y=y,group=group,colour=group),size=group.point.size)+
              facet_wrap(~facet1)
    }else if(multiplots == 'none'){
      if(polygonfill){
        base <- base + geom_polygon(data=group$path,aes(x=x,y=y,col = factor(group), fill = factor(group)),
                                    alpha=polygonfill.transparency,show.legend = F)
      }

      # ... + group (cluster) 'paths'
      base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                               size=group.line.width)

      # ... + group points (cluster data)
      base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)

    }else{
      return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
    }




    #... + amend Legend title
    if (plot.legend){
      if(multiplots=='none'){
        base  <- base + labs(colour=legend.title,size=legend.text.size) +
          theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
          theme(legend.key.height=unit(2,"line"))
      }else if(multiplots=='1D'){
        base  <- base + labs(colour=legend.title,size=legend.text.size) +
          theme(legend.text = element_text(size = legend.text.size), legend.position="bottom") +
          theme(legend.key.height=unit(2,"line"))
      }else{
        return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
      }

    }else{
      base <- base + theme(legend.position = "none")
    }

    # ... + grid-line labels (max; mid; min)

    if (label.gridline.min==TRUE) { base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1) }
    if (label.gridline.max==TRUE) { base <- base + geom_text(aes(x=x,y=y,label=values.radar[length(values.radar)]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1) }

    if(webtype == 'mini'){
      if (label.gridline.mid==TRUE) { base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1) }
    }else if(webtype == 'lux'){
      if (label.gridline.mid==TRUE) {
        base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid1$label,size=grid.label.size*0.8, hjust=1)
        base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$mid2$label,size=grid.label.size*0.8, hjust=1)
        base <- base + geom_text(aes(x=x,y=y,label=values.radar[4]),data=gridline$mid3$label,size=grid.label.size*0.8, hjust=1)
        base <- base + geom_text(aes(x=x,y=y,label=values.radar[5]),data=gridline$mid4$label,size=grid.label.size*0.8, hjust=1)
        }
    }else{
      return("Error: 'webtype' only contains two types ('mini' and 'lux') so far.  ")
    }

    # ... + centre.y label if required [i.e. value of y at centre of plot circle]
    if (label.centre.y==TRUE) {
      centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
      base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5) }

    if (!is.null(group.colours)){
      colour_values=rep(group.colours,100)
    } else {
      colour_values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051",
                          "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)
    }

    base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20)) +
      scale_colour_manual(values=colour_values) +
      theme(legend.title=element_blank())

    if (plot.title != "") {
      base <- base + ggtitle(plot.title)
    }
    if (pannelbackground==FALSE){
      base <- base + theme(strip.background = element_blank())
    }
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]


  return(base)

}

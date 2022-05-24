### Run some simple analyses for Mike
# Started 17 March 2022 by Cat

#Perhaps the equation is something like 
# (CI expenditures)*(growth/yr) + (partner expenditure)*(growth/yr) [for n partners at each time step]… 
#where ‘growth’ is the % of the overall expenditures that are allocated on our priorities/goals?

## I'm not sure I have this sort of information... but I will do the best I can with it I suppose

set.seed(12321)


if(FALSE){
  #costeffect <- 5
  #costsigma <- 1
  
  yeareffect <- 4
  yearsigma <- 0.5
  
  lagtime <- 2
  lagtime_major <- 5
  
  cepf <- "CEPF"
  gef <- "NA"
  worldbank <- "The World Bank"
  europeaid <- "EuropeAid"
  
  df <- findlatlong
  
  #findlatlong <- read.csv("~/Documents/git/ciapp/source/clean_econservation_adm2.csv")
}

################################################################
############# Simulated data for Methods Paper #################
################################################################
dffunc <- function(yeareffect, yearsigma, lagtime, lagtime_major, df){ # costeffect, costsigma,
  
  set.seed(12321)
  
        funders <- c("CI", "GEF", "The World Bank", "Mastercard", "hp")
        
        tracks <- c("aprotect", "manage", "restore")
        years <- c(2022:2050)
        nobs <- 45
        
        ## Start with Scenario A where it is just CI to make things easier
        funder <- "ci"
        
        
        df$latlong <- paste(df$lat, df$long)
        
        listofcoords <- sample(df$latlong, 50)
        
        ci_roi <- data.frame(funder=rep(funder, times=length(years)*nobs), 
                             intervention = rep(rep(tracks, each=nobs/3), times=length(years)),
                             year = rep(years, each=nobs))
        
        ci_roi$latlong <- sample(size = nrow(ci_roi), listofcoords, replace = TRUE)
        
        ## Have investment costs increase over time
        ci_roi$cost <- rnorm(nrow(ci_roi), 50, 10) + (ci_roi$year-2022)*rnorm(n=nrow(ci_roi), mean=yeareffect, sd=yearsigma)
        
        ## Returns will be higher with higher investments
        ci_roi$returns <- ci_roi$cost * rnorm(n=nrow(ci_roi), mean=5, sd=1)
        
        ci_roi$roibase <- (ci_roi$returns/ci_roi$cost) 
        
        ## Manage with have greater returns than Protect and Restore will have the most returns
        # But there will be a lag time
        ci_roi$roi <- ifelse(ci_roi$intervention=="manage" & ci_roi$year > 2022 + lagtime, ci_roi$roibase + rnorm(1, 10, 0.05), ci_roi$roibase)
        ci_roi$roi <- ifelse(ci_roi$intervention=="restore" & ci_roi$year > 2022 + lagtime_major, ci_roi$roi + rnorm(1, 20, 0.05), ci_roi$roi)
        
        ## Returns should increase over time as investments compound 
        ### But introduce a lagtime since it takes time for things to be implemented
        ci_roi$roinew <- (ci_roi$roi + 
          ifelse(ci_roi$year <= 2022 + lagtime, 0, 
                 (ci_roi$year-2022)*rnorm(n=nrow(ci_roi), mean=yeareffect, sd=yearsigma)))/100
        
        ci_roi$roiperint <- ave(ci_roi$roinew, ci_roi$year, ci_roi$intervention, FUN=sum)
        ci_roi$totroi<- ave(ci_roi$roinew, ci_roi$year, FUN=sum)
        
        if(FALSE){
        ##### Plotting Scenario A
        ggplot(ci_roi) + geom_smooth(aes(y=totroi, x=year), se=FALSE, col="black") +
          geom_smooth(aes(y=roiperint, x=year, col=intervention), se=FALSE) + theme_bw() + 
          ylab("Annual climate mitigation (PgCO2e/$1M)") + xlab("") +
          labs(col="Action Track")
        }
        
        #################################
        ## Plot a map of single funder ##
        
        ci_roi$lat <- as.numeric(gsub("\\ .*", "", ci_roi$latlong))
        ci_roi$long <- as.numeric(gsub(".*\\ ", "", ci_roi$latlong))
        ci_roi$totroi <- round(ci_roi$totroi, digits = 0)
        
        ci_roi$roisite <- ave(ci_roi$roinew, ci_roi$latlong, FUN = sum)
        ci_roi$sitefirst <- ave(ci_roi$roinew, ci_roi$latlong, FUN = min)
        ci_roi$sitelast <- ave(ci_roi$roinew, ci_roi$latlong, FUN = max)
        ci_roi$diff <- ci_roi$sitelast - ci_roi$sitefirst
        ci_roi$sitesize <- ci_roi$diff * (ci_roi$roisite/50)
        
        ci.sub <- subset(ci_roi, select=c("lat", "long", "roisite", "sitesize", "latlong"))
        ci.sub <- ci.sub[!duplicated(ci.sub),]
        
        if(FALSE){
          coords = SpatialPoints(ci.sub[c("long", "lat")], proj4string = crs("+proj=longlat +datum=WGS84"))
          
          sites <- gBuffer( coords, width=ci.sub$sitesize, byid=TRUE )
          sites <- fortify(sites)
          sites$sitesize <- ci.sub$sitesize
          sites$roisite <- ci.sub$roisite
          
          myPalette <- colorRampPalette(brewer.pal(9, "Greens")) #### Gives us a heat map look
          sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 150)) ### this is the range of ROI data we have
          filled <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 150)) ### this is the range of ROI data we have
          sized <- scale_size_continuous(range=c(0, 450))
          boundars<-readShapeSpatial("~/Documents/git/ciapp/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
          mapWorld<-fortify(boundars)
          singlemap <- ggplot() +
            geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
                         color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
            geom_polygon(aes(x=sites$long, y=sites$lat, group=sites$group, fill=sites$roisite),
                         alpha = 0.7) +
            geom_point(aes(x=ci.sub$long, y=ci.sub$lat, 
                           color=ci.sub$roisite)) + 
            theme_classic() + ### this removes extra background features from ggplot2
            coord_cartesian(ylim=c(-35,6),xlim=c(-76,-32)) + 
            theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(), 
                  plot.title=element_text(size = 10, face="bold.italic"),
                  axis.title = element_blank(),
                  panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
            sc + filled +
            labs(fill="Climate mitigation\nper site (PgCO2e/$1M)") + guides(col="none")
          singlemap
        }
        
        ###########################################################
        ## Next, Scenario C where we have a coalition of funders ##
        ###########################################################
        listofcoords <- sample(df$latlong, 80)
        coal <- data.frame(funder = rep(rep(funders, each=nobs/length(funders)), times=length(years)), 
                             intervention = rep(rep(tracks, each=nobs/length(tracks)), times=length(years)),
                             year = rep(years, each=nobs))
        
        coal$latlong <- sample(size = nrow(coal), listofcoords, replace = TRUE)
        
        ## Have investment costs increase over time
        coal$cost <- ifelse(coal$funder=="CI", rnorm(nrow(coal), 50, 10) + 
                              (coal$year-2022)*rnorm(n=nrow(coal), mean=yeareffect, sd=yearsigma), NA)
        
        coal$cost <- ifelse(coal$funder%in%c("GEF", "The World Bank"), rnorm(nrow(coal), 100, 15) + 
                              (coal$year-2022)*rnorm(n=nrow(coal), mean=yeareffect, sd=yearsigma), coal$cost)
        
        coal$cost <- ifelse(coal$funder%in%c("Mastercard", "hp"), rnorm(nrow(coal), 75, 10) + 
                              (coal$year-2022)*rnorm(n=nrow(coal), mean=yeareffect, sd=yearsigma), coal$cost)
        
        ## Returns will be higher with higher investments
        coal$returns <- coal$cost * rnorm(n=nrow(coal), mean=5, sd=2)
        
        coal$roibase <- (coal$returns/coal$cost) 
        
        ## Manage with have greater returns than Protect and Restore will have the most returns
        coal$roi <- ifelse(coal$intervention=="manage" & coal$year > 2022 + lagtime, coal$roibase + rnorm(1, 20, 0.05), coal$roibase)
        coal$roi <- ifelse(coal$intervention=="restore"& coal$year > 2022 + lagtime_major, coal$roi + rnorm(1, 30, 0.05), coal$roi)
        
        ## Returns should increase over time as investments compound 
        ### But let's add in a lag time, especially for some funding organizations
        coal$roinew <- coal$roi + 
          ifelse(coal$funder%in%c("CI", "Mastercard", "hp") & coal$year <= 2022 + lagtime, 0, 
                                 (coal$year-2022)*rnorm(n=nrow(coal), mean=yeareffect, sd=yearsigma))
        coal$roinew <- (coal$roinew + 
          ifelse(coal$funder%in%c("GEF", "The World Bank") & coal$year <= 2022 + lagtime_major, 0, 
                 (coal$year-2022)*rnorm(n=nrow(coal), mean=yeareffect, sd=yearsigma)))/100
        
        
        coal$roiperint <- ave(coal$roinew, coal$year, coal$intervention, coal$funder, FUN=sum)
        coal$totroi<- ave(coal$roinew, coal$year, FUN=sum)
        
        if(FALSE){
        ##### Plotting Scenario C
        ggplot(coal) + geom_smooth(aes(y=totroi, x=year), se=FALSE, col="black") +
          geom_smooth(aes(y=roiperint, x=year, col=intervention, linetype=funder), se=FALSE) + theme_bw() + 
          ylab("Annual ROI (PgCO2e/$1M)") + xlab("") +
          labs(col="Action Track", linetype="Funding organization")
        }
        
        coal$lat <- as.numeric(gsub("\\ .*", "", coal$latlong))
        coal$long <- as.numeric(gsub(".*\\ ", "", coal$latlong))
        coal$totroi <- round(coal$totroi, digits = 0)
        
        coal$roisite <- ave(coal$roinew, coal$latlong, FUN = sum)
        coal$sitefirst <- ave(coal$roinew, coal$latlong, FUN = min)
        coal$sitelast <- ave(coal$roinew, coal$latlong, FUN = max)
        coal$diff <- coal$sitelast - coal$sitefirst
        coal$sitesize <- coal$diff * (coal$roisite/50)
        
        coal.sub <- subset(coal, select=c("lat", "long", "roisite", "sitesize"))
        coal.sub <- coal.sub[!duplicated(coal.sub),]
        
        if(FALSE){
          coords = SpatialPoints(coal.sub[c("long", "lat")], proj4string = crs("+proj=longlat +datum=WGS84"))
          
          sites <- gBuffer( coords, width=coal.sub$sitesize, byid=TRUE )
          sites <- fortify(sites)
          sites$sitesize <- coal.sub$sitesize
          sites$roisite <- coal.sub$roisite
          
          coalmap <- ggplot() +
            geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
                         color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
            geom_polygon(aes(x=sites$long, y=sites$lat, group=sites$group, fill=sites$roisite),
                         alpha = 0.7) +
            geom_point(aes(x=coal.sub$long, y=coal.sub$lat, 
                           size=coal.sub$diff,
                           color=coal.sub$roisite)) + 
            theme_classic() + ### this removes extra background features from ggplot2
            coord_cartesian(ylim=c(-35,6),xlim=c(-76,-32)) + 
            theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(), 
                  plot.title=element_text(size = 10, face="bold.italic"),
                  axis.title = element_blank(),
                  panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
            sc + filled +
            labs(fill="Climate mitigation\nper site (PgCO2e/$1M)") + guides(color="none")
          coalmap
        }
        
        mylist <- list(ci_roi, coal, ci.sub, coal.sub)
        
        return(mylist)

}


realfunc <- function(cepf, gef, worldbank, europeaid, dfreal){
  
  namestopick <- c(worldbank, gef, cepf, europeaid)
  
  txctl <- dfreal[dfreal$funder%in%namestopick,]
  
  mylist <- list(txctl)
  
  return(mylist)
  
}


if(FALSE){
  
  protectinvest = 150
  manageinvest = 100
  restoreinvest = 300
  
  numprotect <- 20
  nummanage <- 20
  numrestore <- 20
  
  list <- foreaction(protectinvest, manageinvest, restoreinvest, brazil)
  
  
}



foreaction <- function(protectinvest, numprotect, manageinvest, nummanage, restoreinvest, numrestore, brazil){
  
  set.seed(12321)
  
  newbrazil <- brazil
  
  ##### Let's simulate forecasted data here:
  newbrazilbase <- subset(newbrazil, select=c("lc_ha_rate", "intervention", "NAME", "pdsi",
                                              "numrds", "mst", "elev", "slope", "pas", "forest",
                                              "yrsofproject", "accum.cost", "prevforest", "year"))
  
  newbrazilbase <- newbrazilbase[!duplicated(newbrazilbase),]
  newbrazilbase <- newbrazilbase[complete.cases(newbrazilbase),]
  
  protectprojs <- as.numeric(numprotect) #round(protectinvest/3, digits=0)
  manageprojs <- as.numeric(nummanage) #round(manageinvest/3, digits=0)
  restoreprojs <- as.numeric(numrestore) #round(manageinvest/3, digits=0)
  
  if(FALSE){
  proyrs <- as.numeric(c(2022:2031))
  protectyears <- vector()
  for(i in c(1:length(proyrs))){ 
    
    proyearsadd <- rep(proyrs[i], each=((i-1)^2) + 2)
    protectyears <- c(protectyears, proyearsadd)
    
  }
  
  manyrs <- as.numeric(c(2022:2031))
  manageyears <- vector()
  for(i in c(1:length(manyrs))){ 
    
    manyearsadd <- rep(manyrs[i], each=((i-1)^2) + 2)
    manageyears <- c(manageyears, manyearsadd)
    
  }
  
  resyrs <- as.numeric(c(2022:2031))
  restoreyears <- vector()
  for(i in c(1:length(resyrs))){ 
    
    resyearsadd <- rep(resyrs[i], each=((i-1)^2) + 2)
    restoreyears <- c(restoreyears, resyearsadd)
    
  }
  }
  
  futuredf <- data.frame(intervention=c(rep("protect", protectprojs), rep("control protect", protectprojs),
                                        rep("manage", manageprojs), rep("control manage", manageprojs),
                                        rep("restore", restoreprojs), rep("control restore", restoreprojs)))
  
  
  protectyears <- as.numeric(rep(c(2022:2031), each=round(((protectprojs)/10), digits=0)))
  manageyears <- as.numeric(rep(c(2022:2031), each=round(((manageprojs)/10), digits=0)))
  restoreyears <- as.numeric(rep(c(2022:2031), each=round(((restoreprojs)/10), digits=0)))
  
  futuredf$start <- ifelse(futuredf$intervention %in% c("protect", "control protect"),
                           protectyears, NA)
  futuredf$start <- ifelse(futuredf$intervention %in% c("manage", "control manage"),
                           manageyears, futuredf$start)
  futuredf$start <- ifelse(futuredf$intervention %in% c("restore", "control restore"),
                           restoreyears, futuredf$start)
  
  #endyrs <- data.frame(rep(c(2:7), each=150))
  #endyrs <- endyrs[sample(nrow(endyrs), (protectprojs + manageprojs + restoreprojs)*2, replace=TRUE),]
  
  futuredf$end <- futuredf$start + 4
  
  pronames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("protect forest", "protect wetland")])
  conpronames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("control protect forest", "control protect wetland")])
  
  mannames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("manage agriculture", "manage forest")])
  conmannames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("control manage agriculture", "control manage forest")])
  
  resnames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("restore forest")])
  conresnames <- unique(newbrazilbase$NAME[newbrazilbase$intervention %in% c("control restore forest")])
  
  
  futuredf$NAME <- ifelse(futuredf$intervention=="protect",
                          sample(pronames, length(protectyears), replace=TRUE), NA)
  futuredf$NAME <- ifelse(futuredf$intervention=="control protect",
                          sample(conpronames, length(protectyears), replace=TRUE), futuredf$NAME)
  
  futuredf$NAME <- ifelse(futuredf$intervention=="manage",
                          sample(mannames, length(manageyears), replace=TRUE), futuredf$NAME)
  futuredf$NAME <- ifelse(futuredf$intervention=="control manage",
                          sample(conmannames, length(manageyears), replace=TRUE), futuredf$NAME)
  
  futuredf$NAME <- ifelse(futuredf$intervention=="restore",
                          sample(resnames, length(restoreyears), replace=TRUE), futuredf$NAME)
  futuredf$NAME <- ifelse(futuredf$intervention=="control restore",
                          sample(conresnames, length(restoreyears), replace=TRUE), futuredf$NAME)
  
  
  futdf <- data.frame()
  
  for(i in c(1:nrow(futuredf))) { #i=1
    
    numreps <- futuredf$end[i]-futuredf$start[i] + 1
    
    intervention <- rep(futuredf$intervention[i], each=numreps)
    start <- rep(futuredf$start[i], each=numreps)
    end <- rep(futuredf$end[i], each=numreps)
    NAME <- rep(futuredf$NAME[i], each=numreps)
    year <- futuredf$start[i]:futuredf$end[i]
    
    fooadd <- data.frame(intervention, start, end, NAME, year)
    
    futdf <- rbind(futdf, fooadd)
    
  }
  
  
  params <- newbrazilbase[newbrazilbase$year==2020,]
  params$lc_ha_rate <- params$prevforest <- NULL
  
  params$intervention <- ifelse(params$intervention %in% c("protect forest", "protect wetland"), "protect", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control protect forest", "control protect wetland"), "control protect", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("manage forest", "manage agriculture"), "manage", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control manage forest", "control manage agriculture"), "control manage", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("restore forest"), "restore", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control restore forest"), "control restore", params$intervention)
  
  params$year <- params$forest <- params$yrsofproject <- NULL
  
  params <- params[!duplicated(params$NAME),]
  
  futdf <- dplyr::left_join(futdf, params)
  futdf <- futdf[!duplicated(futdf),]
  futdf <- futdf[complete.cases(futdf),]
  
  for(i in c(1:nrow(futdf))){ #i=1
      
      futdf$mst[i] <- rnorm(1, futdf$mst[i] + 0, 0.05)
      futdf$pdsi[i] <- rnorm(1, futdf$pdsi[i] - 0, 0.05)
      #futdf$pas[i] <- rnorm(1, futdf$pas[i], 0.1)
      #futdf$numrds[i] <- rnorm(1, futdf$numrds[i], 50)
      futdf$cost[i] <- 0 #+ ((futdf$start[i]-2020)*1000)
      
  }
  
  if(TRUE){
  for(i in c(1:nrow(futdf))){
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="protect",
                            futdf$cost[i] + ((protectinvest*1000000)/protectprojs), #+((futdf$end[i]-2020)^2 + 1000) #(brazil.stan$year[i]-2020)*
                            futdf$cost[i])
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="manage",
                            futdf$cost[i] + ((manageinvest*1000000)/manageprojs),#(brazil.stan$year[i]-2020)*rnorm(1, mean=manageinvest*1000000, sd=5),
                            futdf$cost[i])
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="restore",
                            futdf$cost[i] + ((restoreinvest*1000000)/restoreprojs),#(brazil.stan$year[i]-2020)*rnorm(1, mean=restoreinvest*1000000, sd=5),
                            futdf$cost[i])
    
  }
  }
  
  brazilnew <- futdf[!duplicated(futdf),]
  
  
  brazil.stan <- subset(brazilnew, 
                        select=c("start", "end", "intervention", "cost", "NAME", "year",
                                 "pdsi", "numrds", "mst", "elev", "slope", "pas"))
  
  
  brazil.stan <- brazil.stan %>% dplyr::group_by(start, end, intervention, NAME, cost) %>%
    dplyr::arrange(year)
  
  brazil.stan$yrsofproject <- (brazil.stan$year - brazil.stan$start) + 1
  
  brazil.stan <- brazil.stan[(brazil.stan$yrsofproject >= 0),]
  
 # Establish investments per year per project and then look at accumulated investment over time
  controls <- c("control manage", "control protect", "control restore")
  
  brazil.stan$cost <- ifelse(brazil.stan$intervention %in% controls, 0, brazil.stan$cost)
  brazil.stan$numyrs <- (brazil.stan$end - brazil.stan$start) #+ 1
  brazil.stan$costperyr <- brazil.stan$cost/brazil.stan$numyrs
  
  brazil.stan$costperyr <- ifelse(brazil.stan$year > brazil.stan$end, 0, brazil.stan$costperyr)
  brazil.stan$costperyr <- ifelse(brazil.stan$year < brazil.stan$start, 0, brazil.stan$costperyr)
  
  ## Have investment costs increase over time
  if(FALSE){
  for(i in c(1:nrow(brazil.stan))){
    
    brazil.stan$costperyr[i] <- ifelse(brazil.stan$intervention[i]=="protect",
                                    brazil.stan$costperyr[i] + ((brazil.stan$year[i]-2020)*(protectinvest*1000)), #(brazil.stan$year[i]-2020)*
                                    brazil.stan$costperyr[i])
    
    brazil.stan$costperyr[i] <- ifelse(brazil.stan$intervention[i]=="manage",
                                    brazil.stan$costperyr[i] + ((brazil.stan$year[i]-2020)*(manageinvest*1000)),#(brazil.stan$year[i]-2020)*rnorm(1, mean=manageinvest*1000000, sd=5),
                                    brazil.stan$costperyr[i])
    brazil.stan$costperyr[i] <- ifelse(brazil.stan$intervention[i]=="restore",
                                    brazil.stan$costperyr[i] + ((brazil.stan$year[i]-2020)*(restoreinvest*1000)),#(brazil.stan$year[i]-2020)*rnorm(1, mean=restoreinvest*1000000, sd=5),
                                    brazil.stan$costperyr[i])
  
  }
  }

  
  brazil.stan$accum.cost <- ave(brazil.stan$costperyr, brazil.stan$start, brazil.stan$end, brazil.stan$intervention,
                                brazil.stan$NAME, FUN=cumsum)
  
  
  
  
  
  #### Let's explore rate of temperature and precip changes
  brazil.stan <- brazil.stan %>%
    group_by(intervention, NAME, start, end, cost) %>% 
    arrange(year) %>%
    dplyr::mutate(prevpdsi = lag(pdsi, default = first(pdsi))) %>%
    dplyr::mutate(prevmst = lag(mst, default = first(mst)))
  
  brazil.stan <- brazil.stan[brazil.stan$yrsofproject>0,]
  
  brazil.stan$mstrate <- (brazil.stan$mst-brazil.stan$prevmst) ## Annual rate of change
  brazil.stan$pdsirate <- (brazil.stan$pdsi-brazil.stan$prevpdsi) ## annual rate of change
  
  controls <- c("control protect", "control manage", "control restore")
  
  brazilsub <- brazil.stan[brazil.stan$year <= brazil.stan$end,]
  brazilsub$yrsofproject <- ifelse(brazilsub$intervention %in% controls, 0, brazilsub$yrsofproject)
  
  ### Adding in 18 May 2022 to fix z-score values
  brazilsub$flag <- "new"
  oldbrazil <- newbrazil
  oldbrazil$flag <- "old"
  
  brazilsub <- full_join(brazilsub, oldbrazil)
  
  brazilsub$yrs.z <- (brazilsub$yrsofproject-mean(brazilsub$yrsofproject,na.rm=TRUE))/(2*sd(brazilsub$yrsofproject,na.rm=TRUE))
  brazilsub$mst.z <- (brazilsub$mst-mean(brazilsub$mst,na.rm=TRUE))/(2*sd(brazilsub$mst,na.rm=TRUE))
  brazilsub$pdsi.z <- (brazilsub$pdsi-mean(brazilsub$pdsi,na.rm=TRUE))/(2*sd(brazilsub$pdsi,na.rm=TRUE))
  brazilsub$rds.z <- (brazilsub$numrds-mean(brazilsub$numrds,na.rm=TRUE))/(2*sd(brazilsub$numrds,na.rm=TRUE))
  brazilsub$elev.z <- (brazilsub$elev-mean(brazilsub$elev,na.rm=TRUE))/(2*sd(brazilsub$elev,na.rm=TRUE))
  brazilsub$slope.z <- (brazilsub$slope-mean(brazilsub$slope,na.rm=TRUE))/(2*sd(brazilsub$slope,na.rm=TRUE))
  brazilsub$pas.z <- (brazilsub$pas-mean(brazilsub$pas,na.rm=TRUE))/(2*sd(brazilsub$pas,na.rm=TRUE))
  
  brazilsub$cost.z <- (brazilsub$costperyr-mean(brazilsub$costperyr,na.rm=TRUE))/(2*sd(brazilsub$costperyr,na.rm=TRUE))
  brazilsub$mstrate.z <- (brazilsub$mstrate-mean(brazilsub$mstrate,na.rm=TRUE))/(2*sd(brazilsub$mstrate,na.rm=TRUE))
  brazilsub$prate.z <- (brazilsub$pdsirate-mean(brazilsub$pdsirate,na.rm=TRUE))/(2*sd(brazilsub$pdsirate,na.rm=TRUE))
  
  brazilsub <- brazilsub[!brazilsub$flag=="old",]
  
  ##### Predict response now instead with new data...
  newdat <- brazilsub

  mylist <- list(newdat)
  
  return(mylist)
  
  
}


if(FALSE){
  
  action = "Reverse"
  investment = 10
  startyr = 2022
  endyr = 2027
  
  rds <- 10
  pas <- 0
  temp <- 2
  pdsi <- -2
  
  list <- foreuser(action, investment, years, rds, pas, temp, pdsi, brazil)
  
  
}



foreuser <- function(action, investment, startyr, endyr, rds, pas, temp, pdsi, brazil){
  
  set.seed(12321)
  
  action <- as.character(action)
  investment <- as.numeric(investment) 
  startyr <- as.numeric(startyr) 
  endyr <- as.numeric(endyr) 
  rds <- as.numeric(rds) 
  pas <- as.numeric(pas) 
  temp <- as.numeric(temp) 
  pdsi <- as.numeric(pdsi) 
  
  newbrazil <- brazil
  
  ##### Let's simulate forecasted data here:
  newbrazilbase <- subset(newbrazil, select=c("lc_ha_rate", "intervention", "NAME", "pdsi",
                                              "numrds", "mst", "elev", "slope", "pas", "forest",
                                              "yrsofproject", "accum.cost", "prevforest", "year"))
  
  newbrazilbase <- newbrazilbase[!duplicated(newbrazilbase),]
  newbrazilbase <- newbrazilbase[complete.cases(newbrazilbase),]
  
  params <- newbrazilbase
  
  params$intervention <- ifelse(params$intervention %in% c("protect forest", "protect wetland"), "Avoid", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control protect forest", "control protect wetland"), "control Avoid", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("manage forest", "manage agriculture"), "Reduce", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control manage forest", "control manage agriculture"), "control Reduce", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("restore forest"), "Reverse", params$intervention)
  params$intervention <- ifelse(params$intervention %in% c("control restore forest"), "control Reverse", params$intervention)
  
  intstokeep <- c(action, paste("control", action, sep=" "))
  
  params <- params[params$intervention %in% intstokeep,]
  
  
  futuredf <- data.frame(intervention=c(rep(action, 1), rep(paste("control", action, sep=" "), 1)))
  
  futuredf$start <- startyr
  futuredf$end <- endyr
  
  pronames <- unique(params$NAME[params$intervention %in% c("Avoid")])
  conpronames <- unique(params$NAME[params$intervention %in% c("control Avoid")])
  
  mannames <- unique(params$NAME[params$intervention %in% c("Reduce")])
  conmannames <- unique(params$NAME[params$intervention %in% c("control Reduce", "control Reduce")])
  
  resnames <- unique(params$NAME[params$intervention %in% c("Reverse")])
  conresnames <- unique(params$NAME[params$intervention %in% c("control Reverse")])
  
  futuredf$NAME <- ifelse(futuredf$intervention=="Avoid",
                          sample(pronames, length(1), replace=TRUE), NA)
  futuredf$NAME <- ifelse(futuredf$intervention=="control Avoid",
                          sample(conpronames, length(1), replace=TRUE), futuredf$NAME)
  
  futuredf$NAME <- ifelse(futuredf$intervention=="Reduce",
                          sample(mannames, length(1), replace=TRUE), futuredf$NAME)
  futuredf$NAME <- ifelse(futuredf$intervention=="control Reduce",
                          sample(conmannames, length(1), replace=TRUE), futuredf$NAME)
  
  futuredf$NAME <- ifelse(futuredf$intervention=="Reverse",
                          sample(resnames, length(1), replace=TRUE), futuredf$NAME)
  futuredf$NAME <- ifelse(futuredf$intervention=="control Reverse",
                          sample(conresnames, length(1), replace=TRUE), futuredf$NAME)
  
  
  futdf <- data.frame()
  
  for(i in c(1:nrow(futuredf))) { #i=1
    
    numreps <- futuredf$end[i]-futuredf$start[i] + 1
    
    intervention <- rep(futuredf$intervention[i], each=numreps)
    start <- rep(futuredf$start[i], each=numreps)
    end <- rep(futuredf$end[i], each=numreps)
    NAME <- rep(futuredf$NAME[i], each=numreps)
    year <- futuredf$start[i]:futuredf$end[i]
    
    fooadd <- data.frame(intervention, start, end, NAME, year)
    
    futdf <- rbind(futdf, fooadd)
    
  }
  
  
  params <- params[params$year==2020,]
  params$lc_ha_rate <- params$prevforest <- NULL
  
  params$year <- params$forest <- params$yrsofproject <- NULL
  
  params <- params[!duplicated(params$NAME),]
  
  futdf <- dplyr::left_join(futdf, params)
  futdf <- futdf[!duplicated(futdf),]
  futdf <- futdf[complete.cases(futdf),]
  
  for(i in c(1:nrow(futdf))){ #i=1
    
    futdf$mst[i] <- rnorm(1, futdf$mst[i] + temp, 0.05)
    futdf$pdsi[i] <- rnorm(1, futdf$pdsi[i] + pdsi, 0.05)
    futdf$pas[i] <- rnorm(1, futdf$pas[i] + pas, 0.1)
    futdf$numrds[i] <- rnorm(1, futdf$numrds[i] + rds, 50)
    futdf$cost[i] <- investment*1000000
    
  }
  
  
  brazilnew <- futdf[!duplicated(futdf),]
  
  
  brazil.stan <- subset(brazilnew, 
                        select=c("start", "end", "intervention", "cost", "NAME", "year",
                                 "pdsi", "numrds", "mst", "elev", "slope", "pas"))
  
  
  brazil.stan <- brazil.stan %>% dplyr::group_by(start, end, intervention, NAME, cost) %>%
    dplyr::arrange(year)
  
  brazil.stan$yrsofproject <- (brazil.stan$year - brazil.stan$start) + 1
  
  brazil.stan <- brazil.stan[(brazil.stan$yrsofproject >= 0),]
  
  # Establish investments per year per project and then look at accumulated investment over time
  controls <- c("control Reduce", "control Avoid", "control Reverse")
  
  brazil.stan$cost <- ifelse(brazil.stan$intervention %in% controls, 0, brazil.stan$cost)
  brazil.stan$numyrs <- (brazil.stan$end - brazil.stan$start) #+ 1
  brazil.stan$costperyr <- (brazil.stan$cost/brazil.stan$numyrs)
  
  brazil.stan$costperyr <- ifelse(brazil.stan$year > brazil.stan$end, 0, brazil.stan$costperyr)
  brazil.stan$costperyr <- ifelse(brazil.stan$year < brazil.stan$start, 0, brazil.stan$costperyr)
  
  for(i in c(1:nrow(brazil.stan))) { #i=1
  brazil.stan$costperyr[i] <- ifelse(brazil.stan$intervention[i] %in% c("Avoid", "Reduce", "Reverse"),
                                  brazil.stan$costperyr[i] + rnorm(1, 100, 5), brazil.stan$costperyr[i])
  }
  
  
  brazil.stan$accum.cost <- ave(brazil.stan$costperyr, brazil.stan$start, brazil.stan$end, brazil.stan$intervention,
                                brazil.stan$NAME, FUN=cumsum)
  
  
  #### Let's explore rate of temperature and precip changes
  brazil.stan <- brazil.stan %>%
    group_by(intervention, NAME, start, end, cost) %>% 
    arrange(year) %>%
    dplyr::mutate(prevpdsi = lag(pdsi, default = first(pdsi))) %>%
    dplyr::mutate(prevmst = lag(mst, default = first(mst)))
  
  brazil.stan <- brazil.stan[brazil.stan$yrsofproject>0,]
  
  brazil.stan$mstrate <- (brazil.stan$mst-brazil.stan$prevmst) ## Annual rate of change
  brazil.stan$pdsirate <- (brazil.stan$pdsi-brazil.stan$prevpdsi) ## annual rate of change
  
  controls <- c("control Avoid", "control Reduce", "control Reverse")
  
  brazilsub <- brazil.stan[brazil.stan$year <= brazil.stan$end,]
  brazilsub$yrsofproject <- ifelse(brazilsub$intervention %in% controls, 0, brazilsub$yrsofproject)
  
  ### Adding in 18 May 2022 to fix z-score values
  brazilsub$flag <- "new"
  oldbrazil <- newbrazil
  oldbrazil$flag <- "old"
  
  brazilsub <- full_join(brazilsub, oldbrazil)
  
  brazilsub$yrs.z <- (brazilsub$yrsofproject-mean(brazilsub$yrsofproject,na.rm=TRUE))/(2*sd(brazilsub$yrsofproject,na.rm=TRUE))
  brazilsub$mst.z <- (brazilsub$mst-mean(brazilsub$mst,na.rm=TRUE))/(2*sd(brazilsub$mst,na.rm=TRUE))
  brazilsub$pdsi.z <- (brazilsub$pdsi-mean(brazilsub$pdsi,na.rm=TRUE))/(2*sd(brazilsub$pdsi,na.rm=TRUE))
  brazilsub$rds.z <- (brazilsub$numrds-mean(brazilsub$numrds,na.rm=TRUE))/(2*sd(brazilsub$numrds,na.rm=TRUE))
  brazilsub$elev.z <- (brazilsub$elev-mean(brazilsub$elev,na.rm=TRUE))/(2*sd(brazilsub$elev,na.rm=TRUE))
  brazilsub$slope.z <- (brazilsub$slope-mean(brazilsub$slope,na.rm=TRUE))/(2*sd(brazilsub$slope,na.rm=TRUE))
  brazilsub$pas.z <- (brazilsub$pas-mean(brazilsub$pas,na.rm=TRUE))/(2*sd(brazilsub$pas,na.rm=TRUE))
  
  brazilsub$cost.z <- (brazilsub$costperyr-mean(brazilsub$costperyr,na.rm=TRUE))/(2*sd(brazilsub$costperyr,na.rm=TRUE))
  brazilsub$mstrate.z <- (brazilsub$mstrate-mean(brazilsub$mstrate,na.rm=TRUE))/(2*sd(brazilsub$mstrate,na.rm=TRUE))
  brazilsub$prate.z <- (brazilsub$pdsirate-mean(brazilsub$pdsirate,na.rm=TRUE))/(2*sd(brazilsub$pdsirate,na.rm=TRUE))
  
  brazilsub <- brazilsub[!brazilsub$flag=="old",]
  
  ##### Predict response now instead with new data...
  newdat <- brazilsub
  
  mylist <- list(newdat)
  
  return(mylist)
  
  
}


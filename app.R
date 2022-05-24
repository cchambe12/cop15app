### Started 24 March 2022 by Cat
## Building Shiny App for Coalition simulations for Mike


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)
library(viridis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(maptools)
library(grid)
library(sf)
library(rgeos)
library(raster)
library(brms)


set.seed(12321)

#setwd("~/Documents/git/cop15app/")
source("source/coalitionimpact.R")
findlatlong <- read.csv("source/clean_econservation_adm2.csv")
boundars <- readShapeSpatial("natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")

txctldata <- read.csv("source/clean_brazil_simple.csv")
prepped <- read.csv("source/clean_brazil_prepped.csv")

load("source/models/lcrate_brms_2levelitvn_allyears_10yr_noyrcont_costperyr.Rdata")

brazil <- read.csv("source/clean_brazil_stan.csv")

forecasts <- read.csv("source/clean_forecasts_lcrate.csv")


ui <- fluidPage(theme = shinytheme("cosmo"),
                
                navbarPage("Impact Forecasting",
                           tabPanel("Home",
                                    mainPanel(h1("Estimating conservation investment impact on climate change outcomes"),
                                              p(style="text-align: justify;","To progress investments in nature, forecasting impact is essential to effectively implement new conservation strategies. In this Shiny App, we provide a tool where users can consider how conservation investments and various interventions impact climate change mitigation. 
                                              We consider the impacts of various actions on the rate of forest cover change throughout Brazil based on ten major parameters: 1) years of project, 
                                              2) accumulated investment, 3) number of roads, 4) protected areas, 5) Palmer Drought Severity Index (PDSI), 6) rate of change in PDSI, 
                                                7) mean spring temperature, 8) rate of change in mean spring temperature, 9) elevation and 10) slope. "),
                                              br(),
                                              
                                              h3("We determine effects of parameters on conservation strategy returns on investment "),
                                              h5("see page ``Evaluating project impact''"),
                                              br(),
                                              p(style="text-align: justify;","Under the `Funder information` tab, you can see the breakdown of investments by action track type by each funding organzation included in the data."),
                                              br(),
                                              p(style="text-align: justify;","Under the `Parameters` tab, you can use the sidebar to select the parameters of interest and how they vary across intervention type as well as comparisons between treatment and control groups and also see overall model output."),
                                              br(),
                                              br(),
                                              br(),
                                              h3("Next, we begin to run initial forecasts based on base models"),
                                              h5("see page ``Forecasting parameters''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can use the sidebar to select the parameters of interest and project how they vary across intervention type as well as comparisons between treatment and control groups."),
                                              br(),
                                              h5("see page ``Forecasting investments''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can adjust investments by intervention type and see projected change in forest cover in comparison to control groups."),
                                              br(),
                                              br(),
                                              br(),
                                              h3("Next, we permit users to predict ROI by adjusting anticipated shifts in parameters"),
                                              h5("see page ``Forecasting impact by user input''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can adjust changes in parameter values to help select sites and see projected change in forest cover in comparison to control groups."),
                                              br(),
                                              br(),
                                              br(),
                                              h3("Finally, we demonstrate the utility of impact forecasting: "),
                                              h5("see page ``Simulating impact''"),
                                              br(),
                                              p(style="text-align: justify;","On this page, you can use the sidebar to select the effect of investments on returns, the annual effect of returns, and potential lag times for a) interventions to take effect or b) for new policies to be implemented with certain funding organzations (e.g., The World Bank)"),
                                              br(),
                                              h3("We compare two scenarios: "),
                                              h5("1) Scenario A - one funder and 2) Scenario B - a coalition of funders "),
                                              br(),
                                              img(src="cartoon.png", height=450, width=750),
                                              br(),
                                              br(),
                                              br(),
                                              
                                    )
                           ),

                tabPanel("Evaluating project impact",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Funder information", 
                                      selectInput(inputId = "cepf",
                                                  label = "CEPF",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "CEPF"),
                                                  selected="Include"),
                                      selectInput(inputId = "gef",
                                                  label = "GEF",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "GEF"),
                                                  selected="Include"),
                                      selectInput(inputId = "worldbank",
                                                  label = "The World Bank",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "The World Bank"),
                                                  selected="Include"),
                                      selectInput(inputId = "europeaid",
                                                  label = "EuropeAid",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "EuropeAid"),
                                                  selected="Include"),
                                      actionButton("realrun", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Funder information", plotOutput("realraw")), #,plotOutput("heatmap")
                             )
                           )),
                               
                           sidebarLayout(
                             sidebarPanel(
                               tabPanel("Parameters", 
                                        selectInput(inputId = "parameter",
                                                    label = "Parameter breakdown",
                                                    choices = c("---Choose one---",
                                                                "Years of project", "Accumulated investment",
                                                                "Number of roads", "Protected areas (binary)",
                                                                "Palmer Drought Severity Index (PDSI)",
                                                                "PDSI rate of change", "Mean spring temperature",
                                                                "Mean spring temp rate of change",
                                                                "Elevation", "Slope"),
                                                    selected="---Choose one---"),
                                        actionButton("parameterrun", "View Plots",
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                               )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Parameters", 
                                        h5("Darker lines represent the intervention, lighter lines are control"), 
                                        plotOutput("parameteroutput"),
                                        h3("Model output"), plotOutput("modoutput"))
                             )
                           ))
                                      
                                   ),
                
                
                tabPanel("Forecasting investments",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting impact by intervention investment",
                                      numericInput(inputId = "protectinvest",
                                                  label = "Total Avoid investments ($ millions)",
                                                  value = 150), 
                                                  #min = 0, max = 500),
                                      sliderInput(inputId = "numprotect",
                                                  label = "Num of Avoid projects",
                                                  value = 100, 
                                                  min = 10, max = 500),
                                      numericInput(inputId = "manageinvest",
                                                  label = "Total Reduce investments ($ millions)",
                                                  value = 100), 
                                                  #min = 0, max = 500),
                                      sliderInput(inputId = "nummanage",
                                                   label = "Num of Reduce projects",
                                                   value = 100,
                                                  min = 10, max = 500),
                                      numericInput(inputId = "restoreinvest",
                                                  label = "Total Reverse investments ($ millions)",
                                                  value = 300), 
                                                  #min = 0, max = 500),
                                      sliderInput(inputId = "numrestore",
                                                   label = "Num of Reverse projects",
                                                   value = 100, 
                                                  min = 10, max = 500),
                                      actionButton("forecastactiontrackrun", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting impact by intervention investment", 
                                        h5("Please be patient. Forecast data can take a minute to build."),
                                          plotOutput("forecastaction"))
                             )
                             )),
                         
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting parameters",
                                      selectInput(inputId = "forecastparameter",
                                                  label = "Parameter breakdown",
                                                  choices = c("---Choose one---",
                                                              "Years of project", "Accumulated investment",
                                                              "Number of roads", "Protected areas (binary)",
                                                              "Palmer Drought Severity Index (PDSI)",
                                                              "PDSI rate of change", "Mean spring temperature",
                                                              "Mean spring temp rate of change",
                                                              "Elevation", "Slope"),
                                                  selected="---Choose one---"),
                                      actionButton("runforecastparameter", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting parameters", 
                                        h5("Darker lines represent the intervention, lighter lines are control"), 
                                        plotOutput("forecastparameterplot"))
                             )
                           ))
                         ),
                           
                           
                tabPanel("Forecasting by user input",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting by user input",
                                      selectInput(inputId = "action",
                                                  label = "Intervention type",
                                                  choices = c("---Choose one---",
                                                              "Avoid",
                                                              "Reduce",
                                                              "Reverse"),
                                                  selected="---Choose one---"),
                                      sliderInput(inputId = "investment",
                                                  label = "Total investment ($ millions)",
                                                  value = 10, 
                                                  min = 0, max = 100),
                                      sliderInput(inputId = "start",
                                                  label = "Start year of project",
                                                  value = 2022, 
                                                  min = 2022, max = 2035, timeFormat = "%Y"),
                                      sliderInput(inputId = "end",
                                                  label = "End year of project",
                                                  value = 2027, 
                                                  min = 2023, max = 2045),
                                      sliderInput(inputId = "rds",
                                                  label = "Change in number of roads",
                                                  value = 10, 
                                                  min = 0, max = 500),
                                      sliderInput(inputId = "pas",
                                                  label = "Change in number of protected areas (binary)",
                                                  value = 0, 
                                                  min = 0, max = 1),
                                      sliderInput(inputId = "temp",
                                                  label = "Change in mean spring temperature",
                                                  value = 2, 
                                                  min = -10, max = 10),
                                      sliderInput(inputId = "pdsi",
                                                  label = "Change in PDSI",
                                                  value = -2, 
                                                  min = -10, max = 10),
                                      actionButton("runforecastuser", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting user inputs",
                                        plotOutput("forecastuser"))
                             )
                           )) 
                ),
                tabPanel("Simulating impact",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Simulating impact",
                                      #sliderInput(inputId = "costeffect",
                                      #           label = "Investment effect",
                                      #          value = 5,
                                      #         min = -10, max = 10),
                                      #sliderInput(inputId = "costsigma",
                                      #           label = "Investment effect SD",
                                      #          value = 1, 
                                      #         min = 0, max = 5),
                                      sliderInput(inputId = "yeareffect",
                                                  label = "Annual effect",
                                                  value = 4, 
                                                  min = -10, max = 10),
                                      sliderInput(inputId = "yearsigma",
                                                  label = "Annual effect SD",
                                                  value = 0.5, 
                                                  min = 0, max = 5),
                                      sliderInput(inputId = "lagtime",
                                                  label = "Delay in returns",
                                                  value = 2, 
                                                  min = 0, max = 10),
                                      sliderInput(inputId = "lagtime_major",
                                                  label = "Large delay in returns (e.g., reforestation)",
                                                  value = 5, 
                                                  min = 0, max = 10),
                                      textOutput("result"),
                                      actionButton("run", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Simulating impact", plotOutput("plots"),
                                        plotOutput("maps"))
                             )
                           ))
                         
                         
                )
                )
                
)


server <- function(input, output) {
  
  get.data <- eventReactive(input$run, {
      
      dffunc(#as.numeric(input$costeffect), as.numeric(input$costsigma),
               as.numeric(input$yeareffect), as.numeric(input$yearsigma),
               as.numeric(input$lagtime), as.numeric(input$lagtime_major),
               findlatlong
      )
      
    })
  
  
  
  get.realdata <- eventReactive(input$realrun, {
    
    realfunc(if(input$cepf=="Include"){"CEPF"}else {"NA"}, 
             if(input$gef=="Include"){"GEF"}else {"NA"},
    if(input$worldbank=="Include"){"The World Bank"}else {"NA"}, 
    if(input$europeaid=="Include"){"EuropeAid"}else {"NA"}, 
    txctldata
    )
    
  })
  
  
  get.realparameter <- eventReactive(input$parameterrun, {
    
    parametername <- if(input$parameter=="Years of project"){"yrs.z_txbycont.png"}else if(input$parameter=="Accumulated investment"){"cost.z_txbycont.png"}else 
      if(input$parameter=="Number of roads"){"rds.z_txbycont.png"}else 
        if(input$parameter=="Protected areas (binary)"){"pas.z_txbycont.png"}else 
          if(input$parameter=="Palmer Drought Severity Index (PDSI)"){"pdsi.z_txbycont.png"}else 
            if(input$parameter=="PDSI rate of change"){"prate.z_txbycont.png"}else 
              if(input$parameter=="Mean spring temperature"){"mst.z_txbycont.png"}else 
                if(input$parameter=="Mean spring temp rate of change"){"mstrate.z_txbycont.png"}else 
                  if(input$parameter=="Elevation"){"elev.z_txbycont.png"}else 
                    if(input$parameter=="Slope"){"slope.z_txbycont.png"}
    
  })
  
  get.foredata <- eventReactive(input$forecastactiontrackrun, {
    
    foreaction(as.numeric(input$protectinvest), as.numeric(input$numprotect), as.numeric(input$manageinvest), 
               as.numeric(input$nummanage),
               as.numeric(input$restoreinvest), as.numeric(input$numrestore),
               brazil) 
   
   })
  
  
  get.forecastparameter <- eventReactive(input$runforecastparameter, {
    
    forecastparametername <- if(input$forecastparameter=="Years of project"){"yrs.z_txbycont_forecast.png"}else if(input$forecastparameter=="Accumulated investment"){"cost.z_txbycont_forecast.png"}else 
      if(input$forecastparameter=="Number of roads"){"rds.z_txbycont_forecast.png"}else 
        if(input$forecastparameter=="Protected areas (binary)"){"pas.z_txbycont_forecast.png"}else 
          if(input$forecastparameter=="Palmer Drought Severity Index (PDSI)"){"pdsi.z_txbycont_forecast.png"}else 
            if(input$forecastparameter=="PDSI rate of change"){"prate.z_txbycont_forecast.png"}else 
              if(input$forecastparameter=="Mean spring temperature"){"mst.z_txbycont_forecast.png"}else 
                if(input$forecastparameter=="Mean spring temp rate of change"){"mstrate.z_txbycont_forecast.png"}else 
                  if(input$forecastparameter=="Elevation"){"elev.z_txbycont_forecast.png"}else 
                    if(input$forecastparameter=="Slope"){"slope.z_txbycont_forecast.png"}
    
  })
  
  get.foreuser <- eventReactive(input$runforecastuser, {
    
    foreuser(as.character(input$action), as.numeric(input$investment), as.numeric(input$start), 
             as.numeric(input$end),
               as.numeric(input$rds), as.numeric(input$pas),
               as.numeric(input$temp), as.numeric(input$pdsi),
               brazil) 
    
  })
  
  get.modelprep <- prepped
  
  
  observeEvent(input$parameterrun, {
    parametername <- get.realparameter()#[[1]]
    
    output$parameteroutput <- renderImage({
      
      parameter <- normalizePath(file.path(paste0("figures/", parametername)))
      
      list(src = parameter, width="1000", height="300")
    }, deleteFile = FALSE)
    
    
  })
  
  output$modoutput <- renderImage({
    
    modoutput <- normalizePath(file.path("figures/modoutput_forestrateofchange_randintslopesadm2&itvn_10yr_costperyr.png"))
    
    list(src = modoutput, width="600", height="400")
  }, deleteFile = FALSE)
  
  
  boundars <- boundars
  
  observeEvent(input$run, {
      output$plots <- renderPlot({
        ci_roi <- get.data()[[1]]
        coal <- get.data()[[2]]
        
        cols <-viridis_pal(option="viridis")(3)
        scenarioa <- ggplot(ci_roi) + geom_smooth(aes(y=totroi, x=year), se=FALSE, col="black") +
          geom_smooth(aes(y=roiperint, x=year, col=intervention), se=FALSE) + theme_bw() + 
          ylab("Annual climate mitigation (PgCO2e/$1M)") + xlab("") +
          labs(col="Action Track") + 
          scale_color_manual(name="Action Track", values=cols, labels=c(aprotect="Avoid", manage="Reduce",
                                                                      restore="Reverse")) +
          ggtitle("Scenario A: single funder")
        
        scenariob <- ggplot(coal) + geom_smooth(aes(y=totroi, x=year), se=FALSE, col="black") +
          geom_smooth(aes(y=roiperint, x=year, col=intervention, linetype=funder), se=FALSE) + theme_bw() + 
          ylab("Annual ROI (PgCO2e/$1M)") + xlab("") +
          labs(col="Action Track", linetype="Funding organization") +
          scale_color_manual(name="Action Track", values=cols, labels=c(aprotect="Avoid", manage="Reduce",
                                                                      restore="Reverse")) +
          ggtitle("Scenario B: coalition")
        
        
        grid.arrange(scenarioa, scenariob, ncol=2)
      })
  
  })
  
  
  observeEvent(input$run, {
    output$maps <- renderPlot({
      ci.sub <- get.data()[[3]]
      coal.sub <- get.data()[[4]]
      
      coords = SpatialPoints(ci.sub[c("long", "lat")], proj4string = crs("+proj=longlat +datum=WGS84"))
      
      sites <- gBuffer( coords, width=ci.sub$sitesize, byid=TRUE )
      sites <- fortify(sites)
      sites$sitesize <- ci.sub$sitesize
      sites$roisite <- ci.sub$roisite
      
      myPalette <- colorRampPalette(brewer.pal(9, "Greens")) #### Gives us a heat map look
      sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, max(coal.sub$roisite))) ### this is the range of ROI data we have
      filled <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, max(coal.sub$roisite))) ### this is the range of ROI data we have
      sized <- scale_size_continuous(range=c(0, 450))
      mapWorld<-fortify(boundars)
      singlemap <- ggplot() +
        geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
                     color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
        geom_polygon(aes(x=sites$long, y=sites$lat, group=sites$group, fill=sites$roisite),
                     alpha = 1) +
        #geom_point(aes(x=ci.sub$long, y=ci.sub$lat, 
         #              color=ci.sub$roisite)) + 
        theme_classic() + ### this removes extra background features from ggplot2
        coord_cartesian(ylim=c(-35,6),xlim=c(-76,-32)) + 
        theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              axis.title = element_blank(),
              panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
        sc + filled + ggtitle("Scenario A: single funder") + 
        labs(fill="Climate mitigation\nper site (PgCO2e/$1M)") + guides(color="none")
      
      coords2 = SpatialPoints(coal.sub[c("long", "lat")], proj4string = crs("+proj=longlat +datum=WGS84"))
      
      sites2 <- gBuffer( coords2, width=coal.sub$sitesize, byid=TRUE )
      sites2 <- fortify(sites2)
      sites2$sitesize <- coal.sub$sitesize
      sites2$roisite <- coal.sub$roisite
      
      coalmap <- ggplot() +
        geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
                     color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
        geom_polygon(aes(x=sites2$long, y=sites2$lat, group=sites2$group, fill=sites2$roisite),
                     alpha = 1) +
        #geom_point(aes(x=coal.sub$long, y=coal.sub$lat, 
            #          size=coal.sub$diff,
            #            color=coal.sub$roisite)) + 
        theme_classic() + ### this removes extra background features from ggplot2
        coord_cartesian(ylim=c(-35,6),xlim=c(-76,-32)) + 
        theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              axis.title = element_blank(),
              panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
        sc + filled + ggtitle("Scenario B: coalition") +
        labs(fill="Climate mitigation\nper site (PgCO2e/$1M)") + guides(color="none")
      
      
      grid.arrange(singlemap, coalmap, ncol=2)
    })
    
  })
  
  
  observeEvent(input$realrun, {
    output$realraw <- renderPlot({
      txctl <- get.realdata()[[1]]
      
      cols <-viridis_pal(option="viridis")(4)
      invest <- ggplot(txctl) + 
        geom_smooth(se=TRUE, aes(x=year, y=(totcostK), col=intervention), 
                    method="auto", level=0.5) +
        theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                             labels=c(aprotect="Avoid",
                                                      manage="Reduce",
                                                      restore="Reverse")) +
        ylab("Total investments ($1K)") + xlab("")
      
      growth <- ggplot(txctl, aes(x=year, y=growthrate, col=intervention)) +
        geom_smooth(method="lm", aes(linetype=as.factor(treat)), level=0.5) + theme_classic() +
        scale_color_manual(name="Intervention", values=cols,
                           labels=c(aprotect="Avoid",
                                    manage="Reduce",
                                    restore="Reverse")) +
        ylab("Annual change in forest cover") + xlab("")  +
        scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                              labels=c("0"="control", "1"="treatment"))
      
      grid.arrange(invest, growth, ncol=2)
      
    })
    
  })
  
  output$heatmap <- renderImage({
    
    heats <- normalizePath(file.path("figures/heatmap_interventionbyfunder.png"))
    
    list(src = heats, width="400", height="300")
  }, deleteFile = FALSE)
  
  output$forecasts <- renderImage({
    
    forecasts <- normalizePath(file.path("figures/forecasts.png"))
    
    list(src = forecasts, width="700", height="300")
  }, deleteFile = FALSE)
  
  
  observeEvent(input$forecastactiontrackrun, {
    output$forecastaction <- renderPlot({
      fore <- get.foredata()[[1]]
      
      preddata <- predict(mod1, fore, allow_new_levels=TRUE, probs=c(0.25, 0.75))
      preddata <- as.data.frame(preddata)
      
      fore <- cbind(fore, preddata)
      fore$lc_ha_rate <- fore$Estimate
      
      fore$tx <- ifelse(fore$intervention%in%c("control protect",
                                               "control manage",
                                               "control restore"), 0, 1)
      
      fore$int <- ifelse(fore$intervention%in%c("protect", "control protect"), "aprotect", fore$intervention)
      fore$int <- ifelse(fore$intervention%in%c("manage", "control manage"), "manage", fore$int)
      fore$int <- ifelse(fore$intervention%in%c("restore", "control restore"), "restore", fore$int)
      
      fore$costperint <- ave(fore$costperyr, fore$int, fore$year, FUN=sum)
      
      cols <-viridis_pal(option="viridis")(4)
      invest.fore <- ggplot(fore) + 
        geom_smooth(se=TRUE, aes(x=year, y=costperint/1000, col=int), 
                    method="auto", level=0.5) +
        theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                             labels=c(aprotect="Avoid",
                                                      manage="Reduce",
                                                      restore="Reverse")) +
        ylab("Total investments ($1K)") + xlab("")
      
      growth.fore <- ggplot(fore, aes(x=year, y=Estimate, col=int)) +
        geom_smooth(method="lm", aes(linetype=as.factor(tx)), level=0.5) + theme_classic() +
        #geom_ribbon(aes(ymin=Q25, ymax=Q75, col=int, fill=int), 
         #           linetype=0, alpha=0.1) +
        scale_color_manual(name="Intervention", values=cols,
                           labels=c(aprotect="Avoid",
                                    manage="Reduce",
                                    restore="Reverse")) +
        #scale_fill_manual(name="Intervention", values=cols,
         #                  labels=c(aprotect="Protect",
          #                          manage="Manage",
           #                         restore="Restore")) +
        ylab("Annual change in forest cover") + xlab("")  +
        scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                              labels=c("0"="control", "1"="treatment"))
      
      grid.arrange(invest.fore, growth.fore, ncol=2)
      
      
      
      bb <- fore
      if(TRUE){
        bb$itvn <- ifelse(bb$intervention=="control manage", 
                          "reduce (zcontrol)", bb$intervention)
        bb$itvn <- ifelse(bb$intervention=="control protect", 
                          "avoid (zcontrol)", bb$itvn)
        bb$itvn <- ifelse(bb$intervention=="control restore", 
                          "reverse (zcontrol)", bb$itvn)
      }
      
      
      bb$itvn <- Hmisc::capitalize(bb$itvn)
      
      bb$itvn <- ifelse(bb$itvn=="Restore", "Reverse", bb$itvn)
      bb$itvn <- ifelse(bb$itvn=="Manage", "Reduce", bb$itvn)
      bb$itvn <- ifelse(bb$itvn=="Protect", "Avoid", bb$itvn)
      
      itvns <- sort(unique(bb$itvn))
      predslist <- c("yrs.z", "cost.z", "rds.z", "pas.z",
                     "pdsi.z", "prate.z", "mst.z", "mstrate.z",
                     "elev.z", "slope.z")
      
      
      origparams <- c("yrsofproject", "costperyr", "numrds", "pas",
                      "pdsi", "pdsirate", "mst", "mstrate",
                      "elev", "slope")
      
      
      labs <- c("Years since\nstart of project", "Investment per year\n($ thousands)", "Number of roads",
                "Protected areas\n(binary)",
                "PDSI", "Annual PDSI\nrate of change", 
                "Mean spring\ntemperature (ºC)", "Mean spring temp\nrate of change",
                "Elevation", "Slope (*100)")
      
      
      itvnnums <- c(1, 3, 5)
      
      bb$costperyr <- bb$costperyr/1000
      
      colz <- rev(brewer.pal(n = 6, name = "Paired"))
      system.time({
        for(i in 1:length(origparams)){ #i=1
          
          for(l in c(1, 3, 5)){ #l=1
            
            #x <- bb[origparams[i]][bb$itvn %in% itvns[c(l,l+1)],]
            #unname(purrr::as_vector(bb[origparams[i]][bb$itvn %in% itvns[c(l,l+1)],]))
            
            mylist <- lapply(itvnnums, function(l) {
              ggplot(bb[bb$itvn %in% itvns[c(l,l+1)],], aes(x=unname(purrr::as_vector(bb[origparams[i]][bb$itvn %in% itvns[c(l,l+1)],])), 
                                                            y=bb$Estimate[bb$itvn %in% itvns[c(l,l+1)]], col=bb$itvn[bb$itvn %in% itvns[c(l,l+1)]])) + 
                geom_smooth(method="lm") + xlab(labs[i]) + ggtitle(unique(bb$itvn[bb$itvn %in% itvns[l]])) +
                ylab("Rate of forest cover change") + theme_classic() + 
                theme(plot.title = element_text(size=10, face="bold"),
                      legend.position = ifelse(unique(bb$itvn[bb$itvn %in% itvns[l]])=="Reverse", 'right', 'none'), 
                      axis.text=element_text(size=9)) + 
                scale_y_continuous(expand = c(0, 0)) + 
                geom_ribbon(aes(ymin=Q25, ymax=Q75, col=itvn, fill=itvn), 
                            linetype=0, alpha=0.4) +
                #coord_cartesian(ylim=c(0,0.6)) +
                scale_colour_manual(name="Intervention", values=colz[c(l, l+1)],
                                    labels=c("intervention", "control")) +
                scale_fill_manual(name="Intervention", values=colz[c(l, l+1)],
                                  labels=c("intervention", "control"))
            })
          }
          png(paste0("figures/", predslist[i], "_txbycont_forecast.png"), width = 1000, height= 400, res=150)
          grid.arrange(grobs = mylist, layout_matrix=rbind(c(1, 1,  2, 2,  3, 3, 3)))
          dev.off()
        }
      })
      
    })
    
  })
    
    observeEvent(input$runforecastparameter, {
      forecastparametername <- get.forecastparameter()#[[1]]
      
      output$forecastparameterplot <- renderImage({
        
        forecastparameter <- normalizePath(file.path(paste0("figures/", forecastparametername)))
        
        list(src = forecastparameter, width="700", height="300")
      }, deleteFile = FALSE)
    
  })
    
    
    observeEvent(input$runforecastuser, {
      output$forecastuser <- renderPlot({
        foreuser <- get.foreuser()[[1]]
        
        preddata <- predict(mod1, foreuser, allow_new_levels=TRUE, probs=c(0.25, 0.75))
        preddata <- as.data.frame(preddata)
        
        fore <- cbind(foreuser, preddata)
        fore$lc_ha_rate <- fore$Estimate
        
        fore$tx <- ifelse(fore$intervention%in%c("control Avoid",
                                                 "control Reduce",
                                                 "control Reverse"), 0, 1)
        
        fore$int <- ifelse(fore$intervention%in%c("Avoid", "control Avoid"), "aprotect", fore$intervention)
        fore$int <- ifelse(fore$intervention%in%c("Reduce", "control Reduce"), "manage", fore$int)
        fore$int <- ifelse(fore$intervention%in%c("Reverse", "control Reverse"), "restore", fore$int)
        
        fore$costperint <- ave(fore$costperyr, fore$int, fore$year, FUN=sum)
        
        cols <-viridis_pal(option="viridis")(4)
        invest.fore <- ggplot(fore) + 
          geom_smooth(se=TRUE, aes(x=year, y=accum.cost/1000, col=int), 
                      method="auto", level=0.5) +
          theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                               labels=c(aprotect="Avoid",
                                                        manage="Reduce",
                                                        restore="Reverse")) +
          ylab("Accumulated investment ($1K)") + xlab("")
        
        growth.fore <- ggplot(fore, aes(x=year, y=Estimate, col=int)) +
          geom_smooth(method="lm", aes(linetype=as.factor(tx)), level=0.5) + theme_classic() +
          #geom_ribbon(aes(ymin=Q25, ymax=Q75, col=int, fill=int), 
          #           linetype=0, alpha=0.1) +
          scale_color_manual(name="Intervention", values=cols,
                             labels=c(aprotect="Avoid",
                                      manage="Reduce",
                                      restore="Reverse")) +
          #scale_fill_manual(name="Intervention", values=cols,
          #                  labels=c(aprotect="Protect",
          #                          manage="Manage",
          #                         restore="Restore")) +
          ylab("Annual change in forest cover") + xlab("")  +
          scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                                labels=c("0"="control", "1"="treatment"))
        
        grid.arrange(invest.fore, growth.fore, ncol=2)
        
        
      })
      
    })
  
  
}

shinyApp(ui = ui, server = server)

#runApp("~/Documents/git/ciapp/")
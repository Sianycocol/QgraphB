## ----setup, include=FALSE-----------------------------------------------------
library(QgraphB)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(miniUI)

## -----------------------------------------------------------------------------
BE <- read.csv("BE Quantification.csv", stringsAsFactors = F)

## ----echo=FALSE, out.width = '100%'-------------------------------------------
knitr::include_graphics("Raw data.png")

## -----------------------------------------------------------------------------
## Remove individuals, type of which are missing, neither KO nor ctrl
BE1 <- BE %>% filter(Type == "KO" | Type == "ctrl")
## Choose parameters for future use
BE2 <-BE1[, c("Number", "Type", "Cell", "Row", "Label", "Mean.Grey")]
## Filter out different KO objectives
KO <- BE2 %>% filter(Type == "KO")

## -----------------------------------------------------------------------------
## Error report function for checking if the Area is less than 0.126 or if the Channel exactly equals to 2, otherwise, report FALSE
error_report(BE1)

## -----------------------------------------------------------------------------
## Subfunction 2-1: Example of calculating the intensity(mean.grey) ratio of row1/row2 in the range of cells from KO number 1.
KO1 <- KO %>% filter(Number == 1)
KO1_r1r2_ratio(KO1)
## Subfunction 2-2: In order to do that, KO1_r1r2_ratio function needs to be combined into a new function which is used to filter out various KOs. Results will show row1/row2 ratio of every cells from various KOs.
KO_r1r2_ratio(KO)
## Subfunction 2-3: Similar as KO_r1r2_ratio, ctrl_r1r2_ratio can be calculated in the same way
ctrl <- BE2 %>% filter(Type == "ctrl")
ctrl_r1r2_ratio (ctrl)
## Function 2
KOratio <- r1r2_ratio("KO")
ctrlratio <- r1r2_ratio("ctrl")

## -----------------------------------------------------------------------------
## Function 3: create Table function, including unlist, cbind, rbind and name the data frame
Table("ctrl", "KO")

## -----------------------------------------------------------------------------
## Function 4: Create new dataframe which is used for ggplotting,including as.vector, create data frame with different lengths, collapse the dataframe and rebuild new charactor variables from row to columns by using melt() 
newdf <- newdf(KOratio, ctrlratio)
newdf 

## -----------------------------------------------------------------------------
## Function 5: violinplot-Create customized violinplot function to plot "ratio value in control and knockouts"
violinplot(newdf)
## Function 6: boxplot-Create customized boxplot function to plot "ratio value in control and knockouts", including customized color, angle, heights of xy coordinates, size and color of texts and title
boxplot(newdf)

## -----------------------------------------------------------------------------
## Function 7: t_test_violinplot
t_test_violinplot(newdf)
## Function 8: t_test_boxplot
t_test_boxplot(newdf)


## -----------------------------------------------------------------------------
## By dragging mouse on the shiny plot, run pickpoints() will give you the detailed data that chosen by your mouse.
pickpoints <- function(){
  ui<-miniPage(
    gadgetTitleBar("Select Points by Dragging your Mouse"),
    miniContentPanel(
      plotOutput("plot",height = "100%", brush = "brush")
    )
  )
  server<- function (input, output, session){
    output$plot <- renderPlot({
      plot(newdf$genotype, newdf$value, main = "Ratio value in control and knockouts!",
      xlab = "genotype", ylab ="value")
    })
    observeEvent(input$done,{
      stopApp(brushedPoints(newdf, input$brush,
                            xvar= "genotype", yvar = "value"))
    })
  }
  runGadget(ui, server)
}



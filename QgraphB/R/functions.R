#' error_report
#'
#' @param df data frame
#'
#' @return
#' @export
#'
#' @examples
#' error_report <- error_report(BE1)
error_report <- function(df){
  ifelse(max(df$Area) <= 0.126, TRUE, FALSE)
  ifelse(mean(df$Channel) == 2, TRUE, FALSE)
}

#' KO1_r1r2_ratio
#'
#' @param df data frame in  KO-1 objectives
#'
#' @return data
#' @export
#'
#' @examples
#' KO1_r1r2_ratio(KO1)
KO1_r1r2_ratio <- function(df) {
  r1r2_ratio <- 1
  for(n in 1:max(df$Cell)){
    row1 <- df %>% filter (Cell == n, Row == 1)
    row2 <- df %>% filter (Cell == n, Row == 2)
    r1r2_ratio[n] <- mean(row1$Mean.Grey) / mean(row2$Mean.Grey)}
  data <- data.frame(r1r2_ratio)
  return(data)

}

#' KO_r1r2_ratio
#'
#' @param df data frame in  various KO objectives
#'
#' @return
#' @export
#'
#' @examples KO_r1r2_ratio(df)
KO_r1r2_ratio <- function(df){
  x <- 1
  storage <- numeric(max(df$Number))
  for(x in 1:max(df$Number)){
    KOx <- df %>% filter(Number == x)
    KOx_r1r2_ratio <- function(KOx) {
      max <- max(KOx$Cell)
      r1r2_ratio <- 1
      for(n in 1:max){
        row1 <- KOx %>% filter (Cell == n, Row == 1)
        row2 <- KOx %>% filter (Cell == n, Row == 2)
        r1r2_ratio[n] <- mean(row1$Mean.Grey) / mean(row2$Mean.Grey)}
      data <- data.frame(r1r2_ratio)
      return(data)
    }
    storage[x] <- KOx_r1r2_ratio(KOx)
  }
  return(storage)
}

#' ctrl_r1r2_ratio
#'
#' @param df data frame in  various KO objectives
#'
#' @return
#' @export
#'
#' @examples ctrl_r1r2_ratio(df)
ctrl_r1r2_ratio <- function(df){
  x <- 1
  storage <- numeric(max(df$Number))
  for(x in 1:max(df$Number)){
    ctrlx <- df %>% filter(Number == x)
    ctrlx_r1r2_ratio <- function(ctrlx) {
      max <- max(ctrlx$Cell)
      r1r2_ratio <- 1
      for(n in 1:max){
        row1 <- ctrlx %>% filter (Cell == n, Row == 1)
        row2 <- ctrlx %>% filter (Cell == n, Row == 2)
        r1r2_ratio[n] <- mean(row1$Mean.Grey) / mean(row2$Mean.Grey)}
      data <- data.frame(r1r2_ratio)
      return(data)
    }
    storage[x] <- ctrlx_r1r2_ratio(ctrlx)
  }
  return(storage)
}

#' r1r2_ratio
#'
#' @param df data frame in  various ctrl or KO objectives
#'
#' @return
#' @export
#'
#' @examples r1r2_ratio("df")
r1r2_ratio <- function(df){
  df <- BE2 %>% filter(Type == df)
  x <- 1
  storage <- numeric(max(df$Number))
  for(x in 1:max(df$Number)){
    dfx <- df %>% filter(Number == x)
    dfx_r1r2_ratio <- function(dfx) {
      max <- max(dfx$Cell)
      r1r2_ratio <- 1
      for(n in 1:max){
        row1 <- dfx %>% filter (Cell == n, Row == 1)
        row2 <- dfx %>% filter (Cell == n, Row == 2)
        r1r2_ratio[n] <- mean(row1$Mean.Grey) / mean(row2$Mean.Grey)}
      data <- data.frame(r1r2_ratio)
      return(data)
    }
    storage[x] <- dfx_r1r2_ratio(dfx)
  }
  return(storage)
}

#' Table
#'
#' @param df1 data frame
#' @param df2 data frame
#'
#' @return
#' @export
#'
#' @examples Table("ctrl", "KO")
Table <- function(df1, df2){
  Mean_df1 <- mean(unlist(r1r2_ratio(df1)))
  STD_df1 <- sd(unlist(r1r2_ratio(df1)))
  Mean_df2 <- mean(unlist(r1r2_ratio(df2)))
  STD_df2 <- sd(unlist(r1r2_ratio(df2)))
  cdf1 <- cbind(Mean_df1, STD_df1)
  cdf2 <- cbind(Mean_df2, STD_df2)
  df1and2 <- data.frame(rbind(cdf1, cdf2))
  colnames(df1and2) <- c("Mean", "STD")
  rownames(df1and2) <- c(df1, df2)
  df1and2
}

#' newdf
#'
#' @param KOratio data frame from KO objectives
#' @param ctrlratio data frame from ctrl objectives
#'
#' @return
#' @export
#'
#' @examples newdf <-newdf(KOratio, ctrlratio)
newdf <-function(KOratio, ctrlratio){
  KO <-as.vector(unlist(KOratio))
  ctrl <-as.vector(unlist(ctrlratio))
  n <- max(length(KO), length(ctrl))
  length(KO) <- n
  length(ctrl) <- n
  newdf <- cbind(KO, ctrl)
  newdf <- melt(newdf)
  newdf <- newdf %>% drop_na()
  colnames(newdf) <- c("", "genotype", "value")
  return(newdf)
}

#' box_plot
#'
#' @param df data frame
#'
#' @return
#' @export
#'
#' @examples boxplot(df)
boxplot <- function(df){
  grey_theme <- theme(axis.text.x = element_text(colour="grey20", size = 12,
                                                 angle = 45, hjust = 0.5,
                                                 vjust = 0.5),
                      axis.text.y = element_text(colour = "grey20", size = 12),
                      text=element_text(size = 16))
  ggplot(df, aes(x = genotype,y = value))+
    geom_boxplot() +
    ggtitle("Ratio value in control and knockouts")+
    grey_theme
}


#' violinplot
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples violinplot(newdf)
violinplot <- function(df){
  ggviolin(df, x = "genotype", y = "value", fill = "genotype", alpha=0.3, add = "boxplot", palette = c("#00AFBB", "#E7B800"), title="Ratio value in control and knockouts")
}

#' t_test_boxplot
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples t_test_boxplot(newdf)
t_test_boxplot<- function(df){
  boxplot(df) +
    geom_hline(yintercept = mean(df$value), linetype = 2)+
    stat_compare_means(method = "t.test", label.y = (max(df$value)+0.5) )+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = ".all.")
}

#' t_test_violinplot
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples t_test_violinplot(newdf)
t_test_violinplot<- function(df){
  violinplot(df) +
    geom_hline(yintercept = mean(df$value), linetype = 2)+
    stat_compare_means(method = "t.test", label.y = (max(df$value)+0.5) )+
    stat_compare_means(label = "p.signif", label.y = (max(df$value)+0.3), method = "t.test",
                       ref.group = ".all.")
}

#' pickpoints
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples pickpoints(df)
pickpoints <- function(df){
  ui<-miniPage(
    gadgetTitleBar("Select Points by Dragging your Mouse"),
    miniContentPanel(
      plotOutput("plot",height = "100%", brush = "brush")
    )
  )
  server<- function (input, output, session){
    output$plot <- renderPlot({
      plot(df$genotype, df$value, main = "Ratio value in control and knockouts!",
           xlab = "genotype", ylab ="value")
    })
    observeEvent(input$done,{
      stopApp(brushedPoints(df, input$brush,
                            xvar= "genotype", yvar = "value"))
    })
  }
  runGadget(ui, server)
}

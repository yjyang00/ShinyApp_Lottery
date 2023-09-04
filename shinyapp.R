library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)

lottery=readRDS("lottery.rds")

## past drawings part
year <- c(min(lottery$year):max(lottery$year))
month <- c(min(lottery$month):max(lottery$month))
day <- c(min(lottery$day):max(lottery$day))

# set up initial start date and end date
user_choice1 <- c(lottery$year[nrow(lottery)], 
                  lottery$month[nrow(lottery)],
                  lottery$day[nrow(lottery)]) 

user_choice2 <- c(lottery$year[1], 
                  lottery$month[1],
                  lottery$day[1])

lottery_pastdraw <- function(startdate, enddate){
  
  lottery_past = lottery %>%
    mutate(Date = as.Date(paste(year, month, day, sep = "-"))) %>%
    filter(Date >= startdate & Date <= enddate) %>%
    mutate(Winning.Numbers = paste(first, second, third, fourth, fifth,  sep="-")) %>%
    mutate(Megaplier = paste0(Megaplier, "x")) %>%
    mutate(Estimated.Jackpot.MM = paste0("$", Estimated.Jackpot.MM, " Million")) %>%
    select(Date, Winning.Numbers, Mega.Ball, Megaplier, Estimated.Jackpot.MM, Jackpot.Winners, detail,link)
  
  lottery_detail = lottery_past$detail
  
  return(list(past = lottery_past %>% select(-detail), detail = lottery_detail))
}


## Drawing Statistics Part
# choose 10 most popular numbers (among first 5)
find_5num=function(startdate, enddate){
  # create a matrix to store the variable
  t=matrix(NA,ncol=4,nrow=10)
  # keep the useful information
  lottery_past = lottery %>%
    mutate(Date = as.Date(paste(year, month, day, sep = "-"))) %>%
    filter(Date >= startdate & Date <= enddate) %>%
    select(first, second, third, fourth, fifth) %>%
    unlist()
  # calculate the frequencies and store them acorrdingly
  nums <- sort(summary(as.factor(lottery_past)), decreasing = TRUE)
  t[, 1] <- names(nums)[1:10]
  t[, 2] <- nums[1:10]
  t[, 3] <- paste(round(nums[1:10]/sum(nums), 4)*100, "%")
  t[, 4] <- paste(round(nums[1:10]/(sum(nums)/5), 4)*100, "%")
  # reanme columns
  colnames(t) <- c("Number", "Drawn.Times", "% of Drawing Numbers", "% of Draws")
  # return desired values
  return(t)
}

# choose 5 most popular numbers (among mega balls)
find_mega=function(startdate, enddate){
  # create a matrix to store the variable
  t=matrix(NA,ncol=3,nrow=5)
  startdate <- as.Date("2020-12-01")
  enddate <- as.Date("2022-12-01")
  # keep the useful information
  lottery_past = lottery %>%
    mutate(Date = as.Date(paste(year, month, day, sep = "-"))) %>%
    filter(Date >= startdate & Date <= enddate) %>%
    select(Mega.Ball) %>%
    unlist()
  # calculate the frequencies and store them acorrdingly
  nums <- sort(summary(as.factor(lottery_past)), decreasing = TRUE)
  t[, 1] <- names(nums)[1:5]
  t[, 2] <- nums[1:5]
  t[, 3] <- paste(round(nums[1:5]/sum(nums), 4)*100, "%")
  # rename columns
  colnames(t) <- c("Number", "Drawn.Times", "% of Draws")
  # return desired values
  return(t)
}

# Trend of Most Popular Numbers Over Year
find_number_trend=matrix(NA,ncol=3,nrow=length(unique(year)))
for (i in 1:length(2003:2023)){
  df=lottery %>%
    filter(year==2002+i)
  find_number_trend[i,]=tail(names(sort(table(unlist(c(df[,4:8]))))), 3)
}
find_number_trend=as.data.frame(find_number_trend)

find_number_trend=cbind(c(2003:2023),find_number_trend)%>%
  rename("year"="c(2003:2023)",
         "top_1"="V1",
         "top_2"="V2",
         "top_3"="V3")


# get probability for generating
FreqClass=function(freq){
  result=vector()
  q1=quantile(freq,0.25)
  q2=quantile(freq,0.75)
  for (i in 1:length(freq)){
    if(freq[i]>=q2) result[i]=1.2
    else if(freq[i]>=q1) result[i]=1
    else result[i]=0.8
  }
  return(result)
}

# probability for winning numbers
col = c("first","second","third","fourth","fifth")
numbers = lottery%>%filter(year>2018)%>%select(all_of(col))%>%
  pivot_longer('first':'fifth',names_to="order",values_to="number")
numberfreq = table(numbers$number) %>% 
  as.data.frame()
numberfreq$Var1 = as.numeric(numberfreq$Var1)
colnames(numberfreq) = c("Number","Frequency")
numberfreq = numberfreq %>% 
  mutate(Class=FreqClass(numberfreq$Frequency)) %>%
  mutate(Probability=Class/sum(Class))

# probability for mega Ball
ballfreq = table(lottery%>%filter(year>=2018)%>%select(Mega.Ball)) %>%
  as.data.frame()

#ballfreq$Var1 = as.numeric(ballfreq$Var1)
colnames(ballfreq) = c("Number","Frequency")
ballfreq = ballfreq %>% 
  mutate(Class=FreqClass(ballfreq$Frequency)) %>%
  mutate(Probability=Class/sum(Class))

# generate function
Generate=function(){
  return(c(sort(sample(1:70,size=5,prob=numberfreq$Probability)),
           sample(1:25,size=1,prob=ballfreq$Probability)))
}

# shinyApp
shinyApp(
  ui = dashboardPage(
    skin = 'blue',
    header = dashboardHeader(
      titleWidth = 300,
      title = "Texas Lottery"
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(id = "sidebarid",
                  menuItem("Dashboard", tabName = "dashboard"),
                  menuItem("Past Drawings", tabName = "pastdraw"),
                  menuItem("Lottery Statistcs", tabName = "stats"),
                  menuItem("Generate New Winning Numbers", tabName = "generate"),
                  conditionalPanel(
                    'input.sidebarid == "pastdraw"',
                    h4("Start Date"),
                    div(style = "display: flex;",
                        selectInput("year1", "Year", choices = year, 
                                    selected = user_choice1[1]),
                        selectInput("month1", "Month", choices = month, 
                                    selected = user_choice1[2]),
                        selectInput("day1", "Day", choices = day, 
                                    selected = user_choice1[3])),
                    h4("End Date"),
                    div(style = "display: flex;",
                        selectInput("year2", "Year", choices = year, 
                                    selected = user_choice2[1]),
                        selectInput("month2", "Month", choices = month, 
                                    selected = user_choice2[2]),
                        selectInput("day2", "Day", choices = day, 
                                    selected = user_choice2[3])),
                    actionButton(inputId = 'confirmed',
                                 label = 'Submit')
                  ),
                  conditionalPanel(
                    'input.sidebarid == "stats"',
                    h4("Range of Drawings to Analyze"),
                    checkboxInput("notadapt", "Not Follow Previous Input"),
                    conditionalPanel('input.notadapt == true',
                                     h4("Start Date"),
                                     div(style = "display: flex;",
                                         selectInput("year11", "Year", choices = year, 
                                                     selected = user_choice1[1]),
                                         selectInput("month11", "Month", choices = month, 
                                                     selected = user_choice1[2]),
                                         selectInput("day11", "Day", choices = day, 
                                                     selected = user_choice1[3])),
                                     h4("End Date"),
                                     div(style = "display: flex;",
                                         selectInput("year22", "Year", choices = year, 
                                                     selected = user_choice2[1]),
                                         selectInput("month22", "Month", choices = month, 
                                                     selected = user_choice2[2]),
                                         selectInput("day22", "Day", choices = day, 
                                                     selected = user_choice2[3]))),
                    h4("Select the statistics you need:"),
                    checkboxInput("fivenumber", "Summary of First 5 Numbers", value = FALSE, width = NULL),
                    checkboxInput("visual1", "Historgram of First 5 Number Summary", value = FALSE, width = NULL),
                    checkboxInput("megaball", "Summary of Mega Balls", value = FALSE, width = NULL),
                    checkboxInput("visual2", "Historgram of Mega Ball Summary", value = FALSE, width = NULL),
                    checkboxInput("trend", "Trend of Most Popular Numbers Over Year", value = FALSE, width = NULL),
                    actionButton(inputId = 'show',
                                 label = 'Submit')
                  ),
                  conditionalPanel(
                    'input.sidebarid == "generate"',
                    h4("Amount of New Numbers"),
                    div(style = "display: flex;",
                        numericInput("amount","amount",value=1)),
                    actionButton(inputId = 'go',
                                 label = 'Submit')
                  )
      )
    ),
    
    # body ----
    dashboardBody(
      tabItems(
        # DashBoard
        tabItem(tabName = "dashboard", 
                div(tags$b("Mega Millions Summary and Analysis", style = "color: black; font-size: 30px;")),
                div(tags$b("By STA523L Team LLPY", style = "color: black; font-size: 18px")),
                br(),
                div(tags$b("What Is Mega Millions?", style = "color: black; font-size: 22px")),
                "Mega Millions (originally known as The Big Game in 1996 and renamed, temporarily, to The Big Game Mega Millions six years later) is an American multi-jurisdictional lottery game; as of January 30, 2020, it is offered in 45 states, the District of Columbia, and the U.S. Virgin Islands. The first (The Big Game) Mega Millions drawing was in 2002. What is now Mega Millions initially was offered in six states; the logo for all versions of the game following the retirement of The Big Game name featured a gold-colored ball with six stars to represent the game's initial membership, although some lotteries insert their respective logo in the ball.",
                br(),
                "Mega Millions is drawn at 11 p.m. Eastern Time on Tuesday and Friday evenings, including holidays.[5] Mega Millions is administered by a consortium of its 12 original lotteries;[6] the drawings are held at the studios of WSB-TV in Atlanta, Georgia, supervised by the Georgia Lottery.[7] The hosts are John Crow, Carol Blackmon and Adrian Wollford.",
                br(),
                div(tags$b("How to Play Mega Millions?", style = "color: black; font-size: 22px")),
                "Players may pick six numbers from two separate pools of numbers - five different numbers from 1 to 70 (the white balls) and one number from 1 to 25 (the gold Mega Ball) - or select Easy Pick/Quick Pick. You win the jackpot by matching all six winning numbers in a drawing.",
                br(),
                "Until further notice, the starting jackpot will vary based on sales and will be announced before each drawing.",
                br(),
                "There are a total of nine ways to win a prize in Mega Millions, ranging from the jackpot down to $2:",
                img(src="https://www.texaslottery.com/export/sites/lottery/Images/MegaMillions_Megaplier_Prizechart_enlarged.png", 
                    height = '340px', width = '800px', align = "center"),
                div(tags$b("What Our App Can Do?", style = "color: black; font-size: 22px")),
                "In the Dashboard section, you may get a general impression of how the lottery works.", 
                br(),
                "In the Past drawing section, you may enter the start date and end date of the records you want to retrieve. Then simply click 'Submit', the app would provide you the information of previous drawings, including drawing date, winning numbers, estimated Jackpot, etc. You may also click the hyperlink to get detailed information about the winning Jackpot.",
                br(),
                "In the Drawing statistics section, you can get a series of summary statistics of past drawings, such as the most popular numbers and most popular Mega ball numbers. We also provide some visualization here. All these statistics might be helpful for you to choose a winning number.",
                br(),
                "In the Generate New Winning Numbers section, our app would generate a random number of Mega Millions based on the summary of all previous past drawings, which may slightly improve the winning drawing possibility. The users just need to enter the number of draws they wish to get and click submit, they can get the numbers generated by our app.",
                br(), br(),
                "For more information, please check: ", tags$a("Mega Millions Official Website", href="https://www.megamillions.com/", target="https://www.megamillions.com/"),
                br(), br(),
                "Reference:", br(), "https://www.megamillions.com/",
                br(),
                "          https://en.wikipedia.org/wiki/Mega_Millions",
                br(),
                "          https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/index.html"),
        # Past Drawings
        tabItem(tabName = "pastdraw", 
                "On this page, you can retrieve the records of past drawings from 2003 to 2022.",
                br(), br(),
                dataTableOutput("pastdraw_table")),
        # Drawing Statistics
        tabItem(tabName = "stats", 
                "This page presents statistics and visualizations.",
                br(), br(),uiOutput("items1"),uiOutput("items2"),uiOutput("items3"),uiOutput("items4"),uiOutput("items5")),
        # Generate New winning Numbers
        tabItem(tabName = "generate",
                "In this page, you can generate new potential winning numbers to buy your lotteries.",
                uiOutput("numbers"))
      )
    )
  ),
  server = function(input, output) {
    button <- function(tbl){
      function(i){
        sprintf(
          '<button id="button_%s_%d" type="button" onclick="%s">Click me</button>', 
          tbl, i, "Shiny.setInputValue('button', this.id);")
      }
    }
    
    observeEvent(input$confirmed,{
      startdate <- as.Date(paste0(input$day1, "-", input$month1, "-", input$year1), '%d-%m-%Y')
      enddate <- as.Date(paste0(input$day2, "-", input$month2, "-", input$year2), '%d-%m-%Y')
      
      if (startdate > enddate){
          showModal(modalDialog(
            title = tags$b("Error"), "Invalid date: please check your start date and end date!", easyClose = TRUE
          ))
      }
      else{
      
      df1 <- lottery_pastdraw(startdate = startdate, enddate = enddate)$past
      
      df1 <- cbind(df1,
                   Detail = sapply(1:nrow(df1), button("pastdraw_table")), 
                   stringsAsFactors = FALSE)
      
      if (is.null(df1)) {
        showNotification(type = 'error', 'Some Date Are Missing')
        output$pastdraw_table <- NULL
      } else {
        if (nrow(df1) == 0) {
          showNotification(type = 'warning', 'No Result')
          output$pastdraw_table <- NULL
        } else {
          showNotification(type = 'default', 'Success')
          output$pastdraw_table <- renderDataTable(df1 %>% select(-link), escape = FALSE)
        }
      }
    }
  })
    
    observeEvent(input[["button"]], {
      
      splitID <- as.numeric(strsplit(input[["button"]], "_")[[1]][4])
      
      startdate <- as.Date(paste0(input$day1, "-", input$month1, "-", input$year1), '%d-%m-%Y')
      enddate <- as.Date(paste0(input$day2, "-", input$month2, "-", input$year2), '%d-%m-%Y')
      
      df1 <- lottery_pastdraw(startdate = startdate, enddate = enddate)$past
      df2 <- lottery_pastdraw(startdate = startdate, enddate = enddate)$detail
      
      showModal(modalDialog(
        title = tags$b(paste("Detailed Information on", df1$Date[[splitID]])),
        renderDataTable(df2[[splitID]]),
        tags$p(
          tags$b("Web Link :"),
          tags$a(href=df1$link[[splitID]], df1$link[[splitID]], style = "font-size: 15px")
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"))
      ))
    })
    
    state = reactiveValues(
      observers = list()
    )
    
    #statistics summary
    
    observeEvent(input$show,{
      output$items1<-renderUI({
        if (input$fivenumber){
          dataTableOutput("table1")
          
        }
      })
      output$items2<-renderUI({
        
        if (input$visual1){
          plotOutput("plot1")
        }
      })
      output$items3<-renderUI({
        if (input$megaball){
          dataTableOutput("table2")
        }
      })
      output$items4<-renderUI({
        if (input$visual2){
          plotOutput("plot2")
          
        }
      })
      output$items5<-renderUI({
        if (input$trend){
          dataTableOutput("table3")
        }
      })
      
      if(input$notadapt){
        startdate <- as.Date(paste0(input$day11, "-", input$month11, "-", input$year11), '%d-%m-%Y')
        enddate <- as.Date(paste0(input$day22, "-", input$month22, "-", input$year22), '%d-%m-%Y')
      }else{
        startdate <- as.Date(paste0(input$day1, "-", input$month1, "-", input$year1), '%d-%m-%Y')
        enddate <- as.Date(paste0(input$day2, "-", input$month2, "-", input$year2), '%d-%m-%Y')
      }
      
      
      num_plot=find_5num(startdate = startdate, enddate = enddate)
      
      mega_plot=find_mega(startdate = startdate, enddate = enddate)
      
      if (input$fivenumber){
        output$table1= renderDataTable({
          num_plot
        })
        
      }else{
        output$table1=  NULL
      }
      
      if (input$visual1){
        output$plot1=renderPlot(
          ggplot(as.data.frame(num_plot))+
            aes(x=reorder(as.numeric(Number),-as.numeric(Drawn.Times)),y=as.numeric(Drawn.Times))+
            geom_col(position = 'dodge')+
            xlab("Number")+
            ylab("frequency")+
            theme_stata() + scale_color_stata()+
            ggtitle("Top 10 Number for five winning number")
        )
      }else{
        output$plot1=  NULL
      }
      
      if (input$megaball){
        output$table2= renderDataTable({
          mega_plot
        })
      }else{
        output$table2=  NULL
      }
      
      if (input$visual2){
        output$plot2=renderPlot(
          ggplot(as.data.frame(mega_plot),
                 aes(x=reorder(as.numeric(Number),-as.numeric(Drawn.Times)),y=as.numeric(Drawn.Times)))+
            geom_col(position = 'dodge')+
            xlab("Number")+
            ylab("frequency")+
            theme_stata() + scale_color_stata()+
            ggtitle("Top 5 Number for MegaBall Number ")
        )
        
      }else{
        output$plot2= NULL
      }
      
      if (input$trend){
        output$table3= renderDataTable(
          find_number_trend
        )}else{
          output$table3= NULL
        }
      
      
    }
    )
    
    #generate number
    observeEvent(input$go,{
      req(state$observers)
      
      # Destroy existing observers
      purrr::walk(
        state$observers,
        ~ .x$destroy()
      )
      
      ui_elems = purrr::map(
        seq_len(input$amount), 
        function(i) {
          fluidRow(
            div(Generate()[1],"-",Generate()[2],"-",Generate()[3],"-",
                Generate()[4],"-",Generate()[5],"-",Generate()[6])
          )
        }
      )
      output$numbers = renderUI(fluidPage(ui_elems))
    }
    
    )
    
  }
)



# References:
# https://stackoverflow.com/questions/58821886/adding-buttons-to-shiny-dt-to-pull-up-modal

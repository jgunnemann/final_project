# Load the packages 
library(markdown)
library(shiny)
library(tidyverse)
library(dplyr)
library(fivethirtyeight)
library(ggplot2)
library(gt)
library(fivethirtyeight)
library(readxl)
library(janitor)
library(fastDummies)
library(rvest)
library(tidyselect)
library(stringr)
library(ggplot2)
library(scales)
library(skimr)
library(broom)

# Load packages for using input Stata file
library(foreign)
library(readstata13)

#Load a STATA dataset provided by the Government of Chad/local World Bank Office
rentals <- read.dta13("raw-data/rentals_reduced.dta", nonint.factors = TRUE)

#Create graph as per main Rmd. See further comments there. 

graph_1 <- rentals %>%
    drop_na(treated) %>%
    select(start_date, city, yrmonth, Berlin, count_month, cost_m2_month_reg_avg_t_B, cost_m2_month_reg_avg_c_B, cost_m2_month_reg_avg_c_H, treated,  cost_m2_month_reg_avg, cost_m2) %>%
    group_by(yrmonth, treated) %>%
    mutate(month_treated = mean(cost_m2))


#Execute diff-in-diff estimation for Berlin policy, regressing log rental prices
#on treatment, timing, and interaction term, on a series of controls

options(scipen = 999)
did_model <- lm(ln_cost_m2 ~ treated_missing*timing_announced + 
                  Berlin + 
                  post_code + 
                  trend + trend_squared + 
                  nrooms + 
                  built_1918 + built_1918_49 + built_1950_64 + built_1965_72 + built_1973_90 + built_1991_03 + built_2003_13 + built_2014 
                , data = rentals)  

#Take the results from the regression in tidy format and extra estiamte as well as CI

result <- did_model  %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

# I change the format to 3 digits max. 

is.num <- sapply(result, is.numeric)
result[is.num] <- lapply(result[is.num], round, 3)

# And now I finally prepare the final gt table

gt_result <- result %>%
  gt  %>%
  tab_header(title = "Effect of Housing Policy on Rental Prices in Berlin", subtitle = "Logged housing rental prices") %>% 
  cols_label(term = "Variable", 
             estimate = "Estimate", 
             conf.low = "Lower bound", 
             conf.high = "Upper bound")

# Now I set up the user interface in Shiny.

ui <- fluidPage(
    titlePanel(""),
    navbarPage("",
               
               #I start with the first panel, which is the About section
               
               tabPanel("About",
                        
                        h1(tags$b("Did Berlin do well to set rental prices?"), align = "center"),
                        p(tags$em("A difference-in-difference study to assess a new policy"), align = "center"), br(),
                        
                        # Load in the first image for the front page.
                        img(src = "Berlin.png", height = 500, width = 800),
                      
                        h3("An unprecedent aggressive housing policy"),
                        
                        p("Over the past ten years, Berlin's housing prices have grown like in no other global city worldwide. 
                        Rental apartment prices have roughly trippled over the last ten years. This has caused outrage amongst tenants 
                        and put the topic on the very top of agendas for local policy makers. 
                        The 2015 rent control policies appeared to have only further acclerated the price trends. 
                        Last year, the Berlin Senate therefore proposed a rather radical idea: 
                        It decided to put a cap on rental prices and to force landlords to repay tenants for any rental prices above that cap. 
                        This applies to all apartments in Berlin, regardless of their condition or location, except those built after 2014. 
                        The new policy was announced in June 2019 but only came into effect a few weeks ago, on 20 February 2020. What has 
                        happened since? Are landlords complying? Have prices decreased? What happened to volume?"),
                        
                        h3("Context"),
                        
                        p("Rising apartment rental prices have become Berlin’s most prominent public policy issue. Last fall, 
                        tens of thousand of people in Berlin took to the streets to demonstrate against fast increasing apartment rental prices. 
                        The demonstrators claim the rent hike has made housing unaffordable and is spurring gentrification, attacking the core of 
                        what they believe makes Berlin special: a “poor but sexy” city (The Economist, 2006) that draws a diverse crowd that 
                        chooses Berlin for its comparatively low cost of living. Backed by several leading political parties, demonstrators 
                        demand further tightening of regulation as well as the expropriations of leading large real estate investment funds (Deutsche Welle, 2019). 
                        These calls have caused anxiety amongst investors and led large funds to divest from their Berlin portfolios."),
                        
                        p(""),
                        
                        p("Since 2015, the coalition has tried to slow down the hike in apartment rental prices. Concerns over increasing rental 
                        prices have led the leftist city coalition to slow down rental price increases by imposing a restriction on by how much 
                        landlords can raise rents (henceforth “rent brake”). Landlords were allowed to increase rent no more than 15% every three years.
                        When new tenants moved in, landlords could not charge more than 10 percent more than prices listed in a 
                        district-level rental index. However, a few exceptions – such as when apartments were rented furnished or when they 
                        were recently renovated – led some landlords to find ways to charge rents high above the threshold. 
                        The loopholes have made the regulatory attempts largely ineffective, with growth in both real estate prices and rental 
                        prices further accelerating since 2015 (The Economist, 2020, Statista, 2019)."),
                  
                        p(""),
                        
                        p("In 2020, the coalition took a much more drastic measure by imposing a citywide maximum apartment rental price at 
                        less than 50% of current average rental prices. As of 30 January 2020, a new law became effective that imposes a 
                        maximum price per square meter of 6.49 EUR (henceforth “rent ceiling”), for all apartments built after 2004. 
                        Any rental contract (except those for apartments built after 2004) cannot exceed this rental price and landlords are 
                        required to adjust down existing contracts with rental prices above the rent ceiling, even forced to even reimburse 
                        tenants retroactively any rent charged above the rent cover since 18 June 2019. The newly imposed rent cap is drastic. 
                        Before the rent cap, city-wide average rental prices ranged between 13 EUR/sqm to 16 EUR/sqm 
                        (depending on apartment size), with averages in the city center often being as high as 25 EUR/sqm 
                        (Wohnungsboerse, 2020). By exempting new-builds from the regulation, policy makers hope to spur new developments. 
                        Predictably, the new rules were commended by many tenants but caused outrage amongst landlords and professional 
                        investors, calling the new policies a return to socialist housing policy. Liberal as well as center-right political 
                        parties are preparing an appeal before Germany’s Constitutional Court. While many experts predict the Constitutional 
                        Court to rule the new policy as unconstitutional, this process is likely to take several years during which they 
                        city will impose heavy fines on any landlords that to not abide by the new rules (Deloitte, 2019)."),
                      
                        p(""), 
                        
                        p("The new policy can be summarized as follows (left hand side). First, all prices are frozen for five years, 
                          i.e. there are no new increases, even for those below the ceiling. Second, from February 2020, no new rental 
                          contracts should be made above the new ceiling of 6.50 EUR /m2. Third, from November 2020 onwards, 
                          landlords that currently rent apartments above the ceiling need to reimburse tenants for any payments 
                          the ceiling that they have received since February 2020, and cannot charge more than the ceiling price going forward. 
                          Rental prices have increased rapidly over the past ten years, with a median sized apartment costing roughly 
                          375 EUR in 2010 and now costing over 1000 EURs in 2020."),
                        
                        # Load in the second image for the front page.
                        img(src = "Trend.png", height = 300, width = 800),
                        
                        
                        
                        h3("What would economic theory predict?"),
                        
                        p("The policy aims to stabilize prices and increase supply in the medium to longer term. The idea here is simple.
                          The policy makers exempt any new builings built after 2014 and any future developments. This increases the wedge
                          between old buildings and new buildings, possibly providing developers with extra incentives to invest in new buildings. 
                          Supply takes some time to adjust but over the next few years, the policy makers hope to see an upward shift in the 
                          housing supply curve, as well as possibly a slight tilt, which implies that supply has become less elastic. In the short term, 
                          though, the ceiling caps prices, which means that demand increases dramatically but supply also drops dramaically, 
                          leading to a large shortage (Qd-Qs)."), 
                        
                        img(src = "Theory.png", height = 400, width = 600),
                        
                        h3("Why it is difficult to identify housing policies: the related literature"),
                        
                        p("Trends in housing prices are difficult to cleanly identify. Housing policies are usually set at the state or natinal level, 
                        they are affected by a battery of variables, such as the overall economic trends of the area, population growth and growth in 
                        housing supply (permits, geography, interest rates, etc.Simps (2007) uses a difference-in-difference approach using unexpected
                        1995 elimination of rent control in Cambridge, MA, and finds that rent control had little effect on construction activity, 
                        decreased investment in housing stock, and shifts ownership from rental to owner-occupied. Autor et al (2015) exploit the 
                        same Cambridge, MA, policy to assess spillovers on overall housing stock, and find elimination of rent control leads to 
                        large positive spillover effects on never-regulated, nearby housing. Kholodin et al (2015) exploit variation in rent-control 
                        policies at municipality level in Germany and find that rent control has, if anything, led to an increase in rental prices. 
                        My Project exploits the new aggressive rent-cap & reimbursement policy in Berlin, using a difference-in-difference approach to study
                        the new policy in Berlin with Hamburg (where the new policy does not apply). Using detailed data on every rental apartment 
                        and every apartment for purchase that was offered one of the leading online real estate website (ImmoScout24, Immowelt, Immonet, etc.) 
                        for the cities of both Berlin and Hamburg. The data is rich in that it offers a battery of control variables that can affect
                        the housing prices, such as post-code specific locations, the condition of the apartment, its amenities (floor heating, balcony, etc.)"),
                        
                        p(""),
                        
                        p("")                      
                        )
               ,
               
               #I continue with the second panel, which shows the rental price data
               
               
               tabPanel("Rental prices",
                        
                        h3("Data"),
                        
                        p("I was fortunate to get data with about 10.5 million observations. My data have 70,000 individual apartment rental offers 
                        for Hamburg and Berlin, which each have about 150 explanatory variables. The observations each have 
                        all very important variables that allow for clean identification (incl. location, year built, condition
                        of the building, whether or not it is furnished, and data that allows me to identify whether or not the apartment 
                        will be affected by the new policy. The data also include details on amenities, such as whether the apartment has a kitchen, 
                        a second guest bathroom, floor heating, balcony, etc."), 
                        
                        p(""),
                        
                        p("When adjusting for size of the apartments, I can get a sense for the average rental prices paid in apartments per 
                          square meter (EUR/m2), and compare treated with control. As the following graph shows, average rental prices vary around
                          10 EUR/m2, with significant spread in both cities. What's also visible in the graph is that the holidays around Christmas 
                          affect the volume of the market. That is, much fewer rental apartments go online in the days between 15 and 30 December"),

                        p(""),
                        
                        h3("All rental apartment prices (Graph 1)"),
                        p("A more useful way to cut the data is to look at monthly averages in both treated and control. In the graph below, I am 
                          comparing the apartments affected by the policy in Berlin with those not affected (which could be either new builds in 
                          Berlin or apartments in Hamburg. Those not affected by the policy are trading at a premium throughout. This is because 
                          Hamburg's rental prices are higher than Berlin's and also because those apartments in Berlin that are exempt from the policy
                          are new builds from after 2014. Those typically are more popular with tenants, as they include a wider range of amenities")
                        ,
                        
                        h3("Average rental apartment prices treated vs. control (Graph 2)"),
                        p("I also fit a polynomial trend to both the treatment and the control group. At first glance, I can see that while the price for 
                        the control group keeps increasing throughout, the trend for treated apartments indeed appears to change after the policy announcement. 
                        In line with my hypothesis, prices drop for those apartments affected by the policy. However, it is important to note that the prices
                          are nowhere near the actually demanded ceiling of about 6.50 EUR/m2, as shown by the horizontal black line. "),
                        br(),
                        
                        mainPanel(plotOutput("Plot1")),
                        br(),
                        p(""),

                        br(),
                        p(""),
                        p(""),
                        mainPanel(plotOutput("Plot2")),
                        br(),
                        p(""),
                       
                )
                        ,
               
               #I continue with the third panel, which shows the volume of apartments over time. 
               
               tabPanel("Market Volume",
                        h3("Volume of rental apartment offers"),
                        p("I also expect the policy to affect how many apartments are offered on the market overall. Tenant protection is very strong 
                          in Germany. For landlords, this means that once they have let an apartment, it is very difficult to increase rental prices
                          because of rent control (maxium increase 15% every 3 years), and basically impossible to get a tenant to move out of the apartment,
                          unless the tenant stop pays rent, or illegally sublets the apartment etc. Therefore, in face of the uncertainty on the rental 
                          market, I expect a lot of landlords to just keep their apartments empty and to wait for the decision in the constitutional court 
                          on whether the policy will be taken down. Therefore, I think two reactions are likely: (1) Landlords might leave apartments 
                          completely empty, which will reduce volume substantially, and (2) Landlords will advertise their apartments online for the same prices
                          as before, designing contracts with tenants to charge them less while the policy is in place, but then reclaiming any delta, should it be
                          voted unconstitutional"),
            
                        
                        p(""),
                        
                        p("The following decision tree shows likely behavior by landlords. I hypothesise that they would either leave the apartments empty
                          in hope for a postiive decision by the Constitutional Court, or attempt to circumvent the law rent at market prices. When 
                          exploring individual offers above the new price ceiling, I often found ads that explicitly mention that the tenants would need 
                          to sign a contract in which they agree on the market value of the apartment, then only pay the legal ceiling price, but later 
                          reimburse landlords the delta if the policy is indeed voted unconstitutional."),
                        h3("Decision Framework for Landlords"),
                        img(src = "Diagram.png", height = 300, width = 700),
                        
                        
                        p(""),
                        
                        h3("Average Market Volume"),
  
                        p("A look at volume of the offers in the rental market illustrates that both might be happening. The market for treated apartments
                          slowed down dramatically after the onset of the policy. The volume more than halved from levels of about 700 units per month 
                          to less than 300 units per month. This suggests that at least half the landlords indeed keep their apartments vacant."),
                        
                        mainPanel(plotOutput("Plot3")),
                        
                        
                        p(""),
                        )
                        ,
               
               #I continue with the fourth panel, which shows leads through the diff in diff analysis. 
               
               tabPanel("Diff-in-Diff Estimation",
                        h3("Diff-in-Diff Estimation"), 
                        p("To answer the question analytically, I conduct a difference-in-difference estimation, comparing how 
                          the treatment group of apartments (those affected by the policy) evolved relative to the control group. 
                          To do so, I regress the log of apartment rental prices on a dummy variable that is 0 before the policy 
                          goes into effect, and 1 after it has come into effect (variable called timing), on a treatment dummy variable that 
                          is 0 for control units and 1 for treatment units, as well as a range of controls, such as the year the apartment
                          was built, the post code area it is in, its amenities, etc."),
                      
                        
                        p(""),
                        
                        img(src = "diff_in_diff.png", height = 350, width = 600),
                    
                        br(),
                        
                        p("The difference=in-difference estimator (beta 3) is the most important coefficient. It measures the change in the 
                          oucome variable due the treatment (policy change). Beta 1 here is the initial difference between Control and Treatment, and beta
                          2 is the time trend."), 
                        br(), 
                        br(),
                        h3("Diff-in-Diff Regression Analysis"),
                        br(),
                        p("The following table shows the intercept followed by treated_missing which is treatment dummy, i.e. the coefficient that 
                          documents the initial difference, and timing_announced, which is the dummy that picks up the policy announcement. Next follow
                          dummy variables for the City (Berlin) as well as variabel that identifies the post_codes, a time trend, the time trend squared, 
                          the number of rooms per apartment, the time period it was built, and finally the difference-in-difference estimator, which is 
                          the interaction term between the treatment and the timing dummy. "),
                        gt_output(outputId = "table"), 
                        br(),
                        
                        p("The result shows that, the very last coefficient, the diff-in-diff estimator is negative but not precisely estimated and 
                          statistically insignificant at the 1% level. It is also economically very small It means that the policy in Berlin has led 
                          to no measurable decrease in apartment prices. This suggests that policy was not effective. ")
                        )
)
)


# Define server logic to create the objects and regression table. 

server <- function(input, output, session) {
    
  #First I load an image of Berlin from above
  output$image <- renderImage({
    # Return a list containing the filename and alt text
    list(src = 'www/Berlin.png',
         height = 450,
         width = 800, style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
    output$Plot1 <- renderPlot({
      # Plot of all prices, control (HH B) vs treated (B)
      graph_1 %>%
        ggplot( mapping = aes( x= start_date, y = cost_m2, color = factor(treated))) + 
        geom_point(alpha = 0.3, size = 0.1) + 
        geom_vline(xintercept = as.numeric(as.Date("2019-06-18")), linetype="dotted", 
                   color = "red", size=1) + 
        geom_vline(xintercept = as.numeric(as.Date("2019-06-18")), linetype="dotted", 
                   color = "red", size=1) + 
        annotate(geom="text",x=as.Date("2019-02-01"),
                 y=30,label="Policy announcement") +
        
        geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), linetype="dotted", 
                   color = "blue", size=1) + 
        annotate(geom="text",x=as.Date("2019-11-15"),
                 y=30,label="Policy in action") +
        theme_classic() + 
        labs(
          title = "All Rental prices/m2: Treatment vs. Control",
          x = "Date", 
          y = "Monthly rental prices in m2"
        ) + 
        labs(color='Assignment (T=1, C=0)') 
    })
    
    output$Plot2 <- renderPlot({
      #With two trend smooth lines for treated vs. control
      graph_3 %>%
        ggplot( mapping = aes( x= start_date, y = monthly_price_t,  color=factor(treated))) + 
        geom_point(size = 0.5) + 
        geom_smooth(aes(group=treated), 
                    method = "lm", formula = y ~ poly(x, 3), se = TRUE) + 
        geom_vline(xintercept = as.numeric(as.Date("2019-06-18")), linetype="dotted", 
                   color = "red", size=1) + 
        annotate(geom="text",x=as.Date("2019-02-01"),
                 y=18,label="Policy announcement") +
        
        geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), linetype="dotted", 
                   color = "blue", size=1) + 
        annotate(geom="text",x=as.Date("2019-11-15"),
                 y=18,label="Policy in action") +
        
        geom_hline(yintercept = 6.5, linetype="solid", 
                   color = "black", size=0.5) + 
        annotate(geom="text",x=as.Date("2019-02-01"),
                 y=7,label="Price ceiling (treated)") +
        theme_classic() +
        labs(
          title = "Monthly Average Rental prices/m2: Control & Treated",
          x = "Date", 
          y = "Monthly rental prices in m2"
        ) + 
        labs(color='Assignment (T=1, C=0)') 
    })
      
      output$Plot3 <- renderPlot({
      graph_4 %>%
        ggplot( mapping = aes( x= start_date, y = monthly_count,  color=factor(treated))) + 
        geom_point(size = 0.5) + 
        geom_smooth(aes(group=treated), 
                    method = "lm", formula = y ~ poly(x, 3), se = TRUE) + 
        geom_vline(xintercept = as.numeric(as.Date("2019-06-18")), linetype="dotted", 
                   color = "red", size=1) + 
        geom_vline(xintercept = as.numeric(as.Date("2019-06-18")), linetype="dotted", 
                   color = "red", size=1) + 
        annotate(geom="text",x=as.Date("2019-02-01"),
                 y=850,label="Policy announcement") +
        
        geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), linetype="dotted", 
                   color = "blue", size=1) + 
        annotate(geom="text",x=as.Date("2019-11-15"),
                 y=850,label="Policy in action") +
        theme_classic() + 
        labs(
          title = "Berlin Rental Volume: Control & Treated",
          x = "Date", 
          y = "Monthly rental prices in m2"
        )  + 
          labs(color='Assignment (T=1, C=0)') 
    })
      
      
      output$table <-
        render_gt(
          expr = gt_result,
          height = px(700),
          width = px(700)
        )
      
}

# Run the application 
shinyApp(ui = ui, server = server)
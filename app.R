## Libraries ----
## Shiny 
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)

## Data Handling
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)

## Display
library(DT)
source("kowr_functions.R")

## Colors ----
kow_blue <- "#247ba0"
kow_red <- "#f25f5c"
lexi_pink <- "#edadc7" 
lexi_blue <- "#586994"

## Data ----
sheets_auth(path = "google_app.json")
gfuel <- read_sheet("https://docs.google.com/spreadsheets/d/11JC2nnhlY6pg6zDEuzSMJ8uVDfezTswmXGeQxFPoJOk/edit?usp=sharing") %>% 
    clean_names() %>% 
    mutate(
        date_tried = ymd(date),
        flavor_hotlink = str_c("<a href='", url, "' target='_blank'>", flavor, "</a>"),
        image_url = str_c("<img src='", image_url, "' target='_blank' height='45'></img>")
    )

gfuel_long <- gfuel %>% 
    pivot_longer(
        cols = lexi_rating:kyle_rating,
        names_to = "rater",
        values_to = "rating"
    ) %>% 
    pivot_longer(
        cols = lexi_current_favorite:kyle_current_favorite,
        names_to = "person",
        values_to = "favorite"
    ) %>% 
    mutate_at(c("rater", "person"), ~ str_remove_all(., "_rating|_current_favorite") %>% str_to_title()) %>%
    filter(rater == person) %>% 
    select(-rater)

avg_rating <- gfuel_long %>% 
    group_by(person) %>% 
    summarise(avg = round(mean(rating), 2))

current_favorite <- gfuel_long %>% 
    filter(favorite)

recent_new_flavor <- gfuel %>% 
    filter(date == max(date)) %>% 
    pull(flavor)

rn_year <- gfuel$date_tried %>% 
    max() %>% 
    year()

rn_month <- gfuel$date_tried %>% 
    max() %>% 
    month(label = TRUE, abbr = FALSE)

rn_day <- gfuel$date_tried %>% 
    max() %>% 
    day()

rn_date <- str_c(rn_month, " ", rn_day, ", ", rn_year)

flavors_tried <- gfuel %>% 
    pull(flavor) %>% 
    unique() %>% 
    length()

suppressWarnings(
    t_results <- wilcox.test(
        rating ~ person,
        paired = TRUE,
        data = gfuel_long
    ) %>% 
        pluck("p.value")
)

same_taste <- ifelse(t_results >= 0.05, "Yes!*", "No.*")

header = dashboardHeaderPlus(title = "GFuel")
sidebar = dashboardSidebar(
    sidebarMenu(
        ## Profiles
        menuItem(
            "Profiles",
            tabName = "profiles",
            icon = icon("gas-pump")
        ),
        ## About ----
        menuItem(
            "About",
            tabName = "about",
            icon = icon("question")
        )
    )
)
body = dashboardBody(
    includeCSS("custom.css"),
    tabItems(
        tabItem(
            tabName = "profiles",
            ## info boxes ----
            fluidRow(
                infoBox(
                    title = "Flavors Tried",
                    value = flavors_tried,
                    fill = TRUE,
                    color = "blue",
                    icon = icon("calculator")
                ),
                infoBox(
                    title = "Recently Tried",
                    subtitle = rn_date,
                    value = recent_new_flavor,
                    fill = TRUE,
                    color = "blue",
                    icon = icon("clock")
                ),
                infoBox(
                    title = "Do Kyle and Lexi have the Same Taste?",
                    value = textOutput("same_taste_text"),
                    fill = TRUE,
                    color = "blue",
                    icon = icon("utensils")
                ),
                bsTooltip(
                    id = "same_taste_text",
                    title = "Determined using a paired Wilcox test",
                    placement = "bottom",
                    trigger = "hover",
                    options = NULL
                )
            ),
            ## Profiles ----
            fluidRow(
                ## Kyle Profile ----
                box(
                    title = "GFuel Stats",
                    status = "primary",
                    boxProfile(
                        src = "https://pbs.twimg.com/profile_images/1143728869851238402/1SoVACjN_400x400.jpg",
                        title = "Kyle",
                        subtitle = div(
                            class = "text-center",
                            a(icon("globe"), href = "https://koderkow.rbind.io/"),
                            a(icon("twitter"), href = "https://twitter.com/koderkow"),
                            a(icon("linkedin"), href = "https://www.linkedin.com/in/kylewharris/"),
                            a(icon("github"), href = "https://github.com/KoderKow"),
                            a(icon("twitch"), href = "https://www.twitch.tv/koderkow")
                        ),
                        boxProfileItemList(
                            bordered = TRUE,
                            boxProfileItem(
                                title = "Current Favorite",
                                description = current_favorite %>% 
                                    filter(person == "Kyle") %>% 
                                    pull(flavor_hotlink) %>% 
                                    HTML() %>% 
                                    div(class = "pull-right")
                            ),
                            boxProfileItem(
                                title = "Average Rating",
                                description = avg_rating %>% 
                                    filter(person == "Kyle") %>% 
                                    pull(avg)
                            )
                        )
                    )
                ),
                ## Lexi Profile ----
                box(
                    title = "",
                    status = "primary",
                    boxProfile(
                        src = "https://pbs.twimg.com/profile_images/1035355809620930561/410vAP6r_400x400.jpg",
                        title = "Lexi",
                        subtitle = div(
                            class = "text-center",
                            a(icon("globe"), href = "https://thepizzaprincessblog.com/"),
                            a(icon("twitter"), href = "https://twitter.com/plsgivmepizza"),
                            a(icon("linkedin"), href = "https://www.linkedin.com/in/alexismeskowski/")
                        ),
                        boxProfileItemList(
                            bordered = TRUE,
                            boxProfileItem(
                                title = "Current Favorite",
                                description = current_favorite %>% 
                                    filter(person == "Lexi") %>% 
                                    pull(flavor_hotlink) %>% 
                                    HTML() %>% 
                                    div(class = "pull-right")
                            ),
                            boxProfileItem(
                                title = "Average Rating",
                                description = avg_rating %>% 
                                    filter(person == "Lexi") %>% 
                                    pull(avg)
                            )
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    plotOutput("kow_plot")
                ),
                column(
                    width = 6,
                    plotOutput("lexi_plot")
                )
            ),
            br(),
            fluidRow(
                DTOutput("summary_table")
            )
        ),
        tabItem(
            tabName = "about",
            h2("What is this?"),
            p("Lexi and I decided it would be fun to try GFuel after seeing many of our liked internet personalities mentioning it. We got a full container of Sour Grape and a sample pack. I thought it would be fun to track our rating per flavor. As someone who works with data and creates shiny dashboards at work, what better idea to do with this data than to create a pretty slick dashboard? :) This is a fun little project Lexi and I have been working on! Hopefully our ratings and findings will bring joy to those who come across this."),
            hr(),
            h2("How was this built?"),
            HTML("<a href='https://www.r-project.org/about.html' target='_blank'>R</a> and <a href='https://shiny.rstudio.com/' target='_blank'>Shiny</a>. Source code can be found <a href='https://github.com/KoderKow/gfuel_dash' target='_blank'>here</a>!"),
            hr(),
            h2("What is the data source?"), # ADD URLS!
            HTML("We use <a href='https://www.google.com/sheets/about/' target='_blank'>Google Sheets</a> as a data source and the R package <a href='https://github.com/tidyverse/googlesheets4' target='_blank'>googlesheets4</a> to read in the data.")
        )
    )
)

ui <- dashboardPagePlus(
    header = header,
    sidebar = sidebar,
    body = body,
    collapse_sidebar = TRUE
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$same_taste_text <- renderText({same_taste})
    
    output$kow_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        gfuel_long %>% 
            filter(person == "Kyle") %>%
            # mutate(favorite = as.character(favorite)) %>% 
            ggplot(aes(fct_rev(flavor), rating, fill = favorite)) +
            geom_col(position = "dodge") + 
            coord_flip() +
            ylim(0, 5) +
            theme_kow() +
            labs(title = "Kyle's Ratings", x = "", y = "") +
            scale_fill_manual(
                values = c(
                    "TRUE" = as.character(kow_blue),
                    "FALSE" = as.character(kow_red)
                )
            ) +
            guides(fill=FALSE)
    })
    
    output$lexi_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        gfuel_long %>% 
            filter(person == "Lexi") %>%
            ggplot(aes(fct_rev(flavor), rating, fill = favorite)) +
            geom_col(position = "dodge") + 
            coord_flip() +
            ylim(0, 5) +
            theme_kow() +
            labs(title = "Lexi's Ratings", x = "", y = "") +
            scale_fill_manual(
                values = c(
                    "TRUE" = as.character(lexi_blue),
                    "FALSE" = as.character(lexi_pink)
                )
            ) +
            guides(fill=FALSE)
    })
    
    output$summary_table <- renderDT({
        
        gfuel %>% 
            arrange(flavor) %>% 
            select(image = image_url, flavor = flavor_hotlink, date_tried, `Kyle's Rating` = kyle_rating, `Lexi's Rating` = lexi_rating) %>%
            snake_to() %>% 
            datatable(
                escape = FALSE,
                rownames = FALSE,
                options = list(
                    pageLength = nrow(gfuel),
                    dom = "ft"
                )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

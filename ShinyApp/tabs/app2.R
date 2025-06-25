#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

app2_ui <- tabItem(
  tabName = "app2",
  column(width = 12,
         fluidRow(
           tabBox(
             width = 12,
             
             tabPanel("Overview",
                      h3("Oceanus Folk Overview", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        valueBoxOutput("totalperformersBox"),
                        valueBoxOutput("totalreleasesBox"),
                        valueBoxOutput("yearactiveBox")
                      ),
                      fluidRow(
                        column(width = 12,
                               plotOutput("releasePlot2", height = "400px")
                        )
                      )
                      
             ),
             
             tabPanel("Influence Timeline",
                   h3("Oceanus Folk's Influence Over Time", style = "text-align: center; font-weight: bold; margin: 0px; padding: 0px;"),
 
                      br(),
                      tabItem(
                        tabName = "influence_timeline",
                        fluidRow(
                          fluidRow(
                            column(width = 4,
                                   checkboxGroupInput(
                                     inputId = "influence_type",
                                     label = "Influence Type",
                                     choices = c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples"),
                                     selected = c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
                                   ),
                                   sliderInput("influence_year_range", "Year",
                                               min = 1990, max = 2040, value = c(1990, 2040), sep = ""),
                                   box(
                                     title = "Description",
                                     width = 12,
                                     solidHeader = TRUE,
                                     status = "primary",
                                     "This plot shows the number of Oceanus Folk songs influenced over time by type of connection (e.g., CoverOf, InStyleOf, etc.)."
                                   )
                            ),
                            column(width = 8,
                                   plotOutput("oceanusPlot", height = "400px")
                            )
                          )
                          
                        )
                      )
                      
             ),
             
             tabPanel("Outward Impact",
                      h3("Songs/Albums Influenced by Oceanus Folk", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4,
                               box(
                                 title = "Description",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 "This network shows genres and artists influenced by Oceanus Folk, highlighting notable collaborations and genre spread."
                               )
                        ),
                        column(width = 8,
                               box(
                                 title = "Songs / Albums Influenced by Oceanus Folk",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "info",
                                 visNetworkOutput("oceanusInfluenceNetwork", height = "400px")
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               box(
                                 title = "Genres Influenced by Oceanus Folk",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "info",
                                 dataTableOutput("genreInfluenceTable")
                               )
                        ),
                        column(width = 6,
                               box(
                                 title = "Artists with Charted Songs Influenced by Oceanus Folk",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "info",
                                 dataTableOutput("notableArtistTable")
                               )
                        )
                      ) 
             ),
             
             tabPanel("Roots",
                      h3("Oceanus Folk's Evoling Roots", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4,
                               box(
                                 title = "Description",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 "This network shows songs in the Oceanus Folk genre (2023 onward) and their musical influences across genres."
                               )
                        ),
                        column(width = 8,
                               box(
                                 title = "Network Graph",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "info",
                                 visNetworkOutput("rootsNetwork", height = "400px")
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               box(
                                 title = "Genres that Influence Oceanus Folk Songs",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "info",
                                 dataTableOutput("influencedGenreTable")
                               )
                        )
                      )
             )
         )
   )
  )
 )


app2_server <- function(input, output, session) {
  output$totalperformersBox <- renderValueBox({valueBox(n_unique_performers, "Total Performers", width = 4,color = "blue")})
  output$totalreleasesBox <- renderValueBox({valueBox(oceanus_release, "Total Releases", width = 4,color = "blue")})
  output$yearactiveBox <- renderValueBox({valueBox(Year_active, "Year Active", width = 4,color = "blue")})
  
  # Plot output
  output$releasePlot2 <- renderPlot ({
    oceanus_releases <- nodes %>%
      filter(
        genre == "Oceanus Folk",
        type == "Song" | type == "Album"
      ) %>%
      mutate(
        year = as.integer(release_date)
      ) %>%
      filter(!is.na(year)) %>%
      count(year, type, name = "n")
    
    totals_per_year <- oceanus_releases %>%
      group_by(year) %>%
      summarise(total = sum(n), .groups = "drop")
    
    ggplot(oceanus_releases, aes(x = factor(year), y = n, fill = type)) +
      geom_col() +
      scale_fill_manual(
        values = c(
          "Song" = "#96b3c2",
          "Album" = "#466575"
        )
      ) +
      geom_text(aes(label = n),
                position = position_stack(vjust = 0.5),
                size = 2, color = "white") +
      geom_text(data = totals_per_year,
                aes(x = factor(year), y = total + 1, label = total),
                inherit.aes = FALSE,
                size = 3) +
      labs(
        title = "Number of Songs and Albums Released Over Time",
        subtitle = "Oceanus Folk Genre",
        x = "Year",
        y = "Number of Releases",
        fill = "Release Type"
      ) +
      theme_minimal() +
      theme(
        plot.background = theme$background,
        panel.background = theme$panel,
        panel.grid.major = theme$grid,
        text = element_text(size = theme$size),
        plot.title = theme$title,
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
      )
  })
  
  output$oceanusPlot <- renderPlot({
    # Filter Oceanus Folk songs/albums
    oceanus_songs <- nodes %>% 
      filter(
        genre == "Oceanus Folk",
        type %in% c("Song", "Album")
      ) %>% 
      mutate(year = as.integer(release_date))
    
    # Filter based on selected influence types
    oceanus_edges <- edges %>%
      filter(relation %in% input$influence_type, to %in% oceanus_songs$row_id)
    
    # Join edges with song year
    edges_with_year <- oceanus_edges %>%
      left_join(oceanus_songs %>% select(row_id, year), by = c("to" = "row_id")) %>%
      filter(!is.na(year)) %>%
      filter(year >= input$influence_year_range[1],
             year <= input$influence_year_range[2])
    
    # Count by year and relation
    edge_counts <- edges_with_year %>%
      group_by(year, relation) %>%
      summarise(count = n(), .groups = "drop")
    
    # Plot
    ggplot(edge_counts, aes(x = year, y = count, color = relation)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "Oceanus Folk Influences Over Time",
        x = "Year",
        y = "Number of Songs",
        color = "Edge Type"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Network Graph
  
  output$oceanusInfluenceNetwork <- renderVisNetwork({
    oceanus_songs <- nodes %>% 
      filter(
        genre == "Oceanus Folk",
        type == "Song"| type == "Album"
      ) %>% 
      mutate(
        year = as.integer(release_date)
      ) 
    
    recent <- oceanus_songs %>%
      filter(year >= 2023)
    
    influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
    
    oceanus_edges <- edges %>%
      filter(relation %in% influence_types, to %in% oceanus_songs$row_id)
    
    oceanus_nodes_subset <- nodes %>%
      filter(row_id %in% unique(c(oceanus_edges$from, oceanus_edges$to))) %>%
      mutate(
        id = row_id,
        label = name,
        group = type,
        title = genre,
        shape = "icon",
        icon.code = ifelse(genre == "Oceanus Folk", "f13d", "f111"),
        icon.color = ifelse(genre == "Oceanus Folk", "#123456", "#e3a6ab")
      )
    
    oceanus_edges_subset <- oceanus_edges %>%
      mutate(
        arrows = "to",
        title = relation,
        color = edge_colors[relation] %>% unname()
      )
    
    visNetwork(oceanus_nodes_subset, oceanus_edges_subset) %>%
      visEdges(smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      addFontAwesome() %>%
      visLayout(randomSeed = 42)
  })
  
  # Genre Influence Table
  output$genreInfluenceTable <- renderDataTable({
    oceanus_songs <- nodes %>% 
      filter(
        genre == "Oceanus Folk",
        type == "Song"| type == "Album"
      ) %>% 
      mutate(
        year = as.integer(release_date)
      ) 
    
    influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
    
    oceanus_edges <- edges %>%
      filter(relation %in% influence_types, to %in% oceanus_songs$row_id)
    
    influenced_songs <- oceanus_edges %>%
      left_join(nodes %>% select(row_id, genre, type), by = c("from" = "row_id")) %>%
      filter(!is.na(genre), genre != "Oceanus Folk")
    
    genre_influence_counts <- influenced_songs %>%
      group_by(genre) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    datatable(genre_influence_counts,
              colnames = c("Genre", "Number of Songs Influenced by Oceanus Folk"),
              options = list(pageLength = 5))
  })
  
  # Notable Artist Table
  output$notableArtistTable <- renderDataTable({
    
    oceanus_songs <- nodes %>%
      filter(genre == "Oceanus Folk", type == "Song") %>%
      mutate(year = as.integer(release_date))  # or as.integer(release_date) if it's already numeric
    
    recent <- oceanus_songs %>%
      filter(year >= 2023)
    
    oceanus_songs <- nodes %>% 
      filter(
        genre == "Oceanus Folk",
        type == "Song"| type == "Album"
      ) %>% 
      mutate(
        year = as.integer(release_date)
      ) 
    
    influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
    
    oceanus_edges <- edges %>%
      filter(relation %in% influence_types, to %in% oceanus_songs$row_id)
    
    notable_influenced_songs <- oceanus_edges %>%
      left_join(nodes %>% select(row_id, notable, type), by = c("from" = "row_id")) %>%
      filter(notable == TRUE)
    
    notable_performers <- edges %>%
      filter(relation == "PerformerOf", to %in% notable_influenced_songs$from) %>%
      left_join(nodes %>% select(row_id, name, type), by = c("from" = "row_id")) %>%
      filter(type %in% c("Person", "MusicalGroup")) %>%
      count(name, sort = TRUE)
    
    datatable(head(notable_performers, 10),
              colnames = c("Artist", "Number of Notable Songs Influenced by Oceanus Folk"),
              options = list(pageLength = 5))
  })
  output$rootsNetwork <- renderVisNetwork({
    oceanus_songs <- nodes %>% 
      filter(
        genre == "Oceanus Folk",
        type == "Song"| type == "Album"
      ) %>% 
      mutate(
        year = as.integer(release_date)
      ) 
    
    influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
    recent <- oceanus_songs %>% filter(year >= 2023)
    
    influences <- edges %>%
      filter(relation %in% influence_types, from %in% recent$row_id)
    
    node_ids <- unique(c(influences$from, influences$to))
    
    network_nodes <- nodes %>%
      filter(row_id %in% node_ids) %>%
      mutate(
        id = row_id,
        label = name,
        group = ifelse(row_id %in% recent$row_id, "Oceanus Folk", "Influenced Song"),
        title = paste0("<b>Type:</b> ", type, "<br><b>Genre:</b> ", genre)
      )
    
    network_edges <- influences %>%
      mutate(
        arrows = "to",
        title = relation,
        color = edge_colors[relation] %>% unname()
      )
    
    visNetwork(network_nodes, network_edges) %>%
      visEdges(smooth = TRUE) %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend() %>%
      visLayout(randomSeed = 42)
  })
  
  output$influencedGenreTable <- renderDataTable({
    influence_types <- c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples")
    
    oceanus_songs <- nodes %>%
      filter(genre == "Oceanus Folk", type == "Song") %>%
      mutate(year = as.integer(release_date))  # or as.integer(release_date) if it's already numeric
    
    recent <- oceanus_songs %>%
      filter(year >= 2023)
    
    influences <- edges %>%
      filter(relation %in% influence_types, from %in% oceanus_songs$row_id[oceanus_songs$year >= 2023])
    
    influenced_genres <- influences %>%
      left_join(nodes %>% select(row_id, genre, type), by = c("to" = "row_id")) %>%
      filter(type == "Song", !is.na(genre), genre != "Oceanus Folk") %>%
      count(genre, sort = TRUE)
    
    datatable(influenced_genres,
              colnames = c("Genre", "Number of Influenced Songs"),
              options = list(pageLength = 5))
  })
  
}



 
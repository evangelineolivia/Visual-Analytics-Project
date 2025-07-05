#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

app3_ui <- tabItem(
  tabName = "app3",
  column(width = 12,
         fluidRow(
           tabBox(
             width = 12,
             
             tabPanel("Activity",
                      h3("Artist's Activity", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4, selectizeInput("artist_a", "Artist 1", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("artist_b", "Artist 2", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("artist_c", "Artist 3", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE)))
                      ),
                      
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_a"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary", 
                                              visNetworkOutput("artist_a_network"))),
                        column(width = 6, box(title = textOutput("title_b"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary", 
                                              visNetworkOutput("artist_b_network")))
                      ),
                      
                      br(),
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_c"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_c_network", height = "300px"))),
                        column(width = 6, box(title = "Release Timeline",
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              plotOutput("release_timeline", height = "300px")))
                      )
                      
             ),
             
             tabPanel("Contribution",
                      h3("Artist's Contribution", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4, selectizeInput("contrib_artist_a", "Artist 1", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("contrib_artist_b", "Artist 2", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("contrib_artist_c", "Artist 3", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE)))
                      ),
                      
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_a_con"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_a_contribution"))),
                        column(width = 6, box(title = textOutput("title_b_con"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_b_contribution")))
                      ),
                      
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_c_con"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_c_contribution", height = "300px"))),
                        column(width = 6, box(title = "Artist's Contribution Table",
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              DT::dataTableOutput("contribution_table")) # Contribution_table
                        )
                      )
             ),
             
             tabPanel("Collaboration",
                      h3("Artist's Collaboration", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      
                      fluidRow(
                        column(width = 4, selectizeInput("collab_artist_a", "Artist 1", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("collab_artist_b", "Artist 2", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("collab_artist_c", "Artist 3", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE)))
                      ),
                      
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_a_col"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_a_collaboration"))),
                        column(width = 6, box(title = textOutput("title_b_col"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_b_collaboration")))
                      ),
                      
                      fluidRow(
                        column(width = 6, box(title = textOutput("title_c_col"),
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              visNetworkOutput("artist_c_collaboration", height = "300px"))),
                        column(width = 6, box(title = "Artist's Collaboration Table",
                                              width = 12,
                                              solidHeader = TRUE,
                                              status = "primary",
                                              DT::dataTableOutput("collaboration_table")) 
                        )
                      )
             ),
             
             tabPanel("Public Recognition",
                      h3("Artist's Charted vs Uncharted Songs", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4, selectizeInput("recog_artist_a", "Artist 1", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("recog_artist_b", "Artist 2", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE))),
                        column(width = 4, selectizeInput("recog_artist_c", "Artist 3", choices = NULL,
                                                         options = list(
                                                           placeholder = 'Type artist name...',
                                                           maxOptions = 10,
                                                           selectOnTab = TRUE)))
                      ),
                      
                      fluidRow(
                        column(width = 11,
                               plotOutput("charted_vs_uncharted", height = "400px")
                        ),
                        
                        fluidRow(
                          column(width = 11,
                                 box(title = "Charted Data Table",
                                     width = 12,
                                     solidHeader = TRUE,
                                     status = "primary",
                                     DT::dataTableOutput("charted_table", height = "400px")) 
                          )
                        )
                        
                        
                      )
             ),
             
             tabPanel("Prediction",
                      h3("Oceanus Folk Next Rising Star", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 12, box(title = "Rising Star Data Table",
                                               width = 12,
                                               solidHeader = TRUE,
                                               status = "primary",
                                               DT::dataTableOutput("rising_star_table")))
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          box(
                            title = "Description",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            "From the findings, we can see that Beatrice Albright, Daniel O’Connell, and Orla Seabloom are next following Sailor. With 8 of their recent works all being top-charted, their consistency in quality can be one factor that makes them stand out as emerging rising stars."                          )
                        )
                      )
                      
             )
           )
         )
  ))

collab_types <- c("ComposerOf", "ProducerOf", "LyricistOf")


app3_server <- function(input, output, session) {
  
  observe({
    req(nodes)
    
    artist_choices <- nodes %>%
      filter(type == "Person") %>%
      pull(name) %>%
      unique() %>%
      sort()
    
    # Activity tab
    updateSelectInput(session, "artist_a", choices = artist_choices, selected = "Sailor Shift")
    updateSelectInput(session, "artist_b", choices = artist_choices, selected = "Yang Wan")
    updateSelectInput(session, "artist_c", choices = artist_choices, selected = "Jay Walters")
    
    # Contribution tab
    updateSelectInput(session, "contrib_artist_a", choices = artist_choices, selected = "Sailor Shift")
    updateSelectInput(session, "contrib_artist_b", choices = artist_choices, selected = "Yang Wan")
    updateSelectInput(session, "contrib_artist_c", choices = artist_choices, selected = "Jay Walters")
    
    # Collaboration tab
    updateSelectInput(session, "collab_artist_a", choices = artist_choices, selected = "Sailor Shift")
    updateSelectInput(session, "collab_artist_b", choices = artist_choices, selected = "Yang Wan")
    updateSelectInput(session, "collab_artist_c", choices = artist_choices, selected = "Jay Walters")
    
    # Public recognition tab
    updateSelectInput(session, "recog_artist_a", choices = artist_choices, selected = "Sailor Shift")
    updateSelectInput(session, "recog_artist_b", choices = artist_choices, selected = "Yang Wan")
    updateSelectInput(session, "recog_artist_c", choices = artist_choices, selected = "Jay Walters")
    
  })
  
  
  ### artist network a
  
  output$artist_a_network <- renderVisNetwork({
    
    req(input$artist_a)
    artist_name_a <- input$artist_a
    
    duplicates <- nodes %>%
      filter(name == artist_name_a)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    artist_index <- nodes %>%
      filter(name == artist_name_a) %>%
      pull(row_id)
    
    
    member_of_groups <- edges %>%
      filter(from == artist_index, relation == "MemberOf") %>%
      pull(to)
    
    member_edges <- edges %>%
      filter(relation == "MemberOf", from == artist_index)
    
    group_id <- if (length(member_of_groups) > 0) {
      member_of_groups
    } else {
      integer(0)
    }
    performer_ids <- c(artist_index, group_id)
    
    performed_ids <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids) %>%
      pull(to)
    
    
    performed_works <- nodes %>%
      filter(row_id %in% performed_ids,
             (type == "Song" | type == "Album")) %>%
      mutate(year = as.integer(release_date)) %>%
      select(name, year, type, row_id, genre)
    
    a_nodes <- bind_rows(
      nodes %>%
        filter(row_id %in% c(artist_index, group_id)) %>%
        mutate(id = row_id,
               label = name,
               group = type),
      
      performed_works %>%
        mutate(id = row_id,
               title = paste0(
                 "<b>Name:</b> ", name,
                 "<br><b>Year:</b> ", year,
                 "<br><b>Type:</b> ", type,
                 "<br><b>Genre:</b> ", genre
               ),
               group = type,
               color = node_colors[type])
    ) %>%
      mutate(
        color = ifelse(name == artist_name_a, "gold", color),
        shape = ifelse(name == artist_name_a, "star", "dot"),
        size  = ifelse(name == artist_name_a, 30, 15)
      )
    
    member_vis_edges <- member_edges %>%
      mutate(label = "MemberOf",color = edge_colors[relation] %>% unname())
    
    performers <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids, to %in% performed_works$row_id)
    
    
    a_edges <- performers %>%
      mutate(
        from = from,
        to = to,
        arrows = "to",
        title = relation)
    
    a_edges <- bind_rows(member_vis_edges, performers) %>%
      mutate(
        color = edge_colors[relation]
      )
    
    visNetwork(a_nodes, a_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  ### artist network b
  
  output$artist_b_network <- renderVisNetwork({
    
    req(input$artist_b)
    artist_name_b <- input$artist_b
    
    duplicates <- nodes %>%
      filter(name == artist_name_b)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    artist_index <- nodes %>%
      filter(name == artist_name_b) %>%
      pull(row_id)
    
    
    member_of_groups <- edges %>%
      filter(from == artist_index, relation == "MemberOf") %>%
      pull(to)
    
    member_edges <- edges %>%
      filter(relation == "MemberOf", from == artist_index)
    
    group_id <- if (length(member_of_groups) > 0) {
      member_of_groups
    } else {
      integer(0)
    }
    performer_ids <- c(artist_index, group_id)
    
    performed_ids <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids) %>%
      pull(to)
    
    performed_works <- nodes %>%
      filter(row_id %in% performed_ids,
             (type == "Song" | type == "Album")) %>%
      mutate(year = as.integer(release_date)) %>%
      select(name, year, type, row_id, genre)
    
    b_nodes <- bind_rows(
      nodes %>%
        filter(row_id %in% c(artist_index, group_id)) %>%
        mutate(id = row_id,
               label = name,
               group = type),
      
      performed_works %>%
        mutate(id = row_id,
               title = paste0(
                 "<b>Name:</b> ", name,
                 "<br><b>Year:</b> ", year,
                 "<br><b>Type:</b> ", type,
                 "<br><b>Genre:</b> ", genre
               ),
               group = type,
               color = node_colors[type])
    ) %>%
      mutate(
        color = ifelse(name == artist_name_b, "gold", color),
        shape = ifelse(name == artist_name_b, "star", "dot"),
        size  = ifelse(name == artist_name_b, 30, 15)
      )
    
    member_vis_edges <- member_edges %>%
      mutate(label = "MemberOf",color = edge_colors[relation] %>% unname())
    
    performers <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids, to %in% performed_works$row_id)
    
    
    b_edges <- performers %>%
      mutate(
        from = from,
        to = to,
        arrows = "to",
        title = relation)
    
    b_edges <- bind_rows(member_vis_edges, performers) %>%
      mutate(
        color = edge_colors[relation]
      )
    
    visNetwork(b_nodes, b_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  ### artist network c
  
  output$artist_c_network <- renderVisNetwork({
    
    req(input$artist_c)
    artist_name_c <- input$artist_c
    
    duplicates <- nodes %>%
      filter(name == artist_name_c)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    artist_index <- nodes %>%
      filter(name == artist_name_c) %>%
      pull(row_id)
    
    
    member_of_groups <- edges %>%
      filter(from == artist_index, relation == "MemberOf") %>%
      pull(to)
    
    member_edges <- edges %>%
      filter(relation == "MemberOf", from == artist_index)
    
    group_id <- if (length(member_of_groups) > 0) {
      member_of_groups
    } else {
      integer(0)
    }
    performer_ids <- c(artist_index, group_id)
    
    performed_ids <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids) %>%
      pull(to)
    
    
    performed_works <- nodes %>%
      filter(row_id %in% performed_ids,
             (type == "Song" | type == "Album")) %>%
      mutate(year = as.integer(release_date)) %>%
      select(name, year, type, row_id, genre)
    
    c_nodes <- bind_rows(
      nodes %>%
        filter(row_id %in% c(artist_index, group_id)) %>%
        mutate(id = row_id,
               label = name,
               group = type),
      
      performed_works %>%
        mutate(id = row_id,
               title = paste0(
                 "<b>Name:</b> ", name,
                 "<br><b>Year:</b> ", year,
                 "<br><b>Type:</b> ", type,
                 "<br><b>Genre:</b> ", genre
               ),
               group = type,
               color = node_colors[type])
    ) %>%
      mutate(
        color = ifelse(name == artist_name_c, "gold", color),
        shape = ifelse(name == artist_name_c, "star", "dot"),
        size  = ifelse(name == artist_name_c, 30, 15)
      )
    
    member_vis_edges <- member_edges %>%
      mutate(label = "MemberOf",color = edge_colors[relation] %>% unname())
    
    performers <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids, to %in% performed_works$row_id)
    
    
    c_edges <- performers %>%
      mutate(
        from = from,
        to = to,
        arrows = "to",
        title = relation)
    
    c_edges <- bind_rows(member_vis_edges, performers) %>%
      mutate(
        color = edge_colors[relation]
      )
    
    visNetwork(c_nodes, c_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  
  output$release_timeline <- renderPlot({
    
    req(input$artist_a, input$artist_b, input$artist_c)
    artist_names <- c(input$artist_a, input$artist_b, input$artist_c)
    
    artist_nodes <- nodes %>%
      filter(type == "Person", name %in% artist_names)
    
    
    group_edges <- edges %>%
      filter(relation == "MemberOf", from %in% artist_nodes$row_id)
    
    group_artist_map <- group_edges %>%
      left_join(artist_nodes, by = c("from" = "row_id")) %>%
      select(group_id = to, artist_name = name)
    
    performer_ids <- c(artist_nodes$row_id, group_artist_map$group_id)
    
    performed_works <- edges %>%
      filter(relation == "PerformerOf", from %in% performer_ids) %>%
      left_join(nodes, by = c("to" = "row_id")) %>%
      filter(type %in% c("Song", "Album")) %>%
      mutate(
        release_year = as.integer(release_date),
        artist_name = case_when(
          from %in% artist_nodes$row_id ~ artist_nodes$name[match(from, artist_nodes$row_id)],
          from %in% group_artist_map$group_id ~ group_artist_map$artist_name[match(from, group_artist_map$group_id)],
          TRUE ~ NA_character_)
      )
    
    release_counts <- performed_works %>%
      filter(!is.na(release_year)) %>%
      count(artist_name, release_year)
    
    ggplot(release_counts, aes(x = release_year, y = n)) +
      geom_line(color = "#96b3c2", linewidth = 1.2) +
      geom_point(color = "#466575", size = 2) +
      facet_wrap(~ artist_name, nrow = 1, scales = "free_x") + 
      labs(
        title = "Songs or Albums Release Timeline",
        x = "Year",
        y = "Number of Releases"
      ) +
      scale_x_continuous(
        breaks = pretty,
        guide = guide_axis(angle = 45)
      ) +
      scale_y_continuous(breaks = seq(0, max(release_counts$n, na.rm = TRUE), by = 1)) +
      theme_classic() +
      theme(
        strip.text = element_text(face = "bold", size = 12),
        plot.background = theme$background,
        panel.background = theme$panel,
        panel.grid.major = theme$grid,
        text = element_text(size = theme$size),
        plot.title = theme$title
      )
    
  })
  
  
  output$artist_a_contribution <- renderVisNetwork({
    
    req(input$contrib_artist_a)
    artist_name_a <- input$contrib_artist_a
    
    
    duplicates <- nodes %>%
      filter(name == artist_name_a)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    contribution <- c("ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_a) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% contribution) %>%
      pull(to)
    
    contribution_edges <- edges %>%
      filter(from == artist_index, relation %in% contribution, to %in% artist_works)
    
    contribution_nodes <- nodes %>%
      filter(row_id %in% c(artist_index, artist_works)) %>%
      mutate(
        id = row_id,
        label = ifelse(row_id == artist_index, name, NA),
        group = type,
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type),
        shape = ifelse(row_id == artist_index, "star", "dot"),
        color = ifelse(row_id == artist_index, "gold", node_colors[type]),
        size  = ifelse(row_id == artist_index, 30, 15)
      )
    
    contribution_edges_vis <- contribution_edges %>%
      mutate(
        from = from,
        to = to,
        title = paste("<b>Relation: </b>",relation),
        arrows = "to",
        color = edge_colors[relation]
      )
    
    visNetwork(contribution_nodes, contribution_edges_vis) %>%
      visEdges(smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  output$artist_b_contribution <- renderVisNetwork({
    
    req(input$contrib_artist_b)
    artist_name_b <- input$contrib_artist_b
    
    duplicates <- nodes %>%
      filter(name == artist_name_b)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    contribution <- c("ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_b) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% contribution) %>%
      pull(to)
    
    contribution_edges <- edges %>%
      filter(from == artist_index, relation %in% contribution, to %in% artist_works)
    
    contribution_nodes <- nodes %>%
      filter(row_id %in% c(artist_index, artist_works)) %>%
      mutate(
        id = row_id,
        label = ifelse(row_id == artist_index, name, NA),
        group = type,
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type),
        shape = ifelse(row_id == artist_index, "star", "dot"),
        color = ifelse(row_id == artist_index, "gold", node_colors[type]),
        size  = ifelse(row_id == artist_index, 30, 15)
      )
    
    contribution_edges_vis <- contribution_edges %>%
      mutate(
        from = from,
        to = to,
        title = paste("<b>Relation: </b>",relation),
        arrows = "to",
        color = edge_colors[relation]
      )
    
    visNetwork(contribution_nodes, contribution_edges_vis) %>%
      visEdges(smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  output$artist_c_contribution <- renderVisNetwork({
    
    req(input$contrib_artist_c)
    artist_name_c <- input$contrib_artist_c
    
    duplicates <- nodes %>%
      filter(name == artist_name_c)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    contribution <- c("ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_c) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% contribution) %>%
      pull(to)
    
    contribution_edges <- edges %>%
      filter(from == artist_index, relation %in% contribution, to %in% artist_works)
    
    contribution_nodes <- nodes %>%
      filter(row_id %in% c(artist_index, artist_works)) %>%
      mutate(
        id = row_id,
        label = ifelse(row_id == artist_index, name, NA),
        group = type,
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type),
        shape = ifelse(row_id == artist_index, "star", "dot"),
        color = ifelse(row_id == artist_index, "gold", node_colors[type]),
        size  = ifelse(row_id == artist_index, 30, 15)
      )
    
    contribution_edges_vis <- contribution_edges %>%
      mutate(
        from = from,
        to = to,
        title = paste("<b>Relation: </b>",relation),
        arrows = "to",
        color = edge_colors[relation]
      )
    
    visNetwork(contribution_nodes, contribution_edges_vis) %>%
      visEdges(smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 123)
    
  })
  
  ###
  
  output$contribution_table <- DT::renderDataTable({
    contribution <- c("ComposerOf", "ProducerOf", "LyricistOf")
    req(input$contrib_artist_a, input$contrib_artist_b, input$contrib_artist_c)
    
    artist_names <- c(input$contrib_artist_a, input$contrib_artist_b, input$contrib_artist_c)
    
    artist_nodes <- nodes %>%
      filter(type == "Person", name %in% artist_names)
    
    contribution_counts <- edges %>%
      filter(from %in% artist_nodes$row_id, relation %in% contribution) %>%
      left_join(artist_nodes %>% select(row_id, artist_name = name), by = c("from" = "row_id")) %>%
      count(artist_name, relation) %>%
      pivot_wider(names_from = relation, values_from = n, values_fill = 0)
  })
  
  
  ## collaboration 
  
  output$artist_a_collaboration <- renderVisNetwork({
    
    req(input$collab_artist_a)
    artist_name_a <- input$collab_artist_a
    
    duplicates <- nodes %>%
      filter(name == artist_name_a)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    collab_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_a) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% collab_types) %>%
      pull(to)
    
    collab_edges <- edges %>%
      filter(to %in% artist_works, relation %in% collab_types)
    
    artist_node <- nodes %>%
      filter(row_id == artist_index) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Artist",
        shape = "star",
        color = "gold",
        level = 1
      )
    
    work_nodes <- nodes %>%
      filter(row_id %in% artist_works) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Work",
        shape = "dot",
        level = 2,
        color = node_colors[type],
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collaborator_ids <- setdiff(collab_edges$from, artist_index)
    
    collaborator_nodes <- nodes %>%
      filter(row_id %in% collaborator_ids) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Collaborator",
        shape = "icon",
        color = node_colors[type],
        level = 3,
        icon.code = ifelse(type == "Person", "f007", "f0c0"),
        icon.color = ifelse(type == "Person", node_colors["Person"], node_colors["MusicalGroup"]),
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collab_nodes <- bind_rows(artist_node, work_nodes, collaborator_nodes)
    
    collab_edges_vis <- collab_edges %>%
      mutate(
        arrows = "to",
        color = edge_colors[relation] %>% unname(),
        title = paste0("<b>Relation:</b> ", relation)
      )
    
    visNetwork(collab_nodes, collab_edges_vis) %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>% 
      addFontAwesome() %>% 
      visLayout(randomSeed = 42)
    
  }) # Network plot showing Artist A's collaborations
  
  output$artist_b_collaboration <- renderVisNetwork({
    
    req(input$collab_artist_b)
    artist_name_b <- input$collab_artist_b
    
    duplicates <- nodes %>%
      filter(name == artist_name_b)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    collab_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_b) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% collab_types) %>%
      pull(to)
    
    collab_edges <- edges %>%
      filter(to %in% artist_works, relation %in% collab_types)
    
    artist_node <- nodes %>%
      filter(row_id == artist_index) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Artist",
        shape = "icon",
        level = 1,
        color = "gold",
        icon.code = "f005",
        icon.color = "gold",
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    work_nodes <- nodes %>%
      filter(row_id %in% artist_works) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Work",
        shape = "dot",
        level = 2,
        color = node_colors[type],
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collaborator_ids <- setdiff(collab_edges$from, artist_index)
    
    collaborator_nodes <- nodes %>%
      filter(row_id %in% collaborator_ids) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Collaborator",
        shape = "icon",
        color = node_colors[type],
        level = 3,
        icon.code = ifelse(type == "Person", "f007", "f0c0"),
        icon.color = ifelse(type == "Person", node_colors["Person"], node_colors["MusicalGroup"]),
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collab_nodes <- bind_rows(artist_node, work_nodes, collaborator_nodes)
    
    collab_edges_vis <- collab_edges %>%
      mutate(
        arrows = "to",
        color = edge_colors[relation] %>% unname(),
        title = paste0("<b>Relation:</b> ", relation)
      )
    
    visNetwork(collab_nodes, collab_edges_vis) %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>% 
      addFontAwesome() %>% 
      visLayout(randomSeed = 42)
    
    
  }) # Network plot showing Artist B's collaborations
  
  output$artist_c_collaboration <- renderVisNetwork({
    
    req(input$collab_artist_c)
    artist_name_c <- input$collab_artist_c
    
    duplicates <- nodes %>%
      filter(name == artist_name_c)
    
    
    if (nrow(duplicates) > 1) {
      canonical_id <- duplicates$row_id[1]
      other_ids <- duplicates$row_id[-1]
      other_ids
      
      edges <- edges %>%
        mutate(
          from = ifelse(from %in% other_ids, canonical_id, from),
          to   = ifelse(to   %in% other_ids, canonical_id, to)
        )
      
      nodes <- nodes %>%
        filter(!(row_id %in% other_ids))
    }
    
    collab_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    
    artist_index <- nodes %>%
      filter(name == artist_name_c) %>%
      pull(row_id)
    
    artist_works <- edges %>%
      filter(from == artist_index, relation %in% collab_types) %>%
      pull(to)
    
    collab_edges <- edges %>%
      filter(to %in% artist_works, relation %in% collab_types)
    
    artist_node <- nodes %>%
      filter(row_id == artist_index) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Artist",
        shape = "icon",
        level = 1,
        color = "gold",
        icon.code = "f005",
        icon.color = "gold",
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    work_nodes <- nodes %>%
      filter(row_id %in% artist_works) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Work",
        shape = "dot",
        level = 2,
        color = node_colors[type],
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collaborator_ids <- setdiff(collab_edges$from, artist_index)
    
    collaborator_nodes <- nodes %>%
      filter(row_id %in% collaborator_ids) %>%
      transmute(
        id = row_id,
        label = name,
        group = "Collaborator",
        shape = "icon",
        color = node_colors[type],
        level = 3,
        icon.code = ifelse(type == "Person", "f007", "f0c0"),
        icon.color = ifelse(type == "Person", node_colors["Person"], node_colors["MusicalGroup"]),
        title = paste0("<b>Name:</b> ", name, "<br><b>Type:</b> ", type)
      )
    
    collab_nodes <- bind_rows(artist_node, work_nodes, collaborator_nodes)
    
    collab_edges_vis <- collab_edges %>%
      mutate(
        arrows = "to",
        color = edge_colors[relation] %>% unname(),
        title = paste0("<b>Relation:</b> ", relation)
      )
    
    visNetwork(collab_nodes, collab_edges_vis) %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>% 
      addFontAwesome() %>% 
      visLayout(randomSeed = 42)
    
  }) # Network plot showing Artist C's collaborations
  
  output$collaboration_table <- DT::renderDataTable({
    req(input$collab_artist_a, input$collab_artist_b, input$collab_artist_c)
    
    artist_names <- c(input$collab_artist_a, input$collab_artist_b, input$collab_artist_c)
    
    artist_nodes <- nodes %>%
      filter(type == "Person", name %in% artist_names)
    
    collab_summary <- purrr::map_dfr(artist_names, function(name) {
      artist_id <- artist_nodes %>%
        filter(name == !!name) %>%
        pull(row_id)
      
      works <- edges %>%
        filter(from == artist_id, relation %in% collab_types) %>%
        pull(to)
      
      collab_ids <- edges %>%
        filter(to %in% works, relation %in% collab_types, from != artist_id) %>%
        pull(from) %>%
        unique()
      
      nodes %>%
        filter(row_id %in% collab_ids) %>%
        count(type) %>%
        mutate(artist = name)
    })
    
    if (nrow(collab_summary) == 0) {
      # Return a blank 2D table if no data
      return(DT::datatable(data.frame(Message = "No collaborations found"), options = list(dom = 't')))
    }
    
    collab_summary <- collab_summary %>%
      tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
      dplyr::select(artist, dplyr::everything())
    
    DT::datatable(collab_summary, rownames = FALSE, options = list(
      pageLength = 5,
      autoWidth = TRUE
    ))
  })
  # Table showing artist + number of collaborations with Person & MusicalGroup
  
  
  output$charted_vs_uncharted <- renderPlot({
    
    req(input$recog_artist_a, input$recog_artist_b, input$recog_artist_c)
    artist_names <- c(input$recog_artist_a, input$recog_artist_b, input$recog_artist_c)
    
    artist_nodes <- nodes %>%
      filter(type == "Person", name %in% artist_names)
    
    release_charted_summary <- map_dfr(artist_names, function(artist_label) {
      artist_id <- artist_nodes %>% filter(name == artist_label) %>% pull(row_id)
      
      works <- edges %>%
        filter(from == artist_id, relation == "PerformerOf") %>%
        pull(to)
      
      nodes %>%
        filter(row_id %in% works, type %in% c("Song", "Album")) %>%
        mutate(
          artist = artist_label,
          charted_status = ifelse(notable, "Charted", "Uncharted")
        ) %>%
        count(artist, charted_status)
    })
    
    ggplot(release_charted_summary, aes(x = artist, y = n, fill = charted_status)) +
      geom_bar(stat = "identity") + 
      labs(
        title = "Charted vs Uncharted Releases per Artist",
        x = "Artist",
        y = "Number of Releases",
        fill = "Status"
      ) +
      scale_fill_manual(
        values = c(
          "Charted" = "#466575",
          "Uncharted" = "#96b3c2"
        )
      ) +
      theme_minimal() +
      theme(
        plot.background = theme$background,
        panel.background = theme$panel,
        panel.grid.major = theme$grid,
        text = element_text(size = theme$size),
        plot.title = theme$title
      )
    
  })
  
  output$charted_table <- DT::renderDataTable({
    
    req(input$recog_artist_a, input$recog_artist_b, input$recog_artist_c)
    
    artist_names <- c(input$recog_artist_a, input$recog_artist_b, input$recog_artist_c)
    
    artist_nodes <- nodes %>%
      filter(type == "Person", name %in% artist_names)
    
    release_charted_table <- map_dfr(artist_names, function(artist_label) {
      artist_id <- artist_nodes %>% 
        filter(name == artist_label) %>%
        pull(row_id)
      
      works <- edges %>%
        filter(from == artist_id, relation == "PerformerOf") %>%
        pull(to)
      
      nodes %>%
        filter(row_id %in% works, type %in% c("Song", "Album")) %>%
        mutate(
          `Artist Name` = artist_label,
          charted = ifelse(notable, "Yes", "No"),
          Year = as.integer(release_date)
        ) %>%
        select(`Artist Name`, name, Year, type, charted)
    })
    
    DT::datatable(release_charted_table, rownames = FALSE, options = list(
      pageLength = 5,
      autoWidth = TRUE
    ))
  })
  
  output$rising_star_table <- DT::renderDataTable({
    oceanus_nodes <- nodes %>%
      filter(genre == "Oceanus Folk", type %in% c("Song", "Album")) %>%
      pull(row_id)
    
    oceanus_performers <- edges %>%
      filter(to %in% oceanus_nodes, relation == "PerformerOf") %>%
      pull(from) %>% unique()
    
    collabs <- edges %>%
      filter(relation %in% c("ComposerOf", "LyricistOf", "ProducerOf"),
             from %in% oceanus_performers) %>%
      group_by(from) %>%
      summarise(Collabs = n_distinct(to), Creative = n(), .groups = "drop")
    
    release_info <- edges %>%
      filter(from %in% oceanus_performers, relation == "PerformerOf", to %in% oceanus_nodes) %>%
      left_join(nodes %>% select(row_id, release_date, notable), by = c("to" = "row_id")) %>%
      mutate(release_year = as.integer(release_date)) %>%
      filter(!is.na(release_year)) %>%
      left_join(nodes %>% select(row_id, name, type) %>%
                  rename(artist_id = row_id, artist_name = name),
                by = c("from" = "artist_id")) %>%
      filter(type %in% c("Person"))
    
    current_year <- 2040
    
    table <- release_info %>%
      group_by(artist_name, from) %>%
      summarise(
        First = min(release_year, na.rm = TRUE),
        Total = n(),
        Active = n_distinct(release_year),
        Charted = sum(notable == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(First >= 2030, Total >= 3) %>%
      left_join(collabs, by = "from") %>%
      mutate(
        Inactivity = (current_year - First + 1) - Active,
        Collabs = replace_na(Collabs, 0),
        Creative = replace_na(Creative, 0),
        Freshness = exp(-Inactivity * 0.4),
        ChartedRatio = Charted / Total,
        Score = round(((Total * 1.0) + (ChartedRatio * 15) + (Collabs * 1.0) + (Creative * 1.2)) * Freshness, 2)
      ) %>%
      arrange(desc(Score))
    
    colnames(table) <- c(
      "Artist", "ID", "First Release", "Total Works", "Active Years",
      "Charted", "Collabs", "Creative", "Inactivity", "Freshness",
      "Charted Ratio", "Rising Star Score"
    )
    table <- table %>%
      mutate(
        Freshness = round(Freshness, 3),
        `Charted Ratio` = round(`Charted Ratio`, 2)
      )
    DT::datatable(
      table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      class = "display compact stripe hover"
    )
  })
  
  
  output$title_a <- renderText({
    req(input$artist_a)
    paste("Network Graph of ", input$artist_a)
  })
  
  output$title_b <- renderText({
    req(input$artist_b)
    paste("Network Graph of ", input$artist_b)
  })
  
  output$title_c <- renderText({
    req(input$artist_c)
    paste("Network Graph of ", input$artist_c)
  })
  
  output$title_a_con <- renderText({
    req(input$contrib_artist_a)
    paste("Network Graph of ", input$contrib_artist_a)
  })
  
  output$title_b_con <- renderText({
    req(input$contrib_artist_b)
    paste("Network Graph of ", input$contrib_artist_b)
  })
  
  output$title_c_con <- renderText({
    req(input$contrib_artist_c)
    paste("Network Graph of ", input$contrib_artist_c)
  })
  
  output$title_a_col <- renderText({
    req(input$collab_artist_a)
    paste("Network Graph of ", input$collab_artist_a)
  })
  
  output$title_b_col <- renderText({
    req(input$collab_artist_b)
    paste("Network Graph of ", input$collab_artist_b)
  })
  
  output$title_c_col <- renderText({
    req(input$collab_artist_c)
    paste("Network Graph of ", input$collab_artist_c)
  })
  
  
}
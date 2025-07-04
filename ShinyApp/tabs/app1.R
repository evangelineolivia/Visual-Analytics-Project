library(shiny)


# tabs/app1.R
app1_ui <- tabItem(
  tabName = "app1",
  column(width = 12,
         fluidRow(
           tabBox(
             width = 12,
             tabPanel("Overview",
                      h3("Sailor Shift’s Career Overview", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        valueBoxOutput("activeYearsBox"),
                        valueBoxOutput("totalReleasesBox"),
                        valueBoxOutput("collaborators")
                      ),
                      fluidRow(
                        valueBoxOutput("rolesBox"),
                        valueBoxOutput("chartedBox"),
                        valueBoxOutput("chartSuccessBox")
                      )
                      ),
             tabPanel("Career Timeline",
                      h3("Sailor Shift’s Career Timeline", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      br(),
                      fluidRow(
                        column(width = 4,
                               selectInput("release_type", "Release Type", choices = c("Album", "Song"), selected = c("Album", "Song"), multiple = TRUE),
                               sliderInput("year_range", "Year", min = 2024, max = 2040, value = c(2024, 2040), sep = ""),
                               box(
                                 title = "Description",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 p("This timeline showcases Sailor Shift’s creative journey, visualizing the distribution of her songs and albums across the years."),
                                 p("Use the filters above to explore specific release types or focus on particular time periods."),
                                 p("The table provides detailed metadata for each release, including genre, notability, and release format.")
                               )
                        ),
                        column(width = 8,
                          plotOutput("releasePlot", height = "400px")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                            dataTableOutput("releaseTable")
                        )
                      )
                      ),
             tabPanel("Collaborations",
                      h3("Sailor Shift’s Collaborations", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      fluidRow(
                        column(width = 4,
                               selectInput("collab_relation_type", "Collaboration Relation", choices = c("PerformerOf", "ComposerOf","ProducerOf", "LyricistOf"), selected = c("PerformerOf", "ComposerOf","ProducerOf", "LyricistOf"), multiple = TRUE),
                               box(
                                 title = "Description",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 p("Explore Sailor Shift’s creative network through her roles as performer, composer, producer, and lyricist."),
                                 p("Use the filters to view specific collaboration types. The graph shows her connections, while the table lists key details of each partnership.")
                               )
                              ),
                        column(width = 8,
                               visNetworkOutput("collabNetwork", height = "350px")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               dataTableOutput("collabTable")
                        )
                      )
             ),
             tabPanel("Influence Network",
                      h3("Sailor Shift’s Influence Network", style = "text-align: center; font-weight: bold; margin-top: 10px;"),
                      fluidRow(
                        column(width = 4,
                               selectInput("influence_type", "Influence Type", choices = c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples"), selected = c("InStyleOf", "CoverOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples"), multiple = TRUE),
                               box(
                                 title = "Description",
                                 width = 12,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 p("This module explores the influence landscape surrounding Sailor Shift’s work."),
                                 p("Use the filters to focus on specific influence types such as 'CoverOf', 'InStyleOf', or 'DirectlySamples'."),
                                 p("The network graph visualizes directional influence between works, while the table provides detailed metadata on each connection.")
                               )
                        ),
                        column(width = 8,
                               visNetworkOutput("influenceNetwork", height = "350px")
                              )
                      ),
                     fluidRow(
                       column(width = 12,
                              dataTableOutput("influenceTable")
                       )
                     )
             )
           )
         )
  )
)

app1_server <- function(input, output, session) {
output$activeYearsBox <- renderValueBox({valueBox(activeyears, "Active Years", width = 4,color = "blue")})
output$totalReleasesBox <- renderValueBox({valueBox(total_releases, "Total Releases", width = 4,color = "blue")})
output$collaborators <- renderValueBox({valueBox(collaborators, "Collaborators", width = 4,color = "blue")})
output$rolesBox <- renderValueBox({valueBox(roles, "Creative Roles Played", width = 4, color = "blue")})
output$chartedBox <- renderValueBox({valueBox(charted_count, "Charted Songs or Albums", width = 4, color = "blue")})
output$chartSuccessBox <- renderValueBox({valueBox(ratio, "Chart Success Ratio", width = 4, color = "blue")})

output$releasePlot <- renderPlot({
  filtered <- release_summary %>%
    filter(
      type %in% input$release_type,
      year >= input$year_range[1],
      year <= input$year_range[2]
    )
  
  ggplot(filtered, aes(x = factor(year, levels = sort(unique(year), decreasing = TRUE)), y = n, fill = type)) +
    geom_col() +
    scale_fill_manual(values = c("Song" = "#466575", "Album" = "#96b3c2")) +
    labs(
      title = "Sailor Shift's Releases by Year (Songs vs Albums)",
      x = "Year",
      y = "Number of Releases",
      fill = "Release Type"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.background = theme$background,
      panel.background = theme$panel,
      panel.grid.major = theme$grid,
      text = element_text(size = theme$size),
      plot.title = theme$title,
    )
})

output$releaseTable <- renderDataTable({
  release_summary_table %>%
    filter(
      type %in% input$release_type,
      year >= input$year_range[1],
      year <= input$year_range[2]
    ) %>%
    select(name, type, year, genre, notable, single, release_date)
  })

output$collabNetwork <- renderVisNetwork({
  sailor_works <- edges %>%
    filter(from == sailor_id, relation %in% input$collab_relation_type) %>%
    pull(to)
  
  collab_edges <- graph %>%
    activate(edges) %>%
    filter(to %in% sailor_works, relation %in% input$collab_relation_type) %>%
    filter(from != sailor_id) %>%
    as_tibble()
  
  collab_node_ids <- unique(c(sailor_id, collab_edges$from, collab_edges$to))
  
  subgraph <- graph %>%
    as.igraph() %>%
    induced_subgraph(vids = collab_node_ids) %>%
    as_tbl_graph() %>%
    activate(edges) %>%
    filter(relation %in% input$collab_relation_type) %>%          
    activate(nodes) %>%
    mutate(label = case_when(
      type == "Song" ~ paste0(name, " (", release_date, ")"),
      TRUE ~ name
    ))
  
  nodes_df <- subgraph %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(
      id = row_number(),
      group = type,
      title = paste("Type:", type),
      color = ifelse(name == "Sailor Shift", "orange", node_colors[type]),
      shape = ifelse(name == "Sailor Shift", "star", "dot"),
      size  = ifelse(name == "Sailor Shift", 30, 15)
    ) %>%
    select(id, label, group, title, color, shape, size)
  
  id_map <- tibble(index = seq_len(nrow(nodes_df)), node_id = pull(subgraph %>% activate(nodes), name))
  
  
  edges_df <- subgraph %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(
      from = as.integer(from),
      to = as.integer(to),
      color = edge_colors[relation],
      arrows = "to",
      title = paste("Relation:", relation)
    ) %>%
    select(from, to, color, arrows,title) 
  
  visNetwork(nodes_df, edges_df) %>%
    visOptions(highlightNearest = TRUE, selectedBy = list(variable = "group", main = "Filter by Node Type"),) %>%
    visLayout(randomSeed = 123) %>%
    visEdges(smooth = FALSE)})

output$collabTable <- renderDataTable({
  sailor_works <- edges %>%
    filter(from == sailor_id, relation %in% input$collab_relation_type) %>%
    pull(to)
  
  collab_edges <- graph %>%
    activate(edges) %>%
    filter(to %in% sailor_works, relation %in% input$collab_relation_type, from != sailor_id) %>%
    as_tibble() %>%
    mutate(
      from_row = from,
      to_row = to
    )
  
  collab_table <- collab_edges %>%
    left_join(nodes %>% select(row_id, Collaborator = name), by = c("from" = "row_id")) %>%
    left_join(nodes %>% select(row_id, Work = name, Type = type, `Release Date` = release_date), by = c("to" = "row_id")) %>%
    select(
      Collaborator,
      Relation = relation,
      Work,
      Type,
      `Release Date`
    ) %>%
    arrange(`Release Date`)
})

output$influenceNetwork <- renderVisNetwork({
  works <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
  
  sailor_works <- edges %>%
    filter(from == sailor_id, relation %in% works) %>%
    pull(to)
  
  influenced_nodes <- edges %>%
    filter(from %in% sailor_works, relation %in% input$influence_type)
  
  influenced_works <- influenced_nodes$to
  
  performer_edges <- edges %>%
    filter(relation == "PerformerOf", to %in% influenced_works)
  
  performer_ids <- performer_edges$from
  
  sailor_node <- nodes %>%
    filter(row_id == sailor_id) %>%
    transmute(id = row_id, label = name, group = "Sailor", level = 1)
  
  works_nodes <- nodes %>%
    filter(row_id %in% sailor_works) %>%
    transmute(id = row_id, label = name, group = "SailorWork", level = 2)
  
  influencer_nodes <- nodes %>%
    filter(row_id %in% influenced_works) %>%
    transmute(id = row_id, label = name, group = "InfluencedWork", title = paste0("Genre :", genre),level = 3)
  
  performer_nodes <- nodes %>%
    filter(row_id %in% performer_ids) %>%  
    mutate(
      code = ifelse(type == "Person", "f007", "f0c0"),
      color = ifelse(type == "Person", node_colors["Person"], node_colors["MusicalGroup"])
    ) %>%
    transmute(
      id = row_id, 
      label = name, 
      group = "Performer", 
      title = paste("<b>Type:</b> ", type),
      level = 4,
      shape = "icon",
      icon.code = code,
      icon.color = color
    )
  
  all_nodes <- bind_rows(sailor_node, works_nodes, influencer_nodes, performer_nodes)
  
  edges1 <- edges %>%
    filter(from == sailor_id, to %in% sailor_works, relation %in% works) %>% 
    transmute(
      from = from,
      to = to,
      title = paste("<b>Relation:</b> ", relation),
      relation = relation
    )
  
  edges2 <- influenced_nodes %>%
    transmute(
      from = from,
      to = to,
      title = paste("<b>Relation:</b> ", relation),
      relation = relation
    )
  
  edges3 <- performer_edges %>%
    transmute(
      from = from,
      to = to,
      title = paste("<b>Relation:</b> ", relation),
      relation = relation
    )
  
  all_edges <- bind_rows(edges1, edges2, edges3)
  
  visNetwork(all_nodes, all_edges) %>%
    visNodes(shape = "dot", size = 20) %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
    addFontAwesome() %>% 
    visLayout(randomSeed = 42)
})

output$influenceTable <- renderDataTable({
  works <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
  sailor_works <- edges %>%
    filter(from == sailor_id, relation %in% works) %>%
    pull(to)
  
  influenced_nodes <- edges %>%
    filter(from %in% sailor_works, relation %in% input$influence_type)
  
  influence_details <- influenced_nodes %>%
    left_join(nodes %>% select(row_id, from_label = name), by = c("from" = "row_id")) %>%
    left_join(nodes %>% select(row_id, to_label = name, to_genre = genre), by = c("to" = "row_id")) %>%
    select(from_label, relation, to_label, to_genre) %>%
    rename(
      `Sailor Shift Work` = from_label, 
      `Influence Type` = relation,
      `Other Artist Work` = to_label, 
      `Genre` = to_genre
    )
})
}

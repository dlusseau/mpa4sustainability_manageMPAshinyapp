##############################################
#### app to explore how changing human activities occurring in MPAs can affect biodiversity risk
#### this app was created in the Biodiversa project mpa4sustainability
#### it relies on data collated over the 2021-2023 period
#### please ensure that you read the details of the data used before interpreting your results
#### for any queries, comments or interests email David Lusseau (davlu@dtu.dk)


# app.R
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(visNetwork)
  library(igraph)
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(data.table)
})

source("lib/helpers.R")
source("lib/action_sim.R")

# ---- Load data once when the app starts --------------------------------------
data_obj  <- load_network_data(pathnet = "data/SEnetworks_Nov2024.Rdata",pathmeta="data/mpa_metadata.csv")
mpa_index <- data_obj$meta
net_list  <- data_obj$networks
idx<-data_obj$index

# Vector of countries for the first dropdown
countries <- sort(unique(mpa_index$region_name))

mpa_index <- mpa_index |>
  dplyr::rename(country = region_name,mpa_name = NAME, mpa_id = mpa)  # <- key change


# ---- UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "MPA Management Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  
  ## some CSS styling
  
  tags$style(HTML("
  .summary-tiles { display:flex; gap:1rem; flex-wrap:wrap; }
  .summary-tile {
    flex: 1 1 380px;
    padding: 1rem 1.125rem;
    border: 1px solid #e5e7eb;
    border-radius: 12px;
    background: #fff;
    box-shadow: 0 0 0 rgba(0,0,0,0);
  }
  .summary-tile .title { font-weight: 600; margin-bottom: 0.35rem; }
  .summary-tile .content { line-height: 1.55; }
  .summary-tile .hint { font-size: 0.85em; color: #6c757d; margin-top: 0.35rem; display:block; }
  .summary-tile.primary   .title { color: #0b7285; }    /* teal */
  .summary-tile.success   .title { color: #2C7FB8; }    /* blue */
  .summary-tile.danger    .title { color: #D62728; }    /* red */
  .summary-tile.warning   .title { color: #6C7A89; }    /* grey */
")),
  
  
  
  # Tab 1: Networks (the one we implement now)
  nav_panel(
    "Networks",
    layout_sidebar(
      sidebar = sidebar(
        h4("Find a network"),
        # Country first
        selectizeInput(
          inputId = "country",
          label   = "Nation or autonomous region",
          choices = NULL,  # filled at server start
          multiple = FALSE,
          options = list(placeholder = "Start typing a country",
                         onInitialize = I('function() { this.setValue(""); }'),
                         create = FALSE)
        ),
        # Then MPA (filtered by country)
        selectizeInput(
          inputId = "mpa",
          label   = "MPA name",
          choices = NULL,  # updated after country selection
          multiple = FALSE,
          options = list(placeholder = "Type any part of the MPA name",
                         onInitialize = I('function() { this.setValue(""); }'),
                         create = FALSE)
        ),
        helpText("Select a country to enable the MPA dropdown. 
                  You can type any part of an MPA name to filter.")
      ),
      # Main area
      card(
        card_header("Network visualization"),
        plotOutput("network_plot", height = "850px")
      ),
      
      #some text summary
      card(
        card_header("Risk summary"),
        card_body(
          # A compact text block (HTML) that we will fill from the server
          htmlOutput("risk_panel")
        )
      )
      
    )
  ),

  
  
  # --- TAB 2: Network analysis -----------------------------------------------
  nav_panel(
    "Network analysis",
    layout_sidebar(
      sidebar = sidebar(
        h4("Selected network"),
        helpText("Species and Activities"),
      #  checkboxInput("strength_abs", "Use absolute strengths (|weights|)", FALSE),
        numericInput("top_k", "Show top K per group (0 = all)", value = 0, min = 0, step = 1),
      #  helpText("Signed mode: strengths sum signed edge weights. Absolute mode: sums of |weights|.")
      ),
      card(
        card_header("Species"),
        card_body(
          h5("Species: Blue = Ecosystem services (Out-strength), Red = Conservation threat exposure (In-strength)"),
          plotOutput("species_strength_plot", height = "550px")
        )
      ),
      card(
        card_header("Activities"),
        card_body(
          h5("Activities: Blue = Ecosystem service provision (In-strength), Red = Conservation threat contribution (Out-strength)"),
          plotOutput("activity_strength_plot", height = "550px")
        )
      ),
      card(
        card_header("Download"),
        card_body(
          div(style = "display:flex; gap:.5rem; flex-wrap:wrap;",
              downloadButton("dl_strength", "Download strengths (CSV)"))
        )
      )
    )
  ),
  
  
  
  # --- TAB 3: Management simulations ------------------------------------------
  
  nav_panel(
    "Management simulations",
    layout_sidebar(
      sidebar = sidebar(
        h4("Simulate activity removal"),
        helpText("Runs simulations on the MPA selected in the Networks tab."),
        actionButton("run_sim", "Run simulations"),
        radioButtons("sim_display", "Display mode",
                     choices = c("New values" = "value", "Quantile rank (%)" = "rank"),
                     selected = "value", inline = TRUE),
        checkboxInput("sort_bars", "Sort bars by display value", TRUE),
        helpText("Richness/Complexity bars show either new values or percentile ranks against all MPAs."),
        helpText("Dashed line = baseline (value or percentile rank) for the selected MPA.")
      ),
      card(
        card_header("Biodiversity Conservation Risk Richness after removal"),
        plotOutput("sim_plot_rich", height = "500px")
      ),
      card(
        card_header("Biodiversity Conservation Risk Complexity after removal"),
        plotOutput("sim_plot_comp", height = "500px")
      ),
      card(
        card_header("Change in Ecosystem service provision (in-degree sum - Δ)"),
        plotOutput("sim_plot_indeg", height = "500px")
      )
    )
  ),
  
  
  
  # ---  TAB 4: Summary of findings ---------------------------------------------
  
  nav_panel(
    "Summary of findings",
    card(
      card_header("Summary"),
      card_body(
        # (CSS can be injected globally as per step 1; if not, include tags$style here.)
        div(class = "summary-tiles",
            # We will render these four tiles via a single htmlOutput:
            htmlOutput("summary_text")
        )
      )
    )
  ),
  
  
    
  # Placeholder for more tabs you’ll add later
  
  #--- end TABs: explainers
  nav_panel("About", 
      card(
       card_header("About the data"),
       card_body(
         
         markdown("
## This App was created as part of the Biodiversa+ project mpa4sustainability.  
  
The project was funded through the 2020-2021 Biodiversa+ and Water JPI joint call for research projects, 
under the BiodivRestore ERA-NET Cofund (GA N°101003777), with the EU and the funding organisations Innovation Fund Denmark, 
Fundação para a Ciênca e a Tecnologia, Swedish Environmental Protection Agency, Agence Nationale de la Recherche, Agencia Estatal de Investigación.  

---

#### Disclaimer  

The estimates provided in this app are based on global data analyses. This provides a mean to have a harmonised overview of mpa usage (by people and nature)
across the globe, In some regions of the world, more precise information and data may be available for the MPA in which you are interested (e.g., species distribution and density or fishing intensity). 
We therefore recommend that you use this app for intial guidance of mpa management. Feel free to contact the mpa4sustainability team (davlu@dtu.dk) for further guidance. 
Detailed MPA management plan development should involve all local stakeholders and scientists with local knowledge of the ecology of your site.

---

#### How to use this app
Please read the 'How to Use' Tab before starting using this app. 

---

#### Marine Protected Areas (MPA - file mpa_metadata.csv)
- **mpa**: the MPA and their characteristics come from the WDPA MPA data
in addition this file contains key characteristics from WDPA (nation designating mpas, mpa type, mpa name etc) used in the app
- **risk richness**: the biodiversity conservation risk richness of the MPA
- **risk complexity**: the biodiversity conservation risk complexity of the MPA


---

#### SEnetworks (file SEnetworks_Nov2024)
- A list in which each element is an igraph object containing the network for one MPA  
- 15,971 elements for 15,971 MPAs (MPAs for which a network could be estimated out of the 17200 listed in WDPA)  
- Each list element is named after its MPA WDPA ID  

**Graph attributes:**
- Graphs are bipartite (`V(g)$type`)  
- Vertex color: white = species group, gray = activities  
- Vertex size: homogeneous  
- Edge weight: absolute value of edge_list weight × 10  
- Edge sign: sign of edge_list weight  
- Edge color: blue for positive, orange for negative  
- Edge width: edge weight  
- Edge arrow size: 1/10 of edge weight  

---

### Definition of networks 
- **node type**: two node types - species and activities 
- **species**: regrouped as 34 taxonomic groups (ISSCAAP classification) see ISSCAAP for details - the name given to the category does not necessarily describes well all species in the category, but the categories are taxonomically robust
- **human activities**: regrouped as 24 activities
- **from**: the node from which the edge emerges
- **to**: the node receiving the edge  
- **weight**: the weight of the edge (the proportion of the ISSCAAP species group to which the edge is attributed)  

The weight can be:
- **Positive**: goods and services contributions from species to activities (via food provisioning and cultural ecosystem services only at this stage)  
- **Negative**: activities threaten the conservation status of species (as noted in the IUCN Red List Conservation Assessment for the species)  
    ")
       )
      )
  ),
  
  
  
  
  # --- Tab: How to use this app (loads external Markdown) -----------------------
  nav_panel(
    "How to use this app",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("How to use this app"),
        card_body(
          # bslib::markdown can load a file path directly
          markdown(read_file("docs/how_to_use.Rmd"))
        )
      )
    )
  )
  
)


# Numeric percentile of "riskiness" (0..100). Higher = riskier (if higher_is_risk = TRUE).
rank_percent <- function(value, all_values, higher_is_risk = TRUE) {
  if (is.na(value)) return(NA_real_)
  vals <- suppressWarnings(as.numeric(all_values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NA_real_)
  p <- stats::ecdf(vals)(value)        # fraction <= value
  rp <- if (higher_is_risk) p else (1 - p)
  round(rp * 100, 1)                   # 1 decimal for readability
}


# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Initialize country choices (server-side selectize handles large vectors efficiently)
  updateSelectizeInput(
    session, "country",
    choices = countries,
    selected="",
    server = TRUE
  )
  
  # When a country is chosen, update MPA choices accordingly
  observeEvent(input$country, {
    req(input$country)
    mpas <- mpa_index %>%
      filter(country == input$country) %>%
      arrange(mpa_name) %>%
      pull(mpa_name)
    
    updateSelectizeInput(
      session, "mpa",
      choices = mpas,
      selected = "",
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  # Build a reactive that returns the selected igraph object
  selected_graph <- reactive({
    
    if (!shiny::isTruthy(input$country) || !shiny::isTruthy(input$mpa)) {
      return(NULL)
    }
    
    # Find the row in the index
    row <- mpa_index %>%
      filter(country == input$country, mpa_name == input$mpa) %>%
      slice(1)
    req(nrow(row) == 1)

    # Retrieve the network from the list
    # Assumes mpa_index$mpa_id matches names(net_list)
    g <- net_list[[row$mpa_id[1]]]
    validate(
      need(!is.null(g), "Network not found for this MPA (check mpa_id mapping).")
    )
    g
  })
  
  
  # ---- Strengths per node (signed or absolute; in/out) -------------------------
  strength_df <- reactive({
    g <- selected_graph()
    validate(need(!is.null(g), "Select a country and MPA to analyze the network."))
    
    # Ensure weights present
    if (is.null(igraph::E(g)$weight)) {
      stop("Edge attribute 'weight' is missing. Please ensure E(g)$weight is set.")
    }
    w <- igraph::E(g)$weight/10 #move back to real data
    directed <- igraph::is_directed(g)
    
    # Signed strengths
    out_s <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "out" else "all", weights = w)
    in_s  <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "in"  else "all", weights = w)
    
    # Absolute strengths (sum of |weights|)
    abs_out <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "out" else "all", weights = abs(w))
    abs_in  <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "in"  else "all", weights = abs(w))
    
    v_names <- igraph::V(g)$name
    v_type  <- igraph::V(g)$type  # TRUE = Species; FALSE = Activities
    
    df <- dplyr::tibble(
      node          = v_names %||% as.character(seq_along(out_s)),
      type_logical  = v_type,
      type          = ifelse((v_type), "Species", "Activity"),
      out_strength  = if (isTRUE(input$strength_abs)) abs_out else out_s,
      in_strength   = if (isTRUE(input$strength_abs)) abs_in  else in_s
    ) |>
      dplyr::mutate(total_abs = abs(out_strength) + abs(in_strength))
    
    # Apply top-K filter within each group (Species/Activity)
    k <- input$top_k %||% 0
    if (is.numeric(k) && k > 0) {
      df <- df |>
        dplyr::group_by(type) |>
        dplyr::slice_max(order_by = total_abs, n = k, with_ties = FALSE) |>
        dplyr::ungroup()
    }
    
    df
  })
  
  # ---- Species bar chart (TRUE) ------------------------------------------------
  output$species_strength_plot <- renderPlot({
    df <- strength_df()
    df_s <- df |>
      dplyr::filter((type_logical)) |>
      tidyr::pivot_longer(
        cols = c(out_strength, in_strength),
        names_to = "direction",
        values_to = "strength"
      ) |>
      dplyr::mutate(node = reorder(node, total_abs))
    
    ggplot(df_s, aes(x = node, y = strength, fill = direction)) +
      geom_col(position = "dodge") +
   #   coord_flip() +
      scale_fill_manual(
        values = c(
          "out_strength" = "#2C7FB8", # blue = out (species → activities)
          "in_strength"  = "#D62728"  # red  = in  (activities → species)
        ),
        labels = c("out_strength" = "Ecosystem services provision", "in_strength" = "Conservation threats"),
        name   = NULL
      ) +
      labs(
        title = if (isTRUE(input$strength_abs))
          "Species strengths (absolute)"
        else
          "Species strengths (signed)",
        x = NULL, y = if (isTRUE(input$strength_abs)) "∑|weight|" else "∑weight"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # ---- Activities bar chart (FALSE) --------------------------------------------
  output$activity_strength_plot <- renderPlot({
    df <- strength_df()
    df_a <- df |>
      dplyr::filter(!(type_logical)) |>
      tidyr::pivot_longer(
        cols = c(in_strength, out_strength),  # order per request: blue=in, red=out
        names_to = "direction",
        values_to = "strength"
      ) |>
      dplyr::mutate(node = reorder(node, total_abs))
    
    ggplot(df_a, aes(x = node, y = strength, fill = direction)) +
      geom_col(position = "dodge") +
   #   coord_flip() +
      scale_fill_manual(
        values = c(
          "in_strength"  = "#2C7FB8", # blue = in  (species → activities)
          "out_strength" = "#D62728"  # red  = out (activities → species)
        ),
        labels = c("in_strength" = "Ecosystem services", "out_strength" = "Conservation threats"),
        name   = NULL
      ) +
      labs(
        title = if (isTRUE(input$strength_abs))
          "Activity strengths (absolute)"
        else
          "Activity strengths (signed)",
        x = NULL, y = if (isTRUE(input$strength_abs)) "∑|weight|" else "∑weight"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # ---- Download strengths -------------------------------------------------------
  output$dl_strength <- downloadHandler(
    filename = function() {
      paste0("strengths_", gsub("\\s+", "_", input$country), "_",
             gsub("\\s+", "_", input$mpa), ".csv")
    },
    content = function(file) {
      df <- strength_df()
      readr::write_csv(df, file)
    }
  )
  
  
  # ---- Helper: get selected MPA metadata row ----
  selected_mpa_meta <- reactive({
    if (!shiny::isTruthy(input$country) || !shiny::isTruthy(input$mpa)) return(NULL)
    row <- mpa_index %>%
      filter(country == input$country, mpa_name == input$mpa) %>%
      slice(1)
    if (nrow(row) != 1) return(NULL)
    row
  })
  
  
  selected_mpa_id <- reactive({
    meta <- selected_mpa_meta()
    if (is.null(meta)) return(NULL)
    meta$mpa_id
  })
  
  
  # ---- Helper: quantile rank text (e.g., "top 10% of risk" or "bottom 20%") ----
  rank_text <- function(value, all_values, higher_is_risk = TRUE) {
    # Handle missing
    if (is.na(value) || !is.numeric(value) || length(all_values) == 0) return("rank: unavailable")
    all_values <- all_values[is.finite(all_values)]
    if (length(all_values) == 0) return("rank: unavailable")
    
    # Percentile of the value among all MPAs
    p <- ecdf(all_values)(value)  # fraction ≤ value
    # If higher values mean more risk, use p; otherwise flip
    risk_pct <- if (higher_is_risk) p else (1 - p)
    
    pct <- round(risk_pct * 100)  # 0–100 integer
    # Phrase: top/bottom bands (deciles)
    
    band <- paste0(pct, "th percentile")
    
    paste0(band)
  }
  
  # ---- Output: Risk summary panel ----
  
  output$risk_panel <- renderUI({
    meta <- selected_mpa_meta()
    if (is.null(meta)) return(HTML("<em>Select a country and MPA to view risk metrics.</em>"))
    
    richness   <- suppressWarnings(as.numeric(meta$risk_richness))
    complexity <- suppressWarnings(as.numeric(meta$risk_complexity))
    all_richness   <- suppressWarnings(as.numeric(mpa_index$risk_richness))
    all_complexity <- suppressWarnings(as.numeric(mpa_index$risk_complexity))
    
    richness_rank   <- rank_text(richness,   all_richness,   TRUE)
    complexity_rank <- rank_text(complexity, all_complexity, TRUE)
    
    htmltools::tagList(
      htmltools::tags$div(style="display:flex; gap: 1rem; flex-wrap:wrap;",
                          htmltools::tags$div(style="padding:0.75rem 1rem; border:1px solid #dde2e6; border-radius:8px;",
                                              htmltools::tags$div(style="font-weight:600; color:#0b7285;", "Risk richness"),
                                              htmltools::tags$div(style="font-family:monospace;", ifelse(is.na(richness), "NA", format(richness, digits=4))),
                                              htmltools::tags$div(style="font-size:0.9em; color:#6c757d;", richness_rank)
                          ),
                          htmltools::tags$div(style="padding:0.75rem 1rem; border:1px solid #dde2e6; border-radius:8px;",
                                              htmltools::tags$div(style="font-weight:600; color:#9c2f2f;", "Risk complexity"),
                                              htmltools::tags$div(style="font-family:monospace;", ifelse(is.na(complexity), "NA", format(complexity, digits=4))),
                                              htmltools::tags$div(style="font-size:0.9em; color:#6c757d;", complexity_rank)
                          )
      ),
      htmltools::tags$small("Ranks computed against all MPAs using empirical percentiles.")
    )
  })
  
  
  ###### simulate removals
  
  # ---- Activities present in the selected graph --------------------------------
  activities_in_graph <- reactive({
    g <- selected_graph()
    validate(need(!is.null(g), "Select a country and MPA to run simulations."))
    v <- igraph::V(g)
    act_names <- v$name[!v$type]   # type == FALSE are activities
    unique(act_names)
  })
  
  # ---- Baseline values from metadata -------------------------------------------
  baseline_rich <- reactive({
    meta <- selected_mpa_meta()
    suppressWarnings(as.numeric(meta$risk_richness))
  })
  
  baseline_comp <- reactive({
    meta <- selected_mpa_meta()
    suppressWarnings(as.numeric(meta$risk_complexity))
  })
  
  # ---- Simulations: remove each activity one-by-one -----------------------------
  simulate_management <- eventReactive(input$run_sim, {
    g        <- selected_graph()
    mpa_id   <- selected_mpa_id()
    validate(need(!is.null(g) && !is.null(mpa_id),
                  "Select a country and MPA to run simulations."))
    
    acts <- activities_in_graph()
    if (length(acts) == 0) {
      return(dplyr::tibble(activity = character(),
                           richness_new = numeric(),
                           complexity_new = numeric()))
    }
    
  
    withProgress(message = "Running simulations", value = 0, {
      res_list <- lapply(seq_along(acts), function(i) {
        incProgress(1/length(acts), detail = paste("Removing:", acts[i]))
        out <- actions(pathredo = "data/mpa_risk_df.csv", removed = acts[i], mpa_id = mpa_id)
        
        dplyr::tibble(
          activity       = acts[i],
          richness_new   = as.numeric(out$newrichness),
          complexity_new = as.numeric(out$newcomplexity)
        )
      })
      dplyr::bind_rows(res_list)
    })
  })
  
  
  
  # ---- Third metric: change in sum of activity in-degrees (graph-based) --------
  # Baseline sum of in-degrees across all activity nodes (type == FALSE).
  baseline_indeg_sum <- reactive({
    g <- selected_graph()
    validate(need(!is.null(g), "Select a country and MPA to compute in-degrees."))
    directed <- igraph::is_directed(g)
    v <- igraph::V(g)
    act_ids <- which(!as.logical(v$type))
    if (!length(act_ids)) return(0)
    mode <- if (directed) "in" else "all"
    sum(igraph::degree(g, v = act_ids, mode = mode))
  })
  
  
  # For each removal, recompute the sum and return Δ = new - baseline
  simulate_indeg_change <- eventReactive(input$run_sim, {
    g <- selected_graph()
    validate(need(!is.null(g), "Select a country and MPA to compute in-degrees."))
    acts <- activities_in_graph()
    if (!length(acts)) {
      return(dplyr::tibble(activity = character(), indeg_delta = numeric()))
    }
    base_sum <- baseline_indeg_sum()
    directed <- igraph::is_directed(g)
    mode <- if (directed) "in" else "all"
    
    withProgress(message = "Computing in-degree changes", value = 0, {
      deltas <- lapply(seq_along(acts), function(i) {
        incProgress(1/length(acts), detail = paste("Removing:", acts[i]))
        g2 <- igraph::delete_vertices(g, v = acts[i])              # remove activity by name
        v2 <- igraph::V(g2)
        act2_ids <- which(!as.logical(v2$type))
        new_sum <- if (length(act2_ids)) sum(igraph::degree(g2, v = act2_ids, mode = mode)) else 0
        dplyr::tibble(activity = acts[i], indeg_delta = 100*(new_sum - base_sum)/base_sum)
      })
      dplyr::bind_rows(deltas)
    })
  })
  
  # ---- Richness plot (value or percentile rank) --------------------------------
  output$sim_plot_rich <- renderPlot({
    df <- simulate_management()
    validate(need(nrow(df) > 0, "Click 'Run simulations' to compute results."))
    
    display <- input$sim_display
    all_rich <- suppressWarnings(as.numeric(mpa_index$risk_richness))
    base_val <- baseline_rich()
    
    if (identical(display, "rank")) {
      df <- df %>%
        mutate(value = vapply(richness_new, rank_percent, numeric(1),
                              all_values = all_rich, higher_is_risk = TRUE),
               activity = as.character(activity))
      base_line <- rank_percent(base_val, all_rich, TRUE)
      y_lab <- "Percentile rank (%)"
    } else {
      df <- df %>% mutate(value = richness_new, activity = as.character(activity))
      base_line <- base_val
      y_lab <- "New richness"
    }
    
    if (isTRUE(input$sort_bars)) {
      df <- df %>% arrange(value) %>% mutate(activity = factor(activity, levels = activity))
    }
    
    ggplot(df, aes(x = activity, y = value)) +
      geom_col(fill = "#2C7FB8") +
      geom_hline(yintercept = base_line, linetype = "dashed", color = "black") +
      labs(title = "Richness after removing each activity",
           x = NULL, y = y_lab) +
      coord_flip() +
      theme_minimal(base_size = 12)
  })
  
  # ---- Complexity plot (value or percentile rank) -------------------------------
  output$sim_plot_comp <- renderPlot({
    df <- simulate_management()
    validate(need(nrow(df) > 0, "Click 'Run simulations' to compute results."))
    
    display <- input$sim_display
    all_comp <- suppressWarnings(as.numeric(mpa_index$risk_complexity))
    base_val <- baseline_comp()
    
    if (identical(display, "rank")) {
      df <- df %>%
        mutate(value = vapply(complexity_new, rank_percent, numeric(1),
                              all_values = all_comp, higher_is_risk = TRUE),
               activity = as.character(activity))
      base_line <- rank_percent(base_val, all_comp, TRUE)
      y_lab <- "Percentile rank (%)"
    } else {
      df <- df %>% mutate(value = complexity_new, activity = as.character(activity))
      base_line <- base_val
      y_lab <- "New complexity"
    }
    
    if (isTRUE(input$sort_bars)) {
      df <- df %>% arrange(value) %>% mutate(activity = factor(activity, levels = activity))
    }
    
    ggplot(df, aes(x = activity, y = value)) +
      geom_col(fill = "#D62728") +
      geom_hline(yintercept = base_line, linetype = "dashed", color = "black") +
      labs(title = "Complexity after removing each activity",
           x = NULL, y = y_lab) +
      coord_flip() +
      theme_minimal(base_size = 12)
  })
  
  # ---- In-degree change plot (Δ new - baseline) --------------------------------
  output$sim_plot_indeg <- renderPlot({
    df <- simulate_indeg_change()
    validate(need(nrow(df) > 0, "Click 'Run simulations' to compute results."))
    
    # Sorting for readability
    if (isTRUE(input$sort_bars)) {
      df <- df %>% arrange(indeg_delta) %>%
        mutate(activity = factor(activity, levels = activity))
    }
    
    ggplot(df, aes(x = activity, y = indeg_delta)) +
      geom_col(fill = "#6C7A89") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(title = "Change in sum of activity in-degrees (Δ) after removal",
           x = NULL, y = "Loss of ecosystem service provision (% of original links)") +
      coord_flip() +
      theme_minimal(base_size = 12)
  })
  
  
  ######
  ##summary of findings
  
  
  # ---- Helpers: ordinal & percentile (numeric) ---------------------------------
  ordinal <- function(n) {
    n <- as.integer(round(n))
    if (is.na(n)) return("NA")
    if (n %% 100 %in% 11:13) return(paste0(n, "th"))
    suf <- if (n %% 10 == 1) "st" else if (n %% 10 == 2) "nd" else if (n %% 10 == 3) "rd" else "th"
    paste0(n, suf)
  }
  
  # Numeric percentile of "riskiness" (0..100). Higher = riskier.
  rank_percent <- function(value, all_values, higher_is_risk = TRUE) {
    if (is.na(value)) return(NA_real_)
    vals <- suppressWarnings(as.numeric(all_values))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NA_real_)
    p <- stats::ecdf(vals)(value)            # share <= value
    rp <- if (higher_is_risk) p else (1 - p)
    round(rp * 100, 1)
  }
  
  # ---- Strengths per node (recomputed locally for robustness) ------------------
  summary_strength_df <- reactive({
    g <- selected_graph()
    validate(need(!is.null(g), "Select a country and MPA to view the summary."))
    
    w <- igraph::E(g)$weight
    if (is.null(w)) stop("Edge attribute 'weight' is missing (E(g)$weight).")
    directed <- igraph::is_directed(g)
    
    out_s <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "out" else "all", weights = w)
    in_s  <- igraph::strength(g, vids = igraph::V(g), mode = if (directed) "in"  else "all", weights = w)
    
    v_names <- igraph::V(g)$name
    v_type  <- igraph::V(g)$type  # TRUE = Species, FALSE = Activities
    
    dplyr::tibble(
      node         = v_names %||% as.character(seq_along(out_s)),
      type_logical = as.logical(v_type),
      type         = ifelse(isTRUE(v_type), "Species", "Activity"),
      out_strength = out_s,
      in_strength  = in_s,
      total_abs    = abs(out_s) + abs(in_s)
    )
  })
  
  # ---- Safely access simulations (if the tab has been run) ---------------------
  summary_sim_df <- reactive({
    # Try to reuse simulate_management() if present; otherwise return NULL
    tryCatch({
      df <- simulate_management()
      if (is.null(df) || !nrow(df)) NULL else df
    }, error = function(e) NULL)
  })
  
  # ---- Render the narrative -----------------------------------------------------
  output$summary_text <- renderUI({
    g <- selected_graph()
    meta <- selected_mpa_meta()
    validate(need(!is.null(g) && !is.null(meta), "Select a country and MPA to view the summary."))
    
    # Basic identifiers
    mpa_name <- input$mpa
    country  <- input$country
    n_total  <- nrow(mpa_index)
    
    # Count activities in the graph (type == FALSE)
    v <- igraph::V(g)
    n_activities <- sum(!as.logical(v$type))
    
    # Baselines and percentiles
    base_rich <- suppressWarnings(as.numeric(meta$risk_richness))
    base_comp <- suppressWarnings(as.numeric(meta$risk_complexity))
    rich_pct  <- rank_percent(base_rich, mpa_index$risk_richness, TRUE)
    comp_pct  <- rank_percent(base_comp, mpa_index$risk_complexity, TRUE)
    
    # human-nature network contributions
    df <- summary_strength_df()
    
    # ss: species group with largest out-strength (ecosystem services provided)
    ss <- df %>% dplyr::filter((type_logical)) %>%
      dplyr::slice_max(order_by = out_strength, n = 1, with_ties = FALSE) %>%
      dplyr::pull(node)
    if (length(ss) == 0) ss <- "NA"
    
    # aa: activity with largest in-strength (ecosystem services received)
    aa <- df %>% dplyr::filter(!(type_logical)) %>%
      dplyr::slice_max(order_by = in_strength, n = 1, with_ties = FALSE) %>%
      dplyr::pull(node)
    if (length(aa) == 0) aa <- "NA"
    
    # aaa: activity with largest out-strength (conservation threats produced)
    aaa <- df %>% dplyr::filter(!(type_logical)) %>%
      dplyr::slice_max(order_by = out_strength, n = 1, with_ties = FALSE) %>%
      dplyr::pull(node)
    if (length(aaa) == 0) aaa <- "NA"
    
    # bb: top activities by in-strength (ecosystem services). Use top 3 if available.
    
    # Strength-based leaders (unchanged parts)
    df <- summary_strength_df()
        # Activities table
    df_act <- df %>% dplyr::filter(!(type_logical))
    bb_list <- head(df_act$node[order(df_act$in_strength, decreasing = TRUE)], 2)
    
    bb_text <- if (length(bb_list)) paste(bb_list, collapse = ", ") else "NA"
    
    # Management simulations (if available): aaaa = smallest richness_new; aaaaa = smallest complexity_new
    simdf <- summary_sim_df()
    aaaa <- if (!is.null(simdf) && nrow(simdf)) {
      simdf %>% dplyr::slice_min(order_by = richness_new, n = 1, with_ties = FALSE) %>% dplyr::pull(activity)
    } else NA_character_
    aaaaa <- if (!is.null(simdf) && nrow(simdf)) {
      simdf %>% dplyr::slice_min(order_by = complexity_new, n = 1, with_ties = FALSE) %>% dplyr::pull(activity)
    } else NA_character_
    
    # Format percentile text (e.g., "73rd percentile")
    rich_pct_txt <- if (is.na(rich_pct)) "unknown percentile" else paste0(ordinal(round(rich_pct)), " percentile")
    comp_pct_txt <- if (is.na(comp_pct)) "unknown percentile" else paste0(ordinal(round(comp_pct)), " percentile")
    
    # Compose the narrative
    
    # Tiles (badges)
    htmltools::tagList(
      # Overview
      htmltools::div(class = "summary-tile primary",
                     htmltools::div(class = "title", "Overview"),
                     htmltools::div(class = "content",
                                    HTML(sprintf(
                                      "<strong>%d</strong> human activities we can monitor occur in <strong>%s</strong> in <strong>%s</strong>.
           They contribute to conservation threats to the species we can estimate to be observed in the MPA so that it ranks
           <strong>%s</strong> in biodiversity conservation risk <em>richness</em> and <strong>%s</strong> in biodiversity conservation risk <em>complexity</em>
           out of the <strong>%d</strong> MPAs we could appraise around the world.",
                                      n_activities, htmltools::htmlEscape(mpa_name), htmltools::htmlEscape(country),
                                      rich_pct_txt, comp_pct_txt, n_total
                                    ))
                     ),
                     htmltools::span(class = "hint", "Percentiles computed against all MPAs in the dataset.")
      ),
      
      # Network leaders
      htmltools::div(class = "summary-tile success",
                     htmltools::div(class = "title", "Network contributions"),
                     htmltools::div(class = "content",
                                    HTML(sprintf(
                                      "In this MPA, the <strong>%s</strong> species group contributes the most ecosystem services (largest <em>out-strength</em> among species).
           <strong>%s</strong> activities receive the most ecosystem services (largest <em>in-strength</em> among activities).
           <strong>%s</strong> activities produce the most conservation threats (largest <em>out-strength</em> among activities).",
                                      htmltools::htmlEscape(ss), htmltools::htmlEscape(aa), htmltools::htmlEscape(aaa)
                                    ))
                     ),
                     htmltools::span(class = "hint", "Strengths are weighted sums of edge weights (signed); species TRUE, activities FALSE.")
      ),
      
      # Management focus
      htmltools::div(class = "summary-tile danger",
                     htmltools::div(class = "title", "Management focus"),
                     htmltools::div(class = "content",
                                    HTML(sprintf(
                                      "To reduce biodiversity conservation risk <em>richness</em>, actions should concentrate on <strong>%s</strong> activities
           (smallest richness after removal). To reduce risk <em>complexity</em>, actions should concentrate on <strong>%s</strong> activities
           (smallest complexity after removal). %s",
                                      ifelse(is.na(aaaa), "NA", htmltools::htmlEscape(aaaa)),
                                      ifelse(is.na(aaaaa), "NA", htmltools::htmlEscape(aaaaa)),
                                      ifelse(is.null(simdf) || !nrow(simdf),
                                             "<em>(Run the Management simulations tab to populate these recommendations.)</em>", "")
                                    ))
                     ),
                     htmltools::span(class = "hint", "Each simulation removes one activity at a time and recomputes risk richness/complexity.")
      ),
      
      # Ecosystem services caution
      htmltools::div(class = "summary-tile warning",
                     htmltools::div(class = "title", "Ecosystem services caution"),
                     htmltools::div(class = "content",
                                    HTML(sprintf(
                                      "Management actions focussing on <strong>%s</strong> activities should be aware that restrictions of those activities
           can impact the provision of ecosystem services.",
                                      htmltools::htmlEscape(bb_text)
                                    ))
                     ),
                     htmltools::span(class = "hint", "Listed: top activities by in-strength (services received).")
      )
    )
  })
  
  
  
  
  
  # Render the network
  output$network_plot <- renderPlot({
    
    # If the user hasn’t chosen a region/MPA yet, show a placeholder plot
    
    if (!isTruthy(input$country) || !isTruthy(input$mpa)) {
      par(mar = c(0,0,2,0))
      plot.new()
      title("Select a region and MPA to view the network", cex.main = 1)
      return()  # draw placeholder and stop here
    }
    
  
    g <- selected_graph()
    
    if (is.null(g) || !inherits(g, "igraph")) {
      par(mar = c(0,0,2,0)); plot.new()
      title("No network data available for this MPA", cex.main = 1)
      return()
    }
    
    message(sprintf("Rendering %s / %s  |  v=%d e=%d",
                    input$country, input$mpa, igraph::vcount(g), igraph::ecount(g)))
    
    l <- layout_as_bipartite(g,maxiter=100)
    g<-set_graph_attr(g, "layout", l[,2:1])
      
    op <- par(mar = c(1,1,3,1)); on.exit(par(op), add = TRUE)
    plot(g, edge.curved = .2,vertex.label=V(g)$name,
           vertex.size=7, vertex.label.dist=7, layout=l[,2:1],
           vertex.label.degree = pi*V(g)$type)
    return(invisible(NULL))
    
    
      
   
  },height=850)
}

shinyApp(ui, server)

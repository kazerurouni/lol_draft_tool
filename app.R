library(tidyverse)
library(rlang)
library(shiny)

# Load data
stats24 <- read_csv("2024_LoL_esports_match_data_from_OraclesElixir.csv")
stats25 <- read_csv("2025_LoL_esports_match_data_from_OraclesElixir.csv")
stats <- bind_rows(stats24, stats25)

# Data wrangling

team <- stats |>
  select(gameid, teamname, side, position, starts_with("pick"), result) |>
  filter(position == "team") |>
  pivot_longer(
    cols = starts_with("pick"),
    names_to = "order",
    values_to = "champion"
  )

pick_order <- stats |>
  filter(position != "team") |>
  select(gameid, side, position, champion) |>
  left_join(
    team,
    by = c("gameid", "champion")
  ) |>
  select(gameid, side.x, position.x, champion, order, result) |>
  mutate(
    pick_order_num = case_when(
      side.x == "Blue" & order == "pick1" ~ 10,
      side.x == "Red" & order == "pick1" ~ 9,
      side.x == "Red" & order == "pick2" ~ 8,
      side.x == "Blue" & order == "pick2" ~ 7,
      side.x == "Blue" & order == "pick3" ~ 6,
      side.x == "Red" & order == "pick3" ~ 5,
      side.x == "Red" & order == "pick4" ~ 4,
      side.x == "Blue" & order == "pick4" ~ 3,
      side.x == "Blue" & order == "pick5" ~ 2,
      side.x == "Red" & order == "pick5" ~ 1,
      TRUE ~ NA_real_  # Default value for other cases
    )
  )

pick_order <- pick_order |>
  left_join(
    pick_order,
    by = c("gameid", "position.x"),
    relationship = "many-to-many"
  ) |>
  filter(side.x.x != side.x.y) |>
  distinct(gameid, position.x, champion.x, .keep_all = TRUE) |>
  select(-order.x, -order.y, -starts_with("side")) |>
  mutate(
    position_new = case_when(
      position.x == "top" ~ "Top",
      position.x == "jng" ~ "Jungle",
      position.x == "mid" ~ "Mid",
      position.x == "bot" ~ "ADC",
      position.x == "sup" ~ "Support",
      TRUE ~ position.x  # Keep other values as they are
    )
  )

# UI
roles <- c("Top", "Jungle", "Mid", "ADC", "Support")
champs <- c("Aatrox", "Ahri", "Akali", "Akshan", "Alistar", "Ambessa", "Amumu", "Anivia", "Annie", "Aphelios", "Ashe", "Aurelion Sol", "Aurora", "Azir", "Bard", "Bel'Veth", "Blitzcrank", "Brand", "Braum", "Briar", "Caitlyn", "Camille", "Cassiopeia", "Cho'Gath", "Corki", "Darius", "Diana", "Dr. Mundo", "Draven", "Ekko", "Elise", "Evelynn", "Ezreal", "Fiddlesticks", "Fiora", "Fizz", "Galio", "Gangplank", "Garen", "Gnar", "Gragas", "Graves", "Gwen", "Hecarim", "Heimerdinger", "Hwei", "Illaoi", "Irelia", "Ivern", "Janna", "Jarvan IV", "Jax", "Jayce", "Jhin", "Jinx", "K'Sante", "Kai'Sa", "Kalista", "Karma", "Karthus", "Kassadin", "Katarina", "Kayle", "Kayn", "Kennen", "Kha'Zix", "Kindred", "Kled", "Kog'Maw", "LeBlanc", "Lee Sin", "Leona", "Lillia", "Lissandra", "Lucian", "Lulu", "Lux", "Malphite", "Malzahar", "Maokai", "Master Yi", "Mel", "Milio", "Miss Fortune", "Mordekaiser", "Morgana", "Naafiri", "Nami", "Nasus", "Nautilus", "Neeko", "Nidalee", "Nilah", "Nocturne", "Nunu & Willump", "Olaf", "Orianna", "Ornn", "Pantheon", "Poppy", "Pyke", "Qiyana", "Quinn", "Rakan", "Rammus", "Rek'Sai", "Rell", "Renata Glasc", "Renekton", "Rengar", "Riven", "Rumble", "Ryze", "Samira", "Sejuani", "Senna", "Seraphine", "Sett", "Shaco", "Shen", "Shyvana", "Singed", "Sion", "Sivir", "Skarner", "Smolder", "Sona", "Soraka", "Swain", "Sylas", "Syndra", "Tahm Kench", "Taliyah", "Talon", "Taric", "Teemo", "Thresh", "Tristana", "Trundle", "Tryndamere", "Twisted Fate", "Twitch", "Udyr", "Urgot", "Varus", "Vayne", "Veigar", "Vel'Koz", "Vex", "Vi", "Viego", "Viktor", "Vladimir", "Volibear", "Warwick", "Wukong", "Xayah", "Xerath", "Xin Zhao", "Yasuo", "Yone", "Yorick", "Yuumi", "Zac", "Zed", "Zeri", "Ziggs", "Zilean", "Zoe", "Zyra")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("role1", label = "Select role:", choices = roles),
      selectInput("champ", label = "Select champ:", choices = c('',champs)),
      textOutput("overall"),
      verbatimTextOutput("overall_wr"),
      textOutput("strong_text"),
      verbatimTextOutput("strong"),
      textOutput("weak_text"),
      verbatimTextOutput("weak")
    ),
    mainPanel(
      textOutput("teamcomp"),
      verbatimTextOutput("summary")
    )
  )
)


# Server

server <- function(input, output, session) {
  role1 <- reactive({
    input$role1
  })
  other_roles <- reactive({roles[roles != role1()]})
  output$summary <- renderPrint({
    
    match_champs <- stats |>
      filter(!is.na(playername)) |>
      group_by(gameid, league, side) |>
      summarise(
        champs = str_flatten(champion, ", "),
        result = mean(result),
        .groups = "drop"
      ) |>
      separate_wider_delim(
        champs,
        delim = ", ",
        names = c("Top", "Jungle", "Mid", "ADC", "Support"),
        too_few = "align_start"
      )
    
    for (role2 in other_roles()) {
      result <- match_champs |>
        filter(!!sym(role1()) == input$champ) |>
        group_by(!!sym(role2)) |>
        summarise(
          winrate = round(mean(result), 4) * 100,
          games = n()
        ) |>
        arrange(desc(games)) |>
        filter(games > 5)
      print(result, n = 20)
    }
  })
  output$overall_wr <- renderPrint({
    pick_order |>
      filter(position_new == input$role1, champion.x == input$champ) |>
      mutate(
        pick_order = case_when(
          pick_order_num.x > pick_order_num.y ~ "Blind pick",
          pick_order_num.x < pick_order_num.y ~ "Counter pick"
        )
      ) |>
      filter(!is.na(pick_order)) |>
      group_by(pick_order) |>
      summarise(
        winrate = round(mean(result.x), 4) * 100,
        games = n()
      )
  })
  output$weak <- renderPrint({
    result <- pick_order |>
      filter(position_new == input$role1, champion.x == input$champ, pick_order_num.x > pick_order_num.y) |>
      group_by(champion.y) |>
      summarise(
        winrate = round(mean(result.y), 4) * 100,
        games = n()
      ) |>
      arrange(desc(games)) |>
      filter(games > 5, winrate >= 45)
    print(result)
  })
  output$strong <- renderPrint({
    pick_order |>
      filter(position_new == input$role1, champion.x == input$champ, pick_order_num.x < pick_order_num.y) |>
      group_by(champion.y) |>
      summarise(
        winrate = round(mean(result.x), 4) * 100,
        games = n()
      ) |>
      arrange(desc(games)) |>
      filter(games > 5, winrate >= 45)
  })
  output$overall <- renderText({"Overall winrate: "})
  output$weak_text <- renderText({"Should't pick into:"})
  output$strong_text <- renderText({"Can pick into:"})
  output$teamcomp <- renderText({"Team comp:"})
}
shinyApp(ui, server)

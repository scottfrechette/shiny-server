library(shiny)

roll_attack <- function(horde_size, roll_type) {
  
  attack_rolls_a <- sample(1:20, horde_size, replace = T) #+ attack_bonus
  attack_rolls_b <- sample(1:20, horde_size, replace = T) #+ attack_bonus
  ifelse(roll_type == "Normal", 
         attack_rolls <- attack_rolls_a,
         ifelse(roll_type == "Disadvantage", 
                attack_rolls <- pmin(attack_rolls_a, attack_rolls_b),
                attack_rolls <- pmax(attack_rolls_a, attack_rolls_b)))
  
  return(attack_rolls)
  
}
add_attack_bonus <- function(attack_rolls, attack_bonus) {attack_rolls + attack_bonus}
roll_damage <- function(horde_size, damage_die, damage_bonus) {
  
  sample(1:damage_die, horde_size, replace = T) + damage_bonus
  
}
calc_total_ac <- function(ac, buffs, shield) {ac + buffs + (shield * 5)}
calc_num_hits <- function(attack_rolls, total_ac) {sum(attack_rolls > total_ac)}
calc_damage <- function(attack_rolls, total_ac, damage_rolls) {
  
  sum((attack_rolls > total_ac) * damage_rolls)
  
}
sim_attack <- function(horde_size, roll_type, attack_bonus,
                       damage_die, damage_bonus,
                       ac, buffs, shield,
                       verbose = F) {
  
  attack_rolls <- roll_attack(horde_size, roll_type)
  total_attack <- add_attack_bonus(attack_rolls, attack_bonus)
  damage_rolls <- roll_damage(horde_size, damage_die, damage_bonus)
  total_ac <- calc_total_ac(ac, buffs, shield)
  hits <- calc_num_hits(total_attack, total_ac)
  damage <- calc_damage(total_attack, total_ac, damage_rolls)
  
  out <- c(damage,
           paste(hits, "enemies landed a blow for total damage of", damage))
  
  if(!verbose) out <- as.integer(out[1])
  
  return(out)
  
}
sim_many_attacks <- function(sims = 100,
                             horde_size, roll_type, attack_bonus,
                             damage_die, damage_bonus, 
                             ac, buffs, shield) {
  
  replicate(sims, 
            sim_attack(horde_size, roll_type, attack_bonus,
                       damage_die, damage_bonus, 
                       ac, buffs, shield))
  
}

ui <- fluidPage(
  
  titlePanel("Horde Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      ## PC
      selectInput("pc", "Select Character:",
                  c("Cad Bury", "Kirk Saurpike", "Mac Silveen",
                    "Rick Dresden", "Toblakai")),
      htmlOutput("ac"),
      numericInput("buffs", "AC Modifiers (Buff/Debuff)", 0),
      checkboxInput("shield", "Character casts Shield"),
      htmlOutput("mod_ac"),
      
      ## Horde
      numericInput("horde_size", "Horde Size", 10),
      selectInput("roll_type", "Roll Type", c("Advantage", "Normal", "Disadvantage"), selected = "Normal"),
      numericInput("attack_bonus", "Attack Bonus", 4),
      numericInput("damage_die", "Damage Die (1dx)", 6),
      numericInput("damage_bonus", "Damage Bonus", 0)
      
      ## Submit
      # submitButton("Roll Die")
      
    ),
    
    mainPanel(htmlOutput("sim"),
              htmlOutput("nat1"),
              htmlOutput("nat20"),
              br(),
              htmlOutput("default"),
              hr(),
              plotOutput("sim_plot"),
              htmlOutput("median"),
              htmlOutput("mode"),
              htmlOutput("range50"),
              htmlOutput("range95"),
              htmlOutput("range100"))
  )
)

server <- function(input, output) {
  
  pc_ac <- reactive(switch(input$pc,
                           `Cad Bury` = 16,
                           `Kirk Saurpike` = 20,
                           `Mac Silveen` = 19,
                           `Rick Dresden` = 13,
                           `Toblakai` = 18))
  
  attack_rolls <- reactive(roll_attack(input$horde_size, input$roll_type))
  total_attack <- reactive(add_attack_bonus(attack_rolls(), input$attack_bonus))
  damage_rolls <- reactive(roll_damage(input$horde_size, input$damage_die, input$damage_bonus))
  total_ac <- reactive(calc_total_ac(pc_ac(), input$buffs, input$shield))
  hits <- reactive(calc_num_hits(total_attack(), total_ac()))
  damage <- reactive(calc_damage(total_attack(), total_ac(), damage_rolls()))
  
  default_damage <- reactive(floor(input$horde_size * 0.25) * (sample(1:input$damage_die, 1) + input$damage_bonus))
  
  sims <- reactive(sim_many_attacks(sims = 1e4,
                                    input$horde_size, input$roll_type, input$attack_bonus,
                                    input$damage_die, input$damage_bonus,
                                    pc_ac(), input$buffs, input$shield))
  
  lower50 <- reactive(pmax(0, round(qnorm(0.25, mean(sims()), sd(sims())))))
  upper50 <- reactive(pmax(0, round(qnorm(0.75, mean(sims()), sd(sims())))))
  lower95 <- reactive(pmax(0, round(qnorm(0.025, mean(sims()), sd(sims())))))
  upper95 <- reactive(pmax(0, round(qnorm(0.975, mean(sims()), sd(sims())))))
  
  output$ac <- renderText(paste("<i>AC</i>:", pc_ac()))
  output$mod_ac <- renderText(paste("<i>Modified AC</i>:", total_ac()))
  
  output$sim <- renderText(paste("<b>Simulation</b>:", hits(), 
                                 "enemies land a blow for total damage of", damage(), "(red line)"))
  
  output$nat1 <- renderText(paste0("<b>Nat 1 rolls</b> :", sum(attack_rolls() == 1), " (expected ", input$horde_size / 20, ")"))
  output$nat20 <- renderText(paste0("<b>Crit rolls</b>: ", sum(attack_rolls() == 20), " (expected ", input$horde_size / 20, ")"))
  
  output$default <- renderText(paste("<b>Default rules</b>:", floor(input$horde_size * 0.25), 
                                     "enemies hit for", default_damage(), "total damage (blue line)"))
  
  output$median <- renderText(paste("<b>Median damage:</b>", pmax(0, median(sims()))))
  output$mode <- renderText(paste("<b>Most likely damage:</b>", 
                                  pmax(0, unique(sims())[which.max(tabulate(match(sims(), unique(sims()))))])))
  output$range50 <- renderText(paste0("<b>Typical range: </b>", lower50(), "-", upper50()))
  output$range95 <- renderText(paste0("<b>Credible range: </b>", lower95(), "-", upper95()))
  output$range100 <- renderText(paste0("<b>Full range: </b>", pmax(0, min(sims())), "-", max(sims())))
  
  output$sim_plot <- renderPlot({
    
    hist(sims(),
         breaks = max(sims()) - min(sims()),
         border = 'white',
         xlab = "Damage",
         ylab = NULL,
         yaxt = "n",
         main = "Where does this roll fall among 10,000 simulations?")
    abline(v = damage(), col = "red", lwd = 3)
    abline(v = default_damage(), col = 'blue', lwd = 3)
    
  })
  
}

shinyApp(ui = ui, server = server)

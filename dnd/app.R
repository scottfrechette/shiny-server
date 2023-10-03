library(shiny)
library(tidyverse)

roll_attack <- function(horde_size, roll_type) {
  
  attack_rolls_a <- sample(1:20, horde_size, replace = T) 
  attack_rolls_b <- sample(1:20, horde_size, replace = T) 
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


ui  <- navbarPage(
  
  collapsible = T,
  inverse = F,
  
  title = "DND Simulation",
  
  tabPanel("Skill Challenge", 
           
           sidebarLayout(
             sidebarPanel(
               sliderInput("skill_mod_range", "Modifier Range", 1, 20, 
                           value = c(4, 12), step = 1, ticks = F),
               sliderInput("skill_dc_range", "DC Range", 1, 40, 
                           value = c(10, 25), step = 1, ticks = F),
               checkboxInput("skill_advantage", "Advantage"),
               checkboxInput("skill_guidance", "Guidance")
             ),
             
             mainPanel(plotOutput("skill_sim_plot"))
           )
  ),
  
  tabPanel("Horde Attacks",
           sidebarLayout(
             sidebarPanel(
               ## PC
               p(strong("Character Inputs:")),
               selectInput("attack_pc", "Select Character:", c("Cad Bury", "Kirk Saurpike", "Mac Silveen", "Rick Dresden", "Toblakai")),
               htmlOutput("attack_ac"),
               numericInput("attack_buffs", "AC Modifiers (Buff/Debuff)", 0),
               checkboxInput("attack_shield", "Character casts Shield"),
               htmlOutput("attack_mod_ac"),
               hr(),
               
               ## Horde
               p(strong("Horde Inputs:")),
               numericInput("attack_horde_size", "Horde Size", 10),
               # sliderInput("attack_horde_size", "Horde Size", min = 1, max = 100, value = 10),
               selectInput("attack_roll_type", "Roll Type", c("Advantage", "Normal", "Disadvantage"), selected = "Normal"),
               numericInput("attack_attack_bonus", "Attack Bonus", 4),
               numericInput("attack_damage_die", "Damage Die (1dx)", 6),
               numericInput("attack_damage_bonus", "Damage Bonus", 0),
               
               ## Submit
               actionButton("attack_dice_roll", "Roll Die", icon = icon("dice-d20"))
               
             ),
             
             mainPanel(htmlOutput("attack_sim"),
                       htmlOutput("attack_nat1"),
                       htmlOutput("attack_nat20"),
                       br(),
                       htmlOutput("attack_default"),
                       hr(),
                       plotOutput("attack_sim_plot"),
                       htmlOutput("attack_median"),
                       htmlOutput("attack_mode"),
                       htmlOutput("attack_range50"),
                       htmlOutput("attack_range95"),
                       htmlOutput("attack_range100")
             )
           )
  ),
  
  tabPanel("Horde Saves", 
           sidebarLayout(
             sidebarPanel(
               selectInput("saves_pc", "Select Character:", c("Cad Bury", "Kirk Saurpike", "Mac Silveen", "Rick Dresden")),
               htmlOutput("saves_dc"),
               numericInput("saves_horde_size", "Horde Size", 10),
               # numericInput("saves_dc", "Character Spell DC", 13),
               numericInput("saves_mod", "Horde Saving Throw Modifier", 0),
               checkboxInput("saves_resistance", "Resistance"),
               numericInput("saves_damage", "Damage Rolled", 10),
               actionButton("saves_dice_roll", "Roll Die", icon = icon("dice-d20"))
             ),
             
             mainPanel(htmlOutput("saves"),
                       htmlOutput("damage"))
           )
  ),
  
  tabPanel("HP", 
           sidebarLayout(
             sidebarPanel("TBD"),
             
             mainPanel("TBD")
           )
  )
)

server <- function(input, output) {
  
  
  # Skill Challenge ---------------------------------------------------------
  
  skill_rolls <- sample(1:20, 1e4, replace = T)
  skill_adv_rolls <- sample(1:20, 1e4, replace = T)
  skill_guidance <- sample(1:4, 1e4, replace = T)
  
  skill_total_rolls <- reactive(pmax(skill_rolls, 
                                     skill_adv_rolls * input$skill_advantage) + 
                                  skill_guidance * input$skill_guidance)
  
  skill_chances <- reactive(crossing(mod = input$skill_mod_range[1]:input$skill_mod_range[2],
                                     dc = input$skill_dc_range[1]:input$skill_dc_range[2]) %>% 
                              mutate(pct = map2_dbl(mod, dc, 
                                                    ~mean(skill_total_rolls() + .x > .y))))
  
  output$skill_sim_plot <- renderPlot(
    
    skill_chances() %>% 
      ggplot(aes(x = dc, y = mod)) + 
      geom_tile(aes(fill = pct), alpha = 0.5, na.rm = FALSE) + 
      geom_text(aes(label = scales::percent(pct, accuracy = 1))) +
      scale_x_continuous(breaks = input$skill_dc_range[1]:input$skill_dc_range[2], 
                         expand = c(0, 0)) +
      scale_y_reverse(breaks = input$skill_mod_range[2]:input$skill_mod_range[1], 
                      expand = c(0, 0)) +
      scale_fill_gradient(low = "white", high = "#0072B2", limits = c(0, 1)) +
      guides(fill = "none") +
      theme_minimal() +
      theme(axis.text.y = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold", size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = 'Simulated success rate of skill challenge',
           subtitle = "Based on 10,000 simulations within selected range of skill modifier and DC",
           x = "DC",
           y = "Skill Modifier"),
    
    res = 96
    
  )
  
  # Attacks -----------------------------------------------------------------
  
  pc_ac <- reactive(switch(input$attack_pc,
                           `Cad Bury` = 20,
                           `Kirk Saurpike` = 23,
                           `Mac Silveen` = 22,
                           `Rick Dresden` = 15))
  
  attack_data <- reactiveValues(attack_rolls = roll_attack(10, "Normal"),
                                damange_rolls = roll_damage(10, 6, 0),
                                horde_size = 10,
                                roll_type = "Normal")
  
  observeEvent(input$attack_dice_roll, {
    attack_data$attack_rolls <- roll_attack(input$attack_horde_size, input$attack_roll_type)
    attack_data$damage_rolls <- roll_damage(input$attack_horde_size, input$attack_damage_die, input$attack_damage_bonus)
    attack_data$horde_size = input$attack_horde_size
    attack_data$roll_type <- input$attack_roll_type
  })
  
  total_attack <- reactive(add_attack_bonus(attack_data$attack_rolls, input$attack_attack_bonus))
  total_ac <- reactive(calc_total_ac(pc_ac(), input$attack_buffs, input$attack_shield))
  hits <- reactive(calc_num_hits(total_attack(), total_ac()))
  damage <- reactive(calc_damage(total_attack(), total_ac(), attack_data$damage_rolls))
  
  default_damage <- reactive(floor(attack_data$horde_size * 0.25) * (sample(1:input$attack_damage_die, 1) + input$attack_damage_bonus))
  
  sims <- reactive(sim_many_attacks(sims = 1e4,
                                    attack_data$horde_size, attack_data$roll_type, input$attack_attack_bonus,
                                    input$attack_damage_die, input$attack_damage_bonus,
                                    pc_ac(), input$attack_buffs, input$attack_shield))
  
  lower50 <- reactive(pmax(0, round(qnorm(0.25, mean(sims()), sd(sims())))))
  upper50 <- reactive(pmax(0, round(qnorm(0.75, mean(sims()), sd(sims())))))
  lower95 <- reactive(pmax(0, round(qnorm(0.025, mean(sims()), sd(sims())))))
  upper95 <- reactive(pmax(0, round(qnorm(0.975, mean(sims()), sd(sims())))))
  
  output$attack_ac <- renderText(paste("<i>AC</i>:", pc_ac()))
  output$attack_mod_ac <- renderText(paste("<i>Modified AC</i>:", total_ac()))
  
  output$attack_sim <- renderText(paste("<b>Simulation</b>:", hits(), 
                                        "enemies land a blow for total damage of", damage(), "(red line)"))
  
  output$attack_nat1 <- renderText(paste0("<i>Nat 1 rolls</i> :", sum(attack_data$attack_rolls == 1), " (expected ", attack_data$horde_size / 20, ")"))
  output$attack_nat20 <- renderText(paste0("<i>Crit rolls</i>: ", sum(attack_data$attack_rolls == 20), " (expected ", attack_data$horde_size / 20, ")"))
  
  output$attack_default <- renderText(paste("<b>Default rules</b>:", floor(attack_data$horde_size * 0.25), 
                                            "enemies hit for", default_damage(), "total damage (blue line)"))
  
  output$attack_median <- renderText(paste("<b>Median damage:</b>", pmax(0, median(sims()))))
  output$attack_mode <- renderText(paste("<b>Most likely damage:</b>", 
                                         pmax(0, unique(sims())[which.max(tabulate(match(sims(), unique(sims()))))])))
  output$attack_range50 <- renderText(paste0("<b>Typical range: </b>", lower50(), "-", upper50()))
  output$attack_range95 <- renderText(paste0("<b>Credible range: </b>", lower95(), "-", upper95()))
  output$attack_range100 <- renderText(paste0("<b>Full range: </b>", pmax(0, min(sims())), "-", max(sims())))
  
  output$attack_sim_plot <- renderPlot({
    
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
  
  
  # Saves -------------------------------------------------------------------
  
  save_dc <- reactive(switch(input$saves_pc,
                             `Cad Bury` = 16,
                             `Kirk Saurpike` = 16,
                             `Mac Silveen` = 15,
                             `Rick Dresden` = 14))
  
  save_spell_ability <- reactive(switch(input$saves_pc,
                                        `Cad Bury` = "INT",
                                        `Kirk Saurpike` = "CHA",
                                        `Mac Silveen` = "WIS",
                                        `Rick Dresden` = "CHA"))
  
  saves_data <- reactiveValues(saves_dc_success = sample(1:20, 10, replace = T) >= 13 - 0)
  
  observeEvent(input$saves_dice_roll, {
    saves_data$saves_dc_success <- sample(1:20, input$saves_horde_size, replace = T) >= save_dc() - input$saves_mod
  })
  
  horde_damage <- reactive(sum(input$saves_damage * (1 / (saves_data$saves_dc_success + 1)) * (1 / (input$saves_resistance + 1))))
  
  output$saves_dc <- renderText(paste("<i>DC</i>:", save_dc()))
  output$saves <- renderText(paste0("<b>Horde Saves: </b>", sum(saves_data$saves_dc_success), " enemies pass their saving throws and ", sum(!saves_data$saves_dc_success), " fail"))
  output$damage <- renderText(paste0("<b>Horde Damage: </b>", horde_damage()))
  
}

shinyApp(ui = ui, server = server)
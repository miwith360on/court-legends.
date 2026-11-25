# ============================================================
# Court Legends  v1.0  -  Recruiting and your squad
# Run in RStudio: open this file and click "Run App", or in the Console:
#   shiny::runApp('C:/Users/Kqumo/Documents/court_legends_2')
#
# World:  Rookie Town -> Route 1 -> Blacktop City (Gym 1) -> Route 2 -> Riverside (Gym 2)
#
# Battles: possessions alternate.
#  - Your ball: pick a shot. The CPU picks a defense you can't see.
#  - Their ball: pick a defense. The CPU picks a shot you can't see.
#  Defense: Contest (-10% any shot), Block (great vs Drive, weak vs Three),
#           Steal (risky), Rest (+20 stamina, no D).
#  Rebounds: a miss can be an offensive rebound (you keep the ball).
#  Signature moves unlock at Lv 3, once per battle.
#  Gatorade (from the gear shop): +40 stamina, uses your possession.
#
# Progress: shots train shooting, drives train speed, stops train defense,
#  long battles train stamina. No stat cap. Wins give XP and coins.
#  Coins buy gear (permanent stat boosts) and Gatorade.
#  Progress saves to court_legends_save.rds in this folder.
# ============================================================

library(shiny)

SAVE_FILE <- "court_legends_save.rds"
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Species (starting stats) -------------------------------
species <- list(
  Blazehoop = list(shooting = 85, speed = 60, defense = 50, stamina = 100),
  Dunkadon  = list(shooting = 55, speed = 70, defense = 80, stamina = 120),
  Swishtail = list(shooting = 75, speed = 90, defense = 55, stamina = 90),
  # Wild-only species: you can only get these by recruiting them
  Rimrattler = list(shooting = 70, speed = 70, defense = 70, stamina = 100),
  Glasshog   = list(shooting = 50, speed = 55, defense = 85, stamina = 135),
  Zoomander  = list(shooting = 65, speed = 95, defense = 50, stamina = 85)
)
STARTERS  <- c("Blazehoop", "Dunkadon", "Swishtail")
SQUAD_MAX <- 6

new_creature <- function(name, level = 1) {
  b <- species[[name]]
  list(name = name, level = level, xp = 0,
       shooting = b$shooting, speed = b$speed, defense = b$defense,
       max_stamina = b$stamina, stamina = b$stamina, points = 0,
       train = list(shooting = 0, speed = 0, defense = 0, stamina = 0),
       wins = 0, losses = 0,
       coins = 0, gear = character(0), drinks = 0,
       badges = character(0), beaten = character(0), bench = list())
}

# Older saves don't have the newer fields yet
upgrade_save <- function(p) {
  if (is.null(p)) return(NULL)
  p$coins  <- p$coins  %||% 0
  p$gear   <- p$gear   %||% character(0)
  p$drinks <- p$drinks %||% 0
  p$badges <- p$badges %||% character(0)
  p$beaten <- p$beaten %||% character(0)
  p$bench  <- p$bench  %||% list()
  p
}

# ---- Squad -----------------------------------------------------
# The active creature carries the coach's stuff (coins, gear, badges...).
# Gear and badge bonuses belong to the coach, so they move to whoever is active.
COACH_FIELDS <- c("coins", "gear", "drinks", "badges", "beaten", "bench", "wins", "losses")

coach_bonus <- function(p) {
  b <- list(shooting = 0, speed = 0, defense = 0, max_stamina = 0)
  for (g in p$gear) { it <- SHOP[[g]]; if (!is.na(it$stat)) b[[it$stat]] <- b[[it$stat]] + it$amt }
  for (bd in p$badges) { st <- BADGE_STAT[[bd]] %||% "shooting"; b[[st]] <- b[[st]] + 2 }
  b
}
apply_bonus <- function(cr, b, sign) {
  for (st in names(b)) cr[[st]] <- cr[[st]] + sign * b[[st]]
  cr$stamina <- min(cr$stamina, cr$max_stamina)
  cr
}

# Swap the active creature with bench slot i
swap_active <- function(p, i) {
  b   <- coach_bonus(p)
  old <- apply_bonus(p, b, -1)
  new <- p$bench[[i]]
  for (f in COACH_FIELDS) { new[[f]] <- p[[f]]; old[[f]] <- NULL }
  new$bench[[i]] <- old
  new <- apply_bonus(new, b, +1)
  new$stamina <- new$max_stamina
  new
}

# A beaten wild creature, cleaned up to join the bench
to_recruit <- function(cpu) {
  r <- new_creature(cpu$name, cpu$level)
  r$shooting <- cpu$shooting; r$speed <- cpu$speed; r$defense <- cpu$defense
  r$max_stamina <- cpu$max_stamina; r$stamina <- cpu$max_stamina
  for (f in COACH_FIELDS) r[[f]] <- NULL
  r
}

recruit_chance <- function(p, cpu) {
  margin <- p$points - cpu$points
  max(0.30, min(0.90, 0.40 + 0.05 * margin))
}

# CPU opponent. Wild ones land around your level; trainers set their own.
make_cpu <- function(level, kind = NULL, style = NULL, boost = 0, boost_stat = "shooting") {
  lvl  <- max(1, level)
  cpu  <- new_creature(kind %||% sample(names(species), 1), lvl)
  bump <- (lvl - 1) * 2
  cpu$shooting    <- cpu$shooting + bump
  cpu$speed       <- cpu$speed + bump
  cpu$defense     <- cpu$defense + bump
  cpu$max_stamina <- cpu$max_stamina + (lvl - 1) * 3
  cpu$stamina     <- cpu$max_stamina
  cpu[[boost_stat]] <- cpu[[boost_stat]] + boost
  cpu$style       <- style
  cpu
}

# ---- Progression formulas -----------------------------------
train_need <- function(stat) ceiling(10 * (stat / 50)^2)   # practice for the next +1
xp_needed  <- function(level) round(50 * level^1.5)        # XP for the next level
ovr <- function(cr) round(mean(c(cr$shooting, cr$speed, cr$defense)))

# ---- Offense moves ------------------------------------------
moves <- list(
  Jumper = list(stat = "shooting", base = 0.50, pts = 2, cost = 10),
  Three  = list(stat = "shooting", base = 0.35, pts = 3, cost = 15),
  Drive  = list(stat = "speed",    base = 0.55, pts = 2, cost = 20),
  Rest   = list(stat = NA,         base = NA,   pts = 0, cost = -30)
)
move_train <- list(
  Jumper = list(stat = "shooting", try = 2, make = 1),
  Three  = list(stat = "shooting", try = 3, make = 1),
  Drive  = list(stat = "speed",    try = 3, make = 1)
)

# ---- Defense moves ------------------------------------------
def_moves <- list(
  Contest = list(cost = 5,   label = "Contest (-10% on jumpers and threes, -5 stamina)"),
  Block   = list(cost = 10,  label = "Block (great vs Drive, weak vs Three, -10)"),
  Steal   = list(cost = 15,  label = "Steal (risky, -15 stamina)"),
  Rest    = list(cost = -20, label = "Rest (+20 stamina, no D)")
)
block_mod <- c(Jumper = -0.15, Three = -0.05, Drive = -0.25)

# ---- Signature moves (unlock at SIG_LEVEL, 1x per battle) ----
SIG_LEVEL <- 3
sig_moves <- list(
  Blazehoop = list(name = "Deep Three",    stat = "shooting", base = 0.30, pts = 4, cost = 25,
                   desc = "4-point bomb from way downtown"),
  Dunkadon  = list(name = "Posterizer",    stat = "speed",    base = 0.60, pts = 2, cost = 25,
                   effect = "drain", desc = "Slam that drains 25 of their stamina"),
  Swishtail = list(name = "Ankle Breaker", stat = "speed",    base = 0.60, pts = 2, cost = 20,
                   effect = "ankle", desc = "Crossover that ignores their defense"),
  Rimrattler = list(name = "Bank Shot",    stat = "shooting", base = 0.48, pts = 3, cost = 20,
                   desc = "Off the glass: a 3-pointer that's easier to make"),
  Glasshog  = list(name = "Putback Slam",  stat = "defense",  base = 0.60, pts = 2, cost = 20,
                   effect = "drain", desc = "Crashes the rim and drains 25 of their stamina"),
  Zoomander = list(name = "Blur Step",     stat = "speed",    base = 0.60, pts = 2, cost = 20,
                   effect = "ankle", desc = "Euro step that ignores their defense")
)
sig_ready <- function(cr) cr$level >= SIG_LEVEL && !isTRUE(cr$sig_used)

# ---- Rebounding ---------------------------------------------
reb_rating  <- function(cr) cr$defense + cr$max_stamina / 2
oreb_chance <- function(att, def) {
  tired <- if (att$stamina < 30) 0.10 else 0
  max(0.10, min(0.45, 0.25 + (reb_rating(att) - reb_rating(def)) / 250 - tired))
}

# ---- Battle logic -------------------------------------------
make_chance <- function(attacker, defender, move) {
  stat_bonus <- (attacker[[move$stat]] - defender$defense) / 200
  tired_pen  <- if (attacker$stamina < 30) 0.15 else 0
  move$base + stat_bonus - tired_pen
}
clamp <- function(x) max(0.05, min(0.95, x))

# One possession: `att` runs `off_move`, `def` plays `def_move`.
resolve_possession <- function(att, def, off_move, def_move) {
  out <- list(made = NA, stolen = FALSE, oreb = FALSE)
  is_sig <- off_move == "Sig"
  sig    <- if (is_sig) sig_moves[[att$name]] else NULL

  # Defense setup
  if (def_move == "Rest") {
    def$stamina <- min(def$max_stamina, def$stamina + 20)
    dtxt <- sprintf("%s rests on D", def$name)
    dmod <- 0.05
  } else if (def$stamina < def_moves[[def_move]]$cost) {
    dtxt <- sprintf("%s is too tired to %s", def$name, tolower(def_move))
    dmod <- 0.05
    def_move <- "Tired"
  } else {
    def$stamina <- def$stamina - def_moves[[def_move]]$cost
    dtxt <- switch(def_move,
                   Contest = sprintf("%s contests", def$name),
                   Block   = sprintf("%s goes for the block", def$name),
                   Steal   = sprintf("%s goes for the steal", def$name))
    dmod <- switch(def_move,
                   Contest = if (off_move == "Drive") -0.04 else -0.10,   # a hand up barely slows a drive
                   Block   = if (off_move %in% names(block_mod)) block_mod[[off_move]] else 0,
                   Steal   = 0)
  }
  out$def_move <- def_move

  # Offense: resting
  if (off_move == "Rest") {
    att$stamina <- min(att$max_stamina, att$stamina + 30)
    msg <- sprintf("%s holds the ball and catches their breath (+30 stamina).", att$name)
    return(c(out, list(att = att, def = def, msg = msg)))
  }

  m <- if (is_sig) sig else moves[[off_move]]
  move_name <- if (is_sig) toupper(sig$name) else off_move
  if (att$stamina < m$cost) {
    msg <- sprintf("%s is too tired for a %s! Turnover.", att$name, move_name)
    return(c(out, list(att = att, def = def, msg = msg)))
  }

  # Ankle Breaker: the defender's move doesn't matter
  if (is_sig && identical(sig$effect, "ankle")) {
    dmod <- 0
    dtxt <- paste(dtxt, "but gets their ankles broken")
  } else if (def_move == "Block" && is_sig) {
    dmod <- -0.15
  }

  # Steal attempt
  if (def_move == "Steal" && !(is_sig && identical(sig$effect, "ankle"))) {
    # Drives are easiest to pick off; catch-and-shoot jumpers are hardest
    steal_mult <- switch(if (is_sig) "Sig" else off_move, Drive = 1.3, Jumper = 0.6, Three = 0.6, 1)
    p_steal <- max(0.05, min(0.45, (0.20 + (def$speed - att$speed) / 300) * steal_mult))
    if (runif(1) < p_steal) {
      att$stamina <- att$stamina - ceiling(m$cost / 2)
      out$stolen <- TRUE
      msg <- sprintf("%s... STOLEN! %s loses the ball. (%.0f%% steal)",
                     dtxt, att$name, p_steal * 100)
      return(c(out, list(att = att, def = def, msg = msg)))
    }
    dmod <- 0.10
    dtxt <- paste(dtxt, "and gambles wrong")
  }

  # The shot
  att$stamina <- att$stamina - m$cost
  chance <- clamp(make_chance(att, def, m) + dmod)
  made   <- runif(1) < chance
  out$made <- made

  if (made) {
    att$points <- att$points + m$pts
    result <- sprintf("BUCKET! +%d", m$pts)
    if (is_sig && identical(sig$effect, "drain")) {
      def$stamina <- max(0, def$stamina - 25)
      result <- paste(result, sprintf("%s is shook (-25 stamina)", def$name))
    }
  } else if (def_move == "Block") {
    result <- "BLOCKED!"
  } else {
    result <- "missed!"
  }
  msg <- sprintf("%s. %s uses %s... %s (%.0f%% shot)",
                 dtxt, att$name, move_name, result, chance * 100)

  # Rebound on a miss
  if (!made) {
    p_oreb <- oreb_chance(att, def)
    if (runif(1) < p_oreb) {
      out$oreb <- TRUE
      msg <- paste(msg, sprintf("OFFENSIVE REBOUND %s! (%.0f%%)", att$name, p_oreb * 100))
    } else {
      msg <- paste(msg, sprintf("%s grabs the board.", def$name))
    }
  }
  c(out, list(att = att, def = def, msg = msg))
}

# CPU offense: rest when tired, otherwise its best-value shot.
# "shooter" CPUs (Blacktop Gym) love the three ball.
cpu_pick <- function(cpu, player) {
  if (cpu$stamina < 25) return("Rest")
  options <- setdiff(names(moves), "Rest")
  affordable <- options[sapply(options, function(m) moves[[m]]$cost <= cpu$stamina)]
  value <- sapply(affordable, function(m)
    clamp(make_chance(cpu, player, moves[[m]])) * moves[[m]]$pts)
  if (identical(cpu$style, "shooter") && "Three" %in% names(value)) {
    value["Three"] <- value["Three"] + 0.35
  }
  if (identical(cpu$style, "speed") && "Drive" %in% names(value)) {
    value["Drive"] <- value["Drive"] + 0.35
  }
  if (sig_ready(cpu) && sig_moves[[cpu$name]]$cost <= cpu$stamina) {
    s <- sig_moves[[cpu$name]]
    value["Sig"] <- clamp(make_chance(cpu, player, s)) * s$pts + 0.3
  }
  names(which.max(value))
}

# CPU defense: mixes it up so you can't read it
# Speed teams gamble for steals a lot more.
cpu_def_pick <- function(cpu) {
  if (cpu$stamina < 20) return("Rest")
  probs <- if (identical(cpu$style, "speed")) c(0.35, 0.20, 0.45) else c(0.5, 0.3, 0.2)
  pick <- sample(c("Contest", "Block", "Steal"), 1, prob = probs)
  if (def_moves[[pick]]$cost > cpu$stamina) "Contest" else pick
}

# ---- End of battle: training, XP, level-ups -----------------
finish_battle <- function(p, cpu, won) {
  lines <- character(0)

  for (s in c("shooting", "speed", "defense")) {
    while (p$train[[s]] >= train_need(p[[s]])) {
      p$train[[s]] <- p$train[[s]] - train_need(p[[s]])
      p[[s]] <- p[[s]] + 1
      lines <- c(lines, sprintf("Practice paid off: %s +1 (now %d)", s, p[[s]]))
    }
  }
  while (p$train$stamina >= train_need(p$max_stamina / 2)) {
    p$train$stamina <- p$train$stamina - train_need(p$max_stamina / 2)
    p$max_stamina <- p$max_stamina + 2
    lines <- c(lines, sprintf("Conditioning paid off: max stamina +2 (now %d)", p$max_stamina))
  }

  gain <- if (won) 40 + 5 * cpu$level else 15 + 2 * cpu$level
  p$xp <- p$xp + gain
  if (won) p$wins <- p$wins + 1 else p$losses <- p$losses + 1
  lines <- c(lines, sprintf("+%d XP", gain))

  while (p$xp >= xp_needed(p$level)) {
    p$xp <- p$xp - xp_needed(p$level)
    p$level <- p$level + 1
    p$shooting <- p$shooting + 1
    p$speed    <- p$speed + 1
    p$defense  <- p$defense + 1
    p$max_stamina <- p$max_stamina + 3
    lines <- c(lines, sprintf("LEVEL UP! %s is now level %d. All stats +1, max stamina +3",
                              p$name, p$level))
  }
  list(player = p, lines = lines)
}

save_game <- function(p) try(saveRDS(p, SAVE_FILE), silent = TRUE)
load_game <- function() {
  if (!file.exists(SAVE_FILE)) return(NULL)
  upgrade_save(tryCatch(readRDS(SAVE_FILE), error = function(e) NULL))
}

# ---- Gear shop ----------------------------------------------
SHOP <- list(
  shoes  = list(name = "Speed Shoes",     price = 120, stat = "speed",       amt = 3,
                desc = "+3 speed, for good"),
  sleeve = list(name = "Shooter Sleeve",  price = 120, stat = "shooting",    amt = 3,
                desc = "+3 shooting, for good"),
  gloves = list(name = "Lockdown Gloves", price = 120, stat = "defense",     amt = 3,
                desc = "+3 defense, for good"),
  band   = list(name = "Headband",        price = 100, stat = "max_stamina", amt = 8,
                desc = "+8 max stamina, for good"),
  drink  = list(name = "Gatorade",        price = 30,  stat = NA,            amt = 0,
                desc = "Use in a battle: +40 stamina (uses your possession)")
)

COIN_REWARD <- list(wild = 20, trainer = 50, leader = 200, loss = 5)

# ---- UI helpers ---------------------------------------------
bar <- function(frac, color, height = 14) {
  pct <- round(100 * max(0, min(1, frac)))
  div(class = "bar", style = sprintf("height:%dpx;", height),
      div(class = "fill", style = sprintf("width:%d%%; background:%s;", pct, color)))
}

battle_card <- function(cr, label, target, show_items = FALSE) {
  div(class = "card",
      div(class = "who", label),
      h3(sprintf("%s  Lv %d", cr$name, cr$level)),
      div(class = "stats",
          sprintf("SHT %d | SPD %d | DEF %d", cr$shooting, cr$speed, cr$defense),
          if (show_items && (cr$drinks %||% 0) > 0) sprintf("  |  Gatorades: %d", cr$drinks)),
      div(class = "row-label", "Points ", strong(sprintf("%d / %d", cr$points, target))),
      bar(cr$points / target, "#f28c28"),
      div(class = "row-label", "Stamina ", strong(sprintf("%d / %d", cr$stamina, cr$max_stamina))),
      bar(cr$stamina / cr$max_stamina, "#3fb950"))
}

stat_row <- function(label, value, practice, need) {
  div(class = "stat-row",
      div(class = "stat-top", span(label), strong(value)),
      bar(practice / need, "#7f77dd", height = 6),
      div(class = "stat-sub", sprintf("practice %d / %d to next +1", practice, need)))
}

hub_card <- function(p) {
  need <- xp_needed(p$level)
  gear_names <- if (length(p$gear)) paste(sapply(p$gear, function(g) SHOP[[g]]$name), collapse = ", ") else "none yet"
  div(class = "card",
      div(class = "who", "Your creature"),
      h3(sprintf("%s  Lv %d", p$name, p$level)),
      div(class = "stats", sprintf("OVR %d | Record %d-%d | %d coins | Gatorades: %d",
                                   ovr(p), p$wins, p$losses, p$coins, p$drinks)),
      div(class = "stats", sprintf("Badges: %s", if (length(p$badges)) paste(p$badges, collapse = ", ") else "none yet")),
      div(class = "stats", sprintf("Gear: %s", gear_names)),
      div(class = "row-label", "XP ", strong(sprintf("%d / %d", p$xp, need))),
      bar(p$xp / need, "#ffd166"),
      div(class = "sig-line",
          sprintf("Signature: %s - %s", sig_moves[[p$name]]$name, sig_moves[[p$name]]$desc),
          if (p$level < SIG_LEVEL) span(class = "locked", sprintf(" (unlocks at Lv %d)", SIG_LEVEL))
          else span(class = "unlocked", " (unlocked)")),
      stat_row("Shooting", p$shooting, p$train$shooting, train_need(p$shooting)),
      stat_row("Speed",    p$speed,    p$train$speed,    train_need(p$speed)),
      stat_row("Defense",  p$defense,  p$train$defense,  train_need(p$defense)),
      stat_row("Max stamina", p$max_stamina, p$train$stamina, train_need(p$max_stamina / 2)))
}

move_label <- function(m) {
  mv <- moves[[m]]
  if (m == "Rest") "Rest (+30 stamina)"
  else sprintf("%s (%d pts, -%d stamina)", m, mv$pts, mv$cost)
}

sig_button <- function(p) {
  s <- sig_moves[[p$name]]
  if (p$level < SIG_LEVEL) {
    btn <- actionButton("mv_Sig", sprintf("%s (unlocks at Lv %d)", s$name, SIG_LEVEL), class = "btn-sig")
    btn$attribs$disabled <- "disabled"
    btn
  } else {
    actionButton("mv_Sig", sprintf("%s (%d pts, -%d stamina, 1x per battle)",
                                   s$name, s$pts, s$cost), class = "btn-sig")
  }
}

css <- "
body { background: #14161c; color: #e8e8e8; font-family: 'Segoe UI', sans-serif; }
h2, h3 { margin-top: 4px; }
.card { background: #1f232b; border: 2px solid #f28c28; border-radius: 12px;
        padding: 14px 18px; margin-bottom: 14px; }
.who { color: #f28c28; font-weight: 700; text-transform: uppercase; font-size: 12px; }
.stats { color: #9aa4b2; font-size: 13px; margin-bottom: 6px; }
.row-label { margin-top: 6px; font-size: 14px; }
.bar { background: #2d333d; border-radius: 7px; overflow: hidden; margin-top: 3px; }
.fill { height: 100%; transition: width 0.4s; }
.stat-row { margin-top: 10px; }
.stat-top { display: flex; justify-content: space-between; font-size: 14px; }
.stat-sub { color: #9aa4b2; font-size: 11px; margin-top: 2px; }
.btn { margin: 4px 6px 4px 0; background: #f28c28; color: #14161c; border: none; font-weight: 700; }
.btn:hover { background: #ffa94d; color: #14161c; }
.btn-quiet { background: #2d333d; color: #e8e8e8; }
.btn-quiet:hover { background: #3a414d; color: #ffffff; }
.btn-def { background: #7f77dd; color: #ffffff; }
.btn-def:hover { background: #9d97ea; color: #ffffff; }
.btn-sig { background: #ffd166; color: #14161c; }
.btn-sig:hover { background: #ffe199; color: #14161c; }
.btn-drink { background: #3fb950; color: #0b2a12; }
.btn-drink:hover { background: #5fd36f; color: #0b2a12; }
.btn[disabled] { opacity: 0.4; cursor: not-allowed; }
.sig-line { margin-top: 10px; font-size: 13px; color: #ffd166; }
.sig-line .locked { color: #9aa4b2; } .sig-line .unlocked { color: #3fb950; }
.phase { font-size: 16px; font-weight: 700; margin: 4px 0 6px 0; }
.phase-off { color: #f28c28; } .phase-def { color: #9d97ea; }
.boss { color: #ff6b6b; font-weight: 800; font-size: 15px; margin-bottom: 6px; }
.log { background: #1f232b; border-radius: 10px; padding: 10px 14px; margin-top: 14px;
       max-height: 260px; overflow-y: auto; font-family: Consolas, monospace; font-size: 13px; }
.log div:first-child { color: #ffd166; font-weight: 700; }
.result { font-size: 26px; font-weight: 800; margin: 10px 0 4px 0; }
.win { color: #3fb950; } .lose { color: #ff6b6b; }
.summary div { font-size: 14px; margin: 2px 0; }
.summary .lvl { color: #ffd166; font-weight: 700; }
.summary .badge-line { color: #ff9f43; font-weight: 800; font-size: 16px; }
.modal-content { background: #1f232b; color: #e8e8e8; }
.town-status { color: #9aa4b2; font-size: 14px; margin-bottom: 8px; }
.town-status b { color: #f28c28; }
.dialog { max-width: 720px; background: #ffffff; color: #2c2c2a; border: 3px solid #2c2c2a;
          border-radius: 8px; padding: 12px 16px; margin-top: 10px; font-size: 15px; min-height: 64px; }
.dialog .btn { margin-top: 8px; }
.pad-wrap { margin-top: 10px; display: flex; gap: 18px; align-items: center; }
.pad-grid { display: grid; grid-template-columns: 44px 44px 44px; grid-template-rows: 40px 40px; gap: 4px; }
.pad { margin: 0; padding: 0; width: 44px; height: 40px; background: #2d333d; color: #e8e8e8; }
.pad:hover { background: #3a414d; color: #ffffff; }
.pad-a { width: 52px; height: 52px; border-radius: 26px; background: #e24b4a; color: #ffffff; }
.help { color: #9aa4b2; font-size: 12px; }
.shop-item { display: flex; justify-content: space-between; align-items: center;
             border-top: 1px solid #2d333d; padding: 10px 0; }
.shop-item .name { font-weight: 700; font-size: 15px; }
.shop-item .desc { color: #9aa4b2; font-size: 12px; }
.shop-item .price { color: #ffd166; font-weight: 700; margin-right: 10px; }
.coins { color: #ffd166; font-weight: 700; font-size: 16px; }
"

# ============================================================
# ---- The world: tile maps -----------------------------------
# Each letter is one tile.
#   .  ground (grass / sidewalk / stage)    #  path / street
#   T  tree          W  water         g  tall grass (wild creatures)
#   c  wood court    k  blacktop court    w  gym hardwood
#   f  fence         Z  gym wall      Q  bleachers
#   H/C/S/L/A/Y  building walls (home, Coach Center, shop, lab, apartments, gym)
#   h/p/s/l/y    building doors
#   E/U/B/R/O    exits to other areas
#   N  Rookie Rae    1-6, X  trainers    n, m  people to talk to
# ============================================================
TOWN <- c(
  "TTTTTTTTTTTTTTTTTTTT",
  "T.HHHH...##..CCCCC.T",
  "T.HHHH...##..CCCCC.T",
  "T.HhHH...##..CCpCC.T",
  "T##################T",
  "T.SSSS...##.cccccc.T",
  "T.SSSS...##.cccccc.T",
  "T.SsSS...##.ccNccc.T",
  "T........##.cccccc.T",
  "T.LLLLL..##........T",
  "T.LLLLL..##..TTTTT.T",
  "T.LLlLL..##..TTTTT.T",
  "T........##........T",
  "TTTTTTTTTEETTTTTTTTT"
)
ROUTE1 <- c(
  "TTTTTTTTTUUTTTTTTTTT",
  "Tgggggg..##......TTT",
  "Tgggggg..##......T.T",
  "T........#######...T",
  "T........#######...T",
  "TWWW.....gggg.##..2T",
  "TWWW.....gggg.##...T",
  "TWWW.....gggg.##...T",
  "T....###########...T",
  "T....###########...T",
  "T1...##.ggggggg....T",
  "T....##.ggggggg....T",
  "T....##.ggggggg..TTT",
  "T....######.....gggT",
  "TT...######.....gggT",
  "TTT......##.....gggT",
  "TTTTTTTTTBBTTTTTTTTT"
)
CITY <- c(
  "TTTTTTTTTUUTTTTTTTTT",
  "T.CCCCC..##..ffffffT",
  "T.CCCCC..##..fkkkkfT",
  "T.CCpCC..##..fk3kkfT",
  "T........##..fkkk4fT",
  "T.AAA..n.##..fkkkkfT",
  "T.AAA....##..ff.fffT",
  "T##################T",
  "T##################T",
  "T.SSSSS..##.YYYYYY.T",
  "T.SSSSS..##.YYYYYY.T",
  "T.SSsSS..##.YYYYYY.T",
  "T........##.YYyYYY.T",
  "T.TT..m..##........T",
  "T........##........T",
  "TTTTTTTTTRRTTTTTTTTT"
)
GYM <- c(
  "ZZZZZZZZZZZZZZZZZZZZ",
  "ZQQQQQQQ.X..QQQQQQQZ",
  "ZQQQQQQQ....QQQQQQQZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "Zwww6wwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "Zwwwwwwwwwwwwww5wwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZQQQQQQwwwwwwQQQQQQZ",
  "ZQQQQQQwwwwwwQQQQQQZ",
  "ZZZZZZZZZOOZZZZZZZZZ"
)
ROUTE2 <- c(
  "TTTTTTTTTUUTTTTTTTTT",
  "T........##....gggTT",
  "Tggg.....##....gggTT",
  "Tggg.....########..T",
  "Tggg.....########..T",
  "TWWWWWWWWWWWWWW==WWT",
  "TWWWWWWWWWWWWWW==WWT",
  "T.....gggg..8..##..T",
  "T.....gggg.....##..T",
  "T.....##########...T",
  "T.....##########...T",
  "T7....##..ggggggg..T",
  "T.....##..ggggggg..T",
  "Tggg..######.......T",
  "Tggg.....##........T",
  "TTTTTTTTTBBTTTTTTTTT"
)
RIVERSIDE <- c(
  "TTTTTTTTTUUTTTTTTTTT",
  "T.CCCCC..##..SSSSS.T",
  "T.CCCCC..##..SSSSS.T",
  "T.CCpCC..##..SSsSS.T",
  "T##################T",
  "T........##.......oT",
  "WWWWWWWWW==WWWWWWWWW",
  "WWWWWWWWW==WWWWWWWWW",
  "T........##........T",
  "T.VVVVVV.##..q.....T",
  "T.VVVVVV.##........T",
  "T.VVVVVV.##........T",
  "T.VVvVVV.##........T",
  "T........##........T",
  "TTTTTTTTTRRTTTTTTTTT"
)
GYM2 <- c(
  "ZZZZZZZZZZZZZZZZZZZZ",
  "ZQQQQQQQ.K..QQQQQQQZ",
  "ZQQQQQQQ....QQQQQQQZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwJwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "Zw9wwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZwwwwwwwwwwwwwwwwwwZ",
  "ZQQQQQQwwwwwwQQQQQQZ",
  "ZQQQQQQwwwwwwQQQQQQZ",
  "ZZZZZZZZZOOZZZZZZZZZ"
)
to_matrix <- function(rows) do.call(rbind, strsplit(rows, ""))

MAPS <- list(
  town = list(m = to_matrix(TOWN), name = "Rookie Town",
              ground = "#7cc46a", path = "#e8d6a0", exit = "#c9b27a",
              courts = list(list(chars = c("c", "N"), line = "#ffffff", rim = "#e24b4a")),
              signs  = list(E = "Route 1"),
              exits  = list(E = list(to = "route1", row = 2, need = TRUE,
                                     msg = "Route 1. Watch out for wild creatures in the tall grass!"))),
  route1 = list(m = to_matrix(ROUTE1), name = "Route 1",
                ground = "#7cc46a", path = "#e8d6a0", exit = "#c9b27a",
                courts = list(),
                signs  = list(U = "Rookie Town", B = "Blacktop City"),
                exits  = list(U = list(to = "town", row = 13),
                              B = list(to = "city", row = 2,
                                       msg = "Welcome to Blacktop City! Gym leader Splash only respects shooters. Train up at The Cage before you step in."))),
  city = list(m = to_matrix(CITY), name = "Blacktop City",
              ground = "#c4c0b6", path = "#4a4a4f", exit = "#4a4a4f",
              courts = list(list(chars = c("k", "3", "4"), line = "#ffffff", rim = "#f28c28")),
              signs  = list(U = "Route 1", R = "Route 2"),
              exits  = list(U = list(to = "route1", row = 16),
                            R = list(to = "route2", row = 2,
                                     msg = "Route 2. The river runs through here. Cross the bridge and keep heading south to Riverside."),
                            y = list(to = "gym", row = 12, col = 10,
                                     msg = "Blacktop Gym. Splash waits at the far end. Beat both gym trainers first!"))),
  gym = list(m = to_matrix(GYM), name = "Blacktop Gym",
             ground = "#b07a45", path = "#c98b4f", exit = "#7a3e08",
             courts = list(list(box = c(4, 10, 2, 19), line = "#ffffff", rim = "#f28c28")),
             signs  = list(O = "Exit"),
             exits  = list(O = list(to = "city", row = 14, col = 15))),
  route2 = list(m = to_matrix(ROUTE2), name = "Route 2", wild_bonus = 1,
                ground = "#7cc46a", path = "#e8d6a0", exit = "#c9b27a",
                courts = list(),
                signs  = list(U = "Blacktop City", B = "Riverside"),
                exits  = list(U = list(to = "city", row = 15),
                              B = list(to = "riverside", row = 2,
                                       msg = "Welcome to Riverside! Gym leader Blur is the fastest hooper around. Block the drives!"))),
  riverside = list(m = to_matrix(RIVERSIDE), name = "Riverside",
                   ground = "#8fd07c", path = "#e8d6a0", exit = "#c9b27a",
                   courts = list(),
                   signs  = list(U = "Route 2", R = "Route 3"),
                   exits  = list(U = list(to = "route2", row = 15),
                                 R = list(blocked = "Route 3 is still under construction. More coming after the break!"),
                                 v = list(to = "gym2", row = 12, col = 10, need_badge = "Shooting Badge",
                                          msg = "Riverside Gym. Blur waits at the far end. Beat both gym trainers first!"))),
  gym2 = list(m = to_matrix(GYM2), name = "Riverside Gym",
              ground = "#2b4a7a", path = "#3d6fb6", exit = "#1d3557", floor = "#3d6fb6",
              courts = list(list(box = c(4, 10, 2, 19), line = "#ffffff", rim = "#ffd166")),
              signs  = list(O = "Exit"),
              exits  = list(O = list(to = "riverside", row = 14, col = 5)))
)
START_POS <- c(5, 4)
WALKABLE  <- c(".", "#", "c", "k", "w", "g", "=", "E", "U", "B", "R", "O", "h", "p", "s", "l", "y", "v")
TS        <- 28   # tile size in pixels

buildings <- list(
  H = list(door = "h", roof = "#c0503a", label = "Home"),
  C = list(door = "p", roof = "#d4537e", label = "Coach Center"),
  S = list(door = "s", roof = "#378add", label = "Gear shop"),
  L = list(door = "l", roof = "#f28c28", label = "Coach lab"),
  A = list(door = NULL, roof = "#6e6e74", label = NULL, wall = "#d9d4c8", windows = TRUE),
  Y = list(door = "y", roof = "#f28c28", label = "Blacktop Gym"),
  V = list(door = "v", roof = "#1d9e75", label = "Riverside Gym")
)
DOORS <- unlist(lapply(buildings, `[[`, "door"))

# Trainers. dir = where they look; walk into their line of sight and they call you out.
TRAINERS <- list(
  "1" = list(name = "Hooper Hank", jersey = "#3fb950", bg = "#7cc46a", dir = c(0, 1), bonus = 1,
             intro = "Hey! You look like you can hoop. Let's run it!",
             win = "Good game! Blacktop City is just down the road.",
             lose = "Too easy! Train in the tall grass and come back."),
  "2" = list(name = "Baller Bree", jersey = "#378add", bg = "#7cc46a", dir = c(0, -1), bonus = 1,
             intro = "You can't walk past my court without a game!",
             win = "Okay okay, you've got game.",
             lose = "Come back when you've got a jumper."),
  "3" = list(name = "Streetball Sly", jersey = "#ffd166", bg = "#2e2e33", dir = c(1, 0), bonus = 1,
             intro = "Welcome to The Cage. Winners stay, losers walk. Let's go!",
             win = "Respect. You can hoop in The Cage anytime.",
             lose = "Winners stay! Go run it back."),
  "4" = list(name = "Handles Hana", jersey = "#d4537e", bg = "#2e2e33", dir = c(0, -1), bonus = 1,
             kind = "Swishtail",
             intro = "My handle's too tight for you. Try me!",
             win = "Dang, you stayed in front of me. Splash is gonna test you though.",
             lose = "Ankles broken! Work on that defense."),
  "5" = list(name = "Swish Sam", jersey = "#f28c28", bg = "#c98b4f", dir = c(0, -1), bonus = 1,
             kind = "Blazehoop", style = "shooter",
             intro = "Blacktop Gym trainer! We live behind the arc. Can you guard a shooter?",
             win = "Nice contests. Splash shoots even better than me.",
             lose = "Splash! Contest the three next time."),
  "6" = list(name = "Range Rita", jersey = "#f28c28", bg = "#c98b4f", dir = c(0, 1), bonus = 1,
             kind = "Swishtail", style = "shooter",
             intro = "Every shot I take is a three. Stop me if you can!",
             win = "You earned your shot at Splash. Good luck.",
             lose = "Too much range! Crash the boards when I miss."),
  "X" = list(name = "Gym Leader Splash", jersey = "#2c2c2a", bg = "#b07a45", dir = NULL, bonus = 2,
             kind = "Blazehoop", style = "shooter", boost = 16, leader = TRUE,
             requires = c("5", "6"), badge = "Shooting Badge", target = 15,
             not_ready = "Splash: Nah, coach. Beat both of my gym trainers first, then we talk.",
             after = "Splash: That badge looks good on you. Riverside is next, south past Route 2.",
             intro = "Splash: So you're the rookie everybody's talking about. I don't miss. First to 15, let's see what you've got!",
             win = "Splash: ...You've got the touch, coach. Take the Shooting Badge. Riverside's leader plays way faster than me.",
             lose = "Splash: Splash! That's what happens when you leave me open. Come back when you can guard."),
  "7" = list(name = "Speedy Steph", jersey = "#ffd166", bg = "#7cc46a", dir = c(0, 1), bonus = 1,
             kind = "Swishtail",
             intro = "Race you to 11! Nobody on Route 2 is faster than me.",
             win = "Whoa, you kept up. Riverside's gym is all about speed.",
             lose = "Too slow! Try blocking my drives."),
  "8" = list(name = "Fisherman Finn", jersey = "#7f77dd", bg = "#7cc46a", dir = c(0, 1), bonus = 1,
             kind = "Dunkadon",
             intro = "I was fishing, but I'll take a game. Big man in the paint!",
             win = "Hooked and reeled in. Good game.",
             lose = "Couldn't get past the big man, huh? Crash the boards."),
  "9" = list(name = "Quick Quinn", jersey = "#ffffff", bg = "#3d6fb6", dir = c(0, 1), bonus = 1,
             kind = "Swishtail", style = "speed",
             intro = "Riverside Gym trainer! We drive, we steal, we run. Keep up!",
             win = "You blocked me out. Blur is even faster though.",
             lose = "Blew right by you! Block the drive next time."),
  "J" = list(name = "Jet Jada", jersey = "#ffffff", bg = "#3d6fb6", dir = c(0, -1), bonus = 2,
             kind = "Swishtail", style = "speed",
             intro = "Hands up or I'm taking that ball. Let's go!",
             win = "Okay, you protected the rock. Go see Blur.",
             lose = "Pick-pocketed! Don't get greedy with the ball."),
  "K" = list(name = "Gym Leader Blur", jersey = "#2c2c2a", bg = "#2b4a7a", dir = NULL, bonus = 2,
             kind = "Swishtail", style = "speed", boost = 8, boost_stat = "speed", leader = TRUE,
             requires = c("9", "J"), badge = "Speed Badge", badge_stat = "speed", target = 15,
             not_ready = "Blur: Slow down, rookie. Beat both of my gym trainers first.",
             after = "Blur: Still can't believe you kept up with me. Route 3 opens soon.",
             intro = "Blur: You took Splash's badge? Cute. Shooters stand still. I don't. First to 15, try to keep up!",
             win = "Blur: ...Nobody stays in front of me. Respect. Take the Speed Badge.",
             lose = "Blur: Blink and I'm gone! Block my drives, and watch that ball.")
)

BADGE_STAT <- list()
for (t in TRAINERS) if (!is.null(t$badge)) BADGE_STAT[[t$badge]] <- t$badge_stat %||% "shooting"

# People who just talk
NPCS <- list(
  n = list(name = "Old Head Otis", jersey = "#9aa4b2", bg = "#c4c0b6",
           text = "Old Head Otis: Here's a secret: beat a wild creature and it might join you if you ask. Back in my day we played D! Splash's team lives behind the arc. Contest everything and crash the boards when they miss."),
  m = list(name = "Kid Kev", jersey = "#3fb950", bg = "#c4c0b6",
           text = "Kid Kev: The Gear shop sells Gatorade. Clutch when you're gassed late in a game. And Speed Shoes are fire!"),
  o = list(name = "Dock Dee", jersey = "#f28c28", bg = "#8fd07c",
           text = "Dock Dee: Welcome to Riverside! Blur won't even let you in the gym without a badge from Blacktop City."),
  q = list(name = "Coach Kim", jersey = "#d4537e", bg = "#8fd07c",
           text = "Coach Kim: Blur's team drives and gambles for steals. Block the drives, and don't waste stamina, you'll need it.")
)
PERSON_CHARS <- c("N", names(TRAINERS), names(NPCS))

tile_at <- function(pos, M) {
  if (pos[1] < 1 || pos[1] > nrow(M) || pos[2] < 1 || pos[2] > ncol(M)) return("T")
  M[pos[1], pos[2]]
}

# ---- Drawing a map as one SVG picture -----------------------
render_map <- function(mk, pos) {
  mp <- MAPS[[mk]]; M <- mp$m
  nr <- nrow(M); nc <- ncol(M)
  out <- sprintf('<svg viewBox="0 0 %d %d" width="100%%" style="max-width:720px;display:block;border-radius:8px" shape-rendering="crispEdges">',
                 nc * TS, nr * TS)
  rect <- function(i, j, col) sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>',
                                      (j - 1) * TS, (i - 1) * TS, TS, TS, col)
  x0 <- function(j) (j - 1) * TS
  y0 <- function(i) (i - 1) * TS
  font <- 'font-family="Segoe UI, sans-serif" font-weight="700"'

  for (i in 1:nr) for (j in 1:nc) {
    ch <- M[i, j]
    piece <- if (ch == ".") rect(i, j, mp$ground)
    else if (ch == "#") rect(i, j, mp$path)
    else if (ch %in% c("E", "U", "B", "R", "O")) rect(i, j, mp$exit)
    else if (ch == "T") c(rect(i, j, mp$ground),
                          sprintf('<circle cx="%g" cy="%g" r="11" fill="#2f7d3a" shape-rendering="auto"/>',
                                  x0(j) + TS / 2, y0(i) + TS / 2))
    else if (ch == "W") rect(i, j, "#5aa9e6")
    else if (ch == "g") c(rect(i, j, "#4f9e44"),
                          sprintf('<path d="M%g %g l3 -10 l3 10 M%g %g l3 -9 l3 9" fill="none" stroke="#2f6e2a" stroke-width="1.6" shape-rendering="auto"/>',
                                  x0(j) + 5, y0(i) + 22, x0(j) + 15, y0(i) + 24))
    else if (ch == "c") rect(i, j, "#d9a066")
    else if (ch == "k") rect(i, j, "#2e2e33")
    else if (ch == "w") rect(i, j, mp$floor %||% "#c98b4f")
    else if (ch == "=") c(rect(i, j, "#a77b4f"),
                          sprintf('<path d="M%g %g H%g M%g %g H%g" stroke="#6b4a2b" stroke-width="2"/>',
                                  x0(j), y0(i) + 9, x0(j) + TS, x0(j), y0(i) + 19, x0(j) + TS))
    else if (ch == "Z") rect(i, j, "#4a2f1d")
    else if (ch == "Q") c(rect(i, j, "#8a8f96"),
                          sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="#6e737a" stroke-width="2"/>',
                                  x0(j), y0(i) + 14, x0(j) + TS, y0(i) + 14))
    else if (ch == "f") c(rect(i, j, mp$ground),
                          sprintf('<path d="M%g %g L%g %g M%g %g L%g %g" stroke="#8a8f96" stroke-width="1.5" shape-rendering="auto"/>',
                                  x0(j), y0(i), x0(j) + TS, y0(i) + TS, x0(j) + TS, y0(i), x0(j), y0(i) + TS))
    else if (ch == "N") rect(i, j, "#d9a066")
    else if (ch %in% names(TRAINERS)) rect(i, j, TRAINERS[[ch]]$bg)
    else if (ch %in% names(NPCS)) rect(i, j, NPCS[[ch]]$bg)
    else if (ch %in% DOORS) {
      b <- buildings[[names(DOORS)[DOORS == ch][1]]]
      c(rect(i, j, b$wall %||% "#f2e6d0"),
        sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="#6b4a2b"/>',
                x0(j) + 7, y0(i) + 6, TS - 14, TS - 6))
    }
    else if (ch %in% names(buildings)) {
      b <- buildings[[ch]]
      top <- i == 1 || !(M[i - 1, j] %in% c(ch, b$door))
      if (top) rect(i, j, b$roof)
      else c(rect(i, j, b$wall %||% "#f2e6d0"),
             if (isTRUE(b$windows)) sprintf('<rect x="%g" y="%g" width="10" height="8" fill="#7fb3e0"/>',
                                            x0(j) + 9, y0(i) + 8))
    }
    else rect(i, j, mp$ground)
    out <- c(out, piece)
  }

  # Building name signs
  for (b in names(buildings)) {
    if (is.null(buildings[[b]]$label)) next
    idx <- which(M == b | M == (buildings[[b]]$door %||% "~"), arr.ind = TRUE)
    if (!nrow(idx)) next
    minr <- min(idx[, 1]); minc <- min(idx[, 2]); maxc <- max(idx[, 2])
    out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" font-size="11" %s fill="#3a2a1a">%s</text>',
                          (minc - 1 + maxc) / 2 * TS, (minr + 0.5) * TS, font, buildings[[b]]$label))
  }

  # Courts: outline, half-court line, center circle, rims
  for (ct in mp$courts) {
    if (!is.null(ct$box)) {
      r0 <- ct$box[1]; r1 <- ct$box[2]; c0 <- ct$box[3]; c1 <- ct$box[4]
    } else {
      idx <- which(matrix(M %in% ct$chars, nrow = nr), arr.ind = TRUE)
      if (!nrow(idx)) next
      r0 <- min(idx[, 1]); r1 <- max(idx[, 1]); c0 <- min(idx[, 2]); c1 <- max(idx[, 2])
    }
    xa <- (c0 - 1) * TS + 4; xb <- c1 * TS - 4
    ya <- (r0 - 1) * TS + 4; yb <- r1 * TS - 4
    xm <- (xa + xb) / 2; ym <- (ya + yb) / 2
    out <- c(out,
      sprintf('<g fill="none" stroke="%s" stroke-width="2" shape-rendering="auto"><rect x="%g" y="%g" width="%g" height="%g"/><line x1="%g" y1="%g" x2="%g" y2="%g"/><circle cx="%g" cy="%g" r="12"/></g>',
              ct$line, xa, ya, xb - xa, yb - ya, xm, ya, xm, yb, xm, ym),
      sprintf('<circle cx="%g" cy="%g" r="5" fill="none" stroke="%s" stroke-width="2.5" shape-rendering="auto"/>', xa + 9, ym, ct$rim),
      sprintf('<circle cx="%g" cy="%g" r="5" fill="none" stroke="%s" stroke-width="2.5" shape-rendering="auto"/>', xb - 9, ym, ct$rim))
  }

  # Exit signs
  for (s in names(mp$signs)) {
    idx <- which(M == s, arr.ind = TRUE)
    if (!nrow(idx)) next
    txt_col <- if (mk %in% c("city", "gym", "gym2")) "#f2e6d0" else "#3a2a1a"
    out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" font-size="10" %s fill="%s">%s</text>',
                          (min(idx[, 2]) - 1 + max(idx[, 2])) / 2 * TS, (idx[1, 1] - 0.5) * TS, font, txt_col, mp$signs[[s]]))
  }

  # People: a head and a jersey
  person <- function(i, j, jersey) {
    cx <- (j - 0.5) * TS; cy <- (i - 0.5) * TS
    sprintf('<g shape-rendering="auto"><circle cx="%g" cy="%g" r="6" fill="#5b3a29"/><rect x="%g" y="%g" width="14" height="13" rx="3" fill="%s"/></g>',
            cx, cy - 6, cx - 7, cy, jersey)
  }
  for (ch in PERSON_CHARS) {
    idx <- which(M == ch, arr.ind = TRUE)
    if (!nrow(idx)) next
    jersey <- if (ch == "N") "#7f77dd" else if (ch %in% names(TRAINERS)) TRAINERS[[ch]]$jersey else NPCS[[ch]]$jersey
    out <- c(out, person(idx[1, 1], idx[1, 2], jersey))
    if (identical(TRAINERS[[ch]]$leader, TRUE)) {   # a little crown for the gym leader
      cx <- (idx[1, 2] - 0.5) * TS; cy <- (idx[1, 1] - 0.5) * TS
      out <- c(out, sprintf('<path d="M%g %g l3 -5 l3 4 l3 -4 l3 5 z" fill="#ffd166" shape-rendering="auto"/>',
                            cx - 6, cy - 12))
    }
  }
  out <- c(out, person(pos[1], pos[2], "#e24b4a"))

  paste(c(out, "</svg>"), collapse = "")
}

# Arrow keys / WASD move, Space or Enter talks
key_js <- "
document.addEventListener('keydown', function(e) {
  var map = {ArrowUp:'up', ArrowDown:'down', ArrowLeft:'left', ArrowRight:'right',
             w:'up', s:'down', a:'left', d:'right', W:'up', S:'down', A:'left', D:'right',
             ' ':'a', Enter:'a'};
  var k = map[e.key];
  if (!k) return;
  e.preventDefault();
  Shiny.setInputValue('key', k, {priority: 'event'});
});
"
pad_btn <- function(k, label) {
  tags$button(class = "btn pad", label,
              onclick = sprintf("Shiny.setInputValue('key','%s',{priority:'event'})", k))
}

# ---- UI -----------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(css)), tags$script(HTML(key_js))),
  titlePanel("Court Legends"),
  uiOutput("screen")
)

# ---- Server -------------------------------------------------
server <- function(input, output, session) {
  saved <- load_game()
  rv <- reactiveValues(
    stage   = "town",
    phase   = "offense",
    player  = saved, cpu = NULL, opponent = NULL, opponent_id = NULL, target = 11,
    log     = character(0), winner = NULL, summary = character(0),
    map     = "town", pos = START_POS, pending_trainer = NULL,
    dialog  = if (is.null(saved))
      "Welcome to Rookie Town! Head to the coach lab (bottom left) to pick your first creature. Use the arrow keys or WASD to walk."
      else "Welcome back, coach! Rookie Rae is on the court if you want a game.",
    dlg_action = NULL
  )

  say <- function(text, action = NULL) {
    rv$dialog <- text
    rv$dlg_action <- action
  }
  cur_m <- function() MAPS[[rv$map]]$m

  # ---- Screens (re-drawn only when the stage changes) ----
  output$screen <- renderUI({
    if (rv$stage == "pick") {
      choices <- setNames(STARTERS, sapply(STARTERS, function(n) {
        s <- species[[n]]
        sprintf("%s  (SHT %d | SPD %d | DEF %d | STA %d)",
                n, s$shooting, s$speed, s$defense, s$stamina)
      }))
      div(class = "card",
          h3("Choose your creature"),
          radioButtons("pick", NULL, choices = choices),
          actionButton("start", "Let's go"))

    } else if (rv$stage == "town") {
      tagList(
        uiOutput("town_status"),
        uiOutput("map"),
        uiOutput("dialog"),
        div(class = "pad-wrap",
            div(class = "pad-grid",
                div(), pad_btn("up", "\u25B2"), div(),
                pad_btn("left", "\u25C0"), pad_btn("down", "\u25BC"), pad_btn("right", "\u25B6")),
            tags$button(class = "btn pad pad-a", "A",
                        onclick = "Shiny.setInputValue('key','a',{priority:'event'})"),
            div(class = "help", "Arrow keys / WASD to walk", br(), "Space or A to talk"))
      )

    } else if (rv$stage == "hub") {
      tagList(
        uiOutput("hub"),
        uiOutput("squad"),
        actionButton("find", "Quick battle"),
        actionButton("to_town", "Back to the map"),
        actionButton("reset", "Start a new game", class = "btn-quiet")
      )

    } else if (rv$stage == "shop") {
      tagList(
        div(class = "card",
            div(class = "who", "Gear shop"),
            h3("Gear up, coach"),
            uiOutput("shop_list")),
        actionButton("leave_shop", "Leave the shop")
      )

    } else {
      tagList(
        uiOutput("boss_banner"),
        uiOutput("cards"),
        uiOutput("phase_css"),
        uiOutput("phase_label"),
        div(class = "off-group",
            lapply(setdiff(names(moves), "Rest"), function(m)
              actionButton(paste0("mv_", m), move_label(m))),
            sig_button(isolate(rv$player)),
            actionButton("mv_Rest", move_label("Rest")),
            actionButton("mv_Drink", "Gatorade (+40 stamina)", class = "btn-drink")),
        div(class = "def-group",
            lapply(names(def_moves), function(d)
              actionButton(paste0("df_", d), def_moves[[d]]$label, class = "btn-def"))),
        uiOutput("result"),
        div(class = "log", uiOutput("log"))
      )
    }
  })

  # ---- Overworld ----
  output$town_status <- renderUI({
    p <- rv$player
    if (is.null(p)) return(div(class = "town-status", "No creature yet  |  ", MAPS[[rv$map]]$name))
    div(class = "town-status", tags$b(sprintf("%s Lv %d", p$name, p$level)),
        sprintf("  |  OVR %d  |  Record %d-%d  |  %d coins  |  Badges: %d  |  %s",
                ovr(p), p$wins, p$losses, p$coins, length(p$badges), MAPS[[rv$map]]$name))
  })

  output$map <- renderUI(HTML(render_map(rv$map, rv$pos)))

  output$dialog <- renderUI({
    if (!nzchar(rv$dialog)) return(div(class = "dialog", style = "color:#9aa4b2", "..."))
    btns <- NULL
    if (!is.null(rv$dlg_action)) {
      yes <- switch(rv$dlg_action, lab = "Pick my creature", center = "Open my squad",
                    shop = "Let's shop", rae = "Let's run it", trainer = "Let's run it", "Yes")
      btns <- div(actionButton("choice_yes", yes), actionButton("choice_no", "Not now", class = "btn-quiet"))
    }
    div(class = "dialog", rv$dialog, btns)
  })

  is_beaten <- function(t) t %in% rv$player$beaten

  talk_rae <- function() {
    if (is.null(rv$player)) {
      say("Rookie Rae: You don't even have a creature yet! Go see Professor Hoops in the coach lab.")
    } else {
      say("Rookie Rae: Yo coach! Wanna run it? First to 11.", "rae")
    }
  }

  talk_trainer <- function(t) {
    tr <- TRAINERS[[t]]
    if (is_beaten(t)) {
      say(if (isTRUE(tr$leader)) tr$after else sprintf("%s: %s", tr$name, tr$win))
    } else if (!is.null(tr$requires) && !all(tr$requires %in% rv$player$beaten)) {
      say(tr$not_ready)
    } else {
      rv$pending_trainer <- t
      say(if (isTRUE(tr$leader)) tr$intro else sprintf("%s: %s", tr$name, tr$intro), "trainer")
    }
  }

  talk_to <- function(ch) {
    if (ch == "N") { talk_rae(); return(TRUE) }
    if (ch %in% names(TRAINERS)) { talk_trainer(ch); return(TRUE) }
    if (ch %in% names(NPCS)) { say(NPCS[[ch]]$text); return(TRUE) }
    FALSE
  }

  # Trainers who see you walk into their line of sight call you out
  check_trainers <- function() {
    M <- cur_m()
    for (t in names(TRAINERS)) {
      if (is.null(TRAINERS[[t]]$dir)) next
      tp <- which(M == t, arr.ind = TRUE)
      if (!nrow(tp) || is_beaten(t)) next
      cell <- c(tp[1, 1], tp[1, 2])
      for (k in 1:5) {
        cell <- cell + TRAINERS[[t]]$dir
        if (!(tile_at(cell, M) %in% WALKABLE)) break
        if (all(cell == rv$pos)) { talk_trainer(t); return(invisible(TRUE)) }
      }
    }
    invisible(FALSE)
  }

  # What happens when you step on a special tile
  step_event <- function(ch, pos, prev) {
    has <- !is.null(rv$player)

    ex <- MAPS[[rv$map]]$exits[[ch]]
    if (!is.null(ex)) {
      if (!is.null(ex$blocked)) {
        rv$pos <- prev
        say(ex$blocked)
      } else if (!is.null(ex$need_badge) && !(ex$need_badge %in% rv$player$badges)) {
        rv$pos <- prev
        say(sprintf("The doors won't open. You need the %s from Blacktop Gym first.", ex$need_badge))
      } else if (isTRUE(ex$need) && !has) {
        rv$pos <- prev
        say("Professor Hoops: Wait! It's not safe out there without a creature. Come see me in the coach lab.")
      } else {
        rv$map <- ex$to
        rv$pos <- c(ex$row, ex$col %||% pos[2])
        say(ex$msg %||% "")
      }
      return(invisible())
    }

    switch(ch,
      h = say("Home sweet home. Mom: \"Go get 'em, coach!\""),
      p = if (!has) say("Coach Center: You don't have a creature yet. Visit the coach lab first!")
          else say("Coach Center: Your squad is rested and ready. Want to check your creature or swap who's active?", "center"),
      s = if (!has) say("Gear shop: Come back once you have a creature, coach.")
          else say(sprintf("Gear shop: Welcome in! You've got %d coins. Want to take a look?", rv$player$coins), "shop"),
      l = if (!has) say("Professor Hoops: Welcome, coach! Ready to pick your first creature?", "lab")
          else say(sprintf("Professor Hoops: Keep training! Level %d unlocks your signature move. Badges come from gym leaders.", SIG_LEVEL)),
      g = if (runif(1) < 0.15) start_battle(NULL)
    )
  }

  dirs <- list(up = c(-1, 0), down = c(1, 0), left = c(0, -1), right = c(0, 1))

  observeEvent(input$key, {
    if (rv$stage != "town") return()
    k <- input$key
    if (!is.null(rv$dlg_action)) return()          # answer the question first

    if (k == "a") {
      if (nzchar(rv$dialog)) { say(""); return() }  # close the text box
      for (d in dirs) if (talk_to(tile_at(rv$pos + d, cur_m()))) return()
      return()
    }

    say("")
    np <- rv$pos + dirs[[k]]
    ch <- tile_at(np, cur_m())
    if (talk_to(ch)) return()
    if (!(ch %in% WALKABLE)) return()
    prev <- rv$pos
    rv$pos <- np
    step_event(ch, np, prev)
    if (rv$stage == "town" && is.null(rv$dlg_action)) check_trainers()
  })

  observeEvent(input$choice_yes, {
    act <- rv$dlg_action
    say("")
    if (is.null(act)) return()
    if (act == "lab")    rv$stage <- "pick"
    if (act == "center") rv$stage <- "hub"
    if (act == "shop")   rv$stage <- "shop"
    if (act == "rae")    start_battle("Rookie Rae")
    if (act == "trainer" && !is.null(rv$pending_trainer)) {
      t <- rv$pending_trainer
      rv$pending_trainer <- NULL
      start_battle(TRAINERS[[t]]$name, id = t)
    }
  })
  observeEvent(input$choice_no, { rv$pending_trainer <- NULL; say("") })

  # ---- Gear shop ----
  output$shop_list <- renderUI({
    p <- rv$player
    req(p)
    tagList(
      div(class = "coins", sprintf("%d coins", p$coins)),
      div(class = "stats", "Win battles to earn coins: wild 20, trainers 50, gym leaders 200."),
      lapply(names(SHOP), function(k) {
        it <- SHOP[[k]]
        owned <- k %in% p$gear
        btn <- if (owned) {
          b <- actionButton(paste0("buy_", k), "Owned", class = "btn-quiet"); b$attribs$disabled <- "disabled"; b
        } else {
          b <- actionButton(paste0("buy_", k), "Buy")
          if (p$coins < it$price) b$attribs$disabled <- "disabled"
          b
        }
        div(class = "shop-item",
            div(div(class = "name", it$name,
                    if (k == "drink") span(class = "desc", sprintf("  (you have %d)", p$drinks))),
                div(class = "desc", it$desc)),
            div(span(class = "price", sprintf("%d coins", it$price)), btn))
      })
    )
  })

  lapply(names(SHOP), function(k) {
    observeEvent(input[[paste0("buy_", k)]], {
      p <- rv$player
      it <- SHOP[[k]]
      if (is.null(p) || p$coins < it$price || k %in% p$gear) return()
      p$coins <- p$coins - it$price
      if (k == "drink") {
        p$drinks <- p$drinks + 1
      } else {
        p[[it$stat]] <- p[[it$stat]] + it$amt
        p$gear <- c(p$gear, k)
      }
      rv$player <- p
      save_game(p)
    })
  })
  observeEvent(input$leave_shop, {
    rv$stage <- "town"
    say("Gear shop: Come back anytime!")
  })

  # ---- Hub ----
  output$hub <- renderUI({ req(rv$player); hub_card(rv$player) })

  output$squad <- renderUI({
    p <- rv$player
    req(p)
    n <- length(p$bench)
    div(class = "card",
        div(class = "who", sprintf("Your squad  |  %d / %d", n + 1, SQUAD_MAX)),
        if (n == 0) div(class = "stats", "No one on the bench yet. Beat wild creatures in the tall grass, then try to recruit them.")
        else lapply(seq_len(n), function(i) {
          cr <- p$bench[[i]]
          div(class = "shop-item",
              div(div(class = "name", sprintf("%s  Lv %d", cr$name, cr$level)),
                  div(class = "desc", sprintf("SHT %d | SPD %d | DEF %d | STA %d  |  Signature: %s",
                                              cr$shooting, cr$speed, cr$defense, cr$max_stamina,
                                              sig_moves[[cr$name]]$name))),
              tags$button(class = "btn", "Make active",
                          onclick = sprintf("Shiny.setInputValue('swap', %d, {priority:'event'})", i)))
        }),
        div(class = "stats", style = "margin-top:8px",
            "Gear and badge bonuses always go with whoever is active. Bench creatures don't earn XP."))
  })

  observeEvent(input$swap, {
    p <- rv$player
    i <- as.integer(input$swap)
    if (is.null(p) || is.na(i) || i < 1 || i > length(p$bench)) return()
    p <- swap_active(p, i)
    rv$player <- p
    save_game(p)
  })

  # ---- New game (from the coach lab) ----
  observeEvent(input$start, {
    rv$player <- new_creature(input$pick)
    save_game(rv$player)
    rv$stage <- "town"
    say(sprintf("Professor Hoops: Great pick! %s is all yours. Rookie Rae is on the court looking for a game.",
                input$pick))
  })

  # ---- Battles ----
  start_battle <- function(trainer = NULL, id = NULL) {
    p <- rv$player
    p$points  <- 0
    p$stamina <- p$max_stamina
    p$sig_used <- FALSE
    rv$player <- p
    tr <- if (!is.null(id)) TRAINERS[[id]] else NULL
    if (is.null(tr)) {
      lvl <- if (is.null(trainer)) p$level + (MAPS[[rv$map]]$wild_bonus %||% 0) + sample(c(-1, 0, 0, 1), 1) else p$level
      rv$cpu <- make_cpu(lvl)
    } else {
      rv$cpu <- make_cpu(p$level + (tr$bonus %||% 1), kind = tr$kind, style = tr$style,
                         boost = tr$boost %||% 0, boost_stat = tr$boost_stat %||% "shooting")
    }
    rv$target   <- tr$target %||% 11
    rv$opponent <- trainer
    rv$opponent_id <- id
    rv$winner <- NULL
    rv$recruit <- NULL
    rv$summary <- character(0)
    rv$phase  <- "offense"
    rv$log <- if (is.null(trainer)) {
      sprintf("A wild %s (Lv %d) jumps out!", rv$cpu$name, rv$cpu$level)
    } else {
      sprintf("%s sends out %s (Lv %d)!", trainer, rv$cpu$name, rv$cpu$level)
    }
    rv$stage <- "battle"
  }
  observeEvent(input$find, start_battle())

  output$boss_banner <- renderUI({
    if (rv$stage != "battle" || is.null(rv$opponent_id)) return(NULL)
    if (!isTRUE(TRAINERS[[rv$opponent_id]]$leader)) return(NULL)
    div(class = "boss", sprintf("GYM LEADER BATTLE: %s  |  First to %d", rv$opponent, rv$target))
  })

  output$phase_css <- renderUI({
    hide <- if (!is.null(rv$winner)) ".off-group, .def-group"
            else if (rv$phase == "offense") ".def-group" else ".off-group"
    if (isTRUE(rv$player$sig_used)) hide <- paste0(hide, ", #mv_Sig")
    if ((rv$player$drinks %||% 0) < 1) hide <- paste0(hide, ", #mv_Drink")
    tags$style(HTML(paste(hide, "{ display: none; }")))
  })

  output$phase_label <- renderUI({
    if (!is.null(rv$winner)) return(NULL)
    if (rv$phase == "offense") {
      div(class = "phase phase-off", "Your ball: pick a shot")
    } else {
      div(class = "phase phase-def",
          sprintf("%s has the ball: pick your defense", rv$cpu$name))
    }
  })

  output$cards <- renderUI({
    req(rv$player, rv$cpu)
    fluidRow(column(6, battle_card(rv$player, "You", rv$target, show_items = TRUE)),
             column(6, battle_card(rv$cpu, rv$opponent %||% "Wild", rv$target)))
  })

  output$result <- renderUI({
    if (is.null(rv$winner)) return(NULL)
    head <- if (rv$winner == "player") {
      div(class = "result win", sprintf("YOU WIN! %d-%d", rv$player$points, rv$cpu$points))
    } else {
      div(class = "result lose", sprintf("You lost %d-%d", rv$player$points, rv$cpu$points))
    }
    tagList(
      head,
      div(class = "summary", lapply(rv$summary, function(l)
        div(class = if (grepl("^LEVEL UP", l)) "lvl" else if (grepl("BADGE", l)) "badge-line" else NULL, l))),
      uiOutput("recruit_box"),
      actionButton("to_town_after", "Back to the map")
    )
  })

  output$recruit_box <- renderUI({
    if (!identical(rv$winner, "player") || !is.null(rv$opponent)) return(NULL)
    st <- rv$recruit
    if (identical(st, "yes")) return(div(class = "summary", div(class = "badge-line",
      sprintf("%s joined your squad! Swap it in at any Coach Center.", rv$cpu$name))))
    if (identical(st, "no")) return(div(class = "summary", div(class = "lose",
      sprintf("%s shook its head and ran back into the grass.", rv$cpu$name))))
    if (length(rv$player$bench) + 1 >= SQUAD_MAX) return(div(class = "stats",
      sprintf("Your squad is full (%d). You can't recruit more right now.", SQUAD_MAX)))
    actionButton("try_recruit",
                 sprintf("Try to recruit %s (Lv %d)  |  %.0f%% chance", rv$cpu$name, rv$cpu$level,
                         100 * recruit_chance(rv$player, rv$cpu)), class = "btn-sig")
  })

  observeEvent(input$try_recruit, {
    if (!identical(rv$winner, "player") || !is.null(rv$opponent) || !is.null(rv$recruit)) return()
    p <- rv$player
    if (length(p$bench) + 1 >= SQUAD_MAX) return()
    if (runif(1) < recruit_chance(p, rv$cpu)) {
      p$bench[[length(p$bench) + 1]] <- to_recruit(rv$cpu)
      rv$player <- p
      save_game(p)
      rv$recruit <- "yes"
    } else {
      rv$recruit <- "no"
    }
  })

  output$log <- renderUI(lapply(rv$log, div))

  end_battle <- function(p, cpu, winner) {
    won  <- winner == "player"
    done <- finish_battle(p, cpu, won = won)
    p <- done$player
    lines <- done$lines
    tr <- if (!is.null(rv$opponent_id)) TRAINERS[[rv$opponent_id]] else NULL

    coins <- if (!won) COIN_REWARD$loss
             else if (isTRUE(tr$leader)) COIN_REWARD$leader
             else if (!is.null(rv$opponent)) COIN_REWARD$trainer
             else COIN_REWARD$wild
    p$coins <- p$coins + coins
    lines <- c(lines, sprintf("+%d coins (you have %d)", coins, p$coins))

    if (won && !is.null(rv$opponent_id)) {
      p$beaten <- unique(c(p$beaten, rv$opponent_id))
      if (!is.null(tr$badge) && !(tr$badge %in% p$badges)) {
        p$badges <- c(p$badges, tr$badge)
        bs <- tr$badge_stat %||% "shooting"
        p[[bs]] <- p[[bs]] + 2
        lines <- c(sprintf("You earned the %s! (+2 %s)", toupper(tr$badge), bs), lines)
      }
    }
    rv$summary <- lines
    save_game(p)
    p
  }

  # Your ball: offense buttons
  lapply(c(names(moves), "Sig"), function(m) {
    observeEvent(input[[paste0("mv_", m)]], {
      if (rv$stage != "battle" || !is.null(rv$winner) || rv$phase != "offense") return()
      if (m == "Sig" && !sig_ready(rv$player)) return()

      r   <- resolve_possession(rv$player, rv$cpu, m, cpu_def_pick(rv$cpu))
      p   <- r$att
      cpu <- r$def
      if (m == "Sig") p$sig_used <- TRUE

      p$train$stamina <- p$train$stamina + 1
      if (m == "Rest") p$train$stamina <- p$train$stamina + 1
      mt <- if (m == "Sig") list(stat = sig_moves[[p$name]]$stat, try = 3, make = 2) else move_train[[m]]
      if (!is.null(mt) && !is.na(r$made)) {
        p$train[[mt$stat]] <- p$train[[mt$stat]] + mt$try + (if (r$made) mt$make else 0)
      }
      if (r$oreb) p$train$defense <- p$train$defense + 2   # crashing the glass

      rv$log <- c(r$msg, rv$log)
      if (p$points >= rv$target) {
        p <- end_battle(p, cpu, "player")
        rv$winner <- "player"
      } else if (!r$oreb) {
        rv$phase <- "defense"
      }
      rv$player <- p
      rv$cpu    <- cpu
    })
  })

  # Gatorade: +40 stamina, uses your possession
  observeEvent(input$mv_Drink, {
    if (rv$stage != "battle" || !is.null(rv$winner) || rv$phase != "offense") return()
    p <- rv$player
    if ((p$drinks %||% 0) < 1) return()
    p$drinks  <- p$drinks - 1
    p$stamina <- min(p$max_stamina, p$stamina + 40)
    rv$log <- c(sprintf("%s drinks a Gatorade. +40 stamina! (%d left)", p$name, p$drinks), rv$log)
    rv$player <- p
    rv$phase  <- "defense"
  })

  # Their ball: defense buttons
  lapply(names(def_moves), function(d) {
    observeEvent(input[[paste0("df_", d)]], {
      if (rv$stage != "battle" || !is.null(rv$winner) || rv$phase != "defense") return()

      cm  <- cpu_pick(rv$cpu, rv$player)
      r   <- resolve_possession(rv$cpu, rv$player, cm, d)
      cpu <- r$att
      p   <- r$def
      if (cm == "Sig") cpu$sig_used <- TRUE

      p$train$stamina <- p$train$stamina + 1
      if (r$def_move %in% c("Contest", "Block", "Steal")) {
        p$train$defense <- p$train$defense + 1
      }
      if (r$stolen) {
        p$train$defense <- p$train$defense + 3
        p$train$speed   <- p$train$speed + 2
      } else if (!is.na(r$made) && !r$made && !r$oreb) {
        p$train$defense <- p$train$defense + (if (r$def_move == "Block") 4 else 3)
      }

      rv$log <- c(r$msg, rv$log)
      if (cpu$points >= rv$target) {
        p <- end_battle(p, cpu, "cpu")
        rv$winner <- "cpu"
      } else if (!r$oreb) {
        rv$phase <- "offense"
      }
      rv$player <- p
      rv$cpu    <- cpu
    })
  })

  observeEvent(input$to_town, { rv$stage <- "town"; say("") })

  observeEvent(input$to_town_after, {
    won <- identical(rv$winner, "player")
    tr  <- if (!is.null(rv$opponent_id)) TRAINERS[[rv$opponent_id]] else NULL
    if (!is.null(tr)) {
      txt <- if (won) tr$win else tr$lose
      say(if (isTRUE(tr$leader)) txt else sprintf("%s: %s", tr$name, txt))
    } else if (!is.null(rv$opponent)) {
      say(if (won) sprintf("%s: Good game, coach! Run it back anytime.", rv$opponent)
          else sprintf("%s: Too easy! Come back when you've trained some more.", rv$opponent))
    } else {
      say("")
    }
    rv$stage <- "town"
  })

  # Start over (with confirm)
  observeEvent(input$reset, {
    showModal(modalDialog(
      "Start over? Your creature's level, stats, coins, gear and badges will be erased.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_reset", "Erase and start over")),
      easyClose = TRUE))
  })
  observeEvent(input$confirm_reset, {
    removeModal()
    if (file.exists(SAVE_FILE)) file.remove(SAVE_FILE)
    rv$player <- NULL
    rv$pos    <- START_POS
    rv$map    <- "town"
    rv$stage  <- "town"
    say("New game! Head to the coach lab (bottom left) to pick your first creature.")
  })
}

shinyApp(ui, server)

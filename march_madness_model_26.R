setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(data.table)
library(magrittr)
library(dplyr)
library(ggraph)
library(igraph)


options(scipen = 999)
mm_26_dk <- fread('./dk_26.csv')
mm_26_yahoo_wpw <- fread('./yahoo_wpw.csv')

dk_26_adj[Team == 'TCU']

#Create a bracket or group (num_comps > 1) by simulating based on the 
#probabilities defined by the data whether that is expected win perc or 
#the likelihood of being picked
create_bracket_or_group_bw <- function(data, num_sims, num_comps){
  data <- bracket_data_adj
  # num_sims <- 10000
  # num_comps <- 1

  #Make a data tale crossed for each compettior and sim_id
  sims <- dplyr::cross_join(data, data.table(sim_id = 1:num_sims)) %>%
    dplyr::cross_join(data.table(competitor = 1:num_comps))
  
  #Select a champion
  ### Champion
  sims[, champ := Team[sample(.N, 1, prob = six)], by = .(sim_id, competitor)]
  champs <- sims[Team == champ]
  
  #Runner Up
  #Select a runner up from the group that isn't the champ group. 
  #Define the conditional probability of each of those teams making the next
  #round but not winning the next round
  sims[champs, champ_grp := i.final_grp, on = .(sim_id, competitor)]
  semis <- sims[final_grp != champ_grp]
  semis[, cond_prob := (five - six) / (1 - six)]
  semis[, ru := Team[sample(.N, 1, prob = cond_prob)], by = .(sim_id, competitor)]
  semis %<>% .[Team == ru]

  #Final Four Teams
  #Select the two other final four teams from the other regions
  rem_grps <- rbind(champs[, .(sim_id, competitor, ff_grp)], 
                    semis[, .(sim_id, competitor, ff_grp)])
  ff <- sims[!rem_grps, on = .(sim_id, competitor, ff_grp)]
  ff[, cond_prob := (four - five) / (1 - five)]
  ff[, qf  := Team[sample(.N, 1, prob = cond_prob)], by = .(sim_id, competitor, ff_grp)]
  ff %<>% .[Team == qf]

  #Elite Eight Teams
  rem_grps <- rbind(champs[, .(sim_id, competitor, ff_grp, ee_grp)], 
                    semis[, .(sim_id, competitor, ff_grp, ee_grp)], 
                    ff[, .(sim_id, competitor, ff_grp, ee_grp)])
  ee <- sims[!rem_grps, on = .(sim_id, competitor, ff_grp, ee_grp)]
  ee[, cond_prob := (three - four) / (1 - four)]
  ee[, ee  := Team[sample(.N, 1, prob = cond_prob)], by = .(sim_id, competitor, ff_grp, ee_grp)]
  ee %<>% .[Team == ee]
  
  #SS Teams
  rem_grps <- rbind(champs[, .(sim_id, competitor, ff_grp, ss_grp)], 
                    semis[, .(sim_id, competitor, ff_grp, ss_grp)], 
                    ff[, .(sim_id, competitor, ff_grp, ss_grp)], 
                    ee[, .(sim_id, competitor, ff_grp, ss_grp)])
  ss <- sims[!rem_grps, on = .(sim_id, competitor, ff_grp, ss_grp)]
  ss[, cond_prob := (two - three) / (1 - three)]
  ss[, ss  := Team[sample(.N, 1, prob = cond_prob)], by = .(sim_id, competitor, ff_grp, ss_grp)]
  ss %<>% .[Team == ss]
  
  #RTT Teams
  rem_grps <- rbind(champs[, .(sim_id, competitor, ff_grp, rtt_grp)], 
                    semis[, .(sim_id, competitor, ff_grp, rtt_grp)], 
                    ff[, .(sim_id, competitor, ff_grp, rtt_grp)], 
                    ee[, .(sim_id, competitor, ff_grp, rtt_grp)], 
                    ss[, .(sim_id, competitor, ff_grp, rtt_grp)])
  rtt <- sims[!rem_grps, on = .(sim_id, competitor, ff_grp, rtt_grp)]
  rtt[, cond_prob := (one - two) / (1 - two)]
  rtt[, rtt  := Team[sample(.N, 1, prob = cond_prob)], by = .(sim_id, competitor, ff_grp, rtt_grp)]
  rtt %<>% .[Team == rtt]

  #Create the output which signifies a team won in that round
  output <- rbind(
    rtt[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    ss[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    ee[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    ff[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    semis[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    champs[, .(sim_id, round = 1, Team, competitor, grp = paste0(ff_grp, rtt_grp))], 
    
    ss[, .(sim_id, round = 2, Team, competitor, grp = paste0(ff_grp, ss_grp))], 
    ee[, .(sim_id, round = 2, Team, competitor, grp = paste0(ff_grp, ss_grp))], 
    ff[, .(sim_id, round = 2, Team, competitor, grp = paste0(ff_grp, ss_grp))], 
    semis[, .(sim_id, round = 2, Team, competitor, grp = paste0(ff_grp, ss_grp))], 
    champs[, .(sim_id, round = 2, Team, competitor, grp = paste0(ff_grp, ss_grp))],
    
    ee[, .(sim_id, round = 3, Team, competitor, grp = paste0(ff_grp, ee_grp))], 
    ff[, .(sim_id, round = 3, Team, competitor, grp = paste0(ff_grp, ee_grp))], 
    semis[, .(sim_id, round = 3, Team, competitor, grp = paste0(ff_grp, ee_grp))], 
    champs[, .(sim_id, round = 3, Team, competitor, grp = paste0(ff_grp, ee_grp))],
    
    ff[, .(sim_id, round = 4, Team, competitor, grp = paste0(ff_grp))], 
    semis[, .(sim_id, round = 4, Team, competitor, grp = paste0(ff_grp))], 
    champs[, .(sim_id, round = 4, Team, competitor, grp = paste0(ff_grp))],
    
    semis[, .(sim_id, round = 5, Team, competitor, grp = paste0(final_grp))],
    champs[, .(sim_id, round = 5, Team, competitor, grp = paste0(final_grp))],
    
    champs[, .(sim_id, round = 6, Team, competitor, grp = 'final')]
  )
  
  return(output)
  
}

check_wrong_prob <- function(data, col_later, col_earlier){
  if(nrow(data[get(col_earlier) < get(col_later)]) > 0){
    if(min(data[get(col_earlier) < get(col_later), Seed]) >= 10){
      print(paste0('fixing ', nrow(data[get(col_earlier) < get(col_later)]), ' teams'))
      data[get(col_earlier) < get(col_later), eval(col_earlier) := get(col_later) * 2]
    } else {
      stop("Somethings Wrong")
    }
  }
  return(data)
}


#Adjust Bracket so conditional probabilities are possible
#Data is gathered from bettings sites or aggregators and each region
#does not necessarily have coniditonal probabilities that are possible

#It adjusts by looking at each teams likelihood of being picked into a certain
#round amongst 10000 simulations. Then compares that to what the probability
# should have been and adjusts. It does this for all rounds iteratively starting
#in the semi finals to force the base probabilities to a point such that 
#the conditional probabilities implied by the base probabilities reach the final 
#probabilities defined in the base data.
AdjustBracket <- function(bracket_data){
  
  bracket_data <- copy(mm_26_dk)
  bracket_data_adj <- copy(bracket_data)
  
  sim <- create_bracket_or_group_bw(bracket_data, 10000, 1)
  
  #Runner Up
  five <- sim[round == 5, .(perc = .N / 10000), by = Team][
    bracket_data, 
    five := i.five, 
    on = .(Team)
  ][order(five)]
  
  bracket_data_adj[five, five := five * (five / i.perc), on = .(Team)]
  bracket_data_adj[five < six]
  bracket_data_adj <- check_wrong_prob(bracket_data_adj, 'six', 'five')
  
  #Final Four
  sim <- create_bracket_or_group_bw(bracket_data, 10000, 1)
  
  four <- sim[round == 4, .(perc = .N / 10000), by = Team][
    bracket_data, 
    four := i.four, 
    on = .(Team)
  ][order(five)]
  
  bracket_data_adj[four, four := four * (four / i.perc), on = .(Team)]
  bracket_data_adj[four < five]
  bracket_data_adj <- check_wrong_prob(bracket_data_adj, 'five', 'four')
  
  #Elite Eight
  sim <- create_bracket_or_group_bw(bracket_data, 10000, 1)
  
  three <- sim[round == 3, .(perc = .N / 10000), by = Team][
    bracket_data, 
    three := i.three, 
    on = .(Team)
  ][order(five)]
  
  bracket_data_adj[three, three := three * (three / i.perc), on = .(Team)]
  bracket_data_adj[three < four]
  bracket_data_adj <- check_wrong_prob(bracket_data_adj, 'four', 'three')
  
  #Sweet Sixteen
  sim <- create_bracket_or_group_bw(bracket_data, 10000, 1)
  
  two <- sim[round == 2, .(perc = .N / 10000), by = Team][
    bracket_data, 
    two := i.two, 
    on = .(Team)
  ][order(five)]
  
  bracket_data_adj[two, two := two * (two / i.perc), on = .(Team)]
  bracket_data_adj[two < three]
  bracket_data_adj <- check_wrong_prob(bracket_data_adj, 'three', 'two')
  
  #R32
  sim <- create_bracket_or_group_bw(bracket_data, 10000, 1)
  
  one <- sim[round == 1, .(perc = .N / 10000), by = Team][
    bracket_data, 
    one := i.one, 
    on = .(Team)
  ][order(five)]
  
  bracket_data_adj[one, one := one * (one / i.perc), on = .(Team)]
  bracket_data_adj[one < two]
  bracket_data_adj <- check_wrong_prob(bracket_data_adj, 'two', 'one')
  
  #Check it still works
  sim <- create_bracket_or_group_bw(bracket_data_adj, 10000, 1)
  
  return(bracket_data_adj)
  
}

#This function defines a new set of data to select from when building a bracket to test
#the general idea is to start by sampling brackets from the betting odds and seeing
#based on the who picked whom how well those brackets would do in a group of size n
#Then this function looks at the best subset of brackets (results) and recreates a 
#probability matrix for what teams to sample in the next iteration of brackets to test
#The probabilities are determined by looking at the subset of brackets and seeing
#the likelihood that each team makes each round. Then weighting that with the original 
#probability to not reach a local max too quickly
bracket_matrix_from_results <- function(orig_data, results){
  # orig_data <- mm_25_em
  # results <- top_25_perc
  
  comps <- length(unique(results$competitor))
  
  win_perc <- results[, .(win_perc = .N / comps), by = .(Team, round)] %>%
    dcast(Team ~ round, value.var = 'win_perc')
  
  setnames(win_perc, 1:7, c('Team', 'one', 'two', 'three', 'four', 'five', 'six'))
  
  win_perc[, c("one", "two", "three", "four", "five", "six") := 
             lapply(.SD, function(x) fcoalesce(x, 0.00000001)), 
           .SDcols = c("one", "two", "three", "four", "five", "six")]
  
  new_data <- copy(orig_data)[win_perc, `:=` (
    one = one * .25 + 0.75 * i.one, 
    two = two * .25 + 0.75 * i.two, 
    three = three * .25 + 0.75 * i.three, 
    four = four * .25 + 0.75 * i.four, 
    five = five * .25 + 0.75 * i.five, 
    six = six * .25 + 0.75 * i.six
  ), on = .(Team)]
  
  new_data_adj <- AdjustBracket(new_data)
  
  return(new_data_adj)
  
}

optomize_bracket <- function(real_sims, grp_sims, orig_data = mm_24_dk, start_brackets = 1000, seed_brackets = NULL, iterations = 10){
  
  # real_sims <- act_bracket
  # grp_sims <- grp
  # seed_brackets <- NULL
  # iterations <- 10
  # orig_data <- mm_25_em
  # start_brackets <- 1000
  
  # grp_sims[real_sims, winner := i.Team, on = .(round, grp, sim_id)]
  # grp_sims[, pts := (Team == winner) * 64 / (2 ^ (7 - round))]
  # 
  # #score brackets for the groups
  # scoring <- grp_sims[, .(
  #   pts = sum(pts)
  # ), by = .(sim_id, competitor)]
  # 
  # max_scores <- scoring[, max_pts := max(pts), by = sim_id][pts == max_pts][, .(
  #   .N
  # ), by = .(sim_id, max_pts)]
  # 
  # n_sims <- length(unique(max_scores$sim_id))
  
  i <- 1
  while(i < iterations){
    
    real_sims <- create_bracket_or_group_bw(orig_data, 10000, 1)
    
    grp_sims[real_sims, winner := i.Team, on = .(round, grp, sim_id)]
    grp_sims[, pts := (Team == winner) * 64 / (2 ^ (7 - round))]
    
    #score brackets for the groups
    scoring <- grp_sims[, .(
      pts = sum(pts)
    ), by = .(sim_id, competitor)]
    
    max_scores <- scoring[, max_pts := max(pts), by = sim_id][pts == max_pts][, .(
      .N
    ), by = .(sim_id, max_pts)]
    
    n_sims <- length(unique(max_scores$sim_id))
    
    print(paste0("iteration: ", i))
    
    if(i == 1){
      sample_brackets <- create_bracket_or_group_bw(orig_data, 1, start_brackets)
      sample_brackets$sim_id <- NULL
      sample_brackets$type <- 'random'
      
      if(!is.null(seed_brackets)){
        sample_brackets %<>% rbind(seed_brackets[, type := 'seed'])
      }
    }
    if(i > 1){
      
      pop <- create_bracket_or_group_bw(new_data, 1, start_brackets * .5)[, type := 'pop_sample']
      rand <- create_bracket_or_group_bw(orig_data, 1, start_brackets * .5)[, type := 'random'][, competitor := competitor + start_brackets * .5]
      
      sample_brackets <- rbind(pop, rand)
      
      sample_brackets$sim_id <- NULL
      
      sample_brackets %<>% rbind(top_5_perc)
    }
    
    test_bracket <- sample_brackets %>% cross_join(data.table(sim_id = 1:n_sims))
    test_bracket[real_sims, winner := i.Team, on = .(round, grp, sim_id)]
    test_bracket[, pts := (Team == winner) * 64 / (2 ^ (7 - round))]
    
    test_scoring <- test_bracket[, .(
      pts = sum(pts)
    ), by = .(sim_id, competitor)]
    
    test_scoring[max_scores, `:=` (
      N = i.N, 
      max = i.max_pts
    ), on = .(sim_id)]
    
    winners <- test_scoring[pts >= max][, win := 1]
    winners[pts == max, win := 1/(N+1)]
    wins_by_bracket <- winners[, .(wins = sum(win)), by = competitor]
    wins_by_bracket %<>% .[order(wins, decreasing = TRUE), Rank := .I] %>% .[order(wins, decreasing = TRUE)]
    sample_brackets[wins_by_bracket, `:=` (Rank = i.Rank, wins = i.wins), on = .(competitor)]
    
    sample_brackets %<>% .[order(Rank)]
    
    if(i > 1){
      print(paste0(
        "The Previous Winner ranked ", 
        sample_brackets[type == 'winner' & round == 6, Rank], 
        " winning ", 
        sample_brackets[type == 'winner' & round == 6, wins] / n_sims, 
        " percent of brackets"
      ))
    }
    print(paste0(
      "The Current Winner ranked won ", 
      wins_by_bracket[Rank == 1, wins] / n_sims, 
      " percent of bracket pools. It was a type ",
      sample_brackets[competitor %in% wins_by_bracket[Rank == 1, competitor], type][1]
    ))
    print(paste0(
      "The Final Four Was: "
    ))
    print(sample_brackets[competitor %in% wins_by_bracket[Rank == 1, competitor] & round >= 4])
    
    top_25_perc <- sample_brackets[competitor %in% wins_by_bracket[Rank <= start_brackets / 4, competitor]]
    
    top_5_perc <- sample_brackets[competitor %in% wins_by_bracket[Rank <= start_brackets / 20, competitor]][, type := 'carryover']
    top_5_perc[competitor %in% wins_by_bracket[Rank == 1, competitor], type := 'winner']
    top_5_perc[, competitor := competitor + start_brackets]
    top_5_perc[, `:=` (
      Rank = NULL, 
      wins = NULL
    )]
    
    
    new_data <- bracket_matrix_from_results(orig_data, top_25_perc)
    
    i <- i + 1
    
  }
  return(top_5_perc[type == 'winner'])
}

dk_26_adj <- AdjustBracket(mm_26_dk)

wpw_adj <- AdjustBracket(mm_26_yahoo_wpw)

act_bracket <- create_bracket_or_group_bw(dk_26_adj, 10000, 1)
grp <- create_bracket_or_group_bw(wpw_adj, 10000, 2)
best_bracket_2 <- optomize_bracket(act_bracket, grp, orig_data = dk_26_adj, start_brackets = 1000, seed_brackets = NULL, iterations = 10)

fwrite(best_bracket_2, 'best_bracket_2.csv')

first_evens <- function(x) {seq(from=2,to=2*x,length.out=x)}
first_odds <- function(x) {seq(from=1,to=2*x-1,length.out=x)}

### calculate y-values for horizontal lines:
### this is for top-left corner of the bracket,
### but multiplying sequences by -1 makes these 
### values work for bottom right and left corners;
### final round has teams at y=2*off.set

r1.y.width <- 1.5*strheight(s="Virginia Common",units="in") # this effects the width of the first round
r1.y.offset <- 0.125*r1.y.width # this effects distance from y=0

r1.y <- seq(from=r1.y.offset,to=r1.y.offset+r1.y.width,length.out=16)
r2.y <- seq(from=mean(r1.y[1:2]),to=mean(r1.y[15:16]),length.out=8)
r3.y <- seq(from=mean(r2.y[1:2]),to=mean(r2.y[7:8]),length.out=4)
r4.y <- seq(from=mean(r3.y[1:2]),to=mean(r3.y[3:4]),length.out=2)
r5.y <- seq(from=mean(r4.y[1:2]),to=mean(r4.y[1:2]),length.out=1)
r6.y <- 1.5*r1.y.offset

### calculate horizontal bar start and stop coordinates
### note that there are 6 total rounds -- 5 rounds per quadrant
r1.x.width <- 1.25*strwidth("Viriginia Commonwealth","inches") # how long should horizontal lines be?
r1.x.offset <- 1
round.break.points <- -(seq(from=0,to=7*r1.x.width,by=r1.x.width)+r1.x.offset)

r1.x <- round.break.points[7:6]
r2.x <- round.break.points[6:5]
r3.x <- round.break.points[5:4]
r4.x <- round.break.points[4:3]
r5.x <- round.break.points[3:2]
r6.x <- round.break.points[2:1]

### calculate verticals line coordinates: these are based off of
### r1.y values. Round 5 verticals need to connect the four subtrees
### via the top-left <-> bottom-left and top-right <-> bottom-right

r1.verticals.start <- r1.y[first_odds(8)]
r1.verticals.stop <- r1.y[first_evens(8)]

r2.verticals.start <- r2.y[first_odds(4)]
r2.verticals.stop <- r2.y[first_evens(4)]

r3.verticals.start <- r3.y[first_odds(2)]
r3.verticals.stop <- r3.y[first_evens(2)]

r4.verticals.start <- r4.y[first_odds(1)]
r4.verticals.stop <- r4.y[first_evens(1)]

r5.verticals.start <- r5.y[1]
r5.verticals.stop <- -r5.y[1]

empty.bracket <- ggplot() + theme_bw() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), plot.margin=unit(c(0,0,-6,-6),"mm"), text=element_text(size=12,hjust=0,vjust=0)) + coord_cartesian(ylim = c(-1.05*r1.y[16],1.05*r1.y[16]), xlim = c(1.025*r1.x[1],-1.025*r1.x[1]))

### add first round bars, and vertical connectors, make addition of each quadrant verbose
empty.bracket <- empty.bracket + geom_segment(aes(x=r1.x[1],y=r1.y,yend=r1.y,xend=r1.x[2])) + geom_segment(aes(x=r1.x[2],xend=r1.x[2],y=r1.verticals.start,yend=r1.verticals.stop)) + geom_segment(aes(x=r1.x[1],y=-r1.y,yend=-r1.y,xend=r1.x[2])) + geom_segment(aes(x=r1.x[2],xend=r1.x[2],y=-r1.verticals.start,yend=-r1.verticals.stop)) + geom_segment(aes(x=-r1.x[1],y=r1.y,yend=r1.y,xend=-r1.x[2])) + geom_segment(aes(x=-r1.x[2],xend=-r1.x[2],y=r1.verticals.start,yend=r1.verticals.stop)) + geom_segment(aes(x=-r1.x[1],y=-r1.y,yend=-r1.y,xend=-r1.x[2])) + geom_segment(aes(x=-r1.x[2],xend=-r1.x[2],y=-r1.verticals.start,yend=-r1.verticals.stop)) 

### add second round
empty.bracket <- empty.bracket + geom_segment(aes(x=r2.x[1],y=r2.y,yend=r2.y,xend=r2.x[2])) + geom_segment(aes(x=r2.x[2],xend=r2.x[2],y=r2.verticals.start,yend=r2.verticals.stop)) + geom_segment(aes(x=r2.x[1],y=-r2.y,yend=-r2.y,xend=r2.x[2])) + geom_segment(aes(x=r2.x[2],xend=r2.x[2],y=-r2.verticals.start,yend=-r2.verticals.stop)) + geom_segment(aes(x=-r2.x[1],y=r2.y,yend=r2.y,xend=-r2.x[2])) + geom_segment(aes(x=-r2.x[2],xend=-r2.x[2],y=r2.verticals.start,yend=r2.verticals.stop)) + geom_segment(aes(x=-r2.x[1],y=-r2.y,yend=-r2.y,xend=-r2.x[2])) + geom_segment(aes(x=-r2.x[2],xend=-r2.x[2],y=-r2.verticals.start,yend=-r2.verticals.stop)) 

### add third round
empty.bracket <- empty.bracket + geom_segment(aes(x=r3.x[1],y=r3.y,yend=r3.y,xend=r3.x[2])) + geom_segment(aes(x=r3.x[2],xend=r3.x[2],y=r3.verticals.start,yend=r3.verticals.stop)) + geom_segment(aes(x=r3.x[1],y=-r3.y,yend=-r3.y,xend=r3.x[2])) + geom_segment(aes(x=r3.x[2],xend=r3.x[2],y=-r3.verticals.start,yend=-r3.verticals.stop)) + geom_segment(aes(x=-r3.x[1],y=r3.y,yend=r3.y,xend=-r3.x[2])) + geom_segment(aes(x=-r3.x[2],xend=-r3.x[2],y=r3.verticals.start,yend=r3.verticals.stop)) + geom_segment(aes(x=-r3.x[1],y=-r3.y,yend=-r3.y,xend=-r3.x[2])) + geom_segment(aes(x=-r3.x[2],xend=-r3.x[2],y=-r3.verticals.start,yend=-r3.verticals.stop)) 

### add fourth round
empty.bracket <- empty.bracket + geom_segment(aes(x=r4.x[1],y=r4.y,yend=r4.y,xend=r4.x[2])) + geom_segment(aes(x=r4.x[2],xend=r4.x[2],y=r4.verticals.start,yend=r4.verticals.stop)) + geom_segment(aes(x=r4.x[1],y=-r4.y,yend=-r4.y,xend=r4.x[2])) + geom_segment(aes(x=r4.x[2],xend=r4.x[2],y=-r4.verticals.start,yend=-r4.verticals.stop)) + geom_segment(aes(x=-r4.x[1],y=r4.y,yend=r4.y,xend=-r4.x[2])) + geom_segment(aes(x=-r4.x[2],xend=-r4.x[2],y=r4.verticals.start,yend=r4.verticals.stop)) + geom_segment(aes(x=-r4.x[1],y=-r4.y,yend=-r4.y,xend=-r4.x[2])) + geom_segment(aes(x=-r4.x[2],xend=-r4.x[2],y=-r4.verticals.start,yend=-r4.verticals.stop)) 

### add fifth round: add necessary horizontal bars and then
### vertical bars
empty.bracket <- empty.bracket + geom_segment(aes(x=r5.x[1],y=r5.y,yend=r5.y,xend=r5.x[2])) +  geom_segment(aes(x=r5.x[1],y=-r5.y,yend=-r5.y,xend=r5.x[2])) + geom_segment(aes(x=r5.x[2],y=-r5.y, yend=r5.y, xend=r5.x[2])) + geom_segment(aes(x=-r5.x[1],y=r5.y,yend=r5.y,xend=-r5.x[2])) + geom_segment(aes(x=-r5.x[1],y=-r5.y,yend=-r5.y,xend=-r5.x[2])) + geom_segment(aes(x=-r5.x[2],y=-r5.y,yend=r5.y,xend=-r5.x[2])) 

### due to symmetry, the 6th (and final round)
empty.bracket <- empty.bracket  + geom_segment(aes(x=r6.x[1],y=r6.y,xend=r6.x[2],yend=r6.y)) + geom_segment(aes(x=-r6.x[1],y=-r6.y,xend=-r6.x[2],yend=-r6.y))

### add winner location
empty.bracket <- empty.bracket + geom_segment(aes(x=mean(r6.x),xend=-mean(r6.x),y=0,yend=0))

### put some test labels on the bracket slots
Labels_og <- c("Auburn",
               "StFrancis",
               "Louisville",
               "Creighton",
               "Michigan",
               "UCSB",
               "TAMU",
               "Yale",
               "OleMiss",
               "SDSU",
               "ISU",
               "Lipscomb",
               "Marquette",
               "NewMexico",
               "MichSt",
               "Bryant"
)
Labels <- Labels_og
vec <- best_bracket[round == 1 & grp %like% 'S', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 2 & grp %like% 'S', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 3 & grp %like% 'S', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 4 & grp %like% 'S', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])



TextFrame <- data.frame(X = c(rep(r1.x[1], 16), rep(r2.x[1], 8), rep(r3.x[1], 4), rep(r4.x[1], 2), rep(r5.x[1], 1)), Y = c(rev(r1.y), rev(r2.y), rev(r3.y), rev(r4.y), rev(r5.y)), LAB = Labels)
TextFrame <- transform(TextFrame, w = strwidth(LAB, 'inches') + 0.05, h = strheight(LAB, 'inches') + 0.5)

### display results
filled_bracket <- empty.bracket + geom_rect(data = TextFrame, aes(xmin = X, xmax = X + w, ymin = Y, ymax = Y + h),alpha=0) + geom_text(data=TextFrame,aes(x=X,y=Y,label=LAB),size=rel(3),hjust=-.2,vjust=-.2)

### put some test labels on the bracket slots
Labels_og <- c("Florida",
               "Norfolk",
               "Uconn",
               "Oklahoma",
               "Memphis",
               "ColoradoSt",
               "Maryland",
               "GCU",
               "Missouri",
               "Drake",
               "TexasTech",
               "UNCW",
               "Kansas",
               "Arkansas",
               "StJohns",
               'Omaha'
)
Labels <- Labels_og
vec <- best_bracket[round == 1 & grp %like% 'W', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 2 & grp %like% 'W', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 3 & grp %like% 'W', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 4 & grp %like% 'W', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])

TextFrame <- data.frame(X = c(rep(r1.x[1], 16), rep(r2.x[1], 8), rep(r3.x[1], 4), rep(r4.x[1], 2), rep(r5.x[1], 1)), Y = -c(r1.y, r2.y, r3.y, r4.y, r5.y), LAB = Labels)
TextFrame <- transform(TextFrame, w = strwidth(LAB, 'inches') + 0.05, h = strheight(LAB, 'inches') + 0.5)

### display results
filled_bracket <- filled_bracket + geom_rect(data = TextFrame, aes(xmin = X, xmax = X + w, ymin = Y, ymax = Y + h),alpha=0) + geom_text(data=TextFrame,aes(x=X,y=Y,label=LAB),size=rel(3),hjust=-.2,vjust=-.2)


#Top Left

### put some test labels on the bracket slots
Labels_og <- c("Duke",
               "American",
               "MissSt",
               "Baylor",
               "Oregon",
               "Liberty",
               "Arizona",
               "Akron",
               "BYU",
               "VCU",
               "Wisconsin",
               "Montana",
               "StMarys",
               'Vanderbilt',
               "Alabama",
               "RobertMorris"
               
)
Labels <- Labels_og
vec <- best_bracket[round == 1 & grp %like% 'E', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 2 & grp %like% 'E', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 3 & grp %like% 'E', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 4 & grp %like% 'E', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])

TextFrame <- data.frame(X = -c(rep(r1.x[1], 16), rep(r2.x[1], 8), rep(r3.x[1], 4), rep(r4.x[1], 2), rep(r5.x[1], 1)), Y = -c(r1.y, r2.y, r3.y, r4.y, r5.y), LAB = Labels)
TextFrame <- transform(TextFrame, w = strwidth(LAB, 'inches') + 0.05, h = strheight(LAB, 'inches') + 0.5)

### display results
filled_bracket <- filled_bracket + geom_rect(data = TextFrame, aes(xmin = X, xmax = X + w, ymin = Y, ymax = Y + h),alpha=0) + geom_text(data=TextFrame,aes(x=X,y=Y,label=LAB),size=rel(3),hjust=-.2,vjust=-.2)

#Bottom Right

### put some test labels on the bracket slots
Labels_og <- c("Houston",
               "SIUE",
               "Gonzaga",
               "Georgia",
               "Clemson",
               "McNeese",
               "Purdue",
               "HighPoint",
               "Illinois",
               "Texas",
               "Kentucky",
               "Troy",
               "UCLA",
               "UtahSt",
               "Tennessee",
               'Wofford'
               
               
)
Labels <- Labels_og
vec <- best_bracket[round == 1 & grp %like% 'M', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 2 & grp %like% 'M', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 3 & grp %like% 'M', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])
vec <- best_bracket[round == 4 & grp %like% 'M', Team]
Labels %<>% c(vec[order(match(vec, Labels_og))])

TextFrame <- data.frame(X = -c(rep(r1.x[1], 16), rep(r2.x[1], 8), rep(r3.x[1], 4), rep(r4.x[1], 2), rep(r5.x[1], 1)), Y = c(rev(r1.y), rev(r2.y), rev(r3.y), rev(r4.y), rev(r5.y)), LAB = Labels)
TextFrame <- transform(TextFrame, w = strwidth(LAB, 'inches') + 0.05, h = strheight(LAB, 'inches') + 0.5)

### display results
filled_bracket <- filled_bracket + geom_rect(data = TextFrame, aes(xmin = X, xmax = X + w, ymin = Y, ymax = Y + h),alpha=0) + geom_text(data=TextFrame,aes(x=X,y=Y,label=LAB),size=rel(3),hjust=-.2,vjust=-.2)


install.packages("ggplot2")
library(ggraph)
library(igraph)
library(ggplot2)
# detach("package:ggplot2", unload = TRUE)
# Create an edge list for the bracket
dt <- data.table(
  round = c(1, 1, 1, 1, 2, 2, 3),  # Round number
  team1 = c("A", "B", "C", "D", "W1", "W2", "W3"),  # Team 1
  team2 = c("E", "F", "G", "H", "W3", "W4", "Champion")  # Team 2
)

# Generate match winners (e.g., W1, W2 are winners of Round 1)
dt[, winner := paste0("W", .I)]  # Dummy winner names

# Create an edge list for the bracket
edges <- dt[, .(team1, team2, winner)]

# Convert to a directed graph
g <- graph_from_data_frame(edges, directed = TRUE)

# Plot the bracket
ggraph(g, layout = "tree") + 
  geom_edge_diagonal() +  # Draw edges between matchups
  geom_node_label(aes(label = name), size = 5, fill = "white", color = "black") +
  theme_void() +  # Remove background elements
  ggtitle("🏆 Tournament Bracket")

best_bracket[mm_25_em_adj, Seed := i.Seed, on = .(Team)]
best_bracket[order(round, grp)]

best_bracket <- fread('./best_bracket_60.csv')

best_bracket[round == 4 & grp == 'E', Team := 'Alabama']

# best_bracket <- top_5_perc
#out of sample test
# best_bracket %<>% .[type == 'winner']
oos_bracket <- create_bracket_or_group_bw(mm_25_em, 10000, 1)
oos_group <- create_bracket_or_group_bw(mm_25_yahoo_wpw, 10000, 75)

grade_bracket(best_bracket, oos_bracket, oos_group)

grade_bracket <- function(bracket, real_sims, group){
  
  bracket <- best_bracket
  real_sims <- oos_bracket
  group <- oos_group
  
  n_sims <- length(unique(real_sims$sim_id))
  
  test_bracket <- bracket %>% cross_join(data.table(sim_id = 1:n_sims))
  
  group[real_sims, winner := i.Team, on = .(round, grp, sim_id)]
  group[, pts := (Team == winner) * 64 / (2 ^ (7 - round))]
  
  #score brackets for the groups
  grp_scoring <- group[, .(
    pts = sum(pts)
  ), by = .(sim_id, competitor)]
  
  max_scores <- grp_scoring[, max_pts := max(pts), by = sim_id][pts == max_pts][, .(
    .N
  ), by = .(sim_id, max_pts)]
  
  test_bracket[real_sims, winner := i.Team, on = .(round, grp, sim_id)]
  test_bracket[, pts := (Team == winner) * 64 / (2 ^ (7 - round))]
  
  #score brackets for the groups
  br_scoring <- test_bracket[, .(
    pts = sum(pts)
  ), by = .(sim_id)]
  
  br_scoring[max_scores, `:=` (
    N = i.N, 
    max = i.max_pts
  ), on = .(sim_id)]
  
  winners <- br_scoring[pts >= max][, win := 1]
  winners[pts == max, win := 1/(N+1)]
  
  
  comps <- length(unique(group$competitor))
  
  print(paste0("This Bracket won: ", sum(winners$win), " of ", n_sims, " brackets. Expected value is ", n_sims/comps))
}

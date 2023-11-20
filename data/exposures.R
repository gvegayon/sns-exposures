library(data.table)
library(foreign)
library(netdiffuseR)

dat <- read.dta("data-raw/SNS datamerged081315edited.dta") |>
  as.data.table()
  
str(dat)

# Creating a unique id ---------------------------------------------------------
dat[, .(photoid, School)][, lapply(.SD, range)]
dat[, id := sprintf("%03i-%04i", School, photoid)]

# Unique id for alters
netnames <- names(dat)[grepl("sch_friend", names(dat))]

for (f in netnames) {
  isna     <- is.na(dat[[f]])
  dat[[f]] <- sprintf("%03i-%04i", dat$School, dat[[f]])
  dat[[f]][isna] <- NA_character_
}

# Perceived use
net_perceived <- names(dat)[grepl("t[1-4]_a7_f[1-7]", names(dat))]

dat[, nusing_1 := 0L]
dat[, nusing_2 := 0L]
dat[, nusing_3 := 0L]
dat[, nusing_4 := 0L]

for (f in net_perceived) {

  # Grabbing year and friend
  y_i <- as.integer(gsub("t([1-4])_a7_f[1-7]", "\\1", f))
  f_i <- as.integer(gsub("t[1-4]_a7_f([1-7])", "\\1", f))

  # Varname
  nusing_in_year <- sprintf("nusing_%i", y_i)

  dat[, c(nusing_in_year) := .SD[[1]] + fifelse(
    !is.na(.SD[[2]]) & (.SD[[2]] == 1L),
    1L,
    0
  ), .SDcols = c(
    nusing_in_year,
    f
    )]
}

# Tabling
dat[, "nusing_4"] |> table()

# Rooms in household (missing in wave 4)
dat[, t4_i10 := NA_integer_]

# Reshaping the data to long ---------------------------------------------------
dat_long <- melt(
  dat, 
  id.vars = c("id", "hispanic"),
  measure.vars = list(
    female     = c("t1_i2", "t2_i2", "t3_i2", "t4_ni3"),
    grades     = c("t1_i11", "t2_i11", "t3_i11", "t4_ni11"),
    eversmk    = c("t1_j1", "t2_j1", "t3_j1", "t4_nj1"),
    everdrink  = c("t1_j10","t2_j10", "t3_j11", "t4_nj12"),
    # [2022-10-06] Recoded year 3, it is 21 in the survey.
    evermj     = c("t1_j18","t2_j18", "t3_j18", "t4_nj21"), 
    sibsmoke   = c("t1_j7", "t2_j7", "t3_j7", "t4_nj8"),
    sibdrink   = c("t1_j15", "t2_j15", "t3_j15", "t4_nj19"),
    adultdrink = c("t1_j14", "t2_j14", "t3_j14", "t4_nj18"),
    adultsmoke = c("t1_j6", "t2_j6", "t3_j6", "t4_nj7"),
    year_value = c("t1_q6_year", "t2_q6_year", "t3_q6_year", "t4_q6_year"),
    rooms      = c("t1_i10", "t2_i10", "t3_i10", "t4_i10"),
    # Number of friends using
    nusing     = c("nusing_1", "nusing_2", "nusing_3", "nusing_4"),
    present    = paste0("wave", 1:4),
    # Sports
    sport_basket = c("t1_e1_2", "t2_e1_2", "t3_e1_2", "t4_e1_2"),
    sport_soccer = c("t1_e1_10", "t2_e1_10", "t3_e1_10", "t4_e1_10"),
    sport_run    = c("t1_e1_14", "t2_e1_14", "t3_e1_14", "t4_e1_14")
  ),
  variable.name = "year"
)

dat_long[, year := as.integer(year)]

# Fixing year
dat_long[year == 3L, year_value := 2012]
dat_long[year == 2L, year_value := 2011]
dat_long[year == 4L, year_value := 2013]

# We don't need to explicitly remove individuals as we may have
# information about past grades
# dat_long <- dat_long[!is.na(present)]

dat_long[, school := as.integer(gsub("-.+", "", id))]
setorder(dat_long, school, id, year)

# Filling in the blanks --------------------------------------------------------

# Hispanic and Female
dat_long[, table(hispanic, useNA = "always")]
dat_long[, table(female, useNA = "always")]

dat_long[, hispanic := nafill(hispanic, type = "locf"), by = "id"] # Neither makes a change
dat_long[, hispanic := nafill(hispanic, type = "nocb"), by = "id"]

dat_long[, female := nafill(female, type = "locf"), by = "id"] # Makes a huge change
dat_long[, female := nafill(female, type = "nocb"), by = "id"] 
dat_long[!is.na(female), female := female == 1L]

# Rooms
dat_long[, rooms := nafill(rooms, type = "locf"), by = "id"]

# Sports (NAs === 0)
dat_long[, sport_basket := fifelse(sport_basket == 1, 1L, 0L)]
dat_long[, sport_soccer := fifelse(sport_soccer == 1, 1L, 0L)]
dat_long[, sport_run := fifelse(sport_run == 1, 1L, 0L)]

# Adult smoke
dat_long[, adultsmoke := fifelse(
  adultsmoke == 1, 0L, fifelse(
    adultsmoke %in% c(2, 3), 1L, NA_integer_
    ))]

# Grades 
# 1 Mostly A’s
# 2 Mostly A’s and B’s
# 3 Mostly B’s
# 4 Mostly B’s and C’s
# 5 Mostly C’s
# 6 Mostly C’s and D’s
# 7 Mostly D’s
# 8 Mostly D’s and F’s
# 9 Mostly F’s
# I wasn’t in school last year
dat_long[, grades := as.numeric(grades)]
dat_long[, grades := fifelse(grades == 10, NA_real_, grades)]
dat_long[, grades := (11.0 - grades)/2.0] # Now 1:F and 5:A
dat_long[, grades := fcoalesce(grades, mean(grades, na.rm = TRUE)), by = "id"]

# Ever smoke/drink/mj
# If all responses were 0, then 0 can be imputed to the past
dat_long[, table(eversmk, year, useNA = "always")]

dat_long[, table(everdrink, year, useNA = "always")]

dat_long[, table(evermj, year, useNA = "always")]

# Sibling smoke
dat_long[, table(sibsmoke, year, useNA = "always")]
dat_long[, table(sibdrink, year, useNA = "always")]
dat_long[, table(adultdrink, year, useNA = "always")]


dat_long[, sibsmoke := nafill(sibsmoke, type = "locf"), by = "id"]

# Dichotomizing
dat_long[!is.na(eversmk), eversmk := eversmk == 1]
dat_long[!is.na(everdrink), everdrink := everdrink > 1]
dat_long[!is.na(evermj), evermj := evermj == 1]
dat_long[!is.na(sibsmoke), has_sib  := sibsmoke != 5]
dat_long[!is.na(sibsmoke), sibsmoke := sibsmoke < 4]
dat_long[!is.na(sibdrink), sibdrink := fifelse(
  year == 4, sibdrink < 4,
  sibdrink == 3
  )]

dat_long[, has_sib := as.integer(has_sib)]

dat_long[!is.na(adultdrink), adultdrink := adultdrink > 1]

setorder(dat_long, school, id, year)

# Indicator variable to measure if they had a change of response ---------------
# Looking for cases in which they go from yes to no.
dat_long[, c("chng_smk", "chng_drink", "chng_mj") := lapply(.SD, \(e) {
  
  n <- length(e)
  tmp <- e[!is.na(e)]
  if (length(tmp) < 2)
    return(rep(NA, n))
  
  # Going from yes (1) to no (0)?
  if (any(diff(tmp) < 0))
    return(rep(TRUE, n))
  else
    rep(FALSE, n)
  
}), by = "id", .SDcols = c("eversmk", "everdrink", "evermj")]

dat_long <- dat_long[!chng_smk & !chng_drink & !chng_mj]
dat_long[, chng_smk := NULL]
dat_long[, chng_drink := NULL]
dat_long[, chng_mj := NULL]

# Carry forward the ones -------------------------------------------------------
dat_long[, c("eversmk", "everdrink", "evermj") := lapply(.SD, \(e) {
  tmp <- which.max(e)
  # print(e)
  if (!length(tmp))
    e
  else if (e[tmp] == 1) {
    res <- e
    res[tmp:length(res)] <- 1L
    res
  } else 
    e
}), by = "id", .SDcols = c("eversmk", "everdrink", "evermj")]

# Carry Backwards zeros --------------------------------------------------------
dat_long[, c("eversmk", "everdrink", "evermj") := lapply(.SD, \(e) {
  tmp <- which.min(e)
  # print(e)
  if (!length(tmp))
    e
  else if (e[tmp] == 0) {
    res <- e
    res[1:tmp] <- 0L
    res
  } else 
    e
}), by = "id", .SDcols = c("eversmk", "everdrink", "evermj")]

# Generating toa
dat_long[, toa_smoke := which(eversmk == TRUE)[1], by = "id"]
dat_long[, toa_drink := which(everdrink == TRUE)[1], by = "id"]
dat_long[, toa_mj := which(evermj == TRUE)[1], by = "id"]

# Creating edgelist ------------------------------------------------------------
schfriends <- names(dat)[grepl("sch_frie", names(dat))]

edgelist <- NULL
for (i in 1:4) {
  
  # Getting the year
  schfriends_i <- schfriends[grepl(paste0("d", i), schfriends)]
  
  edgelist_i <- subset(dat, select = c("id", schfriends_i))
                       
  edgelist_i[, year := i]
  edgelist_i <- melt(
    edgelist_i,
    id.vars = c("id", "year"),
    measure.vars = schfriends_i
  )
  
  edgelist <- rbind(
    edgelist,
    edgelist_i[complete.cases(edgelist_i)]
  )
    
}

edgelist <- edgelist[, .(year, ego = id, alter = value)]
edgelist[, school := as.integer(gsub("-.+", "", ego))]

# Perceived use ---------------------------------------------------------------

edgelist_perceived <- NULL
for (i in 1:4) {

  # Getting the year
  net_perceived_i <- net_perceived[grepl(paste0("^t", i), net_perceived)]

  edgelist_i_perceived <- subset(dat, select = c("id", net_perceived_i))

  edgelist_i_perceived[, year := i]
  edgelist_i_perceived <- melt(
    edgelist_i_perceived,
    id.vars = c("id", "year"),
    measure.vars = net_perceived_i
  )

}

# Generating exposure variable -------------------------------------------------

# Generating the graph
library(igraph)

# Networks by year
networks <- vector("list", length = 4L)
for (y in 1:4) {
  
  # Removing individuals in edgelist who don't show in the network
  edgelist_y <- edgelist[year == y, .(ego, alter)]
  vertices_y <- dat_long[year == y]
  edgelist_y <- edgelist_y[ego %in% vertices_y$id & alter %in% vertices_y$id]
  
  networks[[y]] <-graph_from_data_frame(
    edgelist_y,
    vertices = vertices_y
  )
  
}

# Identifying schools
schools <- sort(unique(dat_long$school))

# Grouping the networks by year
for (i in seq_along(networks)) {

  networks[[i]] <- lapply(schools, \(s) {
    induced_subgraph(networks[[i]], which(V(networks[[i]])$school == s))
  })

}

# Now, reorganizing by school + year
networks_by_school <- vector("list", length = length(schools))
for (i in seq_along(schools)) {

  # Extracting the ith school in each year.
  # this way the networks_by_school[[i]] will have the
  # four records (years) for each school.
  networks_by_school[[i]] <- lapply(networks, \(n) {
    n[[i]]
  })
}

# Exposures --------------------------------------------------------------------
exposures <- list()

# Smoke
for (s in seq_along(networks_by_school)) {

  # Subsetting the network
  net       <- networks_by_school[[s]]
  school_id <- schools[s]

  # Creating the diffnet object
  exposures[[s]] <- new_diffnet(
    graph            = lapply(net, as_adj),
    vertex.dyn.attrs = lapply(net, get.data.frame, what="vertices"),
    toa              = (
      dat_long[school == school_id, .(id, toa_smoke)]
      |> unique()
      )[, toa_smoke]
  ) 


  # Computing exposures
  exposures[[s]][["exposure_smoke"]]    <- exposure(exposures[[s]])
  exposures[[s]][["exposure_count"]]    <- exposure(exposures[[s]], normalized = FALSE)
  exposures[[s]][["exposure_smoke_se"]] <- exposure(exposures[[s]], alt.graph = "se")
  
  # Same gender
  match_female <- lapply(net, \(x) {
    vertex_covariate_compare(
      as_adj(x),
      X = V(x)$female,
      "=="
    )
  })
  exposures[[s]][["exposure_female"]] <- exposure(
    exposures[[s]], alt.graph = match_female, valued = TRUE
    )
  
  # Two steps away -------------------------------------------------------------

  # The powergraph shows the # of paths of length 2 between i-j
  twostep <- as_spmat(exposures[[s]]^2)
  twostep <- lapply(twostep, \(x) {
    x@x <- rep(1/2, length(x@x))
    x
  })

  # We use the direct and one step away
  twostep <- Map(\(a, b) {
    a@x <- rep(1, length(a@x))
    a + b
  }, a = as_spmat(exposures[[s]]), b = twostep)

  exposures[[s]][["exposure_2steps"]] <- exposure(
    exposures[[s]], 
    alt.graph = twostep,
    valued = TRUE 
  )

  # Indegree alter
  exposures[[s]][["indegree"]] <- dgr(exposures[[s]], cmode = "indegree")
  exposures[[s]][["outdegree"]] <- dgr(exposures[[s]], cmode = "outdegree")
  exposures[[s]][["exposure_indegree"]] <- exposure(
    exposures[[s]], attrs = "indegree", valued = TRUE)

  # Ego-density weighted
  exposures[[s]][["ego_density"]] <- lapply(net, \(x) {
    
    sapply(make_ego_graph(x, mode = "out"), igraph::graph.density)

  })

  exposures[[s]][["ego_density"]] <- lapply(exposures[[s]][["ego_density"]], 
    \(x) {
      fifelse(!is.finite(x), 0, x)
    }
  )
  

  exposures[[s]][["exposure_density"]] <- exposure(
    exposures[[s]], attrs = "ego_density", valued = FALSE
  )

  # Same team
  joint_team <- lapply(net, \(x) {
    b <- vertex_covariate_compare(as_adj(x), X = V(x)$sport_basket,"==")
    s <- vertex_covariate_compare(as_adj(x), X = V(x)$sport_soccer,"==")
    r <- vertex_covariate_compare(as_adj(x), X = V(x)$sport_run,"==")

    ans <- b + s + r
    ans@x <- rep(1, length(ans@x))
    ans
  })

  exposures[[s]][["exposure_team"]] <- exposure(
    exposures[[s]], alt.graph = joint_team, valued = FALSE
  )

  # Simmelian ties
  simmelian_mat <- lapply(as_spmat(exposures[[s]]), \(x) {
    
    as.dgCMatrix(
      sna::simmelian(network::as.network(as.matrix(x)))
    )
  })
  exposures[[s]][["exposure_simmelian"]] <- exposure(
    exposures[[s]], alt.graph = simmelian_mat
  )

  # Turning into a data table object
  exposures[[s]] <- as.data.frame(exposures[[s]]) |>
    as.data.table()

  # Merging with long_dataset
  exposures[[s]] <- merge(
    dat_long[school == school_id,],
    exposures[[s]][,.(
      id, year = as.integer(per),
      # All the computed exposures
      exposure_smoke,
      exposure_count,
      exposure_smoke_se,
      exposure_female,
      exposure_2steps,
      exposure_indegree,
      exposure_density,
      exposure_team,
      exposure_simmelian,
      indegree,
      outdegree,
      ego_density
      )],
    all.x = TRUE, all.y = FALSE
  )

}

# Putting all in a single list
dat_long <- exposures |> rbindlist()

# Exposure based on perceived use
dat_long[, exposure_perceived_count :=  nusing]
dat_long[, exposure_perceived       :=  fifelse(outdegree > 0, nusing/outdegree, 0)]

# Some cases
dat_long[, cumsum(prop.table(table(exposure_perceived)))] |>
  as.data.frame() # ~7 % of the cases go above 100% exposure.
                  # We'll just truncate them.

dat_long[, exposure_perceived := fifelse(
  exposure_perceived > 1, 1, exposure_perceived
  )]

# Lagging exposures
dat_long[, exposure_smoke := shift(
  exposure_smoke, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_count := shift(
  exposure_count, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_smoke_se := shift(
  exposure_smoke_se, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_perceived := shift(
  exposure_perceived, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_female := shift(
  exposure_female, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_2steps := shift(
  exposure_2steps, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_indegree := shift(
  exposure_indegree, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_team := shift(
  exposure_team, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_density := shift(
  exposure_density, n = 1, type = "lag", fill = NA_real_
  ), by = id]

dat_long[, exposure_simmelian := shift(
  exposure_simmelian, n = 1, type = "lag", fill = NA_real_
  ), by = id]

# Final cleaning ---------------------------------------------------------------

# Renaming
new_names <- c(
  everdrink = "alcohol",
  eversmk   = "tobacco",
  evermj    = "mj",
  hispanic  = "Hispanic",
  grades    = "Grades",
  female    = "Female"
)
setnames(
  dat_long,
  names(new_names),
  new_names
  )

fwrite(dat_long, "data/sns_model_data.csv")
saveRDS(networks, "data/sns_igraph.rds")

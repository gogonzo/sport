#' @examples 
#' teams <- letters[1:8]
#' nr <- 1:8
#' teams <- c(rep("A", 4), rep("B", 4))



# from round to round people analyses results and simulates themselves
# then passes just ranking to the next stage to draw a schedulle 
# and go next
# and go next

# one day they will add more and more rules to the sport

# WORLD CUP
# add nba schedulle for 2019
# add ncaa bracket
nba <- c("1:custom(2,15)",
         "quarter-final:ko(8,7) <- 1:{[1.1,1.8],[1.2,1.7],[1.3,1.6],[1.4,1.5],[2.1,2.8],[2.2,2.7],[2.3,2.6],[2.4,2.5]}",
         "semi-final:ko(4,7) <- quarter-final:{[1.1,2.1],[3.1,4.1],[5.1,6.1],[7.1,8.1]}",
         "conference-final:ko(2,7) <- semi-final:{[1.1,2.1],[3.1,4.1]}",
         "final:ko(1,7) <- conference-final:{[1.1,2.1]}")

wc <- c("1:rr(8,4)",
        "2:ko(8,1) <- 1:{[1.1,2.2],[2.1,1.2],[3.1,4.2],[4.1,3.2],[5.1,6.2],[6.1,5.2],[7.1,8.2],[8.1,7.2]}",
        "3:ko(4,1) <- 2:{[1.1,2.1],[3.1,4.1],[5.1,6.1],[7.1,8.1]}",
        "4:ko(2,1) <- 3:{[1.1,2.1],[3.1,4.1]}",
        "5:ko(1) <- 4:{[1.2,2.2]}",
        "6:ko(1) <- 4:{[1.1,2.1]}")

pge <- c("1:rr(1,8)",
         "2:ko(2,2) <- 1:{[1,4],[2,3]}",
         "3:ko(2) <- 2:{[1.2,2.2]}",
         "4:ko(1) <- 3:{[1.1,2.1]}")

pge_match <- c("1:custom(2,8)",
               "2:pairwise(2,2) <- 1:{[1.3,2.3,1.4,2.4]}",
               "3:pairwise(2,2) <- 1:{[1.1,2.1,1.2,2.2]}")

competition <- R6::R6Class(
  classname = "competition",
  public = list(
    round_schedules = NULL,
    initialize = function(rounds) {
      private$round_names <- get_round_names(rounds)
      private$round_types <- get_round_types(rounds)
      private$round_settings <- get_round_settings(rounds) 
      private$round_parents <- get_round_parents(rounds)
      private$advanced_players <- get_advanced_players(rounds)
      #private$init_check_groups_advanced()
      #private$init_check_players_advanced()
      #private$init_check_groups_previous()
      #private$init_check_players_previous()
      
      self$round_schedules <- private$create_round_schedules()
      
      invisible(self)
    }
  ),
  private = list(
    round_names = NULL,
    current_round = 1,
    round_types = NULL,
    round_settings = NULL,
    round_parents = NULL,
    
    advanced_players = NULL,
    init_check_round_names = function() {
      stopifnot(
        duplicated(private$round_names),
        all(unique(private$round_parents) %in% private$round_names)
      )
      
    },
    init_check_groups_advanced = function() {
      # same groups numbers child <- parent
      vapply(seq_along(private$round_names), function(i) {
        c(length(private$advanced_players[[i]]),
          private$round_settings[[i]]["groups"])
      }, FUN.VALUE = double(2))
    },
    init_check_players_advanced = function() {
      # same player numbers child <- parent
      vapply(seq_along(private$round_names), function(i) {
        c(length(unlist(private$advanced_players[[i]])),
          private$round_settings[[i]]["groups"] * private$round_settings[[i]]["players"])
      }, FUN.VALUE = double(2))
      
      # message if any - which
    },
    init_check_groups_previous = function() {
      # groups advanced 
      lapply(1:length(private$round_names), function(i) {
      if (is.null(private$round_parents[[i]])) {
        return(NULL)
      }
      j <- which(private$round_names == private$round_parents[[i]])
      
      setdiff(unique(unlist(lapply(private$advanced_players[[i]], attr, "group"))),
              seq_len(private$round_settings[[j]]["groups"]))
      })
    },
    init_check_players_previous = function() {
      # players advanced number < previous number
      lapply(1:length(private$round_names), function(i) {
        if (is.null(private$round_parents[[i]])) {
          return(NULL)
        }
        j <- which(private$round_names == private$round_parents[[i]])
        
        setdiff(unique(unlist(private$advanced_players[[i]])),
          seq_len(private$round_settings[[j]]["groups"] * private$round_settings[[j]]["players"])
        )
      })
    },
    create_round_schedules = function() {
      rounds <- lapply(seq_along(private$round_names), function(i) {
        browser()
        if (private$round_types[[i]] == "rr") {
          round_robin(private$round_settings[[i]]["players"],
                      private$round_settings[[i]]["groups"],
                      private$round_settings[[i]]["rounds"],
                      private$round_settings[[i]]["home"])
        } else if (private$round_types[[i]] == "ko") {
          ko(private$round_settings[[i]]["groups"],
             private$round_settings[[i]]["rounds"],
             private$round_settings[[i]]["home"])
        }
      })
      names(rounds) <- private$round_names
      rounds
    },
    insert_round_result = function(classification) {
      stopifnot(any(duplicated(classification)),
                length(classification) == (private$groups * private$players))
    }
  )
)

get_round_names = function(rounds) {
  rounds <- lapply(rounds, function(x) gsub("^([^:]+):.*$", "\\1", x))
  if (any(is.null(rounds) || rounds == "")) {
    stop("Round name/id can't be missing - make sure that name:type() is specified")
  }
  rounds
}
get_round_parents = function(rounds) {
  lapply(rounds, function(x) {
    if (grepl("<-", x)) {
      gsub("^.*<-\\s*([^:]+)\\s*:.*$", "\\1", x)          
    }
  })
}
get_round_types <- function(rounds) {
  types <- lapply(rounds, function(x) gsub("^[^:]+:(.*)\\(.*$", "\\1", x))
  if (any(is.null(types) || types == "")) {
    stop("Round type can't be missing - make sure that name:type() is specified")
  }
  
  types
}
get_round_settings <- function(rounds) {
  settings <- lapply(rounds, function(x) {
    type <- gsub("^[^:]+:(.*)\\(.*$", "\\1", x)
    s <- as.integer(unlist(strsplit(gsub("^.+\\((.*)\\).*$", "\\1", x), ",")))
    if (type == "ko" & length(s) == 1) {
      s <- c(s[1], 2, 1, 1)
    } else if (type == "ko" & length(s) == 2) {
      s <- c(s[1], 2, s[2], 1)
    } else if (type == "ko" & length(s) == 3) {
      s <- c(s[1], 2, s[2], s[3])
    } else if (type == "rr" & length(s) == 2) {
      s <- c(s, 1, 1)
    } else if (type == "rr" & length(s) == 3) {
      s <- c(s, 1)
    } else if (!type %in% c("rr", "ko")) {
      s <- rep(1, 4)
    }
    names(s) <- c("groups", "players", "rounds", "home")
    s
  }) 

  settings
}

get_advanced_players <- function(rounds) {
  lapply(rounds, function(x) {
    if (!grepl("<-",x)) return(NULL)
    array <- gsub("^.*:", "", x)
    sets2 <- unlist(stringi::stri_extract_all(array, regex = "\\[[0-9.,]+\\]"))
    sets1 <- lapply(sets2, function(set) unlist(strsplit(gsub("\\[|\\]", "", set), ",")))
    
    lapply(sets1, function(set) {
      obj <- gsub("^.*\\.", "", set)
      grp <- gsub("\\.*[^.]+$", "", set)
      data.frame(player = obj, group = grp, stringsAsFactors = FALSE)
    })
  })
}
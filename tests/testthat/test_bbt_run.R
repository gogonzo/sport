context("bbt_run")
library(dplyr)
data <- data.frame( 
  rank  = as.integer(c(3, 4, 1, 2)),
  team = c("A", "B", "C", "D"), 
  field = 1:4,
  date = seq(Sys.Date() - 3, Sys.Date(), by="1 day"),
  sigma = rep(1.0, 4), 
  beta = 100.0,
  weight = rep(1.05, 4),
  weight2 = 1.0,
  stringsAsFactors = FALSE)

team_df <- data.frame(team = c("A", "B", "C", "D"), player = sample(letters, 4, replace = FALSE))
team_df <- data.frame(team = c("A", "A", "B", "B" , "C", "C", "D", "D"), 
                      player = sample(letters, 8, replace = FALSE),
                      stringsAsFactors = FALSE)

data <- left_join(data, team_df)
data <- lapply(1:3, function(x) data)
data <- dplyr::bind_rows(data, .id = "id")
data$id <- as.integer(data$id)

sigma  <- setNames(rep(1, 26), letters)
r  <- setNames(as.numeric(rep(25, 26)), letters)
rd <- setNames(rep(25/3, 26), letters)
# 
test_that("bbt", {
  sport:::bbt(id = as.integer(data$id), rank = data$rank, 
                team = data$team , player = data$player, 
                r_val = r, rd_val = rd, lambda = data$sigma, weight = data$weight)
})

test_that("Error with NA parameters",{
  gpheats$weight[17] <- NaN
  expect_error(
    bbt_run(
      rank | id ~ rider,
      data = gpheats[17:21, ],
      weight = "weight" 
    ),
    paste0("Parameters error after evaluating id=", gpheats$id[17])
  )
})

test_that("valid bbt computation",{
  expect_equal(
    c(22.52807, 14.06973, 19.57436, 30.46640),
    round(
      bbt_run(
        rank | id ~ name, 
        data = data, 
        r  = c(25, 20, 15, 30) , 
        rd = c(6, 7, 5, 20)
      )$final_r,
      5
    )
  )
})

test_that("init r passed",{
  expect_true( all(
    sum(bbt_run(formula = rank | id ~ name, data=data, init_r = 1000)$final_r)==4000
  ))
})

test_that("init rd passed",{
  expect_true(all(bbt_run(formula = rank | id ~ name, data=data, init_rd = 100)$final_rd<100))
})

test_that("bigger rating change for higher deviation",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank | id ~name, data=data  ,gamma=1.001, rd = setNames( rep(26/6,4), c("A","B","C","D")))$final_r ) >
    abs( 25 - bbt_run(formula = rank | id ~name, data=data, gamma=1.001,   rd = setNames( rep(25/6,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("bigger rating change for higher sigma",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank | id ~ name, data=data, rd = rep(3,4), sigma="sigma")$final_r ) >
    abs( 25 - bbt_run(formula = rank | id ~ name, data=data, rd = rep(3,4)           )$final_r )
  ))
})

test_that("R and RD exacltly proportional to weight",{
  model1 <- bbt_run(formula = rank | id ~ name, data=data, weight="weight")
  model2 <- bbt_run(formula = rank | id ~ name, data=data)
  
  expect_true(all( 
    round(abs( 25 - model1$final_r ) / (abs( 25 - model2$final_r )),2)==1.05
  ))
  
  expect_true( all(
    round(abs( 25/3 - model1$final_rd ) / (abs( 25/3 - model2$final_rd )),2)==1.05
  ))
  
})

test_that("kappa is working",{
  expect_true(all( 
    bbt_run(rank|id~name, data=data, kappa=.99)$final_rd == 25/3*.99
  ))
})

test_that("smaller rating change for higher beta",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank | id ~ name, beta = 25/3, data=data)$final_r ) <
      abs( 25 - bbt_run(formula = rank | id ~ name, beta = 25/4,data=data)$final_r )
  ))
})

test_that("bigger rd change for higher gamma",{
  expect_true( all(
    bbt_run(formula = rank | id ~ name,   gamma = 1.1, data=data)$final_rd  <
    bbt_run(formula = rank | id ~ name, gamma = 1,data=data)$final_rd 
  ))
})

test_that("identifier passed succesfuly",{
  expect_equal(
    as.character(data$date),
    attr(bbt_run(formula = rank | id ~name, data=data, idlab = "date")$r,"identifier")
  )
})

test_that("valid bbt output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( bbt_run( rank | id ~ name, data = data) )
  )
})

test_that("valid bbt attr names",{
  expect_equal(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "rating", 
         method = "bbt",
         formula = rank | id ~name,
         settings=list(sigma="sigma",weight="weight",beta=25/6,kappa=.5,gamma=999,idlab="id",init_r=1000,init_rd=200)),
    attributes( bbt_run( rank | id ~ name, data = data, weight="weight", sigma = "sigma", init_r=1000, init_rd=200) )
  )
})

test_that("r object has date labels attribute",{
  expect_identical(
    list(names = c("id","name","r","rd","p_win"),class=c("data.table","data.frame"),row.names=1:4,identifier=as.character(c(1,1,1,1))),
    attributes(bbt_run( rank | id ~ name, data = data, weight="weight", sigma = "sigma", init_r=1000, init_rd=200)$r)[-4]
  )
})
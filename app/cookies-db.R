library(RMariaDB)
library(DBI)

COOKIE_EXPIRY_DAYS <- 14

to_base64 <- function(x) {
  base64enc::base64encode(charToRaw(x))
}
from_base64 <- function(x) {
  rawToChar(base64enc::base64decode(x))
}

db <- dbConnect(RMariaDB::MariaDB(), group = "shinycromwell")
if (!dbExistsTable(db, "users")) {
  db_columns <- c(
    user = "TEXT",
    proof_token = "TEXT",
    cromwell_url = "TEXT",
    login_time = "TEXT"
  )
  dbCreateTable(db, "users", db_columns)
}
dbDisconnect(db)

make_db_con <- function() {
  dbConnect(RMariaDB::MariaDB(), group = "shinycromwell")
}

user_from_db <- function(user, conn = make_db_con(), expiry = COOKIE_EXPIRY_DAYS) {
  on.exit(dbDisconnect(conn))
  dbReadTable(conn, "users") %>%
    as_tibble() %>%
    filter(
      # !! needed to get the value of the variable
      user == !!user,
      login_time > now() - days(expiry)
    ) %>%
    arrange(desc(login_time))
}
user_to_db <- function(user, token, url, drop_existing = FALSE, conn = make_db_con()) {
  on.exit(dbDisconnect(conn))
  # drop before inserting so we only have 1 row/user
  if (drop_existing) {
    # don't drop if user is empty
    if (nzchar(user)) {
      # don't disconnect from within drop fun
      user_drop_from_db(user, disconnect = FALSE, conn = conn)
    }
  }
  tibble(
    user = user,
    proof_token = token,
    cromwell_url = url,
    login_time = as.character(now())
  ) %>%
    dbWriteTable(conn, "users", ., append = TRUE)
}
user_drop_from_db <- function(user, disconnect = TRUE, conn = make_db_con()) {
  if (disconnect) {
    on.exit(dbDisconnect(conn))
  }
  sql_delete <- glue::glue_sql("
    DELETE from users
    WHERE user = {user}
  ", .con = conn)
  dbExecute(conn, sql_delete)
}

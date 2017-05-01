require(DBI)

rc_master[2347, "stage_url"]
rc_master[2347, "race_id"]


write_race_results_tables(rc_master[2349, "stage_url"], rc_master[2349, "stage_id"])


race_2016_0153_t01


race_2016_0153_t01 <- dbGetQuery(conn_local, "SELECT *
                                FROM race_2016_0153_t01;")
View(race_2016_0153_t01)

dbGetQuery()


type = 'UTF-8', 
nrows = -1

?dbClearResult
?dbListResults

delete_tables <- dbSendQuery(conn_local, "DROP TABLE race_2016_0108_t06;")

test_df <- dbGetQuery(conn_local, "SELECT * FROM race_2016_0153_s03_t01;")
View(test_df)



# Setup database connection
dbpsswddir <- set_db_password()
setwd(dbpsswddir)
psswd <- read.csv("passwords_db.csv")
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.5', port=3306, timeout=3600) 

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)




master_time <- dbGetQuery(conn_local, "SELECT * 
                          FROM test_test_master_results_time 
                          WHERE (stage_date BETWEEN '2016-06-01' AND 2016-12-31');")

master_time <- dbGetQuery(conn_local, "SELECT * 
                          FROM test_test_master_results_time 
                          WHERE stage_date = '2009-06-01';")

sub_time <- dbSendQuery(conn_local, "SELECT YEAR(stage_date)
                       FROM test_test_master_results_time
                       LIMIT 10000;")
View(sub_time)

?dbConnect

, Country, YEAR(DISTINCT stage_date)


pro_cycling_table <- dbGetQuery(conn_local, "SHOW TABLES;")

View(master_time)
View(pro_cycling_table)

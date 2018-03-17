#restart:
sqlQuery(ch, "drop table elechinc;")
sqlQuery(ch, "drop table elec;")
sqlQuery(ch, "drop table hinc;")

sqlQuery(ch, "drop table elechinc2;")
sqlQuery(ch, "drop table hinc2;")
sqlQuery(ch, "drop table elec2;")


#connect to training2 DB
ch <- odbcConnect("training2",uid="gpadmin",
                  case="postgresql",pwd="changeme")

#a.   Cluster the data and plot all 52 data points, along with the centroids. Mark all data points and centroids belonging to a given cluster with their own color. Here, let k=10.

#create tables in SQL
sqlQuery(ch,
         "CREATE TABLE hinc AS
SELECT 
         f.name AS state,  
         round(avg(h.hinc),0) AS income  
         FROM
         housing AS h 
         JOIN
         fips AS f
         ON
         h.state = f.code
         WHERE 
         (h.hinc > 0 )
         GROUP
         by f.name 
         DISTRIBUTED BY (income);"
)

sqlQuery(ch,
         "CREATE TABLE elec AS
SELECT 
         f.name AS state,  
         round(avg(h.elec),0) AS elec  
         FROM
         housing AS h 
         JOIN
         fips AS f
         ON
         h.state = f.code
         WHERE 
         (h.elec > 0 )
         GROUP
         by f.name 
         DISTRIBUTED BY (elec);"
         )

#join both tables for clustering:
sqlQuery(ch,
         "CREATE TABLE elechinc AS (
SELECT * FROM HINC NATURAL JOIN ELEC );")


#assign table as R matrix objects
elechinc <- as.matrix(sqlFetch(ch,"elechinc", rownames="state"))
summary(elechinc)

#kmeans for electricity/household income table, # of centers = 10
kmeanselechinc <- kmeans (elechinc,10)

#w/ clusters
plot(elechinc, col = kmeanselechinc$cluster)
points(kmeanselechinc$centers, col = 1:10, pch = 8)



#Finding the optimal number of clusters (plotting clusters c = 1:15, and looking for elbow):
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(elechinc, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#w/ optimal clusters is 6
kmeanselechinc <- kmeans (elechinc,5)
plot(elechinc, col = kmeanselechinc$cluster)
points(kmeanselechinc$centers, col = 1:5, pch = 8)


#using log10 scale
log_elechinc <- log10(elechinc)

#optimal K for log_elechinc:
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(log_elechinc, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#w/ optimal clusters is 5 for log_elechinc
kmeanslog_elechinc <- kmeans (log_elechinc,5)
plot(log_elechinc, col = kmeanslog_elechinc$cluster)
points(kmeanslog_elechinc$centers, col = 1:5, pch = 8)


#REMOVING THE OUTLIER:

sqlQuery(ch,
         "CREATE table noOutlier from elechinc c
            WHERE c.elec > 700 AND c.income;")

noO <- as.matrix(sqlFetch(ch,"noOutlier", rownames="state"))
kmeansnoO <- kmeans (noO,10)
plot(noO, col = kmeanselenoO$cluster)
points(kmeansnoO2$centers, col = 1:10, pch = 8)

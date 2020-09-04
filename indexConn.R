indexConn<-function(){
  user <- "user"
  password <- "DictMammalia"
  cluster<-"dictmammalia-01-fceex.mongodb.net/test"
  db <- "DictMammalia-01"
  collection <- "index"
  
  conn<-collectionConnection(user, password, cluster, db, collection)
  return(conn)
}
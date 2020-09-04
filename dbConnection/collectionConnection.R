# Function for connect to a collection in a specific database.
# Inputs:
#   user = user name of the Atlas cluster.
#   password = password of the user.
#   cluster = Atlas cluster url.
#   bd = database name.
#   collection = collection name.
#
# Output:
#   Connection environment.

collectionConnection<-function(user, password, cluster, db, collection){
  #generate the ultimate url.
  urlPath<-sprintf("mongodb+srv://%s:%s@%s",
                   user,
                   password,
                   cluster)
  #connect to the collection. 
  collConn<-mongo(db=db,
                  collection=collection,
                  url=urlPath,
                  verbose=TRUE)
  #return the connection environment.
  return(collConn)
}
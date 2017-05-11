library(buffer); library(dplyr)

clean_data <- function(df) {
  
  print("Cleaning data...")
  
  # Rename columns
  colnames(df) <- c('date', 'plan','mrr')
  
  # Set dates as date objects
  df$date <- as.Date(df$date)
  
  # Fix up plan names
  df$plan[df$plan == ""] <- 'other'
  df$plan <- as.character(df$plan)
  df$plan <- as.factor(df$plan)
  
  # Set mrr as type numeric
  df$mrr <- as.numeric(as.character(df$mrr))
  
  df
}

get_mrr_data <- function(){
  
  print("Getting data...")
  
  # Get current date
  day <- as.character(Sys.Date())
  
  # Name file to save data in
  filename <- paste0('./data/','mrr-',day,'.csv')
  
  # Read csv if it exists. Otherwise get data
  if(file.exists(filename)) {
    
    df <- read.csv(filename,header=T)
    df
    
  } else {
    
    # Get data from Looker
    df <- get_look(3836)
    
    }
  
    # Clean the data
    df <- clean_data(df)
    
    # Write csv file in data directory
    write.csv(df, file=filename, row.names =F)
    
    # Return the data frame
    df
  
}


# Define helper functions
createEmptyTable <- function(con,tn,df) {
  sql <- paste0("create table \"",tn,"\" (",paste0(collapse=',','"',names(df),'" ',sapply(df[0,],postgresqlDataType)),");");
  dbSendQuery(con,sql);
  invisible();
};

insertBatch <- function(con,tn,df,size=100L) {
  cnt <- (nrow(df)-1L)%/%size+1L
  
  for (i in seq(0L,len=cnt)) {
    sql <- paste0("insert into \"",tn,"\" values (",do.call(paste,c(sep=',',collapse='),(',lapply(df[seq(i*size+1L,min(nrow(df),(i+1L)*size)),],shQuote))),");");
    dbSendQuery(con,sql);
  }
  
}

write_to_redshift <- function(df) {
  
  print("Writing to Redshift...")
  
  # Connect to Redshift
  con <- redshift_connect()
  
  # Delete existing table
  print("Dropping old table...")
  delete_query <- "drop table mrr_predictions"
  query_db(delete_query, con)
  
  # Insert new forecast table
  print("Creating empty table...")
  createEmptyTable(con, 'mrr_predictions', df)
  
  print("Inserting data...")
  insertBatch(con, 'mrr_predictions', df)
  
}


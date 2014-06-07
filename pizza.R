runTestData <- function()
{
        pizza_test <- loadTestData()
        pizza_test$requester_received_pizza <- NA
        listID <- pizza_test$request_id
        pizza_test$request_id <- NULL
        
        pizza_train <- loadData()
        pizza_train$request_id <- NULL
        
        pizza <- rbind(pizza_train, pizza_test)
        pizza$requester_received_pizza <- as.factor(pizza$requester_received_pizza)
        
        trainedModel <- decisionTree(pizza)
        pizza_pred <- predict(trainedModel, pizza_test, type="class", trials=10, na.action=na.exclude)
        
        result <- NULL
        result$request_id <- listID
        result$requester_received_pizza <- ifelse(pizza_pred==TRUE,1,0)
        
        write.csv(result, '/home/wijnand/R_workspace_pizza/resources/result_test.csv', row.names=FALSE, quote=FALSE)
}


runTrainData <- function(percentageTrain=0.7)
{
        pizza <- loadData()
        pizza$requester_received_pizza <- factor(pizza$requester_received_pizza)
        pizza$request_id <- NULL
        
        # order randomly
        set.seed(12345)
        pizza <- pizza[order(runif(nrow(pizza)))]
        
        # row number of x% of the data for training set
        cutofPoint <- round(nrow(pizza) * percentageTrain, digits=0)
        pizza_train <- pizza[1:cutofPoint,]
        pizza_test <- pizza[cutofPoint:nrow(pizza),]
        
        print(str(pizza_train))
        
        # train a model
        trainedModel <- decisionTree(pizza_train)
        print(summary(trainedModel))
        
        # predict
        pizza_pred <- predict(trainedModel, pizza_test, type="class", trials=10)
        
        # show results
        printPredictionResults(pizza_pred, pizza_test)
}

decisionTree <- function(trainData)
{
        require(C50)
        trainData <- C5.0(trainData[,!"requester_received_pizza",with=FALSE], trainData$requester_received_pizza, trials=10)

}

printPredictionResults <- function(pizza_pred, pizza_test)
{
        print(paste0("number of rows in testset: ", nrow(pizza_test)))
        print(paste0("number of predictions: ", length(pizza_pred)))
        
        require(gmodels)
        table <- CrossTable(pizza_pred, pizza_test$requester_received_pizza, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
                            prop.t = TRUE, dnn=(c('actual result', 'predicted result')))
        precision <- (table$prop.tbl[1,1] + table$prop.tbl[2,2]) * 100
        print(paste0("Precision as percentage: ", round(precision, digits=3)))
}

loadTestData <- function(location='/home/wijnand/R_workspace_pizza/resources/test.json')
{
        require(jsonlite)
        jsonData <- fromJSON(location)
        require(data.table)
        pizzaTable <- as.data.table(jsonData)
        pizzaTable <- pizzaTable[,!c("giver_username_if_known", "request_text_edit_aware", "request_title", 
                                     "requester_subreddits_at_request", "requester_username"),with=FALSE]
}

loadData <- function(location='/home/wijnand/R_workspace_pizza/resources/train.json')
{
        require(jsonlite)
        jsonData <- fromJSON(location)
        require(data.table)
        pizzaTable <- as.data.table(jsonData)
        # select useable columns (present in test-set)
        pizzaTable <- pizzaTable[,c("giver_username_if_known",
                                    "request_id",
                                    "request_text_edit_aware",
                                    "request_title",
                                    "requester_account_age_in_days_at_request", 
                                    "requester_days_since_first_post_on_raop_at_request", 
                                    "requester_number_of_comments_at_request", 
                                    "requester_number_of_comments_in_raop_at_request", 
                                    "requester_number_of_posts_at_request", 
                                    "requester_number_of_posts_on_raop_at_request", 
                                    "requester_number_of_subreddits_at_request", 
                                    "requester_upvotes_minus_downvotes_at_request", 
                                    "requester_upvotes_plus_downvotes_at_request", 
                                    "unix_timestamp_of_request", 
                                    "unix_timestamp_of_request_utc",
                                    "requester_received_pizza")
                                    ,with=FALSE]
        
        pizzaTable <- pizzaTable[,!c("giver_username_if_known", "request_text_edit_aware", "request_title"),with=FALSE]
}

crosstables <- function(data, var="requester_received_pizza")
{
        require(gmodels)
        CrossTable(pizza[[var]])
}
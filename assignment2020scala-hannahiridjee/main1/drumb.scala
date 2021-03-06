// Core Part about a really dumb investment strategy
//===================================================

object CW6b {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 


// (1) The function below takes a stock symbol and a year as arguments.
//     It should read the corresponding CSV-file and then extract the January 
//     data from the given year. The data should be collected in a list of
//     strings (one entry for each line in the CSV-file).

import io.Source
import scala.util._

def get_january_data(symbol: String, year: Int) : List[String] = {
    val csvFile = Source.fromFile(symbol + ".csv").getLines().toList
    val janData = csvFile.filter(csvFile => csvFile.startsWith(year.toString))
    janData
}


// (2) From the output of the get_january_data function, the next function 
//     should extract the first line (if it exists) and the corresponding
//     first trading price in that year with type Option[Double]. If no line 
//     is generated by get_january_data then the result is None; and Some if 
//     there is a price.


def get_first_price(symbol: String, year: Int) : Option[Double] = {
    
    val jan = get_january_data(symbol,year)
    if(jan.isEmpty) None
    else{
        val firstLine = jan.head.split(",").toList
        val price = firstLine(1).toDouble 
        Some(price)
    }
}



// (3) Complete the function below that obtains all first prices
//     for the stock symbols from a portfolio (list of strings) and 
//     for the given range of years. The inner lists are for the
//     stock symbols and the outer list for the years.


def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = {
    val lstOut = for(year <- years) yield {
        val lstIn = for(stock<-portfolio) yield {
            val price = get_first_price(stock,year)
            price
        }
        lstIn
    }
    lstOut.toList
}



// (4) The function below calculates the change factor (delta) between
//     a price in year n and a price in year n + 1. 

def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = {
    if(price_old.isDefined && price_new.isDefined) Option((price_new.get - price_old.get)/price_old.get)
    else None
}



// (5) The next function calculates all change factors for all prices (from a 
//     portfolio). The input to this function are the nested lists created by 
//     get_prices above.

def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = {
    val lstIndex2 = (0 until data.head.size).toList
    val lstIndex = (0 until data.size - 1).toList

    val lstOut = for(x <- lstIndex) yield {
        val lstIn = for (y<-lstIndex2) yield {
            val del = get_delta(data(x)(y),data(x+1)(y))
            del
        }
        lstIn
    }
    lstOut.toList
}




// (6) Write a function that given change factors, a starting balance and an index,
//     calculates the yearly yield, i.e. new balance, according to our dumb investment 
//     strategy. Index points to a year in the data list.

def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    val compYear = data(index)
    val b = balance/(data.head.size)
    if(compYear.isEmpty) balance
    else{
        val newBalance = for(company <- (0 to compYear.size - 1)) yield b*compYear(company).get
        newBalance.sum.toLong+balance
    }


}


// (7) Write a function compound_yield that calculates the overall balance for a 
//     range of years where in each year the yearly profit is compounded to the new 
//     balances and then re-invested into our portfolio. For this use the function and 
//     results generated under (6). The function investment calls compound_yield
//     with the appropriate deltas and the first index.

def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    (0 until index).foldLeft(balance){ 
        case (newBalance, i) => yearly_yield(data, newBalance, i)
    }
 
}



def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = {
    //java.util.NoSuchElementException: None.get
    val changeF = get_deltas(get_prices(portfolio,years))
    val yearDiff = years.end - years.start
    compound_yield(changeF,start_balance,yearDiff)
}




//Test cases for the two portfolios given above

//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}
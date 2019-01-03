// Part 2 about Alcohol-Consumption Worldwide
//============================================
object CW6b {

import io.Source
import scala.util._

val url_alcohol = 
  "https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv"

val file_population = 
  "population.csv"


//(1) Complete the get_csv_page function below. It takes a URL-string
//    as argument and generates a list of strings corresponding to each
//    line in the downloaded csv-list. The URL url_alcohol above is one 
//    possible argument.

def get_csv_page(url: String) : List[String] = {
    val webPage = io.Source.fromURL(url)
    val list = (for (line <- webPage.getLines()) yield line).toList
    webPage.close
    list
}


//    Complete the get_csv_file function below. It takes a file name 
//    as argument and reads the content of the given file. Like above,
//    it should generate a list of strings corresponding to each
//    line in the csv-list. The filename file_population is one possible
//    argument.

def get_csv_file(file: String) : List[String] = {
    val fileData = io.Source.fromFile(file)
    val list = (for (line <- fileData.getLines()) yield line).toList
    fileData.close
    list
}



//(2) Complete the functions that process the csv-lists. For
//    process_alcs extract the country name (as String) and the 
//    pure alcohol consumption (as Double). For process_pops
//    generate a Map of Strings (country names) to Long numbers 
//    (population sizes). 

def process_alcs(lines: List[String]) : List[(String, Double)] = {
    for (i <- List.range(0, lines.size)) yield (lines(i).split(",")(0), lines(i).split(",")(4).toDouble)
}

def process_pops(lines: List[String]) : Map[String, Long] = {
   makeListOfPairs(lines).toMap
}

def makeListOfPairs(lines: List[String]) : List[(String, Long)] = {
  for (i <- List.range(0, lines.size)) yield (lines(i).split(",")(0), lines(i).split(",")(1).toLong)
}



//(3) Calculate for each country the overall alcohol_consumption using
//    the data from the alcohol list and the population sizes list. You
//    should only include countries on the alcohol list that are also
//    on the population sizes list with the exact same name. Note that
//    the spelling of some names in the alcohol list differs from the
//    population sizes list. You can ignore entries where the names differ. 
//    Sort the resulting list according to the country with the highest alcohol 
//    consumption to the country with the lowest alcohol consumption.

def sorted_country_consumption() : List[(String, Long)] = {
    val alcs_raw = get_csv_page("https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv")
    val pops_raw = get_csv_file("population.csv")
    val alcs = process_alcs(alcs_raw.drop(1))
    val pops = process_pops(pops_raw.drop(1))
    val unsortedList = total_consumption_list(alcs, pops)
    val sortedList = unsortedList.sortBy(_._2)
    val finalList = sortedList.reverse
    finalList
}

def total_consumption_list(alcs: List[(String,Double)], pops:Map[String,Long]): List[(String,Long)] = {
    val alcMap = alcs.toMap
    val popList = pops.toList
    for (i <- List.range(0, pops.size) if alcMap.isDefinedAt(popList(i)._1)) yield (popList(i)._1, (popList(i)._2 * alcMap(popList(i)._1)).toLong)
}

/*
def main(args: Array[String]){
   
    Console.println(percentage(164))
  }
*/

//   Calculate the world consumption of pure alcohol of all countries, which 
//   should be the first element in the tuple below. The second element is
//   the overall consumption of the first n countries in the sorted list
//   from above; and finally the double should be the percentage of the 
//   first n countries drinking from the the world consumption of alcohol.          

def percentage(n: Int) : (Long, Long, Double) = {
  val worldList = sorted_country_consumption()
  val (names, numbers) = worldList.unzip
  val worldSum = numbers.sum
  val indexSum = iterateList(worldList, n).sum
  val percentage = (indexSum.toDouble / worldSum.toDouble) * 100
  (worldSum, indexSum, percentage)
}

def iterateList(list: List[(String, Long)], index: Int): List[Long] = {
   for (i <- List.range(0, index)) yield list(i)._2
}
}

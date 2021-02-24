//Data for condition costs derived from: https://www.ncbi.nlm.nih.gov/books/NBK53976/table/sb107.t5/?report=objectonly
//Info about HMO and PPO from: https://www.medmutual.com/For-Individuals-and-Families/Health-Insurance-Education/Compare-Health-Insurance-Plans/HMO-vs-PPO-Insurance.aspx
import io.StdIn._
import scala.collection.mutable.ListBuffer
import scala.io.Source

var dependent = false
var coveredParent = false
var continueParent = ""
var done = false
var coveredNow = true
var householdSize = 0
var annualIncome = 0
var medicaidList = new ListBuffer [medicaidEligible]
var validSize = false
var availableFunds = 0.0
var fundInfo = false
var misc = 0.0
var premium = 0.0
var anotherValid = false
var doneLearning = false
var toBill = 0.0
var doneAdding = false
var finalDiagnosis = ""
var finalCost = 0.0
var Q = 27

case class injuryCost(
  number: Int,
  diagnosis: String,
  cost: Int
)

val hospitalList = new ListBuffer[injuryCost]()
  val hospitalOne = injuryCost(1, "Myocardial Infarction (Heart Attack)", 59475)
  hospitalList.append(hospitalOne)

  val hospitalTwo = injuryCost(2, "Mother's Pregnancy and Delivery)", 10533)
  hospitalList.append(hospitalTwo)

  val hospitalThree = injuryCost(3, "Coronary Artery Disease", 44694)
  hospitalList.append(hospitalThree)

  val hospitalFour = injuryCost(4, "Sepsis", 60542)
  hospitalList.append(hospitalFour)

  val hospitalFive = injuryCost(5, "CVD (Stroke)", 48033)
  hospitalList.append(hospitalFive)

  val hospitalSix = injuryCost(6, "Newborn Infant Treatment", 5105)
  hospitalList.append(hospitalSix)

  val hospitalSeven = injuryCost(7, "Gall bladder disease", 23023)
  hospitalList.append(hospitalSeven)

  val hosptialEight = injuryCost(8, "Skin and Subcutaneous Tissue Infections", 16283)
  hospitalList.append(hosptialEight)

  val hospitalNine = injuryCost(9, "Pneumonia", 22277)
  hospitalList.append(hospitalNine)

  val hospitalTen = injuryCost(10, "Nonspecific Chest Pain", 15279)
  hospitalList.append(hospitalTen)

  val hospitalEleven = injuryCost(11, "Diabetes with complications", 19577)
  hospitalList.append(hospitalEleven)

  val hospitalTwelve = injuryCost (12, "Intracranial Injury", 48000)
  hospitalList.append(hospitalTwelve)

  val hospitalThirteen = injuryCost (13, "Pancreatic Disorders", 24350)
  hospitalList.append(hospitalThirteen)

  val hospitalFourteen = injuryCost(14, "Lower Limb Fracture", 38680)
  hospitalList.append(hospitalFourteen)

  val hospitalFifteen = injuryCost(15, "Appendicitis", 26417)
  hospitalList.append(hospitalFifteen)

  val hospitalSixteen = injuryCost(16, "Congestive heart failure", 30387)
  hospitalList.append(hospitalSixteen)

  val hospitalSeventeen = injuryCost(17, "Crush or Internal Injury", 51056)
  hospitalList.append(hospitalSeventeen)

  val hospitalEighteen = injuryCost(18, "Affective Disorders", 11177)
  hospitalList.append(hospitalEighteen)

  val hospitalNineteen = injuryCost(19, "Alcohol-related Disorders", 13902)
  hospitalList.append(hospitalNineteen)

  val hospitalTwenty = injuryCost(20, "Respiratory Failure/Insufficiency", 52500)
  hospitalList.append(hospitalTwenty)

  val carAccident = injuryCost(21, "Car Accident: Broken Leg, Crush Injury, Head Trauma", 137736)
  hospitalList.append(carAccident)

  val babyDelivery = injuryCost(22, "Childbirth: Mother and Baby Cost", 15638)
  hospitalList.append(babyDelivery)


//Create Medicaid Eligible Object
case class medicaidEligible (
  numPeople: Int,
  famIncome: Int
)

  //Medicaid Standards
  var mediOne = medicaidEligible (1, 16612)
  medicaidList.append(mediOne)

  var mediTwo = medicaidEligible (2, 22491)
  medicaidList.append(mediTwo)

  var mediThree = medicaidEligible (3, 28369)
  medicaidList.append(mediThree)

  var mediFour = medicaidEligible (4, 34248)
  medicaidList.append(mediFour)

  var mediFive = medicaidEligible (5, 40127)
  medicaidList.append(mediFive)

  var mediSix = medicaidEligible (6, 46005)
  medicaidList.append(mediSix)

  var mediSeven = medicaidEligible (7, 51884)
  medicaidList.append(mediSeven)

  var mediEight = medicaidEligible (8, 57762)
  medicaidList.append(mediEight)

println("Welcome! As a young adult, you should know about health insurance and your options, as well as the financial risk associated with not having coverage.")
println("")
println("Please use this application to learn about health insurance, eligibility, and the risk of lifetime debt assumed by uninsured milennials.")
println("")

while (!done) {
  println("")y
  println("Menu:")

  println("1. Medicaid Eligibility")
  println("2. Uninsurance Debt Calculator")
  println("3. Budgeting and Insurance Payments")
  println("4. Facts about US Health Insurance")
  println("5. Enter My Budget/Reset Stored Budget")
  println("6. Quit Program")

  println("")

  println("Please enter the number corresponding an option from the Menu Above:")
  var userInput = readInt

  if (userInput == 6){
    println("Goodbye!")
    done = true
  } else if (userInput == 1) {
    println("Ok! let's find out more about your eligibility and dependency status.")
      println("Are you under the age of 26? (YES or NO)")
    var depAnswer = readLine.toUpperCase
    if (depAnswer == "YES") {
      println("Are you covered under your parent's/caregiver's insurance? (YES/NO)")
      var coveredAnswer = readLine.toUpperCase
      if (coveredAnswer == "YES") {
        coveredParent = true
        println("You will be covered under your parent's plan until you are 26. Would you like to know if you are mediciad eligible for when you go off their insurance? (YES/NO)")
        continueParent = readLine.toUpperCase
        if (continueParent == "YES"){
          done = false
        } else if (continueParent == "NO")
        done = true
      }
    } else {}
        while (!validSize){
          println("What is your household size?")
          householdSize = readInt
          if (householdSize<1 || householdSize>8){
            println("That is not a valid size. Please enter a household size between 1 and 8 people.")
            validSize = false
          } else {
            validSize = true
          }
        }
          var sizeIndex = {householdSize - 1}
          println("And what is your estimated annual household income before taxes? (Leave your best guess, or look at your tax return statements/paystubs from last year.")
          annualIncome = readInt
          println(s"The cutoff medicaid coverage for a $householdSize person household is $$${medicaidList(sizeIndex).famIncome}.")
          println()
            if (annualIncome <= {medicaidList(sizeIndex).famIncome}){
            println(s"Your income of $$$annualIncome falls below the cutoff, and you are eligible for Medicaid!")
            println("Would you like to learn more about medicaid? (YES/NO)")
            var medicaidKnowledge = readLine.toUpperCase
            if (medicaidKnowledge == "YES") {
              println("Visit: https://www.huskyhealthct.org/members.html?hhNav=| for more information!")
            }
          } else {
            println(s"Your income of $annualIncome does not fall below the cutoff, and you are not Medicaid Eligible.")
            println(s"You should, however, strongly consider private insurance to ensure you do not fall into debt in the case of an unexpected medical condition or injury.")
          }
          println("Would you like to EXIT or return to the MENU?")
          var areYouDone = readLine.toUpperCase
          if (areYouDone == "EXIT") {
            done = true
          }
  } else if (userInput == 2){
      var doneWithCost = false
      println("Let's demonstrate the benefits of health insurance, by learning about the risks taken by uninsured people.")
      println("")
      println("Below is a list of common reasons for hospitalization.")
      while(!doneWithCost){
        println("Choose an item from the list and enter the corresponding number to view the average cost of a hospital stay.")
        println("")
        for (i <- 0 until hospitalList.length){
          println(s"${hospitalList(i).number}. ${hospitalList(i).diagnosis}")
        }
        var userSelection = readInt
        var N = userSelection - 1
        finalDiagnosis = {hospitalList(N).diagnosis}
        finalCost = {hospitalList(N).cost}

        println(s"Ok! $finalDiagnosis was added!")

        while (!doneAdding) {
            println("Would you like to add another condition? (YES/NO)")
            var weAdding = readLine.toUpperCase
            if (weAdding == "YES") {
              println("Please select the number corresponding to the condition you'd like to add on:")
              var addedInput = readInt
              var N2 = addedInput - 1
              println(s"Ok! The condition ${hospitalList(N2).diagnosis} was added.")
              finalDiagnosis = finalDiagnosis + ", " + {hospitalList(N2).diagnosis}
              finalCost = finalCost + {hospitalList(N2).cost}
            } else if (weAdding == "NO") {
              doneAdding = true
            }
        }

        var grandTotal = finalCost.toInt

        println(s"Diseases: $finalDiagnosis")
        println(s"Total Hospital Bill: $$$grandTotal")

        println("")

        println("Would you like to see how long it would take you to pay off this bill with your current available income? (YES/NO)")
        var debtView = readLine.toUpperCase
        if (debtView == "YES") {
          if (!fundInfo){
            println("Please enter the following information about your financial status (do not include dollar signs):")
            println("What is your current annual income (before taxes)?")
            var annIncome = readDouble
            var monthlyIncome = {annIncome/12}
            println("And about how much do you pay a month in taxes?")
            var taxes = readDouble
            availableFunds = {monthlyIncome - taxes}
            println("Ok, and how much do you spend a week on groceries?")
            var groceries = {readDouble*4}
            availableFunds = {availableFunds - groceries}
            println("And how much do you spend a week on gas/transportation?")
            var gas = {readDouble*4}
            availableFunds = {availableFunds - gas}
            println("And about how much do you spend a month on other bills (rent, utilities, student loans, etc)?")
            var bills = readDouble
            availableFunds = {availableFunds - bills}
            println("Lastly, how much do you set aside per month for other miscellaneous costs (social life, car repairs, clothing, etc)?")
            var misc = readDouble
            availableFunds = {availableFunds-misc}

            fundInfo = true
        }
        toBill = math.round(availableFunds *100.0)/100.0

        println(s"Your available funds to put towards the bill per month is $$${toBill}.")

        var timeMonths = {{hospitalList(N).cost}/toBill}
        var finalTimeMonths = math.round(timeMonths*100.0)/100.0

        if (toBill<=0) {
          println("You currently don't have any money to put towards the bill! You'd have to seriously rethink your finances or declare bankruptcy!")
        } else{
          println(s"With a hospital bill of $$${hospitalList(N).cost}, it would take you $finalTimeMonths months to pay it off.")
        }
      } else if (debtView == "NO") {}

        println("Would you like to view another item? (YES/NO)")
        var anotherInput = readLine.toUpperCase
          if (anotherInput == "NO"){
            doneWithCost = true
            doneAdding = false
          } else {
            doneAdding = false
          }
      }
  } else if (userInput == 3) {
      println("Most insurance companies require that you pay a monthly premium. This is the amount you pay to the insurance company each month for coverage!")
      println("This tool helps you budget and make sure you save enough for your premium payment.")
      if (!fundInfo){
        println("Please enter the following information about your financial status (do not include dollar signs):")
        println("What is your current annual income (before taxes)?")
        var annIncome = readDouble
        var monthlyIncome = {annIncome/12}
        println("And about how much do you pay a month in taxes?")
        var taxes = readDouble
        availableFunds = {monthlyIncome - taxes}
        println("Ok, and how much do you spend a week on groceries?")
        var groceries = {readDouble*4}
        availableFunds = {availableFunds - groceries}
        println("And how much do you spend a week on gas/transportation?")
        var gas = {readDouble*4}
        availableFunds = {availableFunds - gas}
        println("And about how much do you spend a month on other bills (rent, utilities, student loans, etc)?")
        var bills = readDouble
        availableFunds = {availableFunds - bills}
        println("Lastly, how much do you set aside per month for other miscellaneous costs (social life, car repairs, clothing, etc)?")
        misc = readDouble
        availableFunds = {availableFunds-misc}

        fundInfo = true
    }
      toBill = math.round(availableFunds *100.0)/100.0

    println(s"Currently, your available funds per month is $$${toBill}.")
    if (toBill <= 0){
      println("You have no money!")
    } else{
    println("What is your monthly premium?")
    premium = readDouble
    var save = {premium/4}
    if (premium > toBill) {
      println("We have to rethink your finances!")
      println(s"You currently spend $$${misc} per month on personal and miscellaneous costs.")
      if (save > 0){
        println(s"If you save $$$save more per week, you might be able to pay that premium!")
      } else {
        println("It will be really hard for you to meet that premium. Return to the menu to see if you are Medicaid eligible or to view other insurance plans!")
      }
    } else {
      println(s"You should be able to pay your premium! Just set aside $$${save} per week and you should be fine!")
      println("Bringing you back to the main menu...")
      }
    }
  } else if (userInput == 4){
    doneLearning = false
    while(!doneLearning){
      println("Enter the number corresponding to a FAQ in the menu below.")
      println("Health Insurance FAQs")
      println("1. State Health Insurance")
      println("2. Private Health Insurance")
      println("3. Buying Health Insurance")
      println("4. HMOs and PPOs")
      var FAQ = readInt
      anotherValid = false
        if (FAQ == 1) {
          println("State Health Insurance is mostly government-funded.")
          println(" Unlike private plans, premiums are lower. However, coverage options may be limited.")
          println("To see if you are eligible for state insurance (Medicaid), select option 1 from the main menu.")
          println("")
          while (!anotherValid){
            println("Would you like to view another FAQ? (YES/NO)")
              var anotherFAQ = readLine.toUpperCase
              if (anotherFAQ == "YES") {
                doneLearning = false
                anotherValid = true
              } else if (anotherFAQ == "NO") {
                doneLearning = true
                anotherValid = true
              } else {
                 anotherValid = false
               }
            }
        } else if (FAQ == 2) {
          println("Private Health Insurance is not partially funded by the government.")
          println("Private plans vary more than public options, and depending on your budget, you can shop around for one that fits your personal health needs.")
          println("Popular providers in Connecticut are Anthem and Cigna.")
          println("")
          while (!anotherValid){
            println("Would you like to view another FAQ? (YES/NO)")
              var anotherFAQ = readLine.toUpperCase
              if (anotherFAQ == "YES") {
                doneLearning = false
                anotherValid = true
              } else if (anotherFAQ == "NO") {
                doneLearning = true
                anotherValid = true
              } else {
                 anotherValid = false
               }
             }
            } else if (FAQ == 3) {
              println("Buying private health insurance can be confusing.")
              println("Start by making a list of your health conditions, insurance needs, and projected expenses.")
              println("Then, think about what percent of unexpected accidents/illnesses you may want covered. You can use the UNINSURED CALCULATOR to help!")
              println("Finally, shop around for a plan that suits your needs!")
              println("")
              while (!anotherValid){
                println("Would you like to view another FAQ? (YES/NO)")
                  var anotherFAQ = readLine.toUpperCase
                  if (anotherFAQ == "YES") {
                    doneLearning = false
                    anotherValid = true
                  } else if (anotherFAQ == "NO") {
                    doneLearning = true
                    anotherValid = true
                  } else {
                     anotherValid = false
                   }
                 }
                } else if (FAQ == 4) {
                  println("HMO stands for Health Maintenance Organization.")
                  println("HMOs have lower premiums but have less in-network doctors and may require referrals to see specialists.")
                  println("PPO stands for Preferred Provider Organization.")
                  println("They typically have higher deductibles and premiums but allow more freedom in choosing doctors and seeing specialists.")
                  println("")
                  while (!anotherValid){
                    println("Would you like to view another FAQ? (YES/NO)")
                      var anotherFAQ = readLine.toUpperCase
                      if (anotherFAQ == "YES") {
                        doneLearning = false
                        anotherValid = true
                      } else if (anotherFAQ == "NO") {
                        doneLearning = true
                        anotherValid = true
                      } else {
                         anotherValid = false
                       }
                    }
                  }
    }
  } else if (userInput == 5){
    {
      println("Please enter the following information about your financial status (do not include dollar signs):")
      println("What is your current annual income (before taxes)?")
      var annIncome = readDouble
      var monthlyIncome = {annIncome/12}
      println("And about how much do you pay a month in taxes?")
      var taxes = readDouble
      availableFunds = {monthlyIncome - taxes}
      println("Ok, and how much do you spend a week on groceries?")
      var groceries = {readDouble*4}
      availableFunds = {availableFunds - groceries}
      println("And how much do you spend a week on gas/transportation?")
      var gas = {readDouble*4}
      availableFunds = {availableFunds - gas}
      println("And about how much do you spend a month on other bills (rent, utilities, student loans, etc)?")
      var bills = readDouble
      availableFunds = {availableFunds - bills}
      println("Lastly, how much do you set aside per month for other miscellaneous costs (social life, car repairs, clothing, etc)?")
      var misc = readDouble
      availableFunds = {availableFunds-misc}

      fundInfo = true
    }
    toBill = math.round(availableFunds *100.0)/100.0

    println(s"After considering all these finances, your monthly spending budget is now $$${toBill}.")
  }
}

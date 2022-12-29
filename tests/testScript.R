#### load test functions ####
{
  agsGitlabUtils::updatePackage("simpleTesting")
  require("simpleTesting")

  try(detach("package:avgImpute", unload=TRUE))
  rm(list=ls(all.names = TRUE))
  if (TRUE) {
    devtools::install()
    require("avgImpute")
  } else {
    source("./R/avgImpute.R")
    source("./R/utils.R")
  }

  errorLvl <- 0

  message(paste0(
    "*************\n",
    "*** Start ***\n",
    "*************\n"))

  #### getNearestObs ####
  for (i in 1:15) {
    errorLvl <- errorLvl + local({
      # Make data
      similarCols <- c("ha_LN", "GVE_tot", "Aktiven", "er_BetriebsertragLWS", "au_MaterialWarenDL")

      spb <- matrix(c(
        1, 2, 3, 4, 5,
        2, 3, 4, 5, 6,
        3, 4, 5, 6, 7,
        4, 5, 6, 7, 8,
        5, 6, 7, 8, 9,
        6, 7, 8, 9, 10
      ), byrow=TRUE, ncol=length(similarCols))
      colnames(spb) <- similarCols

      spe <- matrix(c(
        1, 2, 3, 4, 5,
        2, 3, 4, 5, 6,
        3, 4, 5, 6, 7,
        4, 5, 6, 7, 8,
        5, 6, 7, 8, 9,
        6, 7, 8, 9, 10
      ), byrow=TRUE, ncol=length(similarCols))
      spe <- rbind(
        spe + 0.05,
        spe + 10)
      spe <- spe[sample(1:nrow(spe), nrow(spe)),]
      colnames(spe) <- similarCols

      # Calc dist
      a <- getNearestObs(
        data = spb[, similarCols],
        dataGet = spe[, similarCols],
        minObs = 1,
        distFunc = stats::dist)

      # Define right results
      testResult <- rbind(spb, spe[a$ind[,1],])
      #dput(testResult)
      rightResult <- structure(c(1, 2, 3, 4, 5, 6, 1.05, 2.05, 3.05, 4.05, 5.05, 6.05,
                                 2, 3, 4, 5, 6, 7, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05, 3, 4, 5,
                                 6, 7, 8, 3.05, 4.05, 5.05, 6.05, 7.05, 8.05, 4, 5, 6, 7, 8, 9,
                                 4.05, 5.05, 6.05, 7.05, 8.05, 9.05, 5, 6, 7, 8, 9, 10, 5.05,
                                 6.05, 7.05, 8.05, 9.05, 10.05), .Dim = c(12L, 5L), .Dimnames = list(
                                   NULL, c("ha_LN", "GVE_tot", "Aktiven", "er_BetriebsertragLWS",
                                           "au_MaterialWarenDL")))
      #if (!assertthat::are_equal(testResult, rightResult))
      #  browser()
      # Test
      return (testAgainstAndReport(testName = "getNearestObs", testResult = testResult, rightResult = rightResult, testNo = 1, stopOnError = FALSE))
    })
  }

  #### getNearestObs - with maxDist ####
  errorLvl = errorLvl + local({
    set.seed(230984)
    data <- as.data.frame(matrix(rnorm(100), ncol=5))
    dataGet <- as.data.frame(matrix(rnorm(1000), ncol=5))

    # Prepare data & calculate results
    testResult <- getNearestObs(data = data, dataGet = dataGet, maxDist = 3, minObs = 10, maxObs = 20, distFunc = function (x) stats::dist(x, method='manhattan'))

    # Define right results
    rightResult <- list(ind = structure(c(
      40, 148, 66, 188, 31, 195, 52, 55, 10,
      88, 52, 50, 196, 13, 178, 105, 92, 187, 88, 165, 170, 42, 101,
      29, 90, 122, 1, 1, 96, 159, 200, 5, 99, 88, 99, 80, 175, 130,
      151, 105, 166, 11, 91, 48, 41, 172, 63, 177, 175, 180, 125, 153,
      178, 42, 113, 101, 122, 42, 120, 198, 41, 159, 117, 102, 141,
      185, 116, 82, 133, 162, 137, 154, 47, 159, 59, 121, 96, 158,
      87, 114, 35, 67, 200, 182, 138, 127, 55, 63, 18, 29, 66, 97,
      113, 106, 196, 196, 181, 159, 37, 149, 31, 113, 69, 62, 84, 110,
      27, 27, 127, 104, 145, 152, 136, 69, 168, 83, 75, 87, 198, 68,
      79, 106, 113, 16, 79, 118, 144, 46, 122, 182, 26, 128, 68, 130,
      65, 67, 141, 112, 184, 37, 141, 152, 128, 79, 35, 108, 109, 57,
      38, 9, 51, 104, 67, 104, 7, 51, 172, 13, 165, 110, 119, 13, 118,
      162, 197, 38, 137, 140, 181, 13, 67, 112, 80, 146, 66, 99, 127,
      148, 159, 78, 57, 100, 105, 17, 179, 117, 177, 167, 35, 46, 1,
      42, 168, 160, 4, 113, 117, 44, 13, 83, NA, 130, 155, 140, NA,
      133, NA, NA, NA, 146, 106, NA, 59, 126, 191, 200, 118, 152, 42,
      12, NA, 69, 152, 54, NA, 175, NA, NA, NA, 5, NA, NA, 148, 53,
      161, 165, 155, 3, 135, 70, NA, 146, 146, 183, NA, 12, NA, NA,
      NA, 120, NA, NA, 85, 100, 68, 69, 38, 11, 67, NA, NA, 128, 121,
      119, NA, 86, NA, NA, NA, 42, NA, NA, 4, 29, 24, 68, 160, NA,
      17, NA, NA, 77, 132, 30, NA, 23, NA, NA, NA, 69, NA, NA, 83,
      183, 67, 178, NA, NA, 29, NA, NA, 3, 11, 89, NA, 194, NA, NA,
      NA, 4, NA, NA, 121, 151, 136, 74, NA, NA, 131, NA, NA, 162, 51,
      160, NA, 160, NA, NA, NA, 131, NA, NA, 165, 67, 148, 66, NA,
      NA, 182, NA, NA, 99, 49, 46, NA, 75, NA, NA, NA, NA, NA, NA,
      106, 101, 80, 49, NA, NA, 106, NA, NA, 47, 61, 19, NA, 73, NA,
      NA, NA, NA, NA, NA, 51, 108, 106, 198, NA, NA, 183, NA, NA, 155,
      42, 163, NA, 163, NA, NA, NA, NA, NA, NA, 22, 162, 101, 174,
      NA, NA, 6, NA), .Dim = c(20L, 20L)),
      dist = structure(c(
        1.81785291051623,
        1.04008159350349, 1.29962389475354, 1.14208583310776, 3.74754638033092,
        1.71929029746439, 1.75975667318651, 2.50904411672454, 2.53495085713692,
        1.45044001848166, 1.84890867493623, 1.80337063161666, 1.09876424556696,
        0.995296219320235, 1.35582340871259, 1.07040548521258, 1.25389821003033,
        0.760710005521663, 1.45679512810646, 1.67021111618956, 1.98202686361308,
        1.67516613171816, 1.35301357457404, 1.67206952735464, 5.16298132300852,
        1.82961070913782, 2.34914386297668, 2.6710597313345, 2.83634776735037,
        1.68265898843929, 1.95858373282737, 2.30611568018426, 1.24376181917185,
        1.18966288266339, 1.36687445626186, 1.70964060284486, 1.77691309662448,
        1.28014365238427, 1.60673665395706, 2.01374582682268, 2.21985918103275,
        1.80182630238267, 1.69007380524975, 1.77483386438539, 5.22952711344225,
        1.83525261739212, 2.47775934335081, 2.72469328937353, 2.96754772986726,
        1.84552415356596, 2.07088778639806, 2.3067845722746, 1.63481952534176,
        1.33055083471562, 1.41010693784855, 1.73000179474812, 1.89124233556877,
        1.52834978130479, 1.94914592622574, 2.02767508461231, 2.59683493389719,
        1.86756993330091, 1.85293046046879, 1.94252786500665, 5.42312984114801,
        1.88814277712999, 2.74528385636357, 3.1272760285524, 3.18625090168765,
        1.85624833369458, 2.24169531927594, 2.73809642001853, 1.67537658464972,
        1.53004679495419, 1.63426470902697, 1.73130225847402, 1.90112182266578,
        2.12561578012715, 2.2783815034697, 2.40682354234159, 2.81563493273438,
        1.87602324368199, 2.05061487677164, 1.98538127043699, 5.45501570032405,
        2.0747009104157, 2.93245710495017, 3.21912110889203, 3.19353347862359,
        2.11144253973585, 2.2906320414459, 2.97720298969921, 1.8625789092954,
        1.69704709059341, 1.71568066822748, 1.84064825842907, 2.08630105449047,
        2.25140376317852, 2.28247099276588, 2.41556165700296, 3.29464873895904,
        1.88067751823767, 2.05177635209704, 2.01797003144286, 5.53266679544314,
        2.09361044845568, 2.95634487777589, 3.48764986987038, 3.65421892376467,
        2.197328265307, 2.42755409004159, 3.31703568165586, 2.02066976997889,
        1.70380110273356, 1.87789687616545, 1.88209100942189, 2.09767351499725,
        2.33949814949451, 2.4261612396052, 2.44731049017417, 3.32152808283275,
        2.00590209249368, 2.18593180343414, 2.11571170180601, 5.55044540989333,
        2.27217213159005, 3.25614454608312, 3.57446521955704, 3.86345896307607,
        2.20713346742448, 2.58774882809095, 3.34625401155407, 2.04537665988073,
        1.74979076044873, 2.157430069728, 1.92187780771527, 2.3750076435567,
        2.36945611748923, 2.45351967497715, 2.56915253588267, 3.36101232512007,
        2.06372467426195, 2.18733706868057, 2.21591051727951, 5.59794645126,
        2.39337077383799, 3.29822321683731, 3.74831224337195, 3.89653456275027,
        2.33243009540873, 2.58828548618191, 3.37459855559485, 2.08528866541031,
        2.02070489783277, 2.17583242798145, 2.04157372465283, 2.37586555840734,
        2.40343153963327, 2.49506184797654, 2.67672588458499, 3.43886369321884,
        2.18185750097118, 2.2111891481684, 2.38000388066251, 5.67963072190338,
        2.46159859964969, 3.45916886469149, 3.893139014818, 4.05973721150791,
        2.44135410156894, 2.75532787910493, 3.38031555789269, 2.27987617171726,
        2.04044395001108, 2.23410598219125, 2.08301530167581, 2.49298757520415,
        2.42516182156395, 2.54091733741734, 2.68012549441997, 3.56249229266794,
        2.18299373391281, 2.21929323211348, 2.38749695694903, 5.71993199062167,
        2.47166719159505, 3.73598377758802, 3.95204984191008, 4.08064263317947,
        2.5134001403784, 2.80472240494608, 3.40349683332894, 2.35684209248758,
        2.10929442788049, 2.34627955199663, 2.12504872739401, 2.58364105181465,
        2.53277952023386, 2.56826886574821, 2.70618506801129, NA, 2.21749870693143,
        2.26542872598721, 2.40300665995978, NA, 2.48335671194658, NA,
        NA, NA, 2.61647570384082, 2.94363219450679, NA, 2.41907676219862,
        2.26619935064496, 2.51471059242361, 2.16792759419603, 2.6176258100111,
        2.537543324145, 2.5989835675142, 2.84612382419947, NA, 2.31919635014708,
        2.28066870893585, 2.44623404330454, NA, 2.59932607960117, NA,
        NA, NA, 2.61660496414773, NA, NA, 2.43711496328695, 2.30371702954306,
        2.6469543939589, 2.28166786815385, 2.93598121352606, 2.67203480229607,
        2.60076905297317, 2.86732547364039, NA, 2.3432836581341, 2.33478698883562,
        2.49218522960832, NA, 2.63503256809499, NA, NA, NA, 2.65074847073858,
        NA, NA, 2.4530143398298, 2.30836925491978, 2.6902674679886, 2.32891134102452,
        2.93976950391855, 2.69695861358587, 2.602051403788, NA, NA, 2.36256972509123,
        2.34740857232717, 2.56532594431411, NA, 2.65750249047888, NA,
        NA, NA, 2.6573872929647, NA, NA, 2.56592349118433, 2.33934317004149,
        2.73240517962996, 2.37653255206354, 2.96508555625141, NA, 2.61860961375183,
        NA, NA, 2.41942247000046, 2.38421899413788, 2.65154651230547,
        NA, 2.70016326322982, NA, NA, NA, 2.82136211271951, NA, NA, 2.72247087148016,
        2.35092529238007, 2.74490792749641, 2.46178299592055, NA, NA,
        2.62137615711133, NA, NA, 2.47280729261507, 2.38590934864077,
        2.67583081851656, NA, 2.80398365949444, NA, NA, NA, 2.83196414536192,
        NA, NA, 2.7236957011004, 2.36513524765224, 2.76790305088908,
        2.50185410758303, NA, NA, 2.69811910800997, NA, NA, 2.49919516336582,
        2.39246294527444, 2.73766561486752, NA, 2.80629600188203, NA,
        NA, NA, 2.93927287624526, NA, NA, 2.73245896691111, 2.39884235343356,
        2.80852886646188, 2.50985754412547, NA, NA, 2.74336494538782,
        NA, NA, 2.51341807552314, 2.41382209304809, 2.75479097166204,
        NA, 2.83076066145365, NA, NA, NA, NA, NA, NA, 2.77287261628422,
        2.52800681982611, 2.83708868764847, 2.56722416117682, NA, NA,
        2.74803691430338, NA, NA, 2.52595184259356, 2.44044559391057,
        2.83820217550592, NA, 2.83902919197092, NA, NA, NA, NA, NA, NA,
        2.79383299034847, 2.54417684540359, 2.85527766261763, 2.57153015809667,
        NA, NA, 2.78051481363037, NA, NA, 2.67193438464468, 2.4833718719284,
        2.96227747567382, NA, 2.86132860627591, NA, NA, NA, NA, NA, NA,
        2.82404395650546, 2.57473484623721, 2.88218765966137, 2.67401038104671,
        NA, NA, 2.80107852193494, NA), .Dim = c(20L, 20L)))

    # Test
    return (
      testAgainstAndReport(testName = "getNearestObs - with maxDist and Manhattan distance.", testNo = "1.001", testResult = testResult, rightResult = rightResult, stopOnError = FALSE)
    )
  })

  #### getNearestObs - Always return NA matrix, also when no observations were found.
  errorLvl <- errorLvl + local({
    set.seed(23987423)
    dataFill <- data.frame(a=1:10, b=11:20, sim1=51:60 + rnorm(10), ind1=c(2,2,2,1,1,1,3,3,3,3))
    dataGet <- data.frame(a=21:30, c=31:40, sim1=60:51 + rnorm(10), ind1=c(3,3,3,3,2,2,2,1,1,1))

    testResult <- getNearestObs(
      data = dataFill[, "sim1", drop = FALSE],
      dataGet = dataGet[, "sim1", drop = FALSE],
      maxDist = 0)

    rightResult <- list(ind = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), .Dim = c(10L, 1L)), dist = structure(c(NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA), .Dim = c(10L, 1L)))

    errorLvl <- errorLvl + testAgainstAndReport(testName = "Matrix always has at least 1 column.", testNo = "1.0011", rightResult = rightResult, testResult = testResult)
  })

  #### getNearestObs - Expect an error when the distance function does not return the right value.
  errorLvl <- errorLvl + local({
    set.seed(23987423)
    dataFill <- data.frame(a=1:10, b=11:20, sim1=51:60 + rnorm(10), ind1=c(2,2,2,1,1,1,3,3,3,3))
    dataGet <- data.frame(a=21:30, c=31:40, sim1=60:51 + rnorm(10), ind1=c(3,3,3,3,2,2,2,1,1,1))
    expr <- quote(getNearestObs(
      data = dataFill[, "sim1", drop = FALSE],
      dataGet = dataGet[, "sim1", drop = FALSE],
      minObs = 1,
      distFunc = function(x) c(1,2,3)))
    errorLvl <- errorLvl + expectError(expr = expr, exact = FALSE, testNo = "1.002", testName = "Error expected when distFunc returns value with wrong distances.")
  })

  #### getNearestObs - idCol given ####
  errorLvl = errorLvl + local({
    # Prepare data & calculate results
    dataFill <- data.frame(a = 1:10)
    dataGet <- data.frame(a = 1:20 + 0.1, id = letters[1:20], stringsAsFactors = FALSE)
    testResult <- getNearestObs(
      data = dataFill,
      dataGet = dataGet,
      cols = "a",
      idCol = "id",
      minObs = 2)$id
    # Define right results
    rightResult <- structure(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                               "b", "a", "b", "c", "d", "e", "f", "g", "h", "i"), .Dim = c(10L,  2L))
    # Test
    return(testAgainstAndReport(testName = "getNearestObs - idCol given", testNo = "1.003", testResult = testResult, rightResult = rightResult, stopOnError = FALSE))
  })


  #### getNearestObs - index given ####
  errorLvl = errorLvl + local({
    # Prepare data & calculate results
    # Prepare data & calculate results
    dataFill <- data.frame(a = 1:10, index = c(rep(3, 5), rep(2, 5)))
    dataGet <- data.frame(a = c(rep(1, 10), 10:6, 5:1) + 0.1, index = c(rep(1, 10), rep(2, 5), rep(3, 5)), id = letters[1:20], stringsAsFactors = FALSE)
    testResult <- getNearestObs(
      data = dataFill,
      dataGet = dataGet,
      cols = "a",
      indexCols = "index",
      idCol = "id",
      minObs = 2)
    testResult <- lapply(testResult, function(x) if (is.numeric(x)) round(x, 2) else x)

    # Define right results
    rightResult <- list(ind = structure(c(20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 19, 20, 19, 18, 17, 14, 15, 14, 13, 12), .Dim = c(10L, 2L)),
                        dist = structure(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 1.1, 0.9, 0.9, 0.9, 0.9, 1.1, 0.9, 0.9, 0.9, 0.9), .Dim = c(10L, 2L)),
                        id = structure(c("t", "s", "r", "q", "p", "o", "n", "m", "l", "k", "s", "t", "s", "r", "q", "n", "o", "n", "m", "l"), .Dim = c(10L, 2L)))

    # Test
    return(testAgainstAndReport(testName = "getNearestObs - index given", testNo = "1.004", testResult = testResult, rightResult = rightResult, stopOnError = FALSE))
  })

  #### findCircularReferences ####
  errorLvl = errorLvl + local({
    testResult <- avgImpute:::.splitAll(c("a <- b", "e = f + c", "m < z", "u <= k", "c - d"), ignoreLhsAssignment = TRUE)
    rightResult <- c("b", "f", "c", "m", "z", "u", "k", "c", "d")
    errorLvl <- errorLvl + testAgainstAndReport(testName = "avgImpute:::.splitAll, ignoreLhsAssignment = TRUE", testResult = testResult, rightResult = rightResult, testNo = 1.005, stopOnError = FALSE)
    #
    testResult <- avgImpute:::.splitExceptInBracket(c("a <= b", "a=b"), operators = c("<-", "="), notAroundOperator = c("<", ">", "!", "="))
    rightResult <- list("a <= b", c("a", "b"))
    errorLvl <- errorLvl + testAgainstAndReport(testName = "avgImpute:::.splitExceptInBracket, argument `notAroundOperator` is given.", testResult = testResult, rightResult = rightResult, testNo = 1.006, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({
      a <- if (exists("b")) b == 1 else TRUE
      c <- d
    }))
    rightResult <- NULL
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 0.1001. Comparison `b == 1` in RHS is not considered a LHS assignment.", testResult = testResult, rightResult = rightResult, testNo = 1.00100, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(c("x[1 != 2] <- 0"))
    rightResult <- NULL
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 0.1002. Square brackets are not separated.", testResult = testResult, rightResult = rightResult, testNo = 1.00101, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({ a <- b; b <- c; c <- a }))
    rightResult <- c("a <- b", "b <- c", "c <- a")
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 1", testResult = testResult, rightResult = rightResult, testNo = 1.01, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({ a <- if (a == 1) a + 1 else b }))
    rightResult <- "a <- if (a == 1) a + 1 else b"
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 2", testResult = testResult, rightResult = rightResult, testNo = 1.02, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({ a <- if (a == 1) 2 else a + b }))
    rightResult <- "a <- if (a == 1) 2 else a + b"
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 3", testResult = testResult, rightResult = rightResult, testNo = 1.03, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({ a <- b; c <- d }))
    rightResult <- NULL
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 4", testResult = testResult, rightResult = rightResult, testNo = 1.04, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({ a <- if (a == 1) 2 else b }))
    rightResult <- NULL
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 5", testResult = testResult, rightResult = rightResult, testNo = 1.05, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({
      a <- b
      b <- c
      c <- d
      d <- a
    }))
    rightResult <- c("a <- b", "b <- c", "c <- d", "d <- a")
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 6", testResult = testResult, rightResult = rightResult, testNo = 1.06, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({
      a <- b <- c <- d
      d <- a
    }))
    rightResult <- c("c <- d", "b <- c", "a <- b", "d <- a")
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 7", testResult = testResult, rightResult = rightResult, testNo = 1.07, stopOnError = FALSE)
    #
    testResult <- findCircularReferences(quote({
      a <- a + b
      c = a + someFunc(arg = c)
      e = f + someFunc(e = z)
    }))
    rightResult <- c("a <- a + b", "c = a + someFunc(arg = c)")
    errorLvl <- errorLvl + testAgainstAndReport(testName = "findCircularReferences 8. LHS assignment inside function is not considered a circular reference.", testResult = testResult, rightResult = rightResult, testNo = 1.08, stopOnError = FALSE)
    #

    return (errorLvl)
  })

  #### calcMean ####
  errorLvl <- errorLvl + local({
    # Prepare data & calculate retuls
    data <- data.frame(
      a=11:41,
      b=61:91)
    indices <- data.frame(
      ind1=c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  4),
      ind2=c( 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 7, 7, 7, 8, 8, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10),
      all =c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1))
    indexOrder <- list(
      c('ind1', 'ind2'),
      'ind1',
      'all')
    getMean <- calcMean(data = data, indices = indices, indexOrder = indexOrder)

    # Define right results
    #dput(getMean(raw = TRUE)$mean) ; dput(getMean(raw = TRUE)$n)
    rightResultMean <- structure(c(15.5, 22, 24, 26, 28.5, 31.5, 35.5, 39, 41, 17.5,
                                   27, 35, 41, 25.5, 41, 65.5, 72, 74, 76, 78.5, 81.5, 85.5, 89,
                                   91, 67.5, 77, 85, 91, 75.5, 91), .Dim = c(15L, 2L), .Dimnames = list(
                                     c("1-X-7-X-NA", "1-X-8-X-NA", "1-X-9-X-NA", "2-X-7-X-NA",
                                       "2-X-8-X-NA", "3-X-7-X-NA", "3-X-8-X-NA", "3-X-9-X-NA", "4-X-10-X-NA",
                                       "1-X-NA-X-NA", "2-X-NA-X-NA", "3-X-NA-X-NA", "4-X-NA-X-NA",
                                       "NA-X-NA-X-0", "NA-X-NA-X-1"), c("a", "b")))
    rightResultN <- c(`1-X-7-X-NA` = 10L, `1-X-8-X-NA` = 3L, `1-X-9-X-NA` = 1L, `2-X-7-X-NA` = 3L,
                      `2-X-8-X-NA` = 2L, `3-X-7-X-NA` = 4L, `3-X-8-X-NA` = 4L, `3-X-9-X-NA` = 3L,
                      `4-X-10-X-NA` = 1L, `1-X-NA-X-NA` = 14L, `2-X-NA-X-NA` = 5L,
                      `3-X-NA-X-NA` = 11L, `4-X-NA-X-NA` = 1L, `NA-X-NA-X-0` = 30L,
                      `NA-X-NA-X-1` = 1L)

    # Test
    return (
      testAgainstAndReport(testName = "calcMean & getMean -> mean", testResult = getMean(raw = TRUE)$mean, rightResult = rightResultMean, testNo = 2.1, stopOnError = FALSE)
      + testAgainstAndReport(testName = "calcMean & getMean -> n", testResult = getMean(raw = TRUE)$n, rightResult = rightResultN, testNo = 2.2, stopOnError = FALSE)
    )
  })

  #### calcMean with contIndex ####
  errorLvl = errorLvl + local({
    internalErrorLvl <- 0
    # Prepare data & calculate results
    data <- data.frame(
      a=11:41,
      b=61:91)
    indices <- data.frame(
      ind1=c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  4),
      ind2=c( 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 7, 7, 7, 8, 8, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10),
      all =c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1))
    contIndex <-
      c( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 1, 2, 3, 1,11,12,13, 1, 1, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3,  1)
    indexOrder <- list(
      c('ind1', 'ind2'),
      'ind1',
      'all')
    getMean <- calcMean(data = data, cols=c("a","b","I(a/b)"), indices = indices, indexOrder = indexOrder, contIndex = contIndex)#, nCores = 1)
    #print(getMean(raw = TRUE)$mean)
    #print(getMean(raw = TRUE)$n)

    index <- data.frame(
      ind1=c(1, 2, 3, 4),
      ind2=c(7, 8, 9, 10),
      all=c(0, 0, 0, 1))

    if (TRUE) {
      #message("Testing with contIndex, short.")
      system.time({
        rightResult <- list(mean = structure(list(a = c(13, 28.5, 39, 41), b = c(63,
                                                                                 78.5, 89, 91), `I(a/b)` = c(0.206349206349206, 0.363057324840764,
                                                                                                             0.438202247191011, 0.450549450549451)), row.names = c(NA, -4L
                                                                                                             ), class = "data.frame"), index = structure(list(ind1 = c(1,
                                                                                                                                                                       2, 3, 4), ind2 = c(7, 8, 9, 10), all = c(NA_real_, NA_real_,
                                                                                                                                                                                                                NA_real_, NA_real_)), row.names = c(NA, -4L), class = "data.frame"), contIndex = c(1, NA, NA, NA))
        testResult <- getMean(index, contIndex=c(1,2,NA,9), nMin=0, forceValue=TRUE)#, nCores = 1)
        internalErrorLvl <- internalErrorLvl +
          testAgainstAndReport(testName = "calcMean & getMean -> with contIndex",
                               testResult = testResult, rightResult = rightResult, testNo = 2.30, stopOnError = FALSE)
      })}
    if (TRUE) {
      #message("Testing without contIndex, short.")
      system.time({
        rightResult <- list(mean = structure(list(a = c(15.5, 28.5, 39, 41), b = c(65.5,
                                                                                   78.5, 89, 91), `I(a/b)` = c(0.236641221374046, 0.363057324840764,
                                                                                                               0.438202247191011, 0.450549450549451)), row.names = c(NA, -4L
                                                                                                               ), class = "data.frame"), index = structure(list(ind1 = c(1,
                                                                                                                                                                         2, 3, 4), ind2 = c(7, 8, 9, 10), all = c(NA_real_, NA_real_,
                                                                                                                                                                                                                  NA_real_, NA_real_)), row.names = c(NA, -4L), class = "data.frame"), contIndex = c(NA_real_, NA_real_, NA_real_, NA_real_))
        testResult <- getMean(index, contIndex=NULL, nMin=0, forceValue=TRUE)#, nCores = 1)
        internalErrorLvl <- internalErrorLvl +
          testAgainstAndReport(testName = "calcMean & getMean -> without contIndex",
                               testResult = testResult, rightResult = rightResult, testNo = 2.31, stopOnError = FALSE)
      })}
    if (FALSE) {
      message("Testing with contIndex, long. Without test.")
      print(system.time({
        takeMany <- sample(1:nrow(index), 10000, replace = TRUE)
        getMean(index[takeMany,], contIndex=c(1,2,NA,9)[takeMany], nMin=0, forceValue=TRUE)#, nCores = 1)
      }))}
    if (FALSE) {
      message("Testing without contIndex, long. Without test.")
      print(system.time({
        getMean(index[takeMany,], nMin=0, forceValue=TRUE)#, nCores = 1)
      }))}

    return (internalErrorLvl)
  })

  #### imputeDomainDriven ####

  set.seed(23987423)
  dataFill <- data.frame(a=1:10, b=11:20, sim1=51:60 + rnorm(10), ind1=c(2,2,2,1,1,1,3,3,3,3), stringsAsFactors = FALSE)
  dataGet <- data.frame(a=21:30, c=31:40, sim1=60:51 + rnorm(10), ind1=c(3,3,3,3,2,2,2,1,1,1), stringsAsFactors = FALSE)
  # getNearestObs(dataFill["sim1"], dataGet["sim1"], maxDist = 0.5, minObs = 1, maxObs = 5)
  expr <- quote({ e <- a * b; c <- a * lookup(a + c)})


  for (nCores in 1:2) {
    errorLvl <- errorLvl + local({
      #### Weights always 1
      testResult <- imputeDomainDriven(
        dataFill = dataFill,
        dataGet = dataGet,
        lookupIndexNames = 'ind1',
        similarityCols = 'sim1',
        minObs = 2,
        expr = expr,
        nCores = nCores,
        showProgressBar = FALSE,
        distToWeightFunc = function(x) {
          x[] <- 1
          return(x)
        })

      # It is only allowed to compare the first 3 elements because these have ind1 == 2.
      testResult <- round(testResult[1:3, "c"], 5)

      rightResult <- local({
        dataGetTmp <- dataGet[dataGet[, "ind1"] == dataFill[1, "ind1"], , drop = FALSE]
        ind <- getNearestObs(data = dataFill[, "sim1", drop = FALSE], dataGet = dataGetTmp[, "sim1", drop = FALSE], minObs = 2)[["ind"]][1, ]
        dataGetTmp2 <- dataGetTmp[ind, ]
        return(dataFill[1:3, "a"] * (mean(dataGetTmp2[, "a"]) + mean(dataGetTmp2[, "c"])))
      })
      rightResult <- round(rightResult, 5)

      return(errorLvl + testAgainstAndReport(testName = paste0("imputeDomainDriven -> Calc worked. nCores = ", nCores), testResult = testResult, rightResult = rightResult, testNo = 3.1, stopOnError = FALSE))
    })
  }


  errorLvl <- errorLvl + local({

    dataFill[, "I(a/b)"] <- with(dataFill, a/b)
    dataFill[, "z"] <- letters[1:10]
    testResult <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 2,
      expr = expr,
      showProgressBar = FALSE,
      distToWeightFunc = function(x) {
        x[] <- 1
        return(x)
      })

    testResult[] <- lapply(testResult, function(x) if (class(x) == "numeric" ) round(x, 5) else x)

    rightResult <- local({ asdofiue <- data.frame(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L), c(50.68508, 50.5332, 54.09779, 54.1192, 54.48104, 57.86592, 56.42428, 57.60045, 57.76461, 58.50288), c(2, 2, 2, 1, 1, 1, 3, 3, 3, 3), c(0.09091, 0.16667, 0.23077, 0.28571, 0.33333, 0.375, 0.41176, 0.44444, 0.47368, 0.5), c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'), c(63, 126, 189, 268, 335, 402, 385, 432, 486, 540), c(11L, 24L, 39L, 56L, 75L, 96L, 119L, 144L, 171L, 200L), stringsAsFactors=FALSE); colnames(asdofiue) <- c('a', 'b', 'sim1', 'ind1', 'I(a/b)', 'z', 'c', 'e'); rownames(asdofiue) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'); return (asdofiue); })

    return(testAgainstAndReport(testName = "imputeDomainDriven -> Colnames do not change", testResult = testResult, rightResult = rightResult, testNo = 3.11, stopOnError = FALSE))
  })



  #### With minObs specified
  errorLvl <- errorLvl + local({
    testResult <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 2,
      expr = expr,
      showProgressBar = FALSE)

    testResult <- round(testResult[1, "c"], 5)

    rightResult <- local({
      dataGetTmp <- dataGet[dataGet[, "ind1"] == dataFill[1, "ind1"], , drop = FALSE]
      nearestObs <- getNearestObs(data = dataFill[, "sim1", drop = FALSE], dataGet = dataGetTmp[, "sim1", drop = FALSE], minObs = 2)
      ind <- nearestObs[["ind"]][1, ]
      weights <- distToWeight(nearestObs[["dist"]])[1, ]
      dataGetTmp2 <- dataGetTmp[ind, ]
      return(dataFill[1, "a"] * (weighted.mean(dataGetTmp2[, "a"], weights) + weighted.mean(dataGetTmp2[, "c"], weights)))
    })
    rightResult <- round(rightResult, 5)

    return(errorLvl + testAgainstAndReport(testName = "imputeDomainDriven (distToWeightFunc returns only 1 values) -> Calc worked.", testResult = testResult, rightResult = rightResult, testNo = 3.2, stopOnError = FALSE))
  })




  #### With minObs specified
  errorLvl <- errorLvl + local({
    testResult <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 2,
      expr = expr,
      showProgressBar = FALSE)

    testResultError <- quote(imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 2,
      expr = quote({ e <- a * b; a <- 1 + e}),
      showProgressBar = FALSE))

    # Define right results
    rightResult <- local({ asdofiue <- data.frame(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L), c(50.6850781796545, 50.5332043428534, 54.0977874433753, 54.1191971222733, 54.4810426247368, 57.8659162756877, 56.4242825007141, 57.6004456916247, 57.7646054266088, 58.5028755051793), c(2, 2, 2, 1, 1, 1, 3, 3, 3, 3), c(63.1422202236486, 126.275127069078, 191.414633454537, 268.360809163489, 335.396276667427, 402.224106754605, 393.599993035904, 431.792377017257, 485.729107272308, 536.47049453318), c(11L, 24L, 39L, 56L, 75L, 96L, 119L, 144L, 171L, 200L), stringsAsFactors=FALSE); colnames(asdofiue) <- c('a', 'b', 'sim1', 'ind1', 'c', 'e'); rownames(asdofiue) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'); return (asdofiue); })

    # Test
    errorLvl <- errorLvl + testAgainstAndReport(testName = "imputeDomainDriven (minObs) -> Calc worked.", testResult = testResult, rightResult = rightResult, testNo = 3.3, stopOnError = FALSE)
    errorLvl <- errorLvl + expectError(testName = "imputeDomainDriven -> Circular reference yielded error.", expr = eval(testResultError), msg = "Circular references were found in the calculation expression `expr`. Above this error message they should appear in the R console. If you want to suppress this error, set `checkExprForErrors = FALSE`.", testNo = 3.2, stopOnError = FALSE)
    return(errorLvl)
  })



  #### useIndexIgnoreSimilarity = TRUE
  errorLvl <- errorLvl + local({
    testResult <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 4,
      expr = expr,
      showProgressBar = FALSE,
      useIndexIgnoreSimilarity = TRUE)

    # c <- a * lookup(a + c)
    rightResult <- local({
      tmp <- dataGet[dataGet[, "ind1"] == 2, ]
      dataFill[1, "a"] * (mean(tmp[, "a"]) + mean(tmp[, "c"]))
    })
    rightResult <- round(rightResult, 5)

    testResult <- round(testResult[1, "c"], 5)

    return(errorLvl + testAgainstAndReport(testName = "imputeDomainDriven (useIndexIgnoreSimilarity = TRUE) -> Calc worked.", testResult = testResult, rightResult = rightResult, testNo = 3.4, stopOnError = FALSE))
  })





  errorLvl <- errorLvl + local({
    #### With minObs specified, no lookupIndexNames given.
    testResult <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      #lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      minObs = 2,
      expr = expr,
      showProgressBar = FALSE)

    rightResult <- local({ asdofiue <- data.frame(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L), c(50.6850781796545, 50.5332043428534, 54.0977874433753, 54.1191971222733, 54.4810426247368, 57.8659162756877, 56.4242825007141, 57.6004456916247, 57.7646054266088, 58.5028755051793), c(2, 2, 2, 1, 1, 1, 3, 3, 3, 3), c(68.3911706385445, 137.35004801516, 191.414633454537, 255.013082619288, 316.352883561497, 323.799139985731, 412.239928844289, 431.792377017257, 485.729107272308, 536.47049453318), c(11L, 24L, 39L, 56L, 75L, 96L, 119L, 144L, 171L, 200L), stringsAsFactors=FALSE); colnames(asdofiue) <- c('a', 'b', 'sim1', 'ind1', 'c', 'e'); rownames(asdofiue) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'); return (asdofiue); })

    errorLvl <- errorLvl + testAgainstAndReport(testName = "imputeDomainDriven (minObs, lookupIndexNames not given) -> Calc worked.", testResult = testResult, rightResult = rightResult, testNo = 3.5, stopOnError = FALSE)

    #### With maxDist specified
    testResult2 <- imputeDomainDriven(
      dataFill = dataFill,
      dataGet = dataGet,
      lookupIndexNames = 'ind1',
      similarityCols = 'sim1',
      maxDist = 0.5,
      minObs = 1,
      maxObs = 5,
      expr = expr,
      showProgressBar = FALSE)

    rightResult2 <- local({ asdofiue <- data.frame(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L), c(50.6850781796545, 50.5332043428534, 54.0977874433753, 54.1191971222733, 54.4810426247368, 57.8659162756877, 56.4242825007141, 57.6004456916247, 57.7646054266088, 58.5028755051793), c(2, 2, 2, 1, 1, 1, 3, 3, 3, 3), c(64, 128, 192, 272, 340, 408, 406, 416, 468, 536.47049453318), c(11L, 24L, 39L, 56L, 75L, 96L, 119L, 144L, 171L, 200L), stringsAsFactors=FALSE); colnames(asdofiue) <- c('a', 'b', 'sim1', 'ind1', 'c', 'e'); rownames(asdofiue) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'); return (asdofiue); })

    errorLvl <- errorLvl + testAgainstAndReport(testName = "imputeDomainDriven (maxDist) -> Calc worked.", testResult = testResult2, rightResult = rightResult2, testNo = 3.6, stopOnError = FALSE)

    # testResult[nrow(testResult), 'c']    and    testResult2[nrow(testResult2), 'c']
    # must be equal because in both cases the same nearest observations were chosen.
    errorLvl <- errorLvl + testAgainstAndReport(testName = "testResult[nrow(testResult), 'c'] == testResult2[nrow(testResult2), 'c']", testResult = testResult[nrow(testResult), 'c'], rightResult = testResult2[nrow(testResult2), 'c'], testNo = 3.4, stopOnError = FALSE)

    # These two must not be equal because different nearestObs are chosen.
    if (testResult[1, 'c'] == testResult2[1, 'c']) {
      cat("------------------------------------\n")
      cat("*** FAIL *** - Test no. 3.5 failed: testResult[1, 'c'] != testResult2[1, 'c']\n")
      cat("------------------------------------\n")
      errorLvl <- errorLvl + 1
    } else {
      message("Test no. 3.5 passed successfully: testResult[1, 'c'] != testResult2[1, 'c']")
    }

    return(errorLvl)
  })




  #### Summary ####
  message("")

  if (errorLvl != 0)
    stop (paste0(
      "\n***************\n",
      "*** FAILURE ***\n",
      "***************\n",
      "See warnings above and check which tests were not successful."))

  message(paste0(
    "***************\n",
    "*** SUCCESS ***\n",
    "***************\n",
    "All tests passed successfully."
  ))
}
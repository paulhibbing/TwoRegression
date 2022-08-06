data(all_data, package = "TwoRegression")
all_data$PID <-
  rep(
    c("Test1", "Test2"),
    each = ceiling(nrow(all_data) / 2))[seq(nrow(all_data))]

fake_sed <- c("Lying", "Sitting")
fake_lpa <- c("Sweeping", "Dusting")
fake_cwr <- c("Walking", "Running")
fake_ila <- c("Tennis", "Basketball")

fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)

#### ACTIVITY ####

all_data$Activity <- c(
  "Walking", "Tennis", "Sitting", "Sitting", "Dusting", "Tennis",
  "Tennis", "Lying", "Tennis", "Sitting", "Sweeping", "Tennis",
  "Running", "Walking", "Sitting", "Sitting", "Basketball", "Sweeping",
  "Lying", "Running", "Lying", "Sitting", "Tennis", "Basketball",
  "Tennis", "Basketball", "Tennis", "Running", "Tennis", "Dusting",
  "Tennis", "Lying", "Tennis", "Dusting", "Basketball", "Running",
  "Running", "Walking", "Sweeping", "Lying", "Dusting", "Basketball",
  "Sweeping", "Sitting", "Basketball", "Walking", "Walking", "Tennis",
  "Walking", "Running", "Walking", "Running", "Sitting", "Lying",
  "Sitting", "Sitting", "Basketball", "Sitting", "Sitting", "Tennis",
  "Sitting", "Walking", "Basketball", "Tennis", "Sitting", "Tennis",
  "Sweeping", "Tennis", "Tennis", "Walking", "Dusting", "Lying",
  "Sweeping", "Running", "Running", "Tennis", "Sweeping", "Walking",
  "Sitting", "Sweeping", "Tennis", "Dusting", "Tennis", "Tennis",
  "Sitting", "Sweeping", "Sitting", "Lying", "Sweeping", "Sitting",
  "Tennis", "Sweeping", "Lying", "Basketball", "Running", "Running",
  "Sweeping", "Sitting", "Walking", "Basketball", "Basketball",
  "Basketball", "Tennis", "Sweeping", "Running", "Tennis", "Basketball",
  "Tennis", "Walking", "Walking", "Tennis", "Lying", "Running",
  "Sweeping", "Tennis", "Running", "Tennis", "Basketball", "Sweeping",
  "Walking", "Running", "Sitting", "Dusting", "Dusting", "Lying",
  "Dusting", "Dusting", "Tennis", "Running", "Dusting", "Tennis",
  "Sitting", "Tennis", "Sweeping", "Basketball", "Sweeping", "Tennis",
  "Dusting", "Walking", "Basketball", "Sitting", "Tennis", "Lying",
  "Basketball", "Running", "Basketball", "Running", "Running",
  "Basketball", "Sweeping", "Sweeping", "Lying", "Lying", "Walking",
  "Sitting", "Sweeping", "Dusting", "Walking", "Walking", "Tennis",
  "Dusting", "Basketball", "Running", "Sweeping", "Sitting", "Sweeping",
  "Running", "Sitting", "Lying", "Walking", "Tennis", "Running",
  "Running", "Lying", "Tennis", "Sweeping", "Walking", "Walking",
  "Sitting", "Running", "Sitting", "Tennis", "Tennis", "Walking",
  "Walking", "Basketball", "Walking", "Sweeping", "Running", "Sitting",
  "Tennis", "Sweeping", "Lying", "Dusting", "Walking", "Sweeping",
  "Lying", "Running", "Sitting", "Lying", "Sweeping", "Sitting",
  "Running", "Sweeping", "Sweeping", "Sweeping", "Lying", "Sitting",
  "Walking", "Tennis", "Basketball", "Tennis", "Basketball", "Walking",
  "Sweeping", "Basketball", "Basketball", "Dusting", "Running",
  "Sitting", "Dusting", "Lying", "Walking", "Sitting", "Sitting",
  "Lying", "Basketball", "Sitting", "Tennis", "Dusting", "Running",
  "Lying", "Lying", "Basketball", "Walking", "Sweeping", "Running",
  "Sweeping", "Walking", "Tennis", "Lying", "Sweeping", "Sweeping",
  "Running", "Sweeping", "Tennis", "Basketball", "Tennis", "Sweeping",
  "Tennis", "Sitting", "Dusting", "Tennis", "Tennis", "Basketball",
  "Dusting", "Basketball", "Tennis", "Dusting", "Running", "Basketball",
  "Walking", "Tennis", "Sitting", "Running", "Dusting", "Running",
  "Tennis", "Dusting", "Tennis", "Basketball", "Lying", "Sitting",
  "Running", "Lying", "Lying", "Tennis", "Sweeping", "Walking",
  "Tennis", "Tennis", "Running", "Sitting", "Sitting", "Walking",
  "Dusting", "Basketball", "Sitting", "Sweeping", "Sweeping", "Lying",
  "Tennis", "Sitting", "Dusting", "Walking", "Dusting", "Sitting",
  "Tennis", "Walking"
)

#### METs ####

all_data$fake_METs <- c(
  2.66672473901417, 7.23318527906667, 1.72214471013285, 1.75069225952029,
  1.66038223193027, 3.11961796064861, 4.58122248575091, 1.05812261020765,
  7.03602028742898, 1.59750472032465, 1.95467293076217, 7.56024237920064,
  3.71752810501494, 2.98252013779711, 1.72625453164801, 1.88820194453001,
  2.93325493088923, 1.85486620548181, 1.30818105419166, 3.0792160385754,
  1.55084714433178, 1.12575177242979, 6.37013906962238, 3.73074738204014,
  5.74268461344764, 4.2522067137761, 3.49342965288088, 5.67328538722359,
  5.30764904816169, 1.05671559646726, 2.68570268538315, 1.04686651146039,
  3.3746611479437, 1.29967748862691, 7.8339824139839, 7.23999336152337,
  7.76169029378798, 3.60197101242375, 1.64664318272844, 1.05927334772423,
  1.28932650573552, 4.75831096328329, 1.89562822622247, 1.3019809667021,
  5.36185154982377, 6.20905089774169, 4.29343683843035, 6.75049936398864,
  4.68702826532535, 4.47870654566213, 7.8583929662127, 6.53807833313476,
  1.97847025399096, 1.34069868247025, 1.35010042111389, 1.46463822154328,
  7.17832860408816, 1.3275396048557, 1.49480208847672, 6.58579322998412,
  1.22416966455057, 7.39576172258239, 5.78827488061506, 4.48746366496198,
  1.74933720612898, 3.77941707207356, 1.75739053636789, 6.54428913281299,
  3.77451966446824, 3.33450914290734, 1.45451044477522, 1.56801706715487,
  1.70656277774833, 7.12536925787572, 5.1013208766235, 7.87983602751046,
  1.913558536442, 7.09212028945331, 1.67480153939687, 1.83815241418779,
  2.90188868506812, 1.52251407760195, 6.83974280662369, 6.06933555949945,
  1.59202881110832, 1.48840508027934, 1.83677420276217, 1.94635729072616,
  1.0258150913287, 1.14181977300905, 4.52658732607961, 1.35484644048847,
  1.50526135903783, 7.32593546516728, 5.06417409959249, 4.07270682579838,
  1.93429747712798, 1.39225380774587, 6.1016286680242, 6.76576847140677,
  3.91566071601119, 3.19602561939973, 6.39274211600423, 1.96488203108311,
  7.75404184719082, 4.3200246427441, 6.02631393482443, 6.50644777796697,
  4.74006169242784, 3.88388354855124, 2.7631398725789, 1.65406468231231,
  4.39692054130137, 1.94148094626144, 5.30588698841166, 6.21963303722441,
  2.86843520856928, 7.35283303260803, 1.3568156468682, 3.7096225292189,
  7.85249802388716, 1.77188541251235, 1.49460343131796, 1.36802237504162,
  1.9739146491047, 1.53145678294823, 1.35823613987304, 3.27402503637131,
  7.57142899185419, 1.48172793770209, 6.30097270198166, 1.23430477268994,
  5.23423282953445, 1.23818861180916, 6.18402588646859, 1.34685591072775,
  6.70398516277783, 1.76803416037001, 4.48703640839085, 4.43123664718587,
  1.02956243860535, 6.04739845613949, 1.11676378757693, 4.50288413045928,
  6.98142125387676, 6.66005649219733, 6.17080181883648, 5.97126687259879,
  6.85344282188453, 1.94846084085293, 1.60673147556372, 1.4420006503351,
  1.32931217295118, 7.29634021618403, 1.65594987291843, 1.19646221236326,
  1.84465418406762, 7.39454933791421, 7.91657420841511, 3.80221074260771,
  1.07794396812096, 3.400792702334, 4.71852922823746, 1.2505227883812,
  1.63103374792263, 1.41723923175596, 7.80032642837614, 1.72900459868833,
  1.23666297108866, 6.84775806183461, 6.50812830904033, 3.55303734727204,
  7.43816076172516, 1.31190578569658, 4.31055791431572, 1.33770488598384,
  7.5591901014559, 5.81437072379049, 1.55423520831391, 4.22730720590334,
  1.71471353806555, 3.56723295920528, 7.8947759635048, 7.10114947450347,
  4.66798986843787, 7.2432916975813, 5.71994239999913, 1.65471476316452,
  6.91699427424464, 1.82227830705233, 6.87373275554273, 1.05751400231384,
  1.91160896513611, 1.93353735539131, 7.45708800526336, 1.34114637924358,
  1.48517161072232, 6.4604625053471, 1.22017805627547, 1.75953282625414,
  1.05626828270033, 1.36960467952304, 4.96407534915488, 1.82171229971573,
  1.18541230331175, 1.05829182174057, 1.56877130270004, 1.94439610047266,
  4.84706607519183, 5.07359515654389, 3.07906680367887, 5.40756158728618,
  4.81729449320119, 3.14073890261352, 1.07252291031182, 6.36409088934306,
  6.03439652232919, 1.38802891341038, 4.93564442894422, 1.89816915988922,
  1.75804603775032, 1.97884176718071, 2.7550190895563, 1.2740564357955,
  1.95553203229792, 1.16815624758601, 5.60737536451779, 1.40556863439269,
  5.21172047429718, 1.3849121152889, 7.4743098145118, 1.61059514223598,
  1.67909154645167, 3.43589904194232, 5.53210015734658, 1.50547492364421,
  4.16985467122868, 1.90676785679534, 3.118739853031, 7.93989444873296,
  1.3132599692326, 1.75101568643004, 1.36061849840917, 6.64396988519002,
  1.27525872550905, 5.84326307382435, 4.12432935577817, 7.45385540847201,
  1.10603717924096, 6.62445686606225, 1.0875563537702, 1.18499893881381,
  4.57122845633421, 4.58830377238337, 3.29762972937897, 1.75910372287035,
  5.32672229572199, 2.938858881942, 1.2110281707719, 4.1146274714265,
  3.6464822149137, 6.73267040133942, 3.8375827758573, 1.71153728594072,
  3.48833240999375, 1.85188435483724, 3.71396497427486, 7.27265170612372,
  1.37023363215849, 6.82460549019743, 7.8991714612348, 1.30310410703532,
  1.31914384826086, 7.15073177323211, 1.90455233561806, 1.25494101294316,
  7.67369341931771, 1.61670214869082, 2.98176292865537, 5.87899284285959,
  4.42492844362278, 5.48337018711027, 1.38075097859837, 1.57012499379925,
  6.99849049234763, 1.87226536334492, 6.28854220779613, 1.91656973282807,
  1.89168270793743, 1.2744196197018, 1.56199758988805, 3.89779403165448,
  1.42085323459469, 1.98124168580398, 3.64089312229771, 1.8503722907044,
  1.71660659555346, 4.43266413791571, 4.3506061459193
)

#### MAKE THE MODEL ####
ex_2rm <- fit_2rm(
  data = all_data,
  activity_var = "Activity",
  sed_cp_activities = c(fake_sed, fake_lpa),
  sed_activities = fake_sed,
  sed_cp_var = "ENMO",
  sed_METs = 1.25,
  walkrun_activities = fake_cwr,
  walkrun_cp_var = "ENMO_CV10s",
  met_var = "fake_METs",
  walkrun_formula = "fake_METs ~ ENMO",
  intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
)


test_that("summary method works as expected", {

  save_2rm <- function(code) {
    path <- tempfile(fileext = ".rds")
    saveRDS(code, path)
    path
  }

  testthat::expect_snapshot_file(
    save_2rm(
      summary(
        ex_2rm,
        subject_var = "PID",
        MET_var = "fake_METs",
        activity_var = "Activity"
      )
    ),
    "summary2rm.rds"
  )


})

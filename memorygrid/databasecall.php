<?php
// Adds MySQL support for experiment
//check which domain experiment is hosted on

// CONSTANTS
$ROUNDS = 14;
$BONUS_ROUNDS=1;


function curPageURL() {
 $pageURL = 'http';
 if ($_SERVER["HTTPS"] == "on") {$pageURL .= "s";}
 $pageURL .= "://";
 if ($_SERVER["SERVER_PORT"] != "80") {
  $pageURL .= $_SERVER["SERVER_NAME"].":".$_SERVER["SERVER_PORT"].$_SERVER["REQUEST_URI"];
 } else {
  $pageURL .= $_SERVER["SERVER_NAME"].$_SERVER["REQUEST_URI"];
 }
 return $pageURL;
}

//For production
$curURL = curPageURL();
//For debugging
//$curURL = 'http://localhost/memorygrid/databasecall.php?action=initializeDB&iter=10&pass=reproducingkernelhilbertspace';
// initializeDB('localhost', 'root', 'mysql0_rT', 'memorygrid', 10,$ROUNDS,$BONUS_ROUNDS);

$parse = parse_url($curURL);
$domain = $parse['host'];


if ($domain == 'localhost'){
	//MAMP MySQL settings
	$host = 'localhost';
	$user = 'root';
	$pass = 'mysql0_rT';
	$dbname = 'memorygrid';
}
elseif ($domain == 'experiments.hmc-lab.com'){
	$host = 'localhost';
	$user = 'technical_user';
	$pass = 'password_placeholder';
	$dbname = 'memorygrid';
}



//	WHICH FUNCTION TO PERFORM? ////
if( isset($_GET['action']) ){
    $action = $_GET['action'];

    //1. Intialize the database with $iter iterations
    // databasecall.php?action=initializeDB&iter=350&pass=reproducingkernelhilbertspace
    if ($action == 'initializeDB'){
    	$iterations = $_GET['iter'];
    	$handshake = $_GET['pass'];
    	if ($handshake=='reproducingkernelhilbertspace'){
    		// echo "call initDB";
    		initializeDB($host, $user, $pass, $dbname, $iterations, $ROUNDS,$BONUS_ROUNDS);
    	}

    }
    //2. Assign a scenario
    elseif ($action == 'assignScenario'){
    	assignScenario($host, $user, $pass, $dbname,($ROUNDS+$BONUS_ROUNDS));
    }
    //localhost/memorygrid/databasecall.php?action=initProlificExp&PROLIFIC_PID=prid&STUDY_ID=stid&SESSION_ID=seid
    elseif ($action == 'initProlificExp') {
    	if( isset($_GET['PROLIFIC_PID']) && isset($_GET['STUDY_ID']) && isset($_GET['SESSION_ID'])){
    		$prolific_ID = $_GET['PROLIFIC_PID'];
    		$study_ID = $_GET['STUDY_ID'];
    		$session_ID = $_GET['SESSION_ID'];

    		assignScenarioProlific($host, $user, $pass, $dbname,$ROUNDS,$BONUS_ROUNDS, $prolific_ID, $study_ID, $session_ID);
    	}
    }
}
elseif ($_SERVER['REQUEST_METHOD'] === 'POST'){
  //3. Complete scenario
  $UID = $_POST['UID'];
	$scale = $_POST['scale'];
	$envOrder= $_POST['envOrder'];
	$searchHistory= $_POST['searchHistory'];
	$reward = $_POST['reward'];
	$processDescription = $_POST['processDescription'];
	completeScenario($host, $user, $pass, $dbname, $scale, $envOrder, $searchHistory, $reward, $processDescription, $UID);

}


function generateScenarios($n_scenarios,$rounds,$bonusRounds,$bonus_seed){
	//$bonus_seed is used for randomly assigning bonus rounds, even if the initial created ones are full and equally distributed
	// echo $bonus_seed;
	$scenarioArr = array();
	$bonusToggle=0;
	for($iter = 0; $iter < $n_scenarios; $iter ++){
		$conditionArr = array();
		$currentRound = 0;
		while($currentRound < $rounds) {
			// Careful, somehow leading 0s get removed in mysql, even for varchar
			array_push($conditionArr,1);
			$currentRound++;
			array_push($conditionArr,2);
			$currentRound++;
		} 
		shuffle($conditionArr);
		if ($bonus_seed==0) {
			if ($iter % 2 == 0) {
				$bonusToggle=1;
			} else {
				$bonusToggle=2;
			}
		} else {
			if ($bonus_seed % 2 == 0) {
				$bonusToggle=1;
			} else {
				$bonusToggle=2;
			}
		}
		
		
		
		array_push($conditionArr,$bonusToggle);

		$value = array('scenario' => implode("",$conditionArr), 'kernel' => 1); // 1 is smooth kernel	
		array_push ($scenarioArr, $value);
	};

	return $scenarioArr;
}


///////////initial creation of scenarios///////////

function initializeDB($host, $user, $pass, $dbname, $iterations,$ROUNDS,$BONUS_ROUNDS){
	$conn = new mysqli($host, $user, $pass, $dbname);
	if ($conn->connect_error) {
		die("Connection failed: " . $conn->connect_error);
	};
	
	$scenarioArr=generateScenarios($n_scenarios=$iterations,$rounds=$ROUNDS,$bonusRounds=$BONUS_ROUNDS,$bonus_seed=0);

	//Check if scenarios table exists
	$querycheck =  "SELECT 1 FROM scenarios2D";
	$query_result=mysqli_query($conn, $querycheck);

	//if table exists
	if ($query_result !== FALSE){
		//wipe table
		$deleteRows = "TRUNCATE TABLE scenarios2D";
		if (mysqli_query($conn,$deleteRows)) {
	    	echo "db table wiped";
		}
		else {
	    	echo "Error wiping table: " . $conn->error;
		}
	}
	//if table does not exist
	else{
		//Create scenarios table
		$createScenarios = "CREATE TABLE scenarios2D(
		id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
		prolific_ID VARCHAR(255) NULL,
		study_ID VARCHAR(255) NULL,
		session_ID VARCHAR(255) NULL,
		scenario VARCHAR(255) NULL,
		kernel INT(2) NULL,
		scale BLOB,
		envOrder BLOB,
		searchHistory BLOB,
		reward DECIMAL(3,2) DEFAULT 0.00,
		start DATETIME DEFAULT NULL,
		end DATETIME DEFAULT NULL,
		age INT(3) NULL,
		gender INT(2) NULL,
		education VARCHAR(255) NULL,
		processDescription BLOB,
		assigned INT(2) DEFAULT 0,
		completed INT(2) DEFAULT 0
		)
		";

  	if (mysqli_query($conn, $createScenarios)) {
  	    echo "Table created successfully";
  	}
    else {
  	    echo "Error creating table: " . $conn->error;
  	}
	}

	//fill scenario table with data
	foreach($scenarioArr as $condArr){
		$scenario = $condArr['scenario'];
		$kernel = $condArr['kernel'];
		$assigned = 0;
		$completed = 0;
		$sql = "INSERT INTO scenarios2D (scenario, kernel,  assigned, completed)
		VALUES ( $scenario, $kernel, $assigned, $completed)";
		if ($conn->query($sql) === TRUE) {
	} 	else {
	    	echo "Error: " . $sql . "<br>" . $conn->error;
	}
	}

	//check number of entries
	$countQuery = "SELECT COUNT(*) FROM scenarios2D";
	$countResult = mysqli_query($conn,$countQuery);
	$count = mysqli_fetch_row($countResult);
	echo $count[0];
	$conn->close();

}



////////////mark scenario as completed/////////////////
function completeScenario($host, $user, $pass, $dbname, $scale, $envOrder, $searchHistory, $reward, $processDescription, $UID){
	//connect to database
	$conn = new mysqli($host, $user, $pass, $dbname);
	// Check connection
	if ($conn->connect_error) {
    	die("Connection failed: " . $conn->connect_error);
	}
	$processDescription = mysqli_real_escape_string($conn, $processDescription);
	$query = $conn->prepare("UPDATE scenarios2D set end=now(), scale=?,  envOrder = ?, searchHistory=?, reward = ?, processDescription=?, completed=completed + 1 WHERE id=?");
	$query -> bind_param("sssdsi", $scale, $envOrder, $searchHistory, $reward, $processDescription, $UID);
	if ($result = $query->execute()){
  	echo "success";
  	$query->free_result();
	}
	else {
  	echo "error";
	}
	$conn->close();

}


/////////retrieve random scenario that has not yet been assigned////////
function assignScenarioProlific($host, $user, $pass, $dbname,$ROUNDS,$BONUS_ROUNDS, $prolific_ID, $study_ID, $session_ID){
	//connect to database
	$conn = new mysqli($host, $user, $pass, $dbname);
	// Check connection
	if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
	} ;
	//1:randomly find a scenario where assigned<1 and completed <1
	$query = "SELECT * FROM scenarios2D WHERE assigned<1 AND completed<1 ORDER BY RAND() LIMIT 1";
	$result = mysqli_query($conn, $query);
	if ($result->num_rows !== 0){
		$scenarioRow = $result->fetch_array(MYSQLI_ASSOC);
		$UID = $scenarioRow['id'];
		$scenario = $scenarioRow['scenario'];
		$kernel = $scenarioRow['kernel'];

		//Escape query params to avoid SQL injections
		$prolific_ID = mysqli_real_escape_string($conn, $prolific_ID);
		$study_ID = mysqli_real_escape_string($conn, $study_ID);
		$session_ID = mysqli_real_escape_string($conn, $session_ID);
		
		//update 'assigned' to 1
		$update = $conn -> prepare("UPDATE scenarios2D SET start=now(), assigned=assigned + 1,prolific_ID=?,study_ID=?,session_ID=? WHERE id=?");
		$update -> bind_param("sssi",$prolific_ID,$study_ID,$session_ID, $UID);
		if ($result = $update->execute()){
		  	$update->free_result();
		}
		echo json_encode(array('UID' => $UID, 'scenario'=>$scenario, 'kernel'=>$kernel));
		$conn->close();
	}
	//2: if all scenarios are assigned, create a new one
	else{
		$result->close();

		$countquery = "SELECT * FROM scenarios2D";
		$countresult = mysqli_query($conn, $countquery);
		$bonus_seed=mysqli_num_rows($countresult);
		$countresult->close();

		$scenarioArr=generateScenarios($n_scenarios= 1, $rounds= $ROUNDS,$bonusRounds=$BONUS_ROUNDS,$bonus_seed=$bonus_seed);

		//fill scenario table with data
		foreach($scenarioArr as $condArr){
			$scenario = $condArr['scenario'];
			$kernel = $condArr['kernel'];
			$assigned = 0;
			$completed = 0;
			$sql = "INSERT INTO scenarios2D (scenario, kernel,  assigned, completed)
			VALUES ( $scenario, $kernel, $assigned, $completed)";
			if ($conn->query($sql) === TRUE) {
			} 	else {
			    	echo "Error: " . $sql . "<br>" . $conn->error;
			}
		}

		$query = "SELECT * FROM scenarios2D WHERE assigned<1 AND completed<1 ORDER BY RAND() LIMIT 1";
		$result = mysqli_query($conn, $query);
		if ($result->num_rows !== 0){
			$scenarioRow = $result->fetch_array(MYSQLI_ASSOC);
			$UID = $scenarioRow['id'];
			$scenario = $scenarioRow['scenario'];
			$kernel = $scenarioRow['kernel'];
			//update 'assigned' to 1
			$update = $conn -> prepare("UPDATE scenarios2D SET start=now(), assigned=assigned + 1,prolific_ID=?,study_ID=?,session_ID=? WHERE id=?");
			$update -> bind_param("sssi",$prolific_ID,$study_ID,$session_ID, $UID);
		
			if ($result = $update->execute()){
			  	$update->free_result();
			}
			echo json_encode(array('UID' => $UID, 'scenario'=>$scenario, 'kernel'=>$kernel));
			$conn->close();
		}
		else{
			$result->close();
		}

	}


}





?>

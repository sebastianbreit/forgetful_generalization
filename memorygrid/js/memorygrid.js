////////////////////////////////////////////////////////////////////////
//            JS-CODE FOR 2D ACTIVE FUNCTION LEARNING                 //
//                      AUTHORS: SEBASTIAN BREIT                      //
//           BASED ON WORK FROM: ERIC SCHULZ, CHARLEY WU              //
////////////////////////////////////////////////////////////////////////

//Constant values - used for DB
const KERNEL_ROUGH_11x11 = 0;
const KERNEL_SMOOTH_11x11 = 1;
const KERNEL_SMOOTH_8x8 = 2;
//Mysql had problems with 0,1 vector, automatically dropping leading 0s
const COND_NORMAL = 1;
const COND_MEMORY = 2;

//Experiment configuration
const GRIDSIZE = 11;
const MS_CELL_SHOWN = 400;
const MS_FIXATION_SHOWN = 2000;

const DEV_SWITCH_REWARD_BASELINE=1; //0= normal reward calc, 1= with substracting baseline of random performance

const BASE_FEE=3.00;
const PROLIFIC_COMPLETION_CODE="454DE7DA";


const CLICKS_PER_GRID = 25; //25
const NUMBER_TRIALS = 14; //14
const BONUS_ROUNDS=1; //1
const BONUS_CHOICES=10; //10

const INSTRUCTION_COUNT=6;
const EXAMPLE_COUNT=6;

//EXPERIMENT PARAMETERS
var fullurl = document.location.href, //url of incoming MTurk worker
  trials = NUMBER_TRIALS, 
  bonusTrials = BONUS_ROUNDS, 
  trialCounter = 0, 
  bonusCounter = 0, 
  exampleCounter=0,
  tracker = new Array(0), //Tracks selected cells each round
  currentClick=0,
  scoretotal = new Array(trials+bonusTrials).fill(0),
  scorecurrent = 0,
  totalReward = 0.00,
  randomGridAverages,
  envOrder = getRandomSubarray([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19], (trials+bonusTrials)),
  currentEnv = envOrder[0],
  choiceOptions = generateCellOptions(),
  afc_options=Array(0),
  confidenceSliderChanged=false,
  estimateSliderChanged=false,
  afc_switch=false,

  // Randomly scale max value between 60-80
  scale = new Array(trials+bonusTrials).fill(null).map(() => (randomNum(60, 80))), 

  //Color parameters for heatmap
  colors = ['#fff7ec', '#fee8c8', '#fdd49e', '#fdbb84', '#fc8d59', '#ef6548', '#d7301f', '#b30000', '#7f0000'],
  heatmapColor = d3.scale.linear().domain(d3.range(0, 90, 90.0 / (colors.length - 1))).range(colors);




//data collectors for search history
var xcollect = [],
  ycollect = [],
  zcollect = [],
  zcollectScaled = [],
  bonuscollect = [],
  bonusSelectioncollect = null;

//timeout for memory condition
var cellInfoTimeouts = {};

//Populate data collectors with empty arrays
for (var i = 0; i < (trials+bonusTrials); i++) {
  xcollect[i] = Array.apply(null, Array(0)).map(Number.prototype.valueOf, -99);
  ycollect[i] = Array.apply(null, Array(0)).map(Number.prototype.valueOf, -99);
  zcollect[i] = Array.apply(null, Array(0)).map(Number.prototype.valueOf, -99);
  zcollectScaled[i] = Array.apply(null, Array(0)).map(Number.prototype.valueOf, -99);
}

//Declare variables not yet assigned
var UID, // Unique ID assigned to a participant
  scenarioArr, // Array with memory conditions, within participant assignment, randomized
  currentScenario, // Condition of a given round, within participant assignment
  kernel, // Whether smooth or rough environment
  searchHistory,
  kernelFunctionList, // list of 20 kernel-functions, envOrder determines which of these are used
  trialReward,
  xout,
  yout,
  zout;

//Randomly revealed location at start of each trial
var initialObservation = [];


// ################################################################################################################################
// ######################################### Initializing the Experiment, going over instructions  ################################
// ################################################################################################################################

//Access the MySQL database and assigns the participant a scenario under a unique UID
function assignScenario() {

  // //For prolific
  var prolific_ID=getQueryStringParams('PROLIFIC_PID');
  var study_ID=getQueryStringParams('STUDY_ID');
  var session_ID=getQueryStringParams('SESSION_ID');

  if(prolific_ID==null || study_ID==null || session_ID==null){
    alert("Oops, something went wrong. Please try to restart the experiment from Prolific.");
    window.location.href = 'experiment.html';
  }

  var jsonResponse;

    $.get(
      //http://localhost/memorygrid/experiment.html?PROLIFIC_PID=prid&STUDY_ID=stid&SESSION_ID=seid
      'databasecall.php?action=initProlificExp&PROLIFIC_PID='+ prolific_ID +'&STUDY_ID='+study_ID+'&SESSION_ID='+session_ID
    ) 
    .done( function(data,status) {
      jsonResponse=JSON.parse(data);
      console.log(status);

      UID = parseInt(jsonResponse['UID']);
      scenarioArr = convertScenarioStringToÍntArray(jsonResponse['scenario']);
      currentScenario = scenarioArr[0];
      // bonusScenario = scenarioArr[NUMBER_TRIALS-1];

      clicks = CLICKS_PER_GRID; //set initial number of clicks
      change("remaining_clicks", "Number of clicks remaining <b>" + clicks + "</b>");

      var kernelFiles = ['kernelRough_11x11.json', 'kernelSmooth_11x11.json', 'kernelSmooth_8x8.json'];

      $.getJSON(kernelFiles[KERNEL_SMOOTH_11x11], function(data) {
        kernelFunctionList = data;

        //DISABLED AUTO-GENERATED EXAMPLES, using hand-picked ones
        // createExamples();
      });
    })
    .fail( function(xhr, textStatus, errorThrown) {
        alert(xhr.responseText);
    });
}

//Revealing examples and continuing to attention questions
function showExample() {
  //first click
  if(exampleCounter<EXAMPLE_COUNT-1){
    var prev_ins = 'instructions_' + (exampleCounter+1);
    var next_ins = 'instructions_' + (exampleCounter+2);

    var prev_ex = 'example' + (exampleCounter+1),
      next_ex = 'example' + (exampleCounter+2);
    clickStart(prev_ins, next_ins);
    clickStart(prev_ex, next_ex);

    if(exampleCounter==3+(INSTRUCTION_COUNT-1)){
      change('exampleButton', 'Start Task');
    }

    scrollTop();
  }
  else{
    clickStart('page2', 'page3');
  }
  exampleCounter = exampleCounter + 1; //increment counter
}

//Logic for evaluating the attention check
function instructioncheck() {
  if (document.getElementById('radioq1b').checked) {
    var ch1 = 1;
  }
  //none condition specific questions
  if (document.getElementById('radioq2c').checked) {
    var ch2 = 1;
  }
  if (document.getElementById('radioq3a').checked) {
    var ch3 = 1;
  }
  if (document.getElementById('radioq5d').checked) {
    var ch5 = 1;
  }
  var checksum = ch1 + ch2 + ch3  + ch5;

  if (checksum === 4) {
    clickStart('page3', 'page4');
  } else {
    change('error', "You have answered some of the questions wrong. Please try again.");
    $('#errorModal').modal('toggle');

    clickStart('instructions_6','instructions_1');
    clickStart('example6','example1');
    change('exampleButton', 'Next Page');

    exampleCounter=0;
    clickStart('page3', 'page2');
  }
}


function drawFromKernel(kernel) {
  var t = 0;
  for (k = 0; k <= (GRIDSIZE*GRIDSIZE-1); k++) {
    if (kernel[k].y >= 0 && t < 1) {

      var xout = [kernel[k].x1];
      var yout = [kernel[k].x2];
      var zout = [kernel[k].y * 100];
      t = t + 1;

    } else if (kernel[k].y >= 0 && t >= 1) {

      xout = xout.concat([kernel[k].x1]);
      yout = yout.concat([kernel[k].x2]);
      zout = zout.concat([kernel[k].y] * 100);
      t = t + 1;

    }
  }
  var choose = Math.floor(Math.random() * xout.length);
  return ([xout[choose], yout[choose], zout[choose]]);
}

function initializeExp() {
  
  randomGridAverages= calcGridRewardBaselines(kernelFunctionList);

  initialObservation = drawFromKernel(kernelFunctionList[envOrder[0]]);

  var noiseyValue = Math.round(initialObservation[2] + myNorm()), //add noise
    rescaledValue = Math.max(Math.round(noiseyValue / 100 * scale[trialCounter] + 5), 0); //rescale value to randomly generated ceiling
  
  $startCell = $('#' + initialObservation[0] + 'x' + initialObservation[1]);
  revealCell(cell=$startCell,rescaledValue=rescaledValue,rescaledColor=heatmapColor(rescaledValue));
  //add to history
  $startCell.attr('data-history', rescaledValue.toString());

  tracker[0] = $startCell.attr('id');

  if (currentScenario == COND_MEMORY) {
    addNewCellTimerToList($startCell,initial=true);
  }

  //store initial values
  xcollect[trialCounter][0] = parseInt(initialObservation[0], 10);
  ycollect[trialCounter][0] = parseInt(initialObservation[1], 10);
  zcollect[trialCounter][0] = noiseyValue;
  zcollectScaled[trialCounter][0] = rescaledValue;
  //calculate points for both payoff conditions
  scoretotal[trialCounter] = rescaledValue;
  //Update text
  change('scoretotal', "Current Score: " + scoretotal[trialCounter]);

  change("remaining_grids", "Number of grids left: <b>" + (trials+bonusTrials) + "</b>");
  //update number of clicks
  change("remaining_clicks", "Number of clicks left: <b>" + clicks + "</b>");

  updateExperimentInstructions(currentScenario);
  setScenarioHeader(currentScenario);
}




// ################################################################################################################################
// ######################################### Actual Experiment ####################################################################
// ################################################################################################################################

$(document).ready(function() {

  initializeGrid();
  initializeFixationGrid();

  $('#grid * td').click( 
    function() {
      clickResults = onGridClick($(this));

      currentClick = currentClick + 1, tracker[currentClick] = clickResults.id, clicks = clicks - 1;
      
      evaluateClick(clickResults.id, clickResults.x, clickResults.y, clickResults.noiseyValue, clickResults.rescaledValue);
    });
});

function onGridClick(cell_object) {
  var $this = cell_object,
    id = $this.attr('id'),
    x, y, absoluteValue, noiseyValue, rescaledValue;
  // x and y are the coordinates of the cell in the grid
  // with the origin in the top left corner
  x = id.split('x')[0];
  y = id.split('x')[1];
  currentEnvNum = envOrder[trialCounter];
  for (k = 0; k <= (GRIDSIZE*GRIDSIZE-1); k++) { 
    if (kernelFunctionList[currentEnvNum][k].x1 == x && kernelFunctionList[currentEnvNum][k].x2 == y) {
      absoluteValue = kernelFunctionList[currentEnvNum][k].y * 100; 
      noiseyValue = Math.round(absoluteValue + myNorm()); 

      rescaledValue = Math.max(Math.round(noiseyValue / 100 * scale[trialCounter] + 5), 0); 
    }
  }

  if (currentScenario === COND_MEMORY) {
    // if ($this.html() !== "&nbsp") {
      hideCell($this);
    // }
  }

  if (document.getElementById(id).hasAttribute("data-history") == true) {
    var history = JSON.parse("[" + document.getElementById(id).getAttribute("data-history") + "]");
    history.push(rescaledValue);
    
    document.getElementById(id).setAttribute("data-history", history.toString());

    revealCell(cell=$this,rescaledValue=rescaledValue,rescaledColor=heatmapColor(Math.round(average(history))));

  } else {
    
    document.getElementById(id).setAttribute("data-history", rescaledValue.toString());

    revealCell(cell=$this,rescaledValue=rescaledValue,rescaledColor=heatmapColor(rescaledValue));
  }

  if (currentScenario === COND_MEMORY) {
    if (cellInfoTimeouts[$this.attr('id')]) {
      clearTimeout(cellInfoTimeouts[$this.attr('id')]);
    }
    addNewCellTimerToList($this);
  }


  return {
    id,
    x,
    y,
    noiseyValue,
    rescaledValue
  };
}

function evaluateClick(id, x, y, noiseyValue, rescaledValue) {
  xcollect[trialCounter][currentClick] = parseInt(x, 10);
  ycollect[trialCounter][currentClick] = parseInt(y, 10);
  zcollect[trialCounter][currentClick] = noiseyValue;
  zcollectScaled[trialCounter][currentClick] = rescaledValue; //store absolute value
  
  change("remaining_clicks", "Number of clicks left: <b>" + clicks + "</b>");
  scorecurrent = Math.round(rescaledValue);
  scoretotal[trialCounter] = scoretotal[trialCounter] + scorecurrent;
  
  if(DEV_SWITCH_REWARD_BASELINE){
    totalReward=rewardCumWithBaseline(scoretotal,trialCounter,randomGridAverages);
    trialReward=singleTrialRewardWithBaseline(scoretotal[trialCounter],scale[trialCounter],randomGridAverages[trialCounter],CLICKS_PER_GRID);
  }
  else{
    totalReward = rewardCum(scoretotal);
    trialReward = singleTrialReward(scoretotal[trialCounter], scale[trialCounter]);
  }
  change('scoretotal', "Current Score: " + scoretotal[trialCounter]);

  if (trialCounter < NUMBER_TRIALS) { 
    if (clicks == 0) { 
      trials = trials - 1;
      disableGridClicks();
      // collapseExperimentInstructions();
      // hideScenarioHeader();
      showGridFinishedInstructions(toFixed(trialReward*100),toFixed(totalReward*100),(trials+bonusTrials), scenarioArr[trialCounter+1]);
    }

  }
  // BONUS ROUND
  else if (trialCounter>=NUMBER_TRIALS && trialCounter<NUMBER_TRIALS+BONUS_ROUNDS){
    if (clicks == Math.round(CLICKS_PER_GRID/2, 0)) {
      disableGridClicks();
      // Show intermediate instructions + start bonus round
      showBonusEstimateInstructions(toFixed(trialReward*100),toFixed(totalReward*100),(trials+bonusTrials), scenarioArr[trialCounter],bonusJudgements=BONUS_CHOICES);
    }
    else if (clicks == (Math.round(CLICKS_PER_GRID/2, 0)-1)){ // AFC click 
      bonusSelectioncollect={
        x:xcollect[trialCounter][currentClick],
        y:ycollect[trialCounter][currentClick],
        z:zcollect[trialCounter][currentClick]
      };
      hideAFCinstructions(afc_options);
      enableGridClicks();
    }
    else if (clicks == 0){ 
      bonusTrials = bonusTrials - 1;
      disableGridClicks();
      // hideScenarioHeader();
      showGridFinishedInstructions(toFixed(trialReward*100),toFixed(totalReward*100),(trials+bonusTrials), scenarioArr[trialCounter+1]);
    }
  } 
}


function nexttrial() {
  if ((trials+bonusTrials)>0) 
  {
    enableGridClicks();
    trialCounter = trialCounter + 1;

    clearCell($('#' + initialObservation[0] + 'x' + initialObservation[1]));
    
    //get rid of revealed output
    for (h = 0; h < tracker.length; h++) {
      clearCell($('#' + tracker[h]));
    }
    tracker = new Array(0);

    if (trialCounter <= envOrder.length) { 
      initialObservation = drawFromKernel(kernelFunctionList[envOrder[trialCounter]]);
    }
    var noiseyValue = Math.round(initialObservation[2] + myNorm()),
      rescaledValue = Math.max(Math.round(noiseyValue / 100 * scale[trialCounter] + 5), 0);
    
    //get first point on grid for next trial
    $startCell = $('#' + initialObservation[0] + 'x' + initialObservation[1]);
    revealCell(cell=$startCell,rescaledValue=rescaledValue,rescaledColor=heatmapColor(rescaledValue));
    
    //add to history
    $startCell.attr('data-history', rescaledValue.toString());

    tracker[0] = $startCell.attr('id');

    currentScenario= scenarioArr[trialCounter];
    updateExperimentInstructions(currentScenario);

    if (currentScenario == COND_MEMORY) {
      addNewCellTimerToList($startCell,initial=true);
    }

    //store initial values
    xcollect[trialCounter][0] = parseInt(initialObservation[0], 10);
    ycollect[trialCounter][0] = parseInt(initialObservation[1], 10);
    zcollect[trialCounter][0] = noiseyValue;
    zcollectScaled[trialCounter][0] = rescaledValue; //store noisey value
    scoretotal[trialCounter] = rescaledValue;

    //renew investigations
    clicks = CLICKS_PER_GRID;
    currentClick = 0;
    
    showNextGrid(trials+bonusTrials,clicks,scoretotal[trialCounter]);
    setScenarioHeader(currentScenario);
  }
  else{
    clickStart('page5', 'page6');
  }
}


// ################################################################################################################################
// ######################################### Bonus Round: Confidence judgements  ##################################################
// ################################################################################################################################
function confidenceSliderChange(value){
  // bonusCcollect[bonusCounter]=value;
  confidenceSliderChanged=true;
}

function estimateSliderChange(value){
  document.getElementById('output').innerHTML=value + ' points';
  // bonusEcollect[bonusCounter]=value;
  estimateSliderChanged=true;

  $currentEstimate=$('#' + afc_options[bonusCounter]);  
  revealCell(cell=$currentEstimate,rescaledValue=value,rescaledColor=heatmapColor(value));

}

function startBonusEstimate(){
  //TODO highlight 10 unchosen locations one by one
  var novelOptions = generateCellOptions(excludeChosen=tracker);
  afc_options= getRandomSubarray(novelOptions,BONUS_CHOICES);

  startBonusOverlay();
  
  $startCell = $('#' + afc_options[bonusCounter]);
  $startCell.addClass("border-blink");
}

function nextBonusEstimate(){
  $previousCell = $('#' + afc_options[bonusCounter]); 
  $previousCell.removeClass("border-blink");

  bonuscollect[bonusCounter]={
    x:afc_options[bonusCounter].split('x')[0],
    y:afc_options[bonusCounter].split('x')[1],
    est:document.getElementById('estimateSlider').value,
    conf:document.getElementById('confidenceSlider').value
  };

  clearCell($previousCell);
  resetSliders();

  bonusCounter=bonusCounter+1;
  if (bonusCounter<BONUS_CHOICES) {
    $currentCell = $('#' + afc_options[bonusCounter]); 
    $currentCell.addClass("border-blink");
  }
  else{ // IF WE WANT AFC
    showAFCinstructions(afc_options);
  }


}





// ################################################################################################################################
// ######################################### Complete experiment and submit the data ##############################################
// ################################################################################################################################

function senddata() { 

  var processDescription = encodeURIComponent(document.getElementById('processDescription').value);

  searchHistory = {
    'xcollect': xcollect,
    'ycollect': ycollect,
    'zcollect': zcollect,
    'zcollectScaled': zcollectScaled,
    'bonusCollect': bonuscollect,
    'bonusSelectionCollect':bonusSelectioncollect
  };

  $.post({
    url: 'databasecall.php',
    data: {
      'UID': UID,
      'scale': JSON.stringify(scale),
      'envOrder': JSON.stringify(envOrder),
      'searchHistory': JSON.stringify(searchHistory),
      'reward': totalReward,
      'processDescription': processDescription
    }
  })
  .done( function(msg) {
    // alert("success");
  })
  .fail( function(xhr, textStatus, errorThrown) {
      alert(xhr.responseText);
  });

  endExperiment(baseAmount=toFixed(BASE_FEE,2),bonusPercentage=toFixed(totalReward*100),bonusAmount=toFixed(totalReward*BASE_FEE,2),completionCode=PROLIFIC_COMPLETION_CODE);

}

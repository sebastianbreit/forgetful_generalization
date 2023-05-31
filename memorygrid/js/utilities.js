//*************UTILITIES***************************************
function generateCellOptions(excludeChosen=null){
  var choice_options=Array(0)
  for (var i = 0; i < GRIDSIZE; i++) {
    for (var j = 0; j < GRIDSIZE; j++) {
      if(!excludeChosen){
        choice_options[choice_options.length]=i.toString()+'x'+j.toString();
      }
      else{
        var candidate=i.toString()+'x'+j.toString();
        var includeCandidate=true;
        for (const val of excludeChosen) {
          if(val==candidate){
            includeCandidate=false;
          }
        }
        if(includeCandidate){
          choice_options[choice_options.length]=candidate;
        }
      }
      
    }
  }
  return choice_options;
}



function revealCell(cell,rescaledValue,rescaledColor){
  cell.html(rescaledValue);
  cell.css('text', rescaledValue); 
  cell.css('color', 'black');
  cell.css('background-color', rescaledColor); 
  cell.css('border-color','gray');
}

function clearCell(cell){
  //Clear visual info
  cell.html("&nbsp");
  cell.css('text', ''); 
  cell.css('color', '');
  cell.css("background-color", "");
  cell.css('border-color','');

  //Clear additional data
  // cell.attr("title", "");
  cell.removeAttr("data-history");
}

function hideCell(cell){
  //Clear visual info, but not border
  cell.html("&nbsp");
  cell.css('text', ''); 
  cell.css('color', '');
  cell.css("background-color", "");
}

function convertScenarioStringToÍntArray(scenarioString){
  scenarioArray=[];
  for (var x = 0; x < scenarioString.length; x++) {
  var c = scenarioString.charAt(x);
  scenarioArray.push(parseInt(c, 10));
  }
  return scenarioArray;
}

function addNewCellTimerToList(cellID, initial=false) {
  if(initial==true){
    cellInfoTimeouts[cellID.attr('id')] = setTimeout(function() {
      hideCell(cellID);
    }, MS_CELL_SHOWN+MS_FIXATION_SHOWN);
  }
  else{
    cellInfoTimeouts[cellID.attr('id')] = setTimeout(function() {
      hideCell(cellID);
    }, MS_CELL_SHOWN);
  }
  
}

function startFixationPeriod(){
  //document.getElementById("gridFixationDiv") = 
  setTimeout(function() {
    clickStart('gridFixationDiv','gridDiv');
  }, MS_FIXATION_SHOWN);
}

//create Example grids
function createExample(envNum, scale, exNum,gridSize) {
  var i, j, gridHTML = '',
    WIDTH = gridSize,
    HEIGHT = gridSize,
    env = kernelFunctionList[envNum],
    remainder, payoff, col;
  //beginning of gridHTML
  gridHTML += "<table class='gridEx'><tbody id='exGrid" + exNum + "'>";
  //loop through JSON, creating grid HTML
  for (i = 0; i < gridSize*gridSize; i++) {
    //Check if i corresponds to the start of a row
    remainder = i % gridSize;
    if (remainder == 0) {
      //beginning of row
      gridHTML += '<tr>';
    }
    payoff = env[i]['y']; //absolute value of payoff between 0 and 100
    payoff = Math.round(payoff * scale) + 5; //rescaled payoff, rounded to an int, plus a constant of 5 to avoid any negative values
    col = heatmapColor(payoff); //color code
    gridHTML += '<td align="center" bgcolor=' + col + '>' + payoff + '</td>';
    //check if i corresponds to the end of a row
    remainder = (i + 1) % gridSize;
    if (remainder == 0) {
      gridHTML += '</tr>'; //cap end of row
    }
  }
  //cap end of gridHTML
  gridHTML += "</tbody> </table>";
  var divName = "example" + exNum;
  change(divName, gridHTML); //write gridHTML into place
}

//changes from one page to another
function clickStart(hide, show) {
  if(document.getElementById(hide)){
    document.getElementById(hide).style.display = "none";
  } 
  if(document.getElementById(show)){
    document.getElementById(show).style.display = "block";
  }
}

function disableGridClicks() {
  //Disable clicking of the grid
  for (var x = 0; x < GRIDSIZE; x++) {
    for (var y = 0; y < GRIDSIZE; y++) {
      $this = document.getElementById(x + 'x' + y);
      $this.style.pointerEvents = 'none';
    }
  }
}

function createExamples(){
  var envs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19],
      exampleEnvs = getRandomSubarray(envs.filter(function(a) {
        return envOrder.indexOf(a) < 0;
      }), 5);
    //sample 4 new random scaling factors
    var exScale = [randomNum(60, 80), randomNum(60, 80), randomNum(60, 80), randomNum(60, 80), randomNum(60, 80)];
    for (i = 0; i < 5; i++) {
      createExample(exampleEnvs[i], exScale[i], i + 1,GRIDSIZE);
    }
}

function resetSliders(){
  document.getElementById("estimateSlider").value=45;
  document.getElementById("confidenceSlider").value=5;
  document.getElementById('output').innerHTML=document.getElementById("estimateSlider").value + ' points';
}

function enableCellClick(cell){
  //TODO
}

function enableGridClicks() {
  //Disable clicking of the grid
  for (var x = 0; x < GRIDSIZE; x++) {
    for (var y = 0; y < GRIDSIZE; y++) {
      $this = document.getElementById(x + 'x' + y);
      $this.style.pointerEvents = 'auto';
    }
  }
}
//changes inner HTML of div with ID=x to y
function change(x, y) {
  document.getElementById(x).innerHTML = y;
}

//Function to randomly shuffle an array:
function shuffle(o) { //v1.0
  for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
  return o;
}

//Randomly sample n values from an array
function getRandomSubarray(arr, size) {
  var shuffled = arr.slice(0),
    i = arr.length,
    temp, index;
  while (i--) {
    index = Math.floor((i + 1) * Math.random());
    temp = shuffled[index];
    shuffled[index] = shuffled[i];
    shuffled[i] = temp;
  }
  return shuffled.slice(0, size);
}

function getQueryStringParams(sParam)
{
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    for (var i = 0; i < sURLVariables.length; i++) 
    {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) 
        {
            return sParameterName[1];
        }
        
    }
    // If sParam is not found by here, return null
    return null;
}


//Create normal noise distribution
function myNorm(mu = 0, sig = 1) {
  var x1, x2, rad, c;
  do {
    x1 = 2 * Math.random() - 1;
    x2 = 2 * Math.random() - 1;
    rad = x1 * x1 + x2 * x2;
  } while (rad >= 1 || rad == 0);
  c = Math.sqrt(-2 * Math.log(rad) / rad);
  return (((x1 * c) + mu) * sig);
}

//average the values in an array
function average(inputArray) {
  var total = 0;
  for (var i = 0; i < inputArray.length; i++) {
    total += inputArray[i];
  }
  var avg = total / inputArray.length;
  return avg;
}

//Cumulative Score across trials [0.00, 1.00] - Reward starts at 0
function rewardCum(scoreTotal) {
  var r = 0,
    r_i;
  for (var i = 0; i < (NUMBER_TRIALS+BONUS_ROUNDS); i++) {
    r_i =( scoreTotal[i] - 5*(CLICKS_PER_GRID+1)) / (scale[i]* (CLICKS_PER_GRID+1));
    r = r + r_i;
  }
  r=r/((NUMBER_TRIALS+BONUS_ROUNDS));
  return toFixed(r, 2);
}


//Trial Score  [0.00, 1.00] - Reward starts at 0
function singleTrialReward(points, scale) {
  var r = 0;  
  r = (points - 5*CLICKS_PER_GRID)/ (scale * (CLICKS_PER_GRID+1));
  return toFixed(r, 2);
}

//Cumulative Score across trials [0.00, 1.00] - Reward starts at mean of given grid
function rewardCumWithBaseline(scoreTotal, trialCounter,baselines) {
  var r = 0,
    r_i;
  for (var i = 0; i <= trialCounter; i++) {
    baselineValue=(baselines[i]* (CLICKS_PER_GRID+1));
    maxGridValue=(scale[i]+5) * (CLICKS_PER_GRID+1);
    r_i = (scoreTotal[i]-baselineValue)/ (maxGridValue - baselineValue);
    r = r + Math.max(0,r_i);
  }
  r=r/(trialCounter+1);
  return Math.max(0,toFixed(r, 2));
}

//Trial Score  [0.00, 1.00] - Reward starts at mean of given grid
function singleTrialRewardWithBaseline(points, scale,baseline,calcAfterClicks) {
  var r = 0;  
  baselineValue=(baseline* (calcAfterClicks+1));
  maxGridValue=(scale+5) * (calcAfterClicks+1);
  //both points as well as baseline have added +5 to each cell
  r = (points-baselineValue)/ (maxGridValue-baselineValue) ;
  r=Math.max(0,toFixed(r, 2));

  return r;
}


function calcGridRewardBaselines(environmentList){
  rewardBaselines=[];
  for (e=0;e<(NUMBER_TRIALS+BONUS_ROUNDS);e++){
    averageEnvReward=0
    for (k = 0; k < GRIDSIZE*GRIDSIZE; k++) { //sum rewards of all possible x,y locations
      absoluteValue = environmentList[e][k].y * 100; //noise free observation
      rescaledValue = Math.max(Math.round(absoluteValue / 100 * scale[e] + 5), 0); // scale to randomly selected ceiling, add a constant to avoid negatives, and truncate distribution at 0
      averageEnvReward= (averageEnvReward+rescaledValue);
      
    }
    averageEnvReward=averageEnvReward/(GRIDSIZE*GRIDSIZE);
    rewardBaselines[e]=averageEnvReward;
  }
  return rewardBaselines;
}

//random number generator
function randomNum(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min)
}

//Display a float to a fixed percision
function toFixed(value, precision) {
  var precision = precision || 0,
    power = Math.pow(10, precision),
    absValue = Math.abs(Math.round(value * power)),
    result = (value < 0 ? '-' : '') + String(Math.floor(absValue / power));

  if (precision > 0) {
    var fraction = String(absValue % power),
      padding = new Array(Math.max(precision - fraction.length, 0) + 1).join('0');
    result += '.' + padding + fraction;
  }
  return result;
}

// extract URL parameters (FROM: https://s3.amazonaws.com/mturk-public/externalHIT_v1.js)
function turkGetParam(name) {
  var regexS = "[\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  var tmpURL = fullurl;
  var results = regex.exec(tmpURL);
  if (results == null) {
    return "";
  } else {
    return results[1];
  }
}
//Check if arrays contain the same elements
function arraysEqual(_arr1, _arr2) {
  if (!Array.isArray(_arr1) || !Array.isArray(_arr2) || _arr1.length !== _arr2.length)
    return false;
  var arr1 = _arr1.concat().sort();
  var arr2 = _arr2.concat().sort();
  for (var i = 0; i < arr1.length; i++) {
    if (arr1[i] !== arr2[i]) {
      return false;
    }
  }
  return true;
}

//Check if array contains other array
function isArrayInArray(arr, item) {
  var item_as_string = JSON.stringify(item);

  var contains = arr.some(function(ele) {
    return JSON.stringify(ele) === item_as_string;
  });
  return contains;
}





// ################ neurogrid3 functions ################
//Sum of array
function arraySum(arr) {
  var result = arr.reduce((a, b) => parseInt(a) + parseInt(b), 0);
  return (result)
}

//Trajectory score
function trajDollarScore(numCorrect) {
  return (toFixed(numCorrect / trajTrials, 2));
}

//percentScore
function percentScoreRound(arr) {
  return (toFixed(Math.min(arraySum(arr) / (100 * totalTrials) * 100, 100), 0));
}

function percentScoreGame(arr) {
  var total = 0;
  for (var i = 0; i < arr.length; i++) {
    total += arraySum(arr[i]);
  }
  return (toFixed(Math.min(total / (100 * totalTrials * totalRounds) * 100, 100), 0));
}

function calcReward(arr) {
  var total = 0;
  for (var i = 0; i < arr.length; i++) {
    total += arraySum(arr[i]);
  }
  var dollarReward = Math.min(total / (100 * totalTrials * totalRounds), 1) * 5.00;
  return (toFixed(dollarReward, 2));
}
//single trial reward neurogrid3
function performanceScore(points, scale) {
  var r = 0;
  //cumulative regret (as a percentage)
  r = points / ((scale + 5) * totalTrials);
  return toFixed(r * 100);
}

//construct an array in the range [start, count]
function range(start, count) {
  return Array.apply(0, Array(count))
    .map(function(element, index) {
      return index + start;
    });
};


//changes from one page to another
function hideShow(hide, show) {
  document.getElementById(hide).style.display = "none";
  document.getElementById(show).style.display = "block";
}

function scrollTop(){
  document.getElementById('main_body').scrollIntoView();
  // window.scrollTo(0, 0);
}

function scrollBottom(){
  // window.scrollTo(0, document.body.scrollHeight);
  document.getElementById('main_body').scrollIntoView(false);
}

//Show Div
function show(div) {
  document.getElementById(div).style.display = "block";
}

//Hide Div
function hide(div) {
  document.getElementById(div).style.display = "none";
}

//changes inner HTML of div with ID=x to y
function change(x, y) {
  document.getElementById(x).innerHTML = y;
}

//appends y to x
function appendDiv(x, y) {
  document.getElementById(x).appendChild(y)
}

//adds y to inner HTML of div with ID=x
function addToDiv(x, y) {
  document.getElementById(x).innerHTML += y;
}
//END
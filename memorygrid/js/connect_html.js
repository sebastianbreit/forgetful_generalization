// Connecting HTML elements to JS
var clickEventType = "click";


// connects first button functionality when document loads
document.addEventListener("DOMContentLoaded", function(event) {
  
  setExperimentParametersInHTML();

  document.getElementById("agreeToConsent").addEventListener(clickEventType, function() {
    if(document.getElementById("consentCheckbox").checked == true){
      setButtonHandlers();
      //clickStart('page1_consent', 'page2');
      clickStart('page1', 'page2');
      
      assignScenario();
      //Show Instructions
      // document.getElementById("expHeader").style.display = "block";
      document.getElementById("instructions_memory").style.display = "block";
    }
    else{
      change('error', "In order to participate in this study, you have to give your consent by clicking the checkbox above.");
      $('#errorModal').modal('toggle');
    }
  });  
});


// connects all other buttons
function setButtonHandlers() {

  document.getElementById("exampleButton").addEventListener(clickEventType, function() {
    // document.getElementById("instructions_memory").style.display = "none";
    showExample();
  });
  document.getElementById("instructionsButton").addEventListener(clickEventType, function() {
    instructioncheck();
  });
  document.getElementById("beginExpButton").addEventListener(clickEventType, function() {    
    clickStart('page4', 'page5');
    showStartExperimentInstructions();
  });
  document.getElementById("nextTrialButton").addEventListener(clickEventType, function() {
    nexttrial();
  });
  document.getElementById("readyButton").addEventListener(clickEventType, function() {
    initializeExp();
    clickStart('page5ready','gridFixationDiv');
    collapseExperimentInstructions(); 
    // Wait 2 sec, then change from Fixation to actual grid
    startFixationPeriod();
    
  });

  document.getElementById("bonusReadyButton").addEventListener(clickEventType, function() {
    // initializeBonusRound();
    clickStart('page5BonusReady','gridDiv');
    collapseExperimentInstructions(); 
    
    startBonusEstimate();
  });
  document.getElementById("submitBonusJudgmentButton").addEventListener(clickEventType, function() {
    if(confidenceSliderChanged && estimateSliderChanged){
      nextBonusEstimate();  
      estimateSliderChanged=false;
      confidenceSliderChanged=false;
    }
    else{
      change('error', 'Please change each Slider at least once');
      $('#errorModal').modal('toggle');
    }
    
  });
  document.getElementById("submitDataButton").addEventListener(clickEventType, function() {
    senddata();
  });
}

function setExperimentParametersInHTML() {

  document.getElementById("instructions_1").innerHTML = "In the following study, you will be presented with a series of <b>" + (NUMBER_TRIALS + BONUS_ROUNDS) + " different grids to explore</b>. On each grid, you will have <b>" + CLICKS_PER_GRID + " clicks</b> to select tiles and earn points. The number of remaining clicks are displayed above the grid. When you run out of clicks, you will start a new round on a new, unexplored grid.";
  document.getElementById("instructions_2").innerHTML="<b>Points are clustered along the grid</b>, such that areas with high-value points tend to appear close to each other and areas of low-value points tend to appear close to each other. All payoffs are greater than zero, with the <b>maximum payoff differing between grids.</b><br> Here you can see an example of what the distribution of points is like, with the darker tiles indicating higher point values.";
  document.getElementById("instructions_3").innerHTML = "It is your task to <b>gain as many points as possible</b> across all " + (NUMBER_TRIALS + BONUS_ROUNDS) + " grids. You will be assigned a <b>bonus of up to £"+toFixed(BASE_FEE,2)+"</b> based on your performance compared to random choices, summed across all grids.";
  document.getElementById("instructions_4").innerHTML ='Each grid starts with a single tile revealed. Use your mouse to <b>click either new or previously revealed tiles. <br/>Clicking on new tiles</b> will reveal a number corresponding to the points you gain. Revealed tiles are also colour-coded as a visual aid to help you in this task, with darker colours corresponding to larger rewards.';
   document.getElementById("instructions_5").innerHTML='Remember, you can also <b>reclick previously revealed tiles</b>. For instance, reclicking tiles with high-values will help increase your reward. <br>There will be small changes in the points you gain when re-clicking. These changes can be either positive or negative, but will tend to be the same amount of points as before.';
  document.getElementById("instructions_6").innerHTML="<b>There are two different types of rounds.</b> <br/>In the <b>PERMANENT rounds</b>, the reward value for clicked tiles will <b>stay visible</b> throughout the entire round. <br/>In the <b>TEMPORARY rounds</b>, reward values will only be <b>visible for a short duration of "+MS_CELL_SHOWN+"ms</b> before disappearing. <br/><b>This can change with every round,</b> so please read the instructions carefully.<br/>The locations that you revealed already will be highlighted in both types of rounds, so you will only have to memorize their corresponding values.";

  

  document.getElementById("summarized_instructions_I_memory").innerHTML = "<b>I.</b> Below you will see a grid with "+GRIDSIZE+"x"+GRIDSIZE+" tiles. When you click on a tile, the points of that tile are revealed and its value will be displayed. Tiles are coloured, corresponding to the point value.";
  document.getElementById("summarized_instructions_I_nomemory").innerHTML=document.getElementById("summarized_instructions_I_memory").innerHTML;

  document.getElementById("summarized_instructions_V_memory").innerHTML = "<b>V.</b> There are " + (NUMBER_TRIALS + BONUS_ROUNDS) + " different grids with " + CLICKS_PER_GRID + " clicks in each.";
  document.getElementById("summarized_instructions_V_nomemory").innerHTML = "<b>V.</b> There are " + (NUMBER_TRIALS + BONUS_ROUNDS) + " different grids with " + CLICKS_PER_GRID + " clicks in each.";

  document.getElementById("summarized_instructions_VII_memory").innerHTML = "<b>VII.</b> This round, revealed <b>rewards will disappear after "+ MS_CELL_SHOWN +" ms.</b> However, their <b>locations will remain highlighted by a gray border.</b>";

document.getElementById("BonusAFC").innerHTML= "<b>Of the "+BONUS_CHOICES+" red items, which one would you choose? </br>With this choice, your round will resume as normal.</b>";
  

  }

function updateExperimentInstructions(currentCondition){
  if (currentCondition == COND_MEMORY) {
    document.getElementById("instruction_summary_memory").style.display = "block";
  } else if (currentCondition == COND_NORMAL) {
    document.getElementById("instruction_summary_nomemory").style.display = "block";
  }
}

function collapseExperimentInstructions(){
  document.getElementById("instruction_summary_memory").style.display = "none";
  document.getElementById("instruction_summary_nomemory").style.display = "none";
}

function setScenarioHeader(currentCondition){  
  document.getElementById("expHeader").style.display = "block"; 
  if(currentCondition==COND_MEMORY){
    document.getElementById("scenario_header").innerHTML='TEMPORARY round, rewards disappear after '+MS_CELL_SHOWN+'ms'; 
  }
  else{
    document.getElementById("scenario_header").innerHTML='PERMANENT round, rewards stay visible permanently'
  }
  document.getElementById("scenario_header").style.display = "block"; 
}

function hideScenarioHeader(){
  document.getElementById("expHeader").style.display = "none"; 
}

function showNextGrid(remainingTrials, clicks,initialReward){
    document.getElementById("gridDiv").style.display = "none";
    collapseExperimentInstructions();
    clickStart('page5finished', 'gridFixationDiv');
    startFixationPeriod();

    change("remaining_grids", "Number of grids left: <b>" + remainingTrials + "</b>");
    change("remaining_clicks", "Number of clicks left: <b>" + clicks + "</b>");

    change('scoretotal', "Current Score: " + initialReward);
}

function showGridFinishedInstructions(trialReward, totalReward, remainingTrials, nextCondition) {

  setTimeout(function () {
    //move to next page
    hideScenarioHeader();
    document.getElementById('page5finished').style.display = "block";
    //clickStart('gridDiv', 'page5finished');
    if(remainingTrials==0){
      change("trials", "You have finished exploring this grid and <b>gained " + trialReward + "% of the possible maximum as a bonus</b>. You have earned a total performance bonus of " + totalReward + "% of the possible maximum bonus. \
        <br> You are now done exploring grids.");
    }
    else{
      if (nextCondition===COND_MEMORY) {
        document.getElementById('instruction_summary_memory').style.display = "block";
        change("trials", "You have finished exploring this grid and <b>gained " + trialReward + "% of the possible maximum as a bonus</b>. You have currently earned a total performance bonus of " + totalReward + "% of the possible maximum bonus and have " + remainingTrials + " grid(s) left to complete. \
          <br> In the next grid, revealed <b>rewards will disappear after "+ MS_CELL_SHOWN +" ms.</b> Please confirm when you are ready to continue.");  
      }
      else
      {
        document.getElementById('instruction_summary_nomemory').style.display = "block";
        change("trials", "You have finished exploring this grid and <b>gained " + trialReward + "% of the possible maximum as a bonus</b>. You have currently earned a total performance bonus of " + totalReward + "% of the possible maximum bonus and have " + remainingTrials + " grid(s) left to complete. \
          <br> In the next grid, revealed <b>rewards will remain visible</b> throughout the entire round. Please confirm when you are ready to continue.");
      }
    }
    
    document.getElementById('gridDiv').style.display = "none";
    
  }, 1000);  
  
}

function showStartExperimentInstructions(){

  // document.getElementById('gridDiv').style.display = "none";
  // document.getElementById('page5ready').style.display = "block";
  if(currentScenario==COND_MEMORY){    
    document.getElementById('instruction_summary_memory').style.display = "block";
  }
  else{
    document.getElementById('instruction_summary_nomemory').style.display = "block"; 
  }

  document.getElementById('gridFixationDiv').style.display = "none";
  document.getElementById('gridDiv').style.display = "none";

  clickStart('gridFixationDiv','page5ready');

  if(currentScenario==COND_MEMORY){    
    change("ready", "You are about to start exploring your first grid. We provide a summary of the instructions above. In this grid, revealed <b>rewards will disappear after "+ MS_CELL_SHOWN +" ms. Please confirm when you are ready to start");  
  }
  else{
    change("ready", "You are about to start exploring your first grid. We provide a summary of the instructions above. In this grid, revealed <b>rewards will remain visible</b> throughout the entire round. Please confirm when you are ready to start");  
  }

}

function endExperiment(baseAmount, bonusPercentage, bonusAmount,completionCode){
    var prolificCompletionURL="https://api.prolific.co/submissions/complete?cc="+completionCode;
    change("baseReward","The base payment of £"+baseAmount+" will be transferred to your Prolific account shortly.");
    change("bonus", "You earned a performance bonus of " + bonusPercentage + "% of the maximum possible bonus, resulting in a payment of £"+ bonusAmount +", which will be automatically assigned to your Prolific account.");
    change("redirectProlific","<a href='"+prolificCompletionURL+"'>Direct link back to Prolific.</a>");
    change("completionCode",completionCode);
    clickStart('page6', 'page7');

    // No automatic redirect to Prolific
    // setTimeout(function () {
    //    window.location.href = prolificCompletionURL; 
    // }, 5000);

}

function initializeGrid(){
  var i, j, gridHTML = '',
    WIDTH = GRIDSIZE,
    HEIGHT = GRIDSIZE,
    lastRevealedCellId;

  for (i = 0; i < HEIGHT; i++) {
    gridHTML += '<tr>';
    for (j = 0; j < WIDTH; j++) {
      gridHTML += '<td align="center" id="' + i + 'x' + j + '">&nbsp;</td>';
    }
    gridHTML += '</tr>';
  }

  $('#grid tbody').append(gridHTML);
}

function initializeFixationGrid(){
  var i, j, gridHTML = '',
    WIDTH = GRIDSIZE,
    HEIGHT = GRIDSIZE,
    lastRevealedCellId;

  for (i = 0; i < HEIGHT; i++) {
    gridHTML += '<tr>';
    for (j = 0; j < WIDTH; j++) {
      gridHTML += '<td align="center" id="' + i + 'x' + j + '">&nbsp;</td>';
    }
    gridHTML += '</tr>';
  }

  $('#gridFixation tbody').append(gridHTML);
}

function showBonusEstimateInstructions(trialReward, totalReward, remainingTrials, currentCondition, bonusJudgements){
  //move to next page
  document.getElementById('page5BonusReady').style.display = "block";
  // if(currentCondition==COND_MEMORY){
  //   document.getElementById('instruction_summary_memory').style.display = "block";
  // }
  // else{
  //   document.getElementById('instruction_summary_nomemory').style.display = "block";
  // }

  change("bonusReady", "<h3>BONUS ROUND</h3></br>\
    You are <b>halfway done exploring</b> this grid and <b>gained " + trialReward + "%</b> of the possible maximum as a bonus. \
    You are about to be asked to estimate the reward for <b>" + bonusJudgements + " unseen locations on the current grid.</b> \
    Afterwards, you will be asked to <b>choose 1 of the " + bonusJudgements + " options</b>, after which you will <b>continue to explore</b> this final round normally.\
    Please confirm when you are ready to start with the estimates.");  
  

  document.getElementById('gridDiv').style.display = "none";
}

function startBonusOverlay(){
  document.getElementById('main_page').classList.add('col-lg-8');
  document.getElementById('main_page').classList.add('col-12'); //7 , too small for 150% though
  document.getElementById('bonusRoundDiv').classList.add('col-lg-4'); //4
  document.getElementById('bonusRoundDiv').classList.add('col-12'); 
  document.getElementById('bonusRoundDiv').style.display='block';

  resetSliders();
}

function showAFCinstructions(afc_options){
  for (const id of afc_options){
    document.getElementById(id).classList.add('highlight');
    document.getElementById(id).style.pointerEvents='auto';
  }

  document.getElementById('main_page').classList.remove('col-lg-8');
  document.getElementById('main_page').classList.remove('col-12'); //7 , too small for 150% though
  document.getElementById('bonusRoundDiv').classList.remove('col-lg-4'); //4
  document.getElementById('bonusRoundDiv').classList.remove('col-12'); 

  clickStart("bonusRoundDiv","bonusRoundAFC");
}

function hideAFCinstructions(afc_options){
  for (const id of afc_options){
    document.getElementById(id).classList.remove('highlight');
  }
  clickStart("bonusRoundAFC","");
}


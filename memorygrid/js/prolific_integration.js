const BASE_URL = 'https://api.prolific.co';
const V1_API = '/api/v1/';
const API_TOKEN = 'API_TOKEN_PLACEHOLDER';

const EXTERNAL_URL = 'https://experiments.hmc-lab.com/memorygrid/experiment.html';

function getAccountDetails(){
	var endpoint='users/me';

	$.ajax({
  		url: BASE_URL+V1_API+endpoint,
		type: 'GET',
		headers: {"Authorization": API_TOKEN}
	})
	.done(function( json ) {

	})
	.fail(function( xhr, status, errorThrown ) {
		console.log( "Error: " + errorThrown );
		console.log( "Status: " + status );
		console.dir( xhr );
	})
	.always(function( xhr, status ) {

	});
}

function setUpBonusPayment(){

}

function payBonus(){

}

function reservePlace(){

}

function startProlificExperiment(){

}

function completeProlificExperiment(){
	var completionCode;
	var url_completeStudy = '/submissions/complete?cc=' + completionCode ;

	$(location).attr('href',BASE_URL+url_completeStudy);
}

function approveCompletion(){

}

// The current game scenario expressed as a string where:
// 
// a) The string ordinal positions correspond to the following scenario
//    positions:
//
//    0 1 2
//    3 4 5
//    6 7 8
//
// b) The characters in the string have the following meaning:
//    
//    1 - 'O' mark in position
//    2 - No mark in position
//    3 - 'X' mark in position
//    (see PositionState enum)
//
var scenario = Array();

// If the user is currently playing as mark 'X'.
var userIsX = false;

// If a play request has been issued but no response has yet been received.
var requestPending = false;

var GameState = {
    X_WINS:     '0',
    O_WINS:     '1',
    DRAW:       '2',
    ACTIVE:     '3'
}
var PositionState = {
    O:          '1',
    FREE:       '2',
    X:          '3'
}

$(document).ready(function() {
    resetGame();
});

function renderScenario() {
    for (i=0; i<9; i++) {
        var cell = $('#cell_'+i.toString());
        process(cell, scenario[i]);
        cell.html(map(scenario[i]));
    }
}

// TODO: Implement this as a dict?
function map(x) {
    switch (x) {
        case '1': return 'o';
        case '2': return '';
        case '3': return '&times;';
    }
}

function process(cell, x) {
    switch (x) {
        case '1':
            cell.removeClass('cell-free');
            cell.addClass('cell-o');
            cell.off('click');
            break;
        case '3': 
            cell.removeClass('cell-free');
            cell.addClass('cell-x');
            cell.off('click');
            break;
    }
}

// Render the game read-only. Typically in response to the game scenario 
// representing a win or draw.
function disableGame() {
    $('.cell').each(function() {
        $(this).removeClass('cell-free').off('click');
    });
}

function resetGame() {

    // The computer and user switch marks (O -> X, X -> O).
    userIsX = !userIsX;
    $('#player_x').html(userIsX ? "you" : "computer");
    $('#player_o').html(userIsX ? "computer" : "you");

    $('#game_outcome').hide();

    // Reset the current scenario to contain 9 free position.
    for (i=0; i<9; i++) {
        scenario[i] = PositionState.FREE;
    }

    // TODO: Might need to sort out click handlers stacking up.
    // TODO: Chain these up and use 'each'.
    // TODO: Could this be part of 'renderScenario'?
    for (i=0; i<9; i++) {
        var cell = $('#cell_'+i.toString());
        cell.removeClass('cell-x');
        cell.removeClass('cell-o');
        cell.addClass('cell-free');
        cell.empty();
        cell.click(function() {
            userMove(getCellIdx($(this)));
            });
    }

    if (!userIsX) {
        askComputerToMove();
    }
}

// TODO
function getCellIdx(cell) {
    return parseInt(cell.attr('id').split('_')[1]);
}

// End the current game and display an outcome message to the user.
function endGame(outcomeMsg) {
    $('#game_outcome_msg').html(outcomeMsg);
    $('#game_outcome').show();
    disableGame();
}

// User places their mark in the current game scenario.
function userMove(position) {

    // Can't issue a new play request while an existing request is pending.
    if (requestPending) {
        return;
    }
    requestPending = true;

    scenario[position] = userIsX ? PositionState.X : PositionState.O;
    askComputerToMove();
}

// Current game scenario is sent to the server for the computer to place its
// mark.
function askComputerToMove() {
    $.get('/game?q='+scenario.join(''), function(response) {

        response = response.split('');
        var gameState = response.shift();
        scenario = response;

        renderScenario();

        switch (gameState) {
            case GameState.X_WINS:
                endGame(userIsX ? 'OMG! You win.' : 'The computer wins.');
                break;
            case GameState.O_WINS:
                endGame(userIsX ? 'The computer wins.' : 'OMG! You win.');
                break;
            case GameState.DRAW:
                endGame('A draw.');
                break;
        }

        // The user is now free to issue another play request.
        requestPending = false;
    });
}


var state = undefined;
var userIsX = false;

/*
 * 1 = O
 * 2 = free
 * 3 = X
 */

$(document).ready(function() {
    resetGame();
});

function render() {
    var stateArray = state.split('');
    for (i=0; i<9; i++) {
        var cell = $('#cell_'+i.toString());
        process(cell, stateArray[i]);
        cell.html(map(stateArray[i]));
    }
}

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

function resetGame() {

    userIsX = !userIsX;

    $('#game_outcome').hide();
    state = "222222222";

    $('#player_x').html(userIsX ? "you" : "computer");
    $('#player_o').html(userIsX ? "computer" : "you");

    // TODO: Might need to sort out click handlers stacking up.
    for (i=0; i<9; i++) {
        var cell = $('#cell_'+i.toString());
        cell.removeClass('cell-x');
        cell.removeClass('cell-o');
        cell.addClass('cell-free');
        cell.html('');
        cell.click(function() {
            play(getCellIdx($(this)));
            });
    }

    if (!userIsX) {
        play(null);
    }
}

// TODO
function getCellIdx(cell) {
    return parseInt(cell.attr('id').split('_')[1]);
}

function play(position) {

    if (position != null) {
        var stateArray = state.split('');
        stateArray[position] = userIsX ? '3' : '1';
        state = stateArray.join('');
    }

    $.get('/game?q='+state, function(data) {

        var stateArray = data.split('');
        var stat = stateArray.shift();
        data = stateArray.join('');

        switch (stat) {
            case '0':
                // TODO: We might be playing as this mark?
                $('#game_outcome_msg').html('The computer wins.');
                $('#game_outcome').show();
                break;
            case '1':
                $('#game_outcome_msg').html('The computer wins.');
                $('#game_outcome').show();
                break;
            case '2':
                $('#game_outcome_msg').html('A draw.');
                $('#game_outcome').show();
                break;
            case '3':
                break;
        }
        state = data;
        render();
    });
}

var blocksize = 0;

function updateTargetText(target,updateFunction) {
  text = target.innerHTML;
  target.innerHTML = updateFunction(text);
}

function setVisibility(target,bool) {
  if (bool) {
    target.style.visibility = "visible";
  } else {
    target.style.visibility = "hidden";
  }
}

// need to look at what text was previously to get current state & condition on it
function switchButtonState(text) {
  if (text == "New Game") {
    return "Pause";
  } else if (text == "Pause") {
    return "Resume";
  } else if (text == "Resume") {
    return "Pause";
  } else {
    return "should never happen";
  }
}

// draw piece
function drawPiece() {
  var canvas = document.getElementById('tetriscanvas');
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = 'rgb(251, 180, 20)';
  ctx.fillRect(10, 10, 30, 30);
}

function drawNext() {
  var canvas = document.getElementById('next_tetrimino');
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = 'rgb(236, 240, 241)';
  ctx.fillRect(10, 10, 30, 30);
}


// initial state of the button is from the HTML
var gameButtonElt = document.getElementById("fjt-button")
var infoOverlay = document.getElementById("info-text");
var buttonClickE = clicksE(gameButtonElt)
.mapE(function() { 
  var currentState = gameButtonElt.innerHTML;
  updateTargetText(gameButtonElt,switchButtonState);
  setVisibility(infoOverlay,currentState == "Pause");
  drawPiece();
  drawNext();
});


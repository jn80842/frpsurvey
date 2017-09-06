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

}


// initial state of the button is from the HTML
var gameButtonElt = document.getElementById("fjt-button")
var infoOverlay = document.getElementById("info-text");
var buttonClickE = clicksE(gameButtonElt)
.mapE(function() { 
  var currentState = gameButtonElt.innerHTML;
  updateTargetText(gameButtonElt,switchButtonState);
  setVisibility(infoOverlay,currentState == "Pause");
});


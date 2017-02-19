function updateTargetText(target,updateFunction) {
  text = target.innerHTML;
  target.innerHTML = updateFunction(text);
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


// initial state of the button is from the HTML
var gameButtonElt = document.getElementById("fjt-button")
var buttonClickE = clicksE(gameButtonElt)
.mapE(function() { updateTargetText(gameButtonElt,switchButtonState) });
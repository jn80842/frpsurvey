function updateTargetText(target,updateFunction) {
  text = target.innerHTML;
  target.innerHTML = updateFunction(text);
}

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

// had to supply the id of the button twice
// initial state of the button is from the HTML
var buttonClickE = clicksE("fjt-button")
.mapE(function() { updateTargetText(document.getElementById("fjt-button"),switchButtonState) });
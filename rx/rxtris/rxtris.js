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

var buttonElt = document.getElementById("fjt-button");
var buttonClickStream = Rx.Observable.fromEvent(buttonElt,"click");
buttonClickStream.map(_ => updateTargetText(buttonElt,switchButtonState))
.subscribe();
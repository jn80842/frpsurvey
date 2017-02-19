function updateTargetText(target,updateFunction) {
  return function() {
    text = target.innerHTML;
    target.innerHTML = updateFunction(text);
  }
}

function startGameText() {
  return "Pause";
}

// function switchButtonState(text) {
//   if (text == "New Game") {
//     return "Pause";
//   } else if (text == "Pause") {
//     return "Resume";
//   } else if (text == "Resume") {
//     return "Pause";
//   } else {
//     return "should never happen";
//   }
// }

var gameButtonElt = document.getElementById("fjt-button")
var gameButton = ElementA(gameButtonElt);

//var pauseClick = EventA("click").next();


(gameButton.next(EventA("click")).next(updateTargetText(gameButtonElt,startGameText))).run();
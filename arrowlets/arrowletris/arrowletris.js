function updateText(target,text) {
  return function() {
    target.innerHTML = text;
  }
}

var gameButtonElt = document.getElementById("fjt-button")
var gameButton = ElementA(gameButtonElt);

// unlike the other two, the switching behavior is encoded as arrowlets
var pauseClick = gameButton.next(EventA("click").next(updateText(gameButtonElt,"Pause")));
var resumeClick = gameButton.next(EventA("click").next(updateText(gameButtonElt,"Resume")));

var gameLoop = resumeClick.next(pauseClick).next(Repeat);

// note that the initial state is from the state of the HTML at load
gameLoop.repeat().run();
/* drag proxy separates the side effects */
function DragProxy(target) {
    this.target = target;
    this.dragging = false;
}
DragProxy.prototype.addEventListener =
    function(eventname, handler, order) {
        /* use a wider event-source when dragging */
        var eventsource = this.dragging
            ? document : this.target;
        eventsource.addEventListener(eventname, handler, order);
}
DragProxy.prototype.removeEventListener =
    function(eventname, handler, order) {
        var eventsource = this.dragging
            ? document : this.target;
        eventsource.removeEventListener(eventname, handler, order);
}
DragProxy.prototype.setupDrag = function(event) {
    /* setup drag-and-drop */
    this.startx = event.clientX - parseInt(this.target.style.left || 0);
    this.starty = event.clientY - parseInt(this.target.style.top || 0);
    this.target.innerHTML = "Setup";
    this.dragging = true;
    event.preventDefault(); /* prevent text selection */
}
DragProxy.prototype.moveDrag = function(event) {
    this.target.style.left = (event.clientX - this.startx) + "px";
    this.target.style.top = (event.clientY - this.starty) + "px";
    this.target.innerHTML = "Drag";
}
DragProxy.prototype.dropDrag = function(event) {
    this.target.innerHTML = "Drop";
    this.dragging = false;
}
DragProxy.prototype.cancelDrag = function(event) {
    this.target.innerHTML = "Cancel";
    this.dragging = false;
}
function DragElementA(target) {
    return ElementA(target).next(function(el) {
        return new DragProxy(el);
    });
}

/* drag-and-drop states */
function setupA(proxy, event) {
    /* setup drag-and-drop */
    proxy.setupDrag(event);
    return proxy;
}
function dragA(proxy, event) {
    /* perform dragging */
    proxy.moveDrag(event);
    return proxy;
}
function dropA(proxy, event) {
    /* perform dropping */
    proxy.dropDrag(event);
    return proxy;
}
function cancelA(proxy, event) {
    /* cancel drag-and-drop */
    proxy.cancelDrag(event);
    return proxy;
}

/* organize drag-and-drop into three parts */
var dragOrDropA =
    (   ((EventA("mousemove").bind(dragA)).next(Repeat))
     .or((EventA("mouseup").bind(dropA)).next(Done))
    ).repeat();

var dragDropOrCancelA =
       ((EventA("mousemove").bind(dragA)).next(dragOrDropA))
    .or((EventA("mouseup").bind(cancelA)));

var dragAndDropA = /* drag-and-drop */
    (EventA("mousedown").bind(setupA)).next(dragDropOrCancelA);

/* and set up on dragtarget */
DragElementA("dragtarget").next(dragAndDropA)
    .next(Repeat).repeat().run();

    
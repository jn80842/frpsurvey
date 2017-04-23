// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min)) + min;
}
function getMaxOfArray(numArray) {
  return Math.max.apply(null, numArray);
}


// quick contexts for main game and next piece area
var tetris_canvas = document.getElementById("tetriscanvas");
var tetris_ctx = tetris_canvas.getContext('2d');

var next_canvas = document.getElementById("next_tetrimino");
var next_ctx = next_canvas.getContext('2d');

// tetrimino
var longthin = 'rgb(60,199,214)';
var longthin_coords = [ [0,0], [1,0], [2,0], [3,0] ];
var square = 'rgb(251,180,20)';
var square_coords = [ [0,0], [1,0], [0,1], [1,1] ];
var l_shape = 'rgb(176,68,151)';
var l_shape_coords = [ [1,0], [0,1], [1,1], [2,1] ];
var t_shape = 'rgb(57,147,208)';
var t_shape_coords = [ [0,0], [0,1], [1,1], [2,1] ];
var j_shape = 'rgb(237,101,47)';
var j_shape_coords = [ [2,0], [0,1], [1,1], [2,1] ];
var z_shape = 'rgb(149,196,61)';
var z_shape_coords = [ [1,0], [2,0], [0,1], [1,1] ];
var s_shape = 'rgb(232,65,56)';
var s_shape_coords = [ [0,0], [1,0], [1,1], [2,1] ];

var tetriminos_colors = [longthin, square, l_shape, t_shape, j_shape, z_shape, s_shape];
var tetriminos_coords = [longthin_coords, square_coords, l_shape_coords, t_shape_coords, j_shape_coords, z_shape_coords, s_shape_coords];



function Cell(color,coord) {
    this.color = color;
    this.coord = coord;
}
Cell.prototype.getX = function() {
    return this.coord[0];
}
Cell.prototype.getY = function() {
    return this.coord[1];
}


// grid is a set of colored squares
function Grid(cellList) {
    this.cellList = cellList;
}
Grid.prototype.render = function(ctx) {
    this.cellList.foreach(function(c) {
        renderBox(ctx,c)
    })
}
Grid.prototype.centerOfMass = function() {
    cellCount = this.cellList.length;
    xsum = 0;
    ysum = 0;
    this.cellList.forEach(function(c) {
        xsum += c.coord[0];
        ysum += c.coord[1];
    });
    return [ Math.round(xsum / cellCount), Math.round(ysum / cellCount) ];
}
Grid.prototype.size = function() {
    xs = this.cellList.map(function(c) { return c.getX() + 1 });
    ys = this.cellList.map(function(c) { return c.getY() + 1 });
    return [ getMaxOfArray(xs), getMaxOfArray(ys) ];
}
function gridFromList(color, coordList) {
    cells = []
    coordList.forEach(function(cc) {
        c = new Cell(color, cc);
        cells.push(c);
    });
    return new Grid(cells);
}

function getRandomTetrimino() {
    i = getRandomInt(0,7);
    return gridFromList(tetriminos_colors[i],tetriminos_coords[i]);
}


function renderBox(ctx,cell) {
    ctx.fillstyle = cell.color;
    ctx.fillRect(cell.getX(),cell.getY(),30,30);
}
function renderGrid(ctx,grid) {
    grid.cellList.forEach(function(c) {
        ctx.fillstyle = cell.color;
        ctx.fillRect(cell.getX(),cell.getY(),30,30);
    });
}
function renderNext(ctx,cell) {
    ctx.fillstyle = 'rgb(236,240,241)';
    ctx.fillRect(cell.getX(),cell.getY(),30,30);
}
function renderNextGrid(ctx,grid) {
    // var size_pos = grid.size();
    // var x_offset = (1 - size_pos[0]) / 2;
    // var y_offset = (1 - size_pos[1]) / 2;
    ctx.fillStyle = 'rgb(236,240,241)';
    grid.cellList.forEach(function(c) {
        // console.log("x: " + (c.getX() + x_offset)*30)
        // console.log("y: " + (c.getY() + y_offset)*-30)
        //ctx.translate(x_offset,y_offset);
        //ctx.fillRect((c.getX() + x_offset) * 30,(c.getY() + y_offset) * -30,30,30);
        ctx.fillRect(c.getX() * 30, c.getY() * 30, 30, 30);
    })
}

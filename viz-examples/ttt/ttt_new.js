/*
  Visualizing the `ttt.frg` model, which produces traces
  of a tic-tac-toe game. This code uses the April 2023 Sterling 
  D3FX library. If you want to use D3 directly, see `ttt_old.js`.
*/

// the stage contains everything in the visualization
const stage = new Stage()
const numStates = instance.signature('Board').tuples().length

// the *outer* grid produces a one-column "film-strip" style 
// layout for all states in the trace given
let grid = new Grid({
  grid_location: {x: 20, y: 20},
  cell_size: {x_size: 100, y_size: 100},
  grid_dimensions: {x_size: 1, y_size: numStates }
})
stage.add(grid)

// Create an *inner* grid for every TTT board, placed from
// top to bottom of the outer grid:
for(b = 0; b <= 10; b++) {  
  if(Board.atom("Board"+b) != null)
    grid.add({x:0, y:b}, makeBoard(Board.atom("Board"+b)))
}

function makeBoard(stateAtom) {
  let innerGrid = new Grid({
    grid_location: {x: 0, y:0},
    cell_size: {x_size: 30, y_size: 30},
    grid_dimensions: {x_size: 3, y_size: 3 }
  })  
  
  for (r = 0; r <= 2; r++) {
    for (c = 0; c <= 2; c++) {
      const val = stateAtom.board[r][c].toString().substring(0,1)      
      const tbox = new TextBox({
        text: `${val}`, 
        coords: {x:0,y:0}, 
        color: 'black', 
        fontSize: 16})
      innerGrid.add({x: r, y: c}, tbox)
    } 
  }
  return innerGrid
}

// Finally, render the stage
stage.render(svg, document)
const d3 = require('d3')
d3.selectAll("svg > *").remove();

/*
  Visualizing the `ttt.frg` model, which produces traces
  of a tic-tac-toe game. 
  
  This code uses the *OLD* 2022 Sterling visualization, which
  invokes D3 directly. For the updated version, see `ttt_new.js`.

  Note that the require statement needs to be _before anything else_
  in the 2022 visualizations.
*/


function printValue(row, col, yoffset, value) {
  d3.select(svg)
    .append("text")
    .style("fill", "black")
    .attr("x", (row+1)*10)
    .attr("y", (col+1)*14 + yoffset)
    .text(value);
}

function printState(stateAtom, yoffset) {
  for (r = 0; r <= 2; r++) {
    for (c = 0; c <= 2; c++) {
      printValue(r, c, yoffset,
                 stateAtom.board[r][c]
                 .toString().substring(0,1))  
    }
  }
  
  d3.select(svg)
    .append('rect')
    .attr('x', 5)
    .attr('y', yoffset+1)
    .attr('width', 40)
    .attr('height', 50)
    .attr('stroke-width', 2)
    .attr('stroke', 'black')
    .attr('fill', 'transparent');
}


var offset = 0
for(b = 0; b <= 10; b++) {  
  if(Board.atom("Board"+b) != null)
    printState(Board.atom("Board"+b), offset)  
  offset = offset + 55
}
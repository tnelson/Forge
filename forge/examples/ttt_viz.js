const d3 = require('d3')
d3.selectAll("svg > *").remove();

/*
  Visualizer for the in-class tic-tac-toe model. This is written in "raw" 
  D3, rather than using our helper libraries. If you're familiar with 
  visualization in JavaScript using this popular library, this is how you 
  would use it with Forge.

  Note that if you _aren't_ familiar, that's OK; we'll show more examples 
  and give more guidance in the near future. The helper library also makes 
  things rather easier. 

  TN 2024

  Note: if you're using this style, the require for d3 needs to come before anything 
  else, even comments.
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
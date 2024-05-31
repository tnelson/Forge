d3 = require('d3')
d3.selectAll("svg > *").remove();

function printValue(row, col, yoffset, value) {
  d3.select(svg)
    .append("text")
    .style("fill", "black")
    .attr("x", row*25)
    .attr("y", col*26 + yoffset)
    .text(value);
}

const rows = 9
const cols = 9

function printBoard(boardAtom, yoffset) {
  for (r = 1; r <= rows; r++) {
    for (c = 1; c <= cols; c++) {
      printValue(r, c, yoffset,
                 boardAtom.board[r][c]
                 .toString().substring(0,1))
    }
  }
  
  d3.select(svg)
    .append('rect')
    .attr('x', 10)
    .attr('y', yoffset+7)
    .attr('width', 240)
    .attr('height', 240)
    .attr('stroke-width', 3)
    .attr('stroke', 'black')
    .attr('fill', 'transparent');
  
  for (x = 1; x<3; x++) {
    d3.select(svg)
      .append('line')
      .attr('x1', 10+x*(240/3))
      .attr('y1', yoffset+7)
      .attr('x2', 10+x*(240/3))
      .attr('y2', 250+yoffset)
      .attr('stroke-width', 1)
      .attr('stroke', 'black')
      .attr('fill', 'transparent');

    d3.select(svg)
      .append('line')
      .attr('x1', 10)
      .attr('y1', yoffset+5+x*(240/3))
      .attr('x2', 250)
      .attr('y2', yoffset+5+x*(240/3))
      .attr('stroke-width', 1)
      .attr('stroke', 'black')
      .attr('fill', 'transparent');

  }
  

}

printBoard(BoardState.atom("PuzzleState0"), 0)
printBoard(BoardState.atom("SolvedState0"), 240)

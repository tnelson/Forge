/**
 * Click the execute button at the top of
 * this editor to run the script, or
 * press Ctrl + Enter.
 */

const rows = +Matrix$0.Matrix$rows.toString();
const cols = +Matrix$0.Matrix$cols.toString();
const tuples = lodash.chunk(Matrix$0.vals.tuples(), cols);

const w = 100; // Rectangle width for each matrix value
const h = 100; // Rectangle height for each matrix value
const mw = w * cols; // Total matrix width
const mh = h * rows; // Total matrix height

// Select the SVG
const stage = d3.select(svg);
stage.selectAll('*').remove();

// Select an SVG group for each row
const rowGroups = stage.selectAll('g')
  .data(tuples)
  .join('g')
  .attr('transform', (d, i) => `translate(0,${y(d, i)})`);

// Select a rect for each value in row
const colRects = rowGroups
  .selectAll('rect')
  .data(d => d)
  .join('rect')
  .attr('width', w)
  .attr('height', h)
  .attr('x', x)
  .style('stroke', 'black')
  .style('fill', matrixColor);

// Select a text label for each value in row
const colLabels = rowGroups
  .selectAll('text')
  .data(d => d)
  .join('text')
  .attr('x', (d, i) => x(d, i) + w/2)
  .attr('y', h/2)
  .attr('dy', '0.33em')
  .attr('text-anchor', 'middle')
  .style('fill', matrixTextColor)
  .text(matrixValue);

function x (d, i) {
  return width/2 - mw/2 + i*w;
}

function y (d, i) {
  return height/2 - mh/2 + i*h;
}

// Return a matrix cell background color based on value
function matrixColor (d) {
  const [r, c, v] = d.atoms();
  return v.in(Zero) ? 'white' : 'steelblue';
}

// Return a matrix cell text color based on value
function matrixTextColor (d) {
  const [r, c, v] = d.atoms();
  return v.in(Zero) ? 'black' : 'white';
}

// Return a matrix cell string value
function matrixValue (d) {
  const [r, c, v] = d.atoms();
  return v.in(Zero) ? '0' : v.toString();
}
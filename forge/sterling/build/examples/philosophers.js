/**
 * Click the execute button at the top of
 * this editor to run the script, and use
 * the left and right arrow buttons at the
 * top-right of the canvas to step through
 * states.
 */

// Table coordinates and dimensions
const cx = width / 2;
const cy = height / 2;
const radius = 250;

// Philosopher rectangle dimensions
const pw = 125;
const ph = 75;

// Empty arrays to be filled with philosophers and forks
const philosophers = [];
const forks = [];

// Starting with the first philosopher, add the philosopher and
// their right fork to the corresponding arrays. Then continue
// to the philosopher's right until we've gone around the table.
let next = Philosopher$0;
do {
    philosophers.push(next);
    forks.push(next.rightFork); // Note use of proxy dot-join
    next = next.rightPhil;      // Note use of proxy dot-join
} while (next !== Philosopher$0);

// Calculate the angle between philosophers.
const dp = -2 * Math.PI / philosophers.length;

// Clear the SVG stage
d3.select(svg).selectAll('*').remove();

// Create the table
d3.select(svg)
    .selectAll('circle.table')
    .data(['table'])
    .join('circle')
    .attr('class', 'table')
    .attr('cx', cx)
    .attr('cy', cy)
    .attr('r', radius)
    .attr('stroke', 'black')
    .attr('fill', 'moccasin')
    .lower();

// Create the philosopher boxes
d3.select(svg)
    .selectAll('rect.philosopher')
    .data(philosophers)
    .join('rect')
    .attr('class', 'philosopher')
    .attr('width', pw)
    .attr('height', ph)
    .attr('x', -pw / 2)
    .attr('y', -ph / 2)
    .attr('transform', philosopherTransform)
    .style('stroke', '#555')
    .style('fill', 'white');

// Create the philosopher texts
d3.select(svg)
    .selectAll('text.philosopher')
    .data(philosophers)
    .join('text')
    .attr('class', 'philosopher')
    .attr('transform', philosopherTransform)
    .attr('text-anchor', 'middle')
    .attr('dy', '0.31em')
    .text(philosopher => philosopher.id());

// Create the forks
d3.select(svg)
    .selectAll('path.fork')
    .data(forks)
    .join('path')
    .attr('class', 'fork')
    .attr('d', forkSVG(0, 10))
    .attr('transform', forkTransform)
    .style('stroke', 'black')
    .style('stroke-width', 2)
    .style('fill', 'none')
    .each(forkTransform);

// Given a philosopher, will return the SVG transform string for that
// philosopher based on where they are sitting.
function philosopherTransform (philosopher) {

    // Note use of .equals() to check for set equality
    const index = philosophers.findIndex(p => p.equals(philosopher));

    return index === -1
        ? null
        : transform(index * dp);

}

// Given a fork, return the SVG transform string for that fork based on
// who it belongs to, whether or not they are holding it, and if they
// are, whether it's in their right or left hand.
function forkTransform (fork) {

    // Note use of proxy dot-join
    const owner = fork.using;

    // Note use of .empty() to check for empty set
    if (owner.empty()) {

        const index = forks.indexOf(fork);
        return transform(index * dp + dp / 2);

    } else {

        // Note use of proxy dot-join and .equals() to check for set equality
        const dx = owner.rightFork.equals(fork) ? 35 : -35;
        return philosopherTransform(owner) + `translate(${dx})`;

    }
}

// Given an angle in radians, return the SVG transform string that will
// position and rotate an object at that angle around the table.
function transform (radians) {
    const c = Math.cos(radians);
    const s = Math.sin(radians);
    const x = cx + radius * c;
    const y = cy + radius * s;
    const ng = (180 * Math.atan2(s, c) / Math.PI) - 90;
    return `translate(${x}, ${y}) rotate(${ng})`;
}

// Return an SVG path data string for a fork.
function forkSVG (x=0, y=0) {
    return `M${x},${-y}v-25q10,-5,10,-20m-10,20v-20m0,20q-10,-5,-10,-20`
}

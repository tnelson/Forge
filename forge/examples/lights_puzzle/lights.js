d3 = require('d3')
d3.selectAll("svg > *").remove();

STATE_HEIGHT = 100

// No guarantee that Light1 is immediately right
//   of Light0, etc. So we need to build an ordering
//   in advance, based on the "right" relation.
// Passing 'true' to atoms() says to include members of sub-signatures
numLights = instances[0].signature('Light').atoms(true).length

idxToLight = {0: 0} // Light0 is first
lightToIdx = {0: 0}
// NOTE: this will break if >9 lights
for(i = 1; i<=numLights;i++) {
    priorIdx = idxToLight[i-1];
    priorObject = instances[0].atom('Light'+priorIdx)
    lightNum = priorObject.right.toString().slice(-1)
    idxToLight[i] = lightNum
    if(lightNum != 0)
        lightToIdx[lightNum] = i
}
// Debugging 
//console.log(lightToIdx)
//console.log(idxToLight)

// select('svg') doesnt work here
// use select(svg)

states = 
     d3.select(svg)
     // <g> is element for group in SVG
     // D3 is based on these "sandwich" calls:
     //   there's no <g> element yet, but there will be once the data is
     //   processed. Bind the data to those <g> elements...
     .selectAll('g') 
     // Here's the data to bind. The map just makes sure we have the array index
     // Can use either old or new style anonymous functions
     .data(instances.map(function(d,i) {
        return {item: d, index: i}    
     }))
     // Now, for every entry in that array...
     .enter()   
     // Add a <g> (finally)
     .append('g')
     // Give it a class, for ease of debugging and possible use in future visualizations
     .classed('state', true)
     // Give it an 'id' attribute that's the state index (used later for labeling)
     .attr('id', d => d.index)        

// Now, for each state <g>, add a <rect> to the group
states
    .append('rect')
    .attr('x', 10)
     .attr('y', function(d) {
         return 20 + STATE_HEIGHT * d.index
     })
    .attr('width', 300)
    .attr('height', STATE_HEIGHT)
    .attr('stroke-width', 2)
    .attr('stroke', 'black')
    .attr('fill', 'transparent');

// Debugging code, used to confirm that the rects were being 
// re-rendered every run, early in writing this script
// Leaving this in for illustrative purposes
// states
//     .append("text")
//     .style("fill", "black")
//      .attr('y', function(d) {
//          return 40 + 220 * d.index
//      })
//      .attr('x', 50)
//      // If we don't wrap in a function, will be same value for ALL states
//      //.text(Math.random());
//      // Instead, provide a different random number for each state label
//      .text(function(d) {
//          return Math.random()});

// For each state <g>, add a state index label
// Recall that 'd' here is a variable bound to a specific data element,
//   in this case, a state
states
    .append("text")
    .style("fill", "black")
     .attr('y', function(d) {
         return 40 + STATE_HEIGHT * d.index
     })
     .attr('x', 50)
     .text(d => "State "+d.index);


// Now, within each state <g>
lightGroups = 
    states
    // ...as before, we want to bind the sub-data (light values WITHIN a state)
    .selectAll('circle')
    .data( function(d) {        
        inst = d.item
        lit = inst.signature('Lit').tuples()
        unlit = inst.signature('Unlit').tuples()
        lights = lit.concat(unlit)
        return lights.map( function (ld) {
            return {item:  ld, 
                    index: lightToIdx[ld.toString().slice(-1)],
                    on: lit.includes(ld), 
                    state: d.index}
        })
    })
    // for each of them
    .enter()
    // Add a new sub-group
    .append('g')
    .classed('light', true)

// Each light contains a circle...
lightGroups
    .append('circle')    
    // useful for debugging in inspector
    .attr('index', d => d.index)
    .attr('state', d => d.state)
    .attr('item',  d => d.item)
    .attr('r', 20)
    .attr('cx', function(d) {
        return 50 + d.index * 50
    })
    .attr('cy', function(d) {
        return 70 + d.state * STATE_HEIGHT
    })
    .attr('stroke', 'gray')
    .attr('fill', function(ld){
        if(ld.on == true) return 'yellow';
        else return 'gray';
    });

// ...and an index label
lightGroups
     .append('text')
     .attr('y', d => 75 + d.state * STATE_HEIGHT)
     .attr('x', d => 47 + d.index * 50)
     .text(d=>d.index);


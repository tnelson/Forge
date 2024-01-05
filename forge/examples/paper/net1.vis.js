/**
 * Visualization for FMSS23 Zave/Nelson Lab 1
 * Reachability in networks
 * 
 * Assumptions:
 *   - there is only one NetworkState atom in the instance
 *   - there is no temporal component (once state per instance)
 *   - Network members can be topologically ordered by links (no cycles!)
 *   - There are no more than 3 NetHdr atoms in the instance
 *   - The `h$0` atom is most prevalent, and the one appearing in 3 of 4 definitions
 * 
 *   -- TODO: rename relations in the model
 */

/*
 * CONSTANTS
 */

const FONT_SIZE = 18;
const LINE_WIDTH = 2;

/** The radius of circles used to represent network members */
const CIRCLE_RADII = 35;
/** the vertical size of elements in the network member grid */
const VERTICAL_DISTANCE = 180;
/** the horizontal size of elements in the network member grid */
const HORIZONTAL_DISTANCE = 150;
/** The top of the grid used to display states */
const TOP_Y = 150;

/************************************************************************
 * First, derive *logical representations* of objects in the instance.
 * For example, we convert the `members` relation of the network state
 * into an array of member IDs.
 ************************************************************************/

// NOTE: adjust the slicing level depending on how long atoms are; Alloy version used -2
//   but Forge version uses -1. Also adjust expectedAtom below. 

// Array of member node IDs
const networkMembersUnordered = Array.from(
    NetworkState
      .join(members)
      .tuples()
      .map((member) => member.atoms()[0].id().slice(0, -1))
  );

// Logical edges for each reachability relation
// TODO: rename!
const r1Edges = Array.from(
  NetworkState.join(reachable1).tuples().map(t => {
    return [t.atoms()[0].id(), t.atoms()[1].id().slice(0, -1), t.atoms()[2].id().slice(0, -1)]
  }))
const r2Edges = Array.from(
  NetworkState.join(reachable2).tuples().map(t => {
    return [t.atoms()[0].id(), t.atoms()[1].id().slice(0, -1), t.atoms()[2].id().slice(0, -1)]
  }))
const r3Edges = Array.from(
  NetworkState.join(reachable3).tuples().map(t => {
    return [t.atoms()[0].id(), t.atoms()[1].id().slice(0, -1), t.atoms()[2].id().slice(0, -1)]
  }))
const r4Edges = Array.from(
  NetworkState.join(reachable4).tuples().map(t => {
    return [t.atoms()[0].id(), t.atoms()[1].id().slice(0, -1), t.atoms()[2].id().slice(0, -1)]
  }))

// Array of arrays, each of which describes a link in the network graph
const networkLinks = Array.from(
  NetworkState
    .join(links)
    .tuples()
    .map((member) => member.atoms()[0])
    .map((link) => {
      let source =
        NetworkState
          .join(outLinks)
          .join(link)
          .tuples()
          .map((tuple) => tuple.atoms()[0].id().slice(0, -1))[0];

      let sink =
        NetworkState
          .join(inLinks)
          .join(link)
          .tuples()
          .map((tuple) => tuple.atoms()[0].id().slice(0, -1))[0];

      return [link.id().slice(0, -1), source, sink];
    })
);

/*************************************************************************
 * Second, do any necessary post-processing of the logical representation
 ************************************************************************/

/**
 * Sort the network members, assuming the links connect them in a linear fashion.
 *   The behavior of this function is not well-defined if the network isn't linear.
 * 
 * The implementation runs a topological sort on the members, using the links as the
 * input partial order. 
 */
function sortMembersAssumeLinear() {
  const result = [] // array to contain ordering of members
  let edges = networkLinks.slice() // copy of links for mutation
  let todo = networkMembersUnordered.filter(m => {
    return edges.every(e => e[2] != m)
  })  

  while(todo.length > 0) {
    
    const m = todo.pop() // get and remove element 0 
    if(result.includes(m)) continue;
    result.push(m) // this element can be next in the ordering
    const nextEdges = edges.filter(e => {
      return e[1] == m  // source is m?
    })
    edges = edges.filter(e => !nextEdges.includes(e)) // keep all _other_ edges    
    todo = todo.concat(nextEdges.map(e => e[2])) 
  }
  if(edges.length > 0) {    
    throw new Error('unable to topo sort graph; cycle(s) detected')    
  }
  return result
}
const networkMembers = sortMembersAssumeLinear()


/*************************************************************************
 * Finally, visualize the data using Sterling's D3 library functions
 ************************************************************************/

const stage = new Stage();

/** Maintain connection between visual objects and their source logical 
 * representation. E.g., here we keep track of member ID <---> circle object */
let visualObjectMap = {};

/** Create a linear grid to contain network-member representations */
const objectGrid = new Grid({
  grid_location: { x: 0, y: TOP_Y },
  grid_dimensions: {
    x_size: networkMembersUnordered.length,
    y_size: 1
  },
  cell_size: {
    x_size: HORIZONTAL_DISTANCE,
    y_size: VERTICAL_DISTANCE
  }
});
objectGrid.hide_grid_lines();
stage.add(objectGrid);

/** For each network member, add a circle to the grid 
 *  (domain model: nodes in the network) */
networkMembers.forEach((member, i) => {
  let circ = new Circle({
    radius: CIRCLE_RADII,
    color: 'white',
    borderWidth: LINE_WIDTH,
    borderColor: 'black',
    labelSize: FONT_SIZE,
    label: member
  });
  visualObjectMap[member] = circ;
  objectGrid.add({x: i, y: 0}, circ);
});

/**  Draw links between network members (domain model: connectivity) */
networkLinks.forEach((l, i) => {
    let newEdge = new Edge({
        obj1: visualObjectMap[l[1]],
        obj2: visualObjectMap[l[2]],
        lineProps: {arrow: true},
        textProps: {
          text: '',//l[0],
          fontSize: FONT_SIZE
        },
        textLocation: "right"
    })
    stage.add(newEdge)
})

/**  Produce edges for selected definition of "reachable" */

// Distinguish edges related to different headers 
// (Remember that, when using .signature(), you must prepend module name if any)
const headerAtoms = instance.signature("NetHdr").atoms(true).map(a => a.id()).sort()
const edgeColors = ['red', 'blue', 'brown']
// TODO: should really sort by DECREASING expected distance
const edgeYRadiiMultipliers = [0.45, 1.0, 1.8]
const headerColors = Object.fromEntries(headerAtoms.map((a, i) => [a, edgeColors[i]]))
const headerYRadiiMultipliers = Object.fromEntries(headerAtoms.map((a, i) => [a, edgeYRadiiMultipliers[i]]))

// Separate out one atom to display above the graph
const expectedAtom = 'hdr0'

function prettyHeader(hstr) { 
    return hstr.slice(0, hstr.indexOf('$'));
}

// Track current set of edge objects, so they can be replaced
let activeReachableEdgeObjects = []

// Reactive updating of which definition is active
let activeBox;
function ab() { return activeBox ?? 1}

function redoReachability(newEdges, boxIdx) {

  // Draw reachability relation (system model) that has been selected
  const newEdgeObjects = newEdges.map((l, i) => {
    return new Edge({
        obj1: visualObjectMap[l[1]],
        obj2: visualObjectMap[l[2]],
        lineProps: {arrow: true, color: headerColors[l[0]], curve: 
          {curveType: 'arc', 
          // Have a small enough baseline radius that short arcs won't need heavy 
          // scaling (which makes it easier for radii multipliers to be consistent)
          xradius:10, 
          yradius:headerYRadiiMultipliers[l[0]] * 10, 
           sweep: l[0] == expectedAtom ? 1 : 0}
        },
        textProps: {
          text: prettyHeader(l[0]),
          fontSize: FONT_SIZE,  
          color: headerColors[l[0]]        
        },
        textLocation: l[0] == expectedAtom ? "left" : "right"
    })
  })
  // remove old edges
  activeReachableEdgeObjects.forEach(eo => stage.remove(eo))
  // add new edges
  activeReachableEdgeObjects = newEdgeObjects    
  stage.addAll(newEdgeObjects)
  activeBox = boxIdx
  stage.render(svg, document);
}


//////////////////////////////////////
// Toggle buttons

function makeToggleBox(relname, coords, idx, edgearr) {
  return new TextBox({
    text: `View ${relname}`,
    coords: coords,
    fontSize: FONT_SIZE,
    fontWeight: () => ab() == idx ? 700 : 400,
    events: [{
        event: 'click', 
        callback: function (ele, ev, d) {             
            redoReachability(edgearr, idx)
        }}]})
}

stage.add(new TextBox({
    text: `Click to toggle definition of reachability shown:`,
    coords: {x:250, y:20},
    fontSize: FONT_SIZE}))

stage.add(makeToggleBox('reachable1', {x: 150, y:45}, 1, r1Edges)) 
stage.add(makeToggleBox('reachable2', {x: 150, y:70}, 2, r2Edges)) 
stage.add(makeToggleBox('reachable3', {x: 350, y:45}, 3, r3Edges)) 
stage.add(makeToggleBox('reachable4', {x: 350, y:70}, 4, r4Edges)) 

/////////////////////////////////////////
// For reference, show atom lists
  const headersGrid = new Grid({
  grid_location: { x: 0, y: TOP_Y+(1.1*VERTICAL_DISTANCE) },
  grid_dimensions: {
    x_size: 3,
    y_size: Math.max(
      headerAtoms.length + 1, 
      networkMembers.length+1,
      networkLinks.length+1)
  },
  cell_size: {
    x_size: HORIZONTAL_DISTANCE,
    y_size: 25 }
});
stage.add(headersGrid);

headersGrid.add({x: 0, y:0}, 
                new TextBox({text: `Packet headers:`}))
headerAtoms.forEach((ha, i) => {
  headersGrid.add({x: 0, y:i+1}, new TextBox({
    text: `${prettyHeader(ha)}`,
    color: headerColors[ha],
    fontSize: FONT_SIZE}))
})

headersGrid.add({x: 1, y:0}, 
                new TextBox({text: `Network members:`}))
networkMembers.forEach((ma, i) => {
  headersGrid.add({x: 1, y:i+1}, new TextBox({
    text: `${ma}`,
    fontSize: FONT_SIZE}))
})

headersGrid.add({x: 2, y:0}, 
                new TextBox({text: `Physical links:`}))
networkLinks.forEach((link, i) => {
  headersGrid.add({x: 2, y:i+1}, new TextBox({
    text: `${link[1]} -> ${link[2]}`,
    fontSize: FONT_SIZE}))
})



headersGrid.hide_grid_lines();

// Final rendering command:
redoReachability(r1Edges) // start with definition 1
stage.render(svg, document);

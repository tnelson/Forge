/**
 * Click the execute button at the top of
 * this editor to run the script, or
 * press Ctrl + Enter.
 *
 * The mesh nodes are interactive, and can
 * be repositioned by dragging with the
 * mouse.
 */

const cx = width / 2;
const cy = height / 2;
const r = 200;

const mesh = buildMesh(Mesh$0);
layoutMesh(mesh);
renderMesh(mesh);

function buildMesh (meshAtom) {
  
  let mesh = {
    nodes: {},
    elements: {},
    edges: []
  };
  
  triangles.tuples()
    .filter(tuple => tuple.atoms()[0] === Mesh$0)
    .map(tuple => tuple.atoms()[1])
    .forEach(elemAtom => {
    
    let element = {
      id: elemAtom.id(),
      nodes: {},
      edges: []
    };
    
    edges.tuples()
      .filter(tuple => tuple.atoms()[0] === elemAtom)
      .forEach(tuple => {
      
        let n1 = tuple.atoms()[1].id();
        let n2 = tuple.atoms()[2].id();
        let node1 = mesh.nodes[n1];
        let node2 = mesh.nodes[n2];
      
        if (!node1) {
          node1 = { id: n1 };
          mesh.nodes[n1] = node1;
        }
        element.nodes[n1] = node1;
        
        if (!node2) {
          node2 = { id: n2 };
          mesh.nodes[n2] = node2;
        }
        element.nodes[n2] = node2;
      
        element.edges.push([node1, node2]);
        mesh.edges.push([node1, node2]);
        
      });
    
    mesh.elements[elemAtom.id()] = element;
    
  });
  
  return mesh;
  
}

function layoutMesh (mesh) {
  
  // Initialize node positions
  let nodeMap = {};
  for (let id in mesh.nodes) {
    let node = mesh.nodes[id];
    node.x = width/2;
    node.y = height/2;
    node.fixed = false;
    nodeMap[node.id] = node;
  }

  // Create vertex neighborhoods
  let neighborhood = {};
  let edges = [];
  for (let id in mesh.elements) {

    let element = mesh.elements[id];
    element.edges.forEach(edge => {
      let n1 = edge[0];
      let n2 = edge[1];

      if (!(n1.id in neighborhood)) neighborhood[n1.id] = new Set();
      if (!(n2.id in neighborhood)) neighborhood[n2.id] = new Set();
      neighborhood[n1.id].add(n2);
      neighborhood[n2.id].add(n1);
      edges.push(edge);

    });
  }

  for (let id in neighborhood) {
    neighborhood[id] = Array.from(neighborhood[id]);
  }

  // Determine half-edges
  let halves = new Set();
  let edge;

  while (edge = edges.pop()) {
    let e = edge.map(n => n.id).join('->');
    let r = edge.reverse().map(n => n.id).join('->');
    if (halves.has(r)) {
      halves.delete(r);
    } else {
      halves.add(e);
    }
  }

  halves = Array.from(halves).map(e => e.split('->'));

  // Create a map that contains the outer ring of the mesh
  let ring = {};
  halves.forEach(h => ring[h[0]] = h[1]);

  // Get the set of vertices that are on the outer boundary
  let ringVerts = new Set();
  halves.forEach(h => {
    ringVerts.add(h[0]);
    ringVerts.add(h[1]);
  });
  ringVerts = Array.from(ringVerts);

  // Determine the angle that will separate vertices and place ring nodes
  let angle = 360 / ringVerts.length;
  let start = ringVerts[0];
  let v = start;
  let ng = 0;

  nodeMap[start].x = cx + r * Math.cos(ng * Math.PI / 180);
  nodeMap[start].y = cy + r * Math.sin(ng * Math.PI / 180);
  nodeMap[start].fixed = true;

  while ((v = ring[v]) !== start) {
    ng += angle;
    nodeMap[v].x = cx + r * Math.cos(ng * Math.PI / 180);
    nodeMap[v].y = cy + r * Math.sin(ng * Math.PI / 180);
    nodeMap[v].fixed = true;
  }

  // Iteratively place the rest of the nodes using the averaging method
  let biggest = Infinity;
  let tolerance = 1;

  while (biggest > tolerance) {
    let biggestIt = 0;
    for (let id in mesh.nodes) {
      let node = mesh.nodes[id];
      if (!node.fixed) {
        let neighbors = neighborhood[node.id];
        let x = neighbors.reduce((acc, n) => acc + n.x, 0) / neighbors.length;
        let y = neighbors.reduce((acc, n) => acc + n.y, 0) / neighbors.length;
        let dist = Math.sqrt((node.x - x) ** 2 + (node.y - y) ** 2);
        node.x = x;
        node.y = y;
        if (dist > biggestIt) biggestIt = dist;
      }
    }
    biggest = biggestIt;
  }
  
}

function renderMesh (mesh) {
  
  const stage = d3.select(svg);
  stage.selectAll('*').remove();
  
  stage.call(d3.drag()
            .container(svg)
            .subject(dragsubject)
            .on('drag', dragged));
  
  function dragged (event) {
    event.subject.x = event.x;
    event.subject.y = event.y;
    positionNodes();
    positionElements();
  }
  
  function dragsubject (event) {
    return findNode(event.x, event.y);
  }
  
  function findNode (x, y, radius) {
    
    let data = stage.selectAll('circle').data(),
        n = data.length,
        i,
        dx,
        dy,
        d2,
        node,
        closest;
    
    if (radius === undefined) radius = Infinity;
    else radius *= radius;
    
    for (i=0; i<n; ++i) {
      node = data[i];
      dx = x - node.x;
      dy = y - node.y;
      d2 = dx * dx + dy * dy;
      if (d2 < radius) closest = node, radius = d2;
    }
    
    return closest;
    
  }
  
  function positionNodes () {
    stage.selectAll('circle.node')
      .attr('cx', node => node.x)
      .attr('cy', node => node.y);
    stage.selectAll('text.node')
      .attr('x', node => node.x)
      .attr('y', node => node.y);
  }
  
  function positionElements () {
    stage.selectAll('path')
      .attr('d', elementPath);
    stage.selectAll('text.element')
      .attr('x', element => centroidX(Object.values(element.nodes)) || 0)
      .attr('y', element => centroidY(Object.values(element.nodes)) || 0)
  }

  const links = stage.append('g').attr('id', 'links');
  const nodes = stage.append('g').attr('id', 'nodes');
  
  nodes.selectAll('circle')
    .data(Object.values(mesh.nodes))
    .join('circle')
    .attr('class', 'node')
    .attr('r', 15)
    .style('cursor', 'pointer')
    .style('stroke', 'black')
    .style('stroke-width', 1)
    .style('fill', 'white');
  
  nodes.selectAll('text')
    .data(Object.values(mesh.nodes))
    .join('text')
    .attr('class', 'node')
    .attr('dy', '0.33em')
    .style('cursor', 'pointer')
    .style('font-weight', 'bold')
    .style('text-anchor', 'middle')
    .style('user-select', 'none')
    .text(node => node.id.split('$').pop());
  
  const elements = links.selectAll('g')
    .data(Object.values(mesh.elements))
    .join('g');
  
  elements.selectAll('path')
    .data(d => [d])
    .join('path')
    .style('stroke', 'black')
    .style('fill', '#15B371');
  
  links.selectAll('text')
    .data(Object.values(mesh.elements))
    .join('text')
    .attr('class', 'element')
    .attr('dy', '0.33em')
    .style('text-anchor', 'middle')
    .style('user-select', 'none')
    .style('fill', 'black')
    .style('font-weight', 'bold')
    .text(element => element.id.split('$').pop());
  
  positionNodes();
  positionElements();
  
}

function elementPath (element) {
  const edges = element.edges;
  const p0 = edges[0][0];
  const p1 = edges[1][0];
  const p2 = edges[2][0];
  return `M ${p0.x} ${p0.y} L ${p1.x} ${p1.y} L ${p2.x} ${p2.y} Z`;
}

function centroidX (nodes) {
  return nodes.reduce((s, n) => s + n.x, 0) / nodes.length;
}

function centroidY (nodes) {
  return nodes.reduce((s, n) => s + n.y, 0) / nodes.length;
}
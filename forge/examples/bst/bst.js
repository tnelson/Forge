/*
  Script for bst.frg. Expectations:
    - sig Node { left: lone Node, right: lone Node }
    - binary_tree pred is satisfied (e.g., unique root exists)
    - no empty trees 
*/

const RADIUS = 16;
const LEAF_RADIUS = 2;

function makeLeaf() {
    return {
        visualObject: new Circle({radius:LEAF_RADIUS, color: 'black', borderColor: 'black'}), 
        children: [] }
}

function firstAtomOf(expr) {
  if(!expr.empty())
    return expr.tuples()[0].atoms()[0].id()
  return 'none'
}

// Construct a nested JS object that describes the tree structure.
function buildNested(root) {
  let obj = new Circle({radius: RADIUS, color: 'white', borderColor: 'black', 
                        label: firstAtomOf(root.key)});
  let dataTree = { visualObject: obj, children: []}
  dataTree.children[0] = root.left.empty() ? 
    makeLeaf() : buildNested(root.left)
  dataTree.children[1] = root.right.empty() ? 
    makeLeaf() : buildNested(root.right)
  return dataTree
}

// Which node is the root of the tree?
function findRoot() {
  const noParents = Node.atoms().filter(a => left.join(a).empty() &&
                                             right.join(a).empty())
  if(noParents.length != 1) 
    throw Error('Instance had more than one root node: '+noParents)
  return noParents[0]
}

// A Tree takes a nested JS object that describes the tree structure.
const tree = new Tree({
    root: buildNested(findRoot()), 
    height: 500, 
    width: 500, 
    coords: { x: 20, y: 50 }
    });

const stage = new Stage() 
stage.add(tree)
stage.render(svg)




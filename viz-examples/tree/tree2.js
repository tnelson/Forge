/*
  Tree visualization example for instances of the `tree.frg` model. 
  For a basic version that doesn't rely on Forge instances, see  
  the `tree.js` file instead.
*/

// Step 1: find the root of the binary tree. This will be the `Tree`
// atom without any parent, which we can find by reversing the `left`
// and `right` fields.
const roots = instance.signature('Tree').tuples().filter(t => {
    return (left.join(t).tuples().length < 1) &&
           (right.join(t).tuples().length < 1)
})
const root = roots[0]

// Note: if you are trying to do something like the above, and get an 
// error message like "[dot join] tried to join tree with filter, 
// but no set filter defined", check that you haven't forgotten to
// call .tuples() on the sig. The sig itself isn't an array.

// Step 2: construct the tree structure recursively 
const visTree = buildTree(root)

function buildLeaf() {
    // Each leaf should be a distinct primitive object
    return {
        visualObject: new Circle({radius: 5, color: 'black'}),
        children: []
    }
}

function buildTree(t) {    
    const myValue = t.join(val)
    const myLeftTuples = t.join(left).tuples()
    const myRightTuples = t.join(right).tuples()
    const obj = new Circle({
        radius: 20, 
        borderColor: "black", 
        color: "white", 
        // For debugging:
        //label: `${myValue};${t}`});    
        // For final visualization:
        label: `${myValue}`});
        
    // If this node has left/right children, build their subtree structure
    const leftSubtree = myLeftTuples.length > 0 ? 
                        [buildTree(myLeftTuples[0])] : 
                        [buildLeaf()]
    const rightSubtree = myRightTuples.length > 0 ? 
                        [buildTree(myRightTuples[0])] : 
                        [buildLeaf()]
    return {
        visualObject: obj,
        // Using ... will collapse to empty array if there are no children
        children: [...leftSubtree, ...rightSubtree]
    }
}

// Note: if you are trying to do something like the above helper, and 
// get a TypeError about the function parameter being undefined, be 
// aware that JavaScript may be reporting that the value `undefined`
// was passed in, and is being used unsafely---not that somehow the
// parameter name isn't recognized!



// Create a compound tree object with the above structure
let tree = new Tree({
    root: visTree, 
    height: 200, 
    width: 300, 
    coords: { x: 100, y: 100 }
    });

// Finally, add to the stage and render it
stage = new Stage()
stage.add(tree)
stage.render(svg)
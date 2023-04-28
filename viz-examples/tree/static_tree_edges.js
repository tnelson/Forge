/*
  Basic tree visualization example that doesn't interpret any 
  specific Forge instance. This version also includes a cross-edge
  between leaf nodes using the Edge class.
*/

// Define primitive objects to go in the tree
let obj1 = new Circle({radius: 10, color: 'red', borderColor: "black", label: '1'});
let obj2 = new Circle({radius: 10, color: 'red', borderColor: "black", label: '2'});
let obj3 = new Rectangle({height: 20, width: 20, color: 'green', borderColor: "black", label: '3'});
let obj4 = new Circle({radius: 10, color: 'red', borderColor: "black", label: '4'});
let obj5 = new Circle({radius: 10, color: 'red', borderColor: "black", label: '5'});
let obj6 = new Circle({radius: 10, color: 'red', borderColor: "black", label: '6'});
let obj7 = new Circle({radius: 10, color: 'blue', borderColor: "black", label: '7'});
let obj8 = new Circle({radius: 10, color: 'blue', borderColor: "black", label: '8'});

// Define the recursive tree structure, using the above primitive
// objects as leaves:
let visTree = {
  visualObject: obj1,
  children: [
    {
      visualObject: obj2,
      children: [
        { visualObject: obj4, children: [] },
        {
          visualObject: obj5,
          children: [{ visualObject: obj8, children: [] }]
        },
        { visualObject: obj7, children: [] }
      ]
    },
    {
      visualObject: obj3,
      children: [{ visualObject: obj6, children: [] }]
    }
  ]
};

// Create a compound tree object with the above structure
let tree = new Tree({
    root: visTree, 
    height: 200, 
    width: 200, 
    coords: { x: 100, y: 100 }
    });

// Add an extra self-managed edge:
const e = new Edge({
    obj1: obj6, obj2: obj8,
    textProps: {text: "foo", fontSize: 11},
    textLocation: "left"})


// Finally, add to the stage and render it
stage = new Stage()
stage.add(tree)
stage.add(e)
stage.render(svg)

var sigs = {};

json.sig_names.forEach(function(name){
	sigs[name] = json.relations[name].map(function(singleton){
		return singleton[0];
	});
});

var parent = function(node_name){
	return Object.keys(sigs).filter(function(sig_name){
		return sigs[sig_name].includes(node_name);
	})[0];
};

var nodes = json.nodes.map(function(name){
	return {
		data: {
			id: name,
			label: name,
			color: "#666",
			parent: parent(name)
		}
	};
}).concat(json.sig_names.map(function(name){
	return {
		data: {
			id: name,
			// label: name
		}
	};
}));

var edges = Object.entries(json.relations).reduce(function(acc, val){
	relation_name = val[0];

	return acc.concat(val[1].map(function(tuple){
		console.assert(tuple.length >= 1);

		var start = tuple[0];
		var end = tuple[tuple.length - 1];

		var label = relation_name + "\n\n\u2060";
		if (tuple.length > 2){
			var between = tuple.slice(1, tuple.length - 1);
			label = relation_name + "[" + between.join("->") + "]\n\n.";
		}

		return {
			data: {
				id: tuple.join(),
				source: start,
				target: end,
				label: label,
				color: "#ccc"
			}
		}
	}));
}, []);

//graph_els =
// What's the question I want answered? I want to know how compound nodes are laid out.
// specifically: does it lay out the sub nodes first, and then making the bounding box for the compound node
// based on the locations of the sub nodes?

// Or maybe it depends on which elements I call the layout on? In which case, do I call the layout on the subnodes,
// and then on the compound nodes? Let's find out.
// On this site? No, lemme make a test site.


// ok, what are my problems now??
// nodes should be arranged like a grid inside each compound box.
// also, compound boxes should be placed in a consistent way across instances.
// also, there are just too many lines on screen, with no way to tell them apart.

// Well, I can't think of any good way of placing the compound boxes, besides in a circle.
// cuz I can't do optimization across every instance. and grids are stupid, as are concentric circles.


// --------------------------------------------------------------------------------------------------
// ok, put them in a circle, arrange the nodes in a grid inside each compound box. What else?
// make the lines consistently color-coded to the relation (relations won't change across instances)
// make line highlighting better: other nodes/edges should fade out a little, highlighted line/node should grow
// by default, maybe represent binary relations as attributes.
// ALSO: when a sig doesn't connect to anything, put it by the side, NOT in the big circle.
// --------------------------------------------------------------------------------------------------

// Alrght, so the actual layout part is clearly the hardest. How do I do it?
// I don't think I can use any existing tools? Well, actually, maybe I can lay things out in a circle,
// then reparent and expand the circle as necessary? Yeah, that may actually be the easiest way.

// Or I could just do the trig mnyself... Can just use the preset layout and a function that maps
// to a circle correctly. I think that makes more sense than adjusting this other shit weirdly.

// would the transform function be at all useful? No, I don't think so, not really.

// OK, let's try cise, and if that doesn't look nice, and it probably won't, let's give up and roll our own.
// alright, yeah, let's try cise.

var cy = cytoscape({

	container: document.getElementById('cy'), // container to render in

	elements: nodes.concat(edges),

	style: [ // the stylesheet for the graph
		{
			selector: 'node',
			style: {
				'background-color': 'yellow',
				'label': 'data(id)',
				'font-size': 12,
				'color': 'data(color)',
				'shape': 'barrel',
				'width': 100,
				'border-color': 'black',
				'border-width': 2,
				'text-valign': 'center',
				'text-halign': 'center'
			}
		},

		{
			selector: 'edge',
			style: {
			 	'width': 3,
				'line-color': "data(color)",
				'target-arrow-color': 'data(color)',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				'label': 'data(label)',
				'text-wrap': 'wrap',
				'control-point-weight': 0.8,
				'control-point-step-size': 50,
				'font-size': 12,
				'color': 'data(color)',
				"edge-text-rotation": "autorotate"
			}
		}
	],

	userZoomingEnabled: false
});

var layout_options = {
	name: "cise",
	animate: false,
	clusters: Object.values(sigs),
	// idealInterClusterEdgeLengthCoefficient: 2,
	// springCoeff: 0.9,
	nodeRepulsion: 100,
	// gravity: 0.2
	// gravityRange: 3
	// klay: {
	// 	crossingMinimization: 'INTERACTIVE'
	// }
};

cy.nodes().children().layout(layout_options).run()
// cy.nodes().layout(layout_options).run()

// var cy = cytoscape({
//
// 	container: document.getElementById('cy'), // container to render in
//
// 	layout: {
// 		name: "klay"
// 	},
//
// 	userZoomingEnabled: false
// });

function toggle_zoom(ele) {
	if (cy.userZoomingEnabled()){
		cy.userZoomingEnabled(false);
		ele.innerHTML = "Zoom DISABLED. Click to toggle."
	} else {
		cy.userZoomingEnabled(true);
		ele.innerHTML = "Zoom ENABLED. Click to toggle."
	}
}

cy.edges().on("mouseover", function(evt){
	evt.target.data("color", "green");
});

cy.edges().on("mouseout", function(evt){
	evt.target.data("color", "#ccc");
});

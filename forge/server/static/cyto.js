var sigs = {};

// json = {
// 	nodes: ["state0", "state1", "state2", "state3", "state4", "state5"],
// 	relations: {
// 		next: [["state0", "state2"],
// 				["state1", "state3"],
// 				["state2", "state5"],
// 				["state3", "state0"],
// 				["state4", "state1"]],
// 		state: [["state0"], ["state1"], ["state2"], ["state3"], ["state4"], ["state5"]]
// 	},
// 	sig_names: ["state"]
// }

json.sig_names.forEach(function(name){
	sigs[name] = json.relations[name].map(function(singleton){
		return singleton[0];
	});
});

var relation_colors = [
	"#003366",
	"#3366cc",
	"#33ccff",
	"#0000ff",
	"#6600cc",
	"#cc00cc",
	"#ff66ff",
	"#ff0000",
	"#ff9900",
	"#993333",
	"#996600",
	"#663300",
	"#ffcc00",
	"#999966",
	"#669900",
	"#00ff00",
	"#00ff99",
	"#669999",
];

var compound_colors = [
	"#66ff99",
	"#ffff99",
	"#ccff99",
	"#ffcc99",
	"#ff9999",
	"#ff99cc",
	"#ffccff",
	"#cc99ff",
	"#99ccff",
	"#66ccff",
	"#66ffff",
	"#99ffcc",
];

var parent = function(node_name){
	return Object.keys(sigs).filter(function(sig_name){
		return sigs[sig_name].includes(node_name);
	})[0];
};

var compound_color_index = 0;

var nodes = json.nodes.map(function(name){
	return {
		data: {
			id: name,
			label: name,
			color: "#e6e6e6",
			parent: parent(name),
			padding: 10,
			z_compound_depth: "auto",
		}
	};
}).concat(json.sig_names.map(function(name){
	compound_color_index += 1;
	return {
		data: {
			id: name,
			label: "",
			color: compound_colors[compound_color_index % compound_colors.length],
			padding: 10,
			z_compound_depth: "auto",
		}
	};
}));

var relation_color_index = 0;

var edges = Object.entries(json.relations).reduce(function(acc, val){
	relation_name = val[0];

	if (val[1].length > 0 && val[1][0].length >= 2) {
		relation_color_index += 1;
	}

	return acc.concat(val[1].map(function(tuple){
		console.assert(tuple.length >= 1);

		var start = tuple[0];
		var end = tuple[tuple.length - 1];

		var label = relation_name + "\n\n\u2060";
		if (tuple.length > 2){
			var between = tuple.slice(1, tuple.length - 1);
			label = relation_name + "[" + between.join("->") + "]\n\n\u2060";
		}

		return {
			// I think these should be assigned classes based on the relation they came form.
			classes: [relation_name],

			data: {
				id: tuple.join(),
				source: start,
				target: end,
				label: label,
				active_color: relation_colors[relation_color_index % relation_colors.length],
				base_color: relation_colors[relation_color_index % relation_colors.length],
				width: 3,
				font_size: 16,
				color: "black",
				z_compound_depth: "auto",
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
// 	 arrange the nodes in a grid inside each compound box. (high priority) (turns out its a bad idea)

// make the lines consistently color-coded to the relation (relations won't change across instances) (high priority)
// make line highlighting better: other nodes/edges should fade out a little,
// highlighted line/node should grow (high priority)

// by default, maybe represent binary relations as attributes. (NOPE)

// ALSO: when a sig doesn't connect to anything, put it by the side, NOT in the big circle. (low priority)

// if every atom in A connects to B0, just have an edge from the A compound box
// to atom B0. (tricky, but high ish priority)
// --------------------------------------------------------------------------------------------------

// Alrght, so the actual layout part is clearly the hardest. How do I do it?
// I don't think I can use any existing tools? Well, actually, maybe I can lay things out in a circle,
// then reparent and expand the circle as necessary? Yeah, that may actually be the easiest way.

// Or I could just do the trig mnyself... Can just use the preset layout and a function that maps
// to a circle correctly. I think that makes more sense than adjusting this other shit weirdly.

// would the transform function be at all useful? No, I don't think so, not really.

// OK, let's try cise, and if that doesn't look nice, and it probably won't, let's give up and roll our own.
// alright, yeah, let's try cise.


// cose bilkent and fcose]
// ahh fuck, fcose relies on numericjs which is clearly in EOL.
// and cose bilkent?
// more ideas: if every atom in A connects to B0, just have an edge from the A compound box
// to atom B0.
// Maybe use branching edges? nah, can't separate two edges between the same nodes.

// make labels only appear when moused over.

// so, while cose-bilkent sorta works, none of its options do anything, so it's basically useless.

// How about cola? NOPE, the options are a lil too complicated to use effectively.

// Cose can't be used on children alone, since it throws an errror when it realizes that
// the parents aren't included. and it sucks when used on the whole thing. Yep, confirmed on cytotest.

var cy = cytoscape({

	container: document.getElementById('cy'), // container to render in

	elements: nodes.concat(edges),
	// elements: nodes,

	style: [ // the stylesheet for the graph
		{
			selector: 'node',
			style: {
				'background-color': 'data(color)',
				'label': 'data(label)',
				'font-size': 20,
				'width': 'label',
				'padding': 'data(padding)',
				'text-valign': 'center',
				'text-halign': 'center',
				'border-width': 1,
				'border-color': 'black',
				'z-compound-depth': 'data(z_compound_depth)',
				'min-zoomed-font-size': 10,
			}
		},

		{
			selector: 'edge',
			style: {
			 	'width': "data(width)",
				'line-color': "data(active_color)",
				'target-arrow-color': 'data(active_color)',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				'label': 'data(label)',
				'text-wrap': 'wrap',
				// 'source-endpoint': 'inside-to-node',
				'control-point-weight': 0.6,
				'control-point-step-size': 100,
				'font-size': "data(font_size)",
				'color': 'data(color)',
				"edge-text-rotation": "autorotate",
				// 'text-margin-x': 60,
				"z-compound-depth": 'data(z_compound_depth)',
				'min-zoomed-font-size': 10,
			}
		}
	],

	userZoomingEnabled: false
});

var layout_options = {
	// it seems that cose is not determinstic. That's bad.
	// name: "cose",
	// nodeRepulsion: 1000,
	// animate: 'end'


	// below options are for cise, which IS deterministic.
	name: "cise",
	animate: false,
	clusters: Object.values(sigs),
	nodeRepulsion: 3000,
	nodeSeparation: 100

};

var breadlayout = {
	name: "breadthfirst",
	directed: true,
	// grid: true
	circle: true
	// avoidOverlap: false
}

var layout2 = {
	name: "avsdf"
}

cy.layout(layout_options).run();
// cy.layout(layout2).run();
cy.resize();
cy.fit();

// layout_options = {
// 	name: "breadthfirst"
// }

// ah, doesn't work , because they make reference to non-existent parent nodes.
// cy.nodes().children().layout(layout_options).run();

// cy.nodes().children().layout(layout_options).run()
//
// cy.resize();
// cy.fit();


// var bigParents = cy.filter(function(ele, i, eles){
// 	return ele.children().size() >= 3;
// });

// bigParents.forEach(function(ele){
	// var pos = {};
	// Object.assign(pos, ele.position());
	// ele.children().layout({name: "grid", fit: false, condense: true}).run();

	// Object.assign(ele.position(), pos);
// });
// what do I need? every node with 3 or more children.



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
	cy.edges().data("active_color", "#cccccc")
	cy.edges().data("color", "#cccccc")
	evt.target.data("active_color", "black");
	evt.target.data("color", "black");
	evt.target.data("width", 6);
	evt.target.data("font_size", 20);
	evt.target.data("z_compound_depth", "top");
	cy.$id(evt.target.data("source")).data("color", "#a6a6a6");
	cy.$id(evt.target.data("source")).data("z_compound_depth", "top");
	cy.$id(evt.target.data("target")).data("color", "#a6a6a6");

	// Changing the padding fucks with the edge labels. So don't do that for now.
	// cy.$id(evt.target.data("target")).data("padding", 30);
	// cy.$id(evt.target.data("source")).data("padding", 30);
});

cy.edges().on("mouseout", function(evt){
	cy.edges().forEach(function(ele, i, eles){
		ele.data("active_color", ele.data("base_color"));
	});
	cy.edges().data("color", "black");
	evt.target.data("width", 3);
	evt.target.data("font_size", 16);
	evt.target.data("z_compound_depth", "auto");
	cy.$id(evt.target.data("source")).data("color", "#e6e6e6");
	cy.$id(evt.target.data("source")).data("z_compound_depth", "auto");
	cy.$id(evt.target.data("target")).data("color", "#e6e6e6");

	// cy.$id(evt.target.data("target")).data("padding", 10);
	// cy.$id(evt.target.data("source")).data("padding", 10);
});


/// YOOOOO this works! but it puts things in actual circles
// if there are no edges, which is pretty fucking dumb.
//

// var internalEdges = cy.collection();
//
// cy.nodes().parents().forEach(function(ele, i, eles){
// 	internalEdges = internalEdges.add(ele.children().edgesWith(ele.children()));
// });


// var adjustLayouts = function(){

// var savedEdges = cy.edges().remove();
// internalEdges.restore();
//
// var state;
// var x;
// var y;
// var thisInternalEdges;
//
// state = cy.$id("state");
// x = state.position().x;
// y = state.position().y;
// thisInternalEdges = state.children().edgesWith(state.children());
// state.children().add(thisInternalEdges).layout(breadlayout).run();
// state.position({x: x, y: y});
//
// state = cy.$id("goat");
// x = state.position().x;
// y = state.position().y;
// thisInternalEdges = state.children().edgesWith(state.children());
// state.children().add(thisInternalEdges).layout(breadlayout).run();
// state.position({x: x, y: y});

// ok so for state, it works fine, for others it doesn't. WHY?



// cy.nodes().parents().forEach(function(ele, i, eles){
// 	console.log(ele); // these are all right. What's happening on that line??
// 	var x;
// 	var y;
// 	var id = ele.id();
// 	x = ele.position().x;
// 	y = ele.position().y;
// 	var thisInternalEdges = ele.children().edgesWith(ele.children());
// 	// ele.children().add(thisInternalEdges).layout(breadlayout).run();
// 	cy.$id(id).position({x: x, y: y});
// });

// savedEdges.restore();
// };



// var myeles = cy.$("#state").children().union(cy.edges().filter(function(ele, i, eles){
// 	return ele.connectedNodes().ancestors().same(cy.$("#state"));
// }));
//
// cy.$()


// How would I handle the sig condensation algorithm?

// OK, take a look at each node. Then check each sig, for that node. Does this node touch every atom in the sig?
// If so, make the replacement. Do this for every atom. Do the same thing for incoming edges.
// Hmm, wait, what about higher order relations? Yeah, this algorithm would actually be tricky.

// OK, what sorts of basic theming do I want to support?
